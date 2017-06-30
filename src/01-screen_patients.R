# patient screening

# run MBO query:
#   * Patients - by Medication (Generic)
#       - Facility (Curr): HH HERMANN;HH Trans Care;HH Rehab;HH Clinics
#       - Date Only - Admit (Start): 7/1/2012 12:00:00 AM # in 12 month increments
#       - Date Only - Admit (End): 1/1/2017 12:00:00 AM
#       - Medication (Generic): WARFARIN, warfarin, apixaban, rivaroxaban,
#       dabigatran, edoxaban

library(tidyverse)
library(lubridate)
library(stringr)
library(edwr)
library(icd)

# inclusion criteria -----------------------------------

screen_pts <- read_data("data/raw", "patients", FALSE) %>%
    as.patients() %>%
    filter(age >= 18,
           discharge.datetime <= mdy("12/31/2016", tz = "US/Central"),
           !(visit.type %in% c("Emergency", "Day Surgery", "Bedded Outpatient")))

mbo_doac <- concat_encounters(screen_pts$millennium.id)

# run EDW query:
#   * Identifiers

screen_id <- read_data("data/raw",  "identifiers") %>%
    as.id()

first_visit <- screen_pts %>%
    left_join(screen_id, by = "millennium.id") %>%
    group_by(person.id) %>%
    arrange(discharge.datetime, .by_group = TRUE) %>%
    distinct(person.id, .keep_all = TRUE)

visit_mbo <- concat_encounters(first_visit$millennium.id)
visit_pie <- concat_encounters(first_visit$pie.id)

# run EDW query:
#   * Lookup - Zipcodes - Encounter
#   * Lookup - Zipcodes - Current

# use zip code from the encounter; if missing, use current zip code
mhhs_zipcodes <- read_csv("data/external/mhhs_region_zipcodes.csv") %>%
    rename(zipcode = `USPS Zip Code`) %>%
    mutate_at("zipcode", as.character)

screen_zipcodes <- read_data("data/raw", "zipcodes") %>%
    distinct() %>%
    rename(pie.id = `PowerInsight Encounter Id`,
           zipcode = `Person Address- Zip Code`) %>%
    mutate(zip5 = str_extract(zipcode, "....."))

screen_zip_curr <- read_data("data/raw", "zip-curr") %>%
    distinct() %>%
    rename(pie.id = `PowerInsight Encounter Id`,
           zip_curr = `Person Address- Zip Code-Curr`) %>%
    mutate(zip5_curr = str_extract(zip_curr, "....."))

zips <- full_join(screen_zipcodes, screen_zip_curr, by = "pie.id") %>%
    mutate(zip_pt = coalesce(zip5, zip5_curr))

local_pts <- zips %>%
    filter(zip5 %in% mhhs_zipcodes$zipcode) %>%
    distinct(pie.id)

local_pie <- concat_encounters(local_pts$pie.id)

# run EDW query:
#   * Medications - Home and Discharge - All

meds_home <- read_data("data/raw", "meds-home") %>%
    as.meds_home()

# remove pts on oac at home
oac <- c("warfarin", "apixaban", "rivaroxaban", "dabigatran", "edoxaban")
home_oac <- meds_home %>%
    filter(med %in% oac,
           med.type == "Recorded / Home Meds") %>%
    distinct(pie.id)

new_oac <- anti_join(local_pts, home_oac, by = "pie.id")

# find pts with d/c Rx for oac
dc_oac <- meds_home %>%
    semi_join(new_oac, by = "pie.id") %>%
    filter(med %in% oac,
           med.type == "Prescription / Discharge Order")

# remove pts with > 1 oac on d/c
dc_pts <- dc_oac %>%
    distinct(pie.id, med) %>%
    count(pie.id) %>%
    filter(n == 1)

oac_pie <- concat_encounters(dc_pts$pie.id)

# run EDW query:
#   * Diagnosis (ICD-9/10-CM) - All

# ICD-9-CM
# Afib/flutter: 427.31, 427.32
# DVT, acute: 453.82, 453.83, 453.84, 453.85, 453.86, 453.87, 453.89, 453.9
# PE, acute: 415.12, 415.13, 415.19

indications_9 <- c("427.31", "427.32", "453.82", "453.83", "453.84", "453.85",
                 "453.86", "453.87", "453.89", "453.9", "415.12", "415.13",
                 "415.19")

cols <- fwf_empty("data/external/2016_I9gem.txt", col_names = c("icd9", "icd10", "other"))
icd10_gem <- read_fwf("data/external/2016_I9gem.txt", cols) %>%
    filter(icd10 != "NoDx")

icd9_nod <- str_replace_all(indications_9, "\\.", "")

icd10 <- filter(icd10_gem, icd9 %in% icd9_nod)

indications <- c(icd9_nod, icd10$icd10)

diagnosis <- read_data("data/raw", "diagnosis") %>%
    as.diagnosis() %>%
    tidy_data()

screen_icd <- diagnosis %>%
    mutate(icd = str_replace_all(diag.code, "\\.", "")) %>%
    filter(icd %in% indications,
           diag.type == "Final")

include_pts <- distinct(screen_icd, pie.id) %>%
    left_join(screen_id, by = "pie.id")

include_edw <- concat_encounters(include_pts$pie.id)
include_mbo <- concat_encounters(include_pts$millennium.id)

# exclusion criteria -----------------------------------

# run MBO queries:
#   * Demographics
#   * Measures
#   * Warfarin Information

# run EDW queries:
#   * Labs - Pregnancy
#   * Labs - Renal

excl_preg_icd <- diagnosis %>%
    semi_join(include_pts, by = "pie.id") %>%
    check_pregnant()

excl_preg_labs <- read_data("data/raw", "labs-preg") %>%
    as.labs() %>%
    check_pregnant()

excl_preg <- excl_preg_icd %>%
    bind_rows(excl_preg_labs) %>%
    distinct()

include_pts <- anti_join(include_pts, excl_preg, by = "pie.id")

# Hepatitis, Cirrhosis
# 571.5
# ccs 6

ccs <- read_csv("data/external/icd9-ccs_codes.csv")

icd9_hepatitis <- filter(ccs, CCS.CATEGORY == 6)
icd9_liver <- c("5715", icd9_hepatitis$ICD.9.CM.CODE)
icd_liver <- filter(icd10_gem, icd9 %in% icd9_liver)
indications_liver <- c(icd_liver$icd9, icd_liver$icd10)

excl_liver <- diagnosis %>%
    semi_join(include_pts, by = "pie.id") %>%
    mutate(icd = str_replace_all(diag.code, "\\.", "")) %>%
    filter(icd %in% indications_liver,
           diag.type == "Final") %>%
    distinct(pie.id)

include_pts <- anti_join(include_pts, excl_liver, by = "pie.id")

# CrCl < 30; HD
# icd9 HD: 585.5, 585.6, V45.11

icd9_hd <- c("5855", "5856", "V4511")
icd_hd <- filter(icd10_gem, icd9 %in% icd9_hd)
indications_hd <- c(icd_hd$icd9, icd_hd$icd10)

excl_hd <- diagnosis %>%
    semi_join(include_pts, by = "pie.id") %>%
    mutate(icd = str_replace_all(diag.code, "\\.", "")) %>%
    filter(icd %in% indications_hd,
           diag.type == "Final")

labs_renal <- read_data("data/raw", "labs-renal") %>%
    as.labs() %>%
    tidy_data()

measures <- read_data("data/raw", "measures", FALSE) %>%
    as.measures()

weight <- measures %>%
    filter(measure == "weight",
           measure.units == "kg") %>%
    semi_join(include_pts, by = "millennium.id") %>%
    group_by(millennium.id) %>%
    arrange(measure.datetime, .by_group = TRUE) %>%
    summarize_all("last") %>%
    select(millennium.id, weight = measure.result)

height <- measures %>%
    filter(measure == "height",
           measure.units == "cm",
           measure.result >= 100) %>%
    semi_join(include_pts, by = "millennium.id") %>%
    group_by(millennium.id) %>%
    arrange(measure.datetime, .by_group = TRUE) %>%
    summarize_all("last") %>%
    select(millennium.id, height = measure.result)

demographics <- read_data("data/raw", "demographics", FALSE) %>%
    as.demographics() %>%
    semi_join(include_pts, by = "millennium.id")

excl_crcl <- labs_renal %>%
    filter(lab == "creatinine lvl") %>%
    group_by(pie.id) %>%
    arrange(lab.datetime, .by_group = TRUE) %>%
    inner_join(include_pts, by = "pie.id") %>%
    left_join(demographics, by = "millennium.id") %>%
    left_join(weight, by = "millennium.id") %>%
    left_join(height, by = "millennium.id") %>%
    rowwise() %>%
    mutate(crcl = calc_crcl(age, gender, lab.result, weight, height)) %>%
    filter(crcl <= 30) %>%
    distinct(pie.id)

excl_renal <- excl_hd %>%
    bind_rows(excl_crcl) %>%
    distinct(pie.id)

include_pts <- anti_join(include_pts, excl_renal, by = "pie.id")

# Transfer

excl_transfer <- demographics %>%
    inner_join(include_pts, by = "millennium.id") %>%
    filter(!str_detect(disposition, "Home")) %>%
    distinct(pie.id)

include_pts <- anti_join(include_pts, excl_transfer, by = "pie.id")

# Duration < 3 mo

warfarin_info <- read_data("data/raw", "^warfarin-info", FALSE) %>%
    as.warfarin() %>%
    semi_join(include_pts, by = "millennium.id")

excl_duration <- warfarin_info %>%
    filter()


x <- semi_join(dc_oac, include_pts, by = "pie.id") %>%
    distinct(pie.id, med) %>%
    count(med)

# run MBO query:
#   * Medications - Inpatient - Prompt
#       - Medication (Generic): apixaban, rivaroxaban, dabigatran, edoxaban


# edw_persons <- concat_encounters(screen_id$person.id)

# run EDW query:
#   * Encounters - by Person ID

screen_encounters <- read_data("data/raw", "encounters") %>%
    as.encounters() %>%
    group_by(person.id) %>%
    arrange(admit.datetime, .by_group = TRUE)


meds_doac <- read_data("data/raw", "meds-inpt", FALSE) %>%
    as.meds_inpt()

meds_summary <- meds_doac %>%
    distinct(med, millennium.id) %>%
    group_by(med) %>%
    count()
