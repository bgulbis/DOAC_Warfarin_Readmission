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

oac_med <- distinct(dc_oac, pie.id, med)

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

eligible_pts <- distinct(screen_icd, pie.id) %>%
    left_join(screen_id, by = "pie.id") %>%
    left_join(oac_med, by = "pie.id")

eligible_edw <- concat_encounters(eligible_pts$pie.id)
eligible_mbo <- concat_encounters(eligible_pts$millennium.id)

write_rds(eligible_pts, "data/tidy/eligible_pts.Rds", "gz")

# run MBO queries:
#   * Demographics
#   * Measures
#   * Warfarin Information

# run EDW queries:
#   * Labs - Pregnancy
#   * Labs - Renal
