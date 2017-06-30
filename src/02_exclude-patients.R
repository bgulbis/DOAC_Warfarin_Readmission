library(tidyverse)
library(lubridate)
library(stringr)
library(edwr)
library(icd)

eligible_pts <- read_rds("data/tidy/eligible_pts.Rds")

include_pts <- eligible_pts

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
    filter(warfarin.event == "warfarin planned duration",
           str_detect(warfarin.result, "varies|unknown|1 month|short term|1 day|45 days|not specified")) %>%
    inner_join(include_pts, by = "millennium.id") %>%
    distinct(pie.id)

include_pts <- anti_join(include_pts, excl_duration, by = "pie.id")

oac_count <- semi_join(dc_oac, include_pts, by = "pie.id") %>%
    distinct(pie.id, med) %>%
    count(med)

study_edw <- concat_encounters(include_pts$pie.id)

# run EDW query:
#   * Encounters - by Person ID



# run MBO query:
#   * Medications - Inpatient - Prompt
#       - Medication (Generic): apixaban, rivaroxaban, dabigatran, edoxaban


# edw_persons <- concat_encounters(screen_id$person.id)


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
