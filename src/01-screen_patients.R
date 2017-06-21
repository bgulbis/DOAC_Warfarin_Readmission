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
    mutate(zip_pt = if_else(!is.na(zip5), zip5, zip5_curr))

local_pts <- zips %>%
    filter(zip5 %in% mhhs_zipcodes$zipcode) %>%
    distinct(pie.id)

local_pie <- concat_encounters(local_pts$pie.id)

# run EDW query:
#   * Medications - Home and Discharge - All

# run MBO query:
#   * Diagnosis - ICD-9/10-CM
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
