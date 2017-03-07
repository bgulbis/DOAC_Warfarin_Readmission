# patient screening

# run MBO query:
#   * Patients - by Medication (Generic)
#       - Facility (Curr): HH HERMANN;HH Trans Care;HH Rehab;HH Clinics
#       - Date Only - Admit (Start): 7/1/2014 12:00:00 AM # in 12 month increments
#       - Date Only - Admit (End): 1/1/2017 12:00:00 AM
#       - Medication (Generic): apixaban, rivaroxaban, dabigatran, edoxaban

library(tidyverse)
library(edwr)

doac_pts <- read_data("data/raw", "patients", FALSE) %>%
    as.patients()

mbo_doac <- concat_encounters(doac_pts$millennium.id)

# run MBO query:
#   * Medications - Inpatient - Prompt
#       - Medication (Generic): apixaban, rivaroxaban, dabigatran, edoxaban

meds_doac <- read_data("data/raw", "meds-inpt", FALSE) %>%
    as.meds_inpt()

meds_summary <- meds_doac %>%
    distinct(med, millennium.id) %>%
    group_by(med) %>%
    count()
