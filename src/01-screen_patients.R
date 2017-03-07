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
