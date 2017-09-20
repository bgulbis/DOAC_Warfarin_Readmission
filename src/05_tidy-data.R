library(tidyverse)
library(edwr)

dir_raw <- "data/raw"

x <- dirr::get_rds("data/tidy")

data_bs <- patients_bs %>%
    left_join(manual_bs, by = "millennium.id") %>%
    filter(!other, new_oac) %>%
    select(-c(fin, person.id, revisit.pie.id, revisit.fin))

data_rk <- patients_rk %>%
    left_join(manual_rk, by = "millennium.id") %>%
    filter(!other, !exclude, new_oac) %>%
    select(-c(fin, person.id, revisit.pie.id, revisit.fin, exclude))

demographics <- read_data(dir_raw, "demographics", FALSE) %>%
    as.demographics

data_demographics_bs <- semi_join(demographics, data_bs, by = "millennium.id")

data_demographics_rk <- semi_join(demographics, data_rk, by = "millennium.id")

meds_home <- read_data(dir_raw, "meds-home") %>%
    as.meds_home()

data_bridging_bs <- meds_home %>%
    semi_join(data_bs, by = "pie.id") %>%
    filter(med.type == "Prescription / Discharge Order",
           med == "enoxaparin")

data_bridging_rk <- meds_home %>%
    semi_join(data_rk, by = "pie.id") %>%
    filter(med.type == "Prescription / Discharge Order",
           med == "enoxaparin")

diagnosis <- read_data(dir_raw, "diagnosis") %>%
    as.diagnosis()

data_diagnosis_bs <- diagnosis %>%
    semi_join(data_bs, by = "pie.id") %>%
    filter(diag.type == "Final")

data_diagnosis_rk <- diagnosis %>%
    semi_join(data_rk, by = "pie.id") %>%
    filter(diag.type == "Final")

dirr::save_rds("data/final", pattern = "data_")
