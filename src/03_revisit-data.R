library(tidyverse)
library(lubridate)
library(stringr)
library(edwr)
library(icd)

include_pts <- read_rds("data/tidy/include_pts.Rds")

patients <- read_data("data/raw", "patients", FALSE) %>%
    as.patients() %>%
    select(millennium.id, discharge.datetime) %>%
    semi_join(include_pts, by = "millennium.id")

revisit <- read_data("data/raw", "encounters") %>%
    as.encounters() %>%
    filter(visit.type %in% c("Inpatient", "Emergency", "Bedded Outpatient", "Observation", "Inpatient Rehab")) %>%
    group_by(person.id) %>%
    arrange(admit.datetime, .by_group = TRUE) %>%
    left_join(patients, by = "millennium.id") %>%
    fill(discharge.datetime) %>%
    filter(!is.na(discharge.datetime)) %>%
    mutate(revisit_days = difftime(admit.datetime, discharge.datetime, units = "days")) %>%
    filter(revisit_days > 0,
           revisit_days <= 90) %>%
    distinct(person.id, .keep_all = TRUE) %>%
    left_join(include_pts[c("person.id", "med")], by = "person.id")

y <- include_pts %>%
    ungroup() %>%
    count(med)

x <- revisit %>%
    ungroup() %>%
    count(med)

