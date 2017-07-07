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

revisit_pie <- concat_encounters(revisit$pie.id)

# run EDW query
#   * Identifiers

revisit_ids <- read_data("data/raw", "revisit-ids") %>%
    as.id() %>%
    inner_join(revisit, by = c("pie.id", "millennium.id", "person.id")) %>%
    rename(revisit.pie.id = pie.id,
           revisit.millennium.id = millennium.id,
           revisit.fin = fin,
           revisit.admit.datetime = admit.datetime)

# y <- include_pts %>%
#     ungroup() %>%
#     count(med)
#
# x <- revisit %>%
#     ungroup() %>%
#     count(med)

screen_pts <- read_data("data/raw", "patients", FALSE) %>%
    as.patients() %>%
    semi_join(include_pts, by = "millennium.id")

rivarx <-  filter(include_pts, med == "rivaroxaban")

apix <- filter(include_pts, med == "apixaban")

set.seed(77123)
group_rk <- include_pts %>%
    filter(med == "warfarin") %>%
    sample_frac(0.4) %>%
    bind_rows(rivarx) %>%
    left_join(revisit_ids, by = c("person.id", "med")) %>%
    select(-discharge.datetime, -visit.type, -facility) %>%
    left_join(screen_pts, by = "millennium.id")

group_bs <- include_pts %>%
    filter(med == "warfarin") %>%
    anti_join(group_rk) %>%
    bind_rows(apix) %>%
    left_join(revisit_ids, by = c("person.id", "med")) %>%
    select(-discharge.datetime, -visit.type, -facility) %>%
    left_join(screen_pts, by = "millennium.id")

write_rds(group_rk, "data/tidy/patients_rk.Rds", "gz")
write_rds(group_bs, "data/tidy/patients_bs.Rds", "gz")

group_rk %>%
    select(fin, discharge.datetime, revisit.fin, revisit.admit.datetime, revisit_days) %>%
    write_csv("data/external/patients_rk.csv")

group_bs %>%
    select(fin, discharge.datetime, revisit.fin, revisit.admit.datetime, revisit_days) %>%
    write_csv("data/external/patients_bs.csv")

