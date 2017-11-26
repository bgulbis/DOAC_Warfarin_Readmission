library(tidyverse)
library(readxl)
library(edwr)

identifiers <- read_data("data/raw", "identifiers") %>%
    as.id

data_manual_rk <- read_excel("data/external/manual_data_rk.xlsx") %>%
    select(-`ACCESS DATE`) %>%
    mutate(exclude = str_detect(revisit_anticoag_related, "EXCLUDE")) %>%
    mutate_at(c("other", "new_bleed", "new_thrombus"), funs(. != "FALSE")) %>%
    mutate_at(c("afib", "new_oac"),
              str_replace_all,
              pattern = "\\((.*)\\)",
              replacement = "") %>%
    mutate_at("revisit_anticoag_related",
              str_replace_all,
              pattern = "\\?",
              replacement = "") %>%
    mutate_at(c("afib", "new_oac", "revisit_anticoag_related"), as.logical) %>%
    mutate_at("fin", as.character) %>%
    left_join(identifiers, by = "fin") %>%
    select(millennium.id, afib:exclude)

data_manual_bs <- read_excel("data/external/manual_data_bs.xlsx") %>%
    mutate_at(c("afib", "dvt", "pe", "other", "new_oac",
                "revisit_anticoag_related", "new_bleed", "new_thrombus"),
              funs(na_if(., "-"))) %>%
    mutate_at("other", funs(. != "FALSE")) %>%
    mutate_at(c("afib", "dvt", "pe", "new_oac", "revisit_anticoag_related",
                "new_bleed", "new_thrombus"),
              as.logical) %>%
    mutate_at("fin", as.character) %>%
    left_join(identifiers, by = "fin") %>%
    select(millennium.id, afib:notes)

write_rds(data_manual_rk, "data/tidy/manual_rk.Rds", "gz")
write_rds(data_manual_bs, "data/tidy/manual_bs.Rds", "gz")
