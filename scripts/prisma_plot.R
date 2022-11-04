library(tidyverse)
library(here)
#library(googlesheets4)
library(readxl)

source(here("scripts/prisma_diagram.R"))

# Read in Data 
#d <- read_sheet("https://docs.google.com/spreadsheets/d/1ctTyXRj0Sd037P0zTd-eulFQjrWEbuOx-c-n5VAcwr0/edit#gid=0")
d <- readxl::read_xlsx(here("Online Testing Paper Decision Spreadsheet.xlsx"))


# identify total records screend
d_found <- d %>% 
  mutate(
    source_cat = case_when(
      Source %in% c("Frontiers", "Lookit1", 
                    "Lookit2", "Rhodes", "Sheskin&Keil") ~ "database_search", 
      TRUE ~ "expert_opinion"
    )) %>% 
  group_by(source_cat) %>% 
  count()
 
# identify total duplicates records 
d_duplicate <- d %>% 
  mutate(duplicate = case_when(
    Title_screening_decision == "duplicate" ~ "yes", 
    TRUE ~ "no"
  )) %>% 
  group_by(duplicate) %>% 
  count()

# identify records screened 
d_record_screened <- d %>% 
  filter(Title_screening_decision != "duplicate") %>% 
  group_by(Title_screening_decision) %>% 
  count()

# final decision 
d_full_text <- d %>% 
  filter(Title_screening_decision == "yes") %>% 
  filter(!is.na(Eligibility_decision)) %>% 
  group_by(Eligibility_decision) %>% 
  count()


prisma2(found = filter(d_found, source_cat == "database_search")$n, 
       found_other = filter(d_found, source_cat == "expert_opinion")$n, 
       no_dupes = filter(d_duplicate, duplicate == "no")$n, 
       screened = filter(d_duplicate, duplicate == "no")$n, 
       screen_exclusions =  filter(d_duplicate, duplicate == "no")$n - sum(d_full_text$n), 
       full_text = sum(d_full_text$n), 
       full_text_exclusion = filter(d_full_text, Eligibility_decision == "no")$n, 
       quantitative = filter(d_full_text, Eligibility_decision == "yes")$n,
       font_size = 10, 
       dpi = 72)

  