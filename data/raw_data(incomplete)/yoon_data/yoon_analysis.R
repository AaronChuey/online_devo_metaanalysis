library(tidyverse)

d <- read_csv("polcon_all_data.csv")
drops <- read_csv("v3_drop.csv")

d %>% 
  filter(expt %in% c("v3","v3_lookit"), 
         !(subid %in% drops$subid),
         !is.na(request_type)) %>%
  mutate(age_group = floor(age)) %>%
  filter(age_group > 2) %>%
  group_by(subid, expt, age_group) %>%
  summarise(correct = mean(correct, na.rm=TRUE)) %>%
  group_by(expt, age_group) %>%
  summarise(mean = mean(correct, na.rm=TRUE), 
            sd = sd(correct, na.rm=TRUE), 
            d = (mean - .5)/ sd) 
