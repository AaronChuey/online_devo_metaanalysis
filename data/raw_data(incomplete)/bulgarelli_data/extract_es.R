library(tidyverse)

### code heavily copied from Data_analysis.R from Bulgarelli & Bergelson

tvs_participants_all <- read_csv("TVS_participant_demographics_questionnaires.csv") %>%
  filter(!Reason %in% c("cancelled"))

tvs_participants_final <- tvs_participants_all %>%
  filter(Exclude == "no") %>%
  mutate(SubjectID = Name)

inperson <- read_csv("TVS_data_exp1_all.csv") %>%
  filter(SubjectID %in% tvs_participants_final$Name) %>%
  left_join(tvs_participants_final) %>%
  mutate(TVCondition = factor(TVCondition), 
         Order = factor(Order), 
         Pair = factor(Pair)) %>%
  select(-Trial, -X12) %>% 
  filter(TVCondition=="NoVariability") %>% 
  filter(Phase=="Test")

online <- read_csv("tvs_online_final.csv") %>%
  filter(SubjectID %in% tvs_participants_final$Name) %>%
  left_join(tvs_participants_final) %>%
  mutate(TVCondition = factor(TVCondition), 
         Order = factor(Order), 
         Pair = factor(Pair)) %>% 
  filter(TVCondition=="NoVariability") %>% 
  filter(Phase=="Test")

#Age,part count
inperson %>% select(SubjectID,Age_days) %>% unique() %>% summarize(n=n(),mean_age=mean(Age_days)/30.3475)
online %>% select(SubjectID,Age_days) %>% unique() %>% summarize(n=n(),mean_age=mean(Age_days)/30.3475)

# looking time counts
inperson_times <- inperson %>% group_by(StimName) %>% summarize(m=mean(TotalLook),s=sd(TotalLook))
online_times <- online %>% group_by(StimName) %>% summarize(m=mean(TotalLook),s=sd(TotalLook))
