library(tidyverse)

#main question of interest = do they go for the one with more support
#coded as "correct"
#from https://osf.io/j5k9n/
study1 <- read_csv("Study 1 data.csv") %>% 
  filter(is.na(Excluded))

s1_lab <- study1 %>% filter(Site=="NewHaven_Museum")
s1_online <- study1 %>% filter(Site=="TheChildLab_online")

s1_online %>% nrow()
s1_online %>% summarize(mean(Age))
s1_online %>% summarize(mean(Correct), sd(Correct))

s1_lab %>% nrow()
s1_lab %>% summarize(mean(Age))
s1_lab %>% summarize(mean(Correct), sd(Correct))

#study 2
#main question of interest = do they go for the one with more support
#coded as "correct"
# from https://osf.io/4a9bu/

study2 <- read_csv("Study 2 data.csv") %>% 
  filter(is.na(Excluded))

s2_lab <- study2 %>% filter(Site=="NewHaven_Museum")
s2_online <- study2 %>% filter(Site=="TheChildLab_online")

s2_online %>% nrow()
s2_online %>% summarize(mean(Age))
s2_online %>% summarize(mean(Correct), sd(Correct))

s2_lab %>% nrow()
s2_lab %>% summarize(mean(Age))
s2_lab %>% summarize(mean(Correct), sd(Correct))
