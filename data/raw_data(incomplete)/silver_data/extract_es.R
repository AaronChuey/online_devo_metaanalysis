library(tidyverse)
library(readxl)
# data from https://osf.io/scw6q/

#note:excluded children are not in their data
data <- read_excel("Measuring emerging number knowledge in toddlers_Dataset.xlsx") %>% 
  select(AgeInMonths, ModeOfTesting, TotalCorrect, PercentCorrect) %>% group_by(ModeOfTesting)


age <- data %>% mutate(age=AgeInMonths/12) %>% summarize(age=mean(age), n=n())

#we take as the main measure how often they were correct
#in the point-to-X task, versus chance (.5)
performance <- data %>% summarize(mean_pct=mean(PercentCorrect), sd=sd(PercentCorrect))
