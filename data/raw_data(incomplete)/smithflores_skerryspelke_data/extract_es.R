library(tidyverse)

## heavily copied from the provided Rmd

skerry.data <- read_csv("skerry.data.csv") 

skerry.data.long <- skerry.data%>%
  select(`Subject ID`, Condition, `CompleteFirst?`, EmoOrder, HC:SF)%>%
  rename(sub.num = `Subject ID`,
         experiment = Condition, 
         block.order = `CompleteFirst?`,
         test.order = EmoOrder, 
         complete.happy = HC,
         complete.sad = SC,
         failed.happy = HF,
         failed.sad = SF)%>%
  pivot_longer(cols = complete.happy:failed.sad, 
               names_to = "block.type.trial", 
               values_to = "looking")%>%
  separate(block.type.trial, into = c("block.type", "trial.type"))%>%
  mutate(experiment = ifelse(experiment == "eaeg", "skerry"),
         test.order = ifelse(test.order == "SH", "sad.first", "happy.first"),
         block.order = ifelse(block.order == 1, "complete.first", "failed.first"),
         congruency = ifelse(block.type == "complete" & trial.type == "happy" | 
                               block.type == "failed" & trial.type == "sad", "congruent", "incongruent"))


smithflores.data <- read_csv("smithflores.data.csv") %>% 
  mutate(congruency = ifelse(block.type == "complete" & trial.type == "happy" | 
                               block.type == "failed" & trial.type == "sad", 
                             "congruent", 
                             "incongruent"))

#participants / group

skerry.data %>% select(`Subject ID`, Age) %>% unique() %>% summarize(n=n(), mean_age=mean(Age)/30.4375)

smithflores.data %>% select(sub.num,experiment) %>% unique() %>% 
  group_by(experiment) %>% summarize(n=n())
#age isn't given, but from .Rmd text, younger seems to be 11 mo
# older is 17 mo

# test for congruency effect
skerry.data.long %>% group_by(congruency) %>% 
  summarize(m=mean(looking), sd=sd(looking))

smithflores.data %>% filter(experiment=="younger") %>% 
  group_by(congruency) %>% 
  summarize(m=mean(looking), sd=sd(looking))

smithflores.data %>% filter(experiment=="older") %>% 
  group_by(congruency) %>% 
  summarize(m=mean(looking), sd=sd(looking))
