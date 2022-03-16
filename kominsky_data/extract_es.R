library(tidyverse)
library(readxl)
# data from https://osf.io/zk986/

#see also https://osf.io/v8u5w/ for their R code which is important for decoding
#O,B refer to orange, blue puppets
#which of these in self-propelled v inert is counterbalanced, as determined by conditions 
#(looks like A,B go with O self-propelled; C,D with B self-propelled)

#heavily copied from their R code
study <- read_excel("Insides Animacy Baseline.xlsx", sheet=2) %>% 
  filter(GNG==1) %>% #this is where exclusion was recorded
  mutate(OT = as.character(OT), BT = as.character(BT)) %>%
  mutate(SelfProp = ifelse(Cond=="A","O",ifelse(Cond=="B","O","B"))) %>%
  mutate(SPResp = ifelse(SelfProp=="O",OT,BT)) %>%
  mutate(IResp = ifelse(SelfProp=="O",BT,OT)) %>%
  mutate(SPResp = factor(SPResp), IResp = factor(IResp))

lab <- study %>% filter(Loc=="in-person")
online <- study %>% filter(Loc=="Online")

lab %>% nrow()
online %>% nrow()
#they give age in integer months
lab %>% summarise(mean(Months)*30.44/365.25)
online %>% summarize(mean(Months)*30.44/365.25)


#As found by going through their code, the main question of interest (I think) is if type of motion (inert v self-propelled) predicts 
# innard response (Empty v not empty)

dat.long <- study %>%
  group_by(snum) %>%
  gather(key="Item", value="Choice", SPResp, IResp) %>%
  mutate(choiceLabel = ifelse(Choice=="B","Biological",ifelse(Choice=="M","Mechanical","Empty"))) %>%
  mutate(Item = ifelse(Item=="SPResp", "Self-propelled","Inert"))

study1.bin <- dat.long %>%
  mutate(insides=ifelse(Choice == "E", 0, 1))

mod0_lab <- glmer(insides~(1|snum), data=study1.bin %>% filter(Loc=="in-person"), 
              family=binomial(link="logit"))
mod1_lab <- glmer(insides~Item + (1|snum), data=study1.bin %>% filter(Loc=="in-person"), 
              family=binomial(link="logit"))

anova(mod1_lab, mod0_lab)

mod0_online <- glmer(insides~(1|snum), data=study1.bin %>% filter(Loc=="Online"), 
                  family=binomial(link="logit"))
mod1_online <- glmer(insides~Item + (1|snum), data=study1.bin %>% filter(Loc=="Online"), 
                  family=binomial(link="logit"))

anova(mod1_online, mod0_online)
