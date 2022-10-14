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
  mutate(insides=ifelse(Choice == "E", "empty", "stuff")) %>% 
  ungroup() %>% 
  select(Loc,Item, insides) %>% 
  group_by(Loc, Item, insides) %>% 
  tally() %>% 
  pivot_wider(names_from=insides, values_from=n) %>% 
  mutate(checksum=empty+stuff)

# messing around with odds ratio
# [(self-propelled & stuff)x(inert & empty)] / [(self-propelled & empty) x (inert & stuff) ] 
# congruent / incongruent

# according to https://www.escal.site/ the conversion log odds = d * pi / root(3)
#online or
online_or <- 19 * 17 / (11* 13)
log(online_or)*sqrt(3)/pi

inperson_or <- 53 * 14 / (48 * 9)
log(inperson_or)*sqrt(3)/pi

# following log odds method from leonard data instead
# except that the glm needs a logit link
# these d's line up with above! 
library(effectsize)
for_mod <- dat.long %>% mutate(insides.bin=ifelse(Choice == "E", 0, 1),
                               animate.bin=ifelse(Item=="Inert", 0, 1))
online_mod <- glm(animate.bin ~ insides.bin, family=binomial(), data=for_mod %>% filter(Loc=="Online"))

online_diff_es <- logoddsratio_to_d(online_mod[1]$coefficients[2])

inperson_mod <- glm(animate.bin ~ insides.bin, family=binomial(), data=for_mod %>% filter(Loc=="in-person"))

inperson_diff_es <- logoddsratio_to_d(inperson_mod[1]$coefficients[2])
