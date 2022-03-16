rm(list=ls())

library(tidyverse)
library(ggplot2)
library(lme4)
library(afex)
library(BaylorEdPsych)
library(effsize)
library(pwr)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

dat=read.csv("Insides Animacy Baseline online.csv")

dat.2 <- dat %>%
  filter(GNG==1) %>%
  mutate(OT = as.character(OT), BT = as.character(BT)) %>%
  mutate(SelfProp = ifelse(Cond=="A","O",ifelse(Cond=="B","O","B"))) %>%
  mutate(SPResp = ifelse(SelfProp=="O",OT,BT)) %>%
  mutate(IResp = ifelse(SelfProp=="O",BT,OT)) %>%
  mutate(SPResp = factor(SPResp), IResp = factor(IResp))
    
dat.long <- dat.2 %>%
  group_by(snum) %>%
  gather(key="Item", value="Choice", SPResp, IResp) %>%
  mutate(choiceLabel = ifelse(Choice=="B","Biological",ifelse(Choice=="M","Mechanical","Empty"))) %>%
  mutate(Item = ifelse(Item=="SPResp", "Self-propelled","Inert"))
tbl <- table(dat.long$Item, dat.long$Choice)
tbl

dat.checkItems <- dat.2 %>%
  group_by(snum)%>%
  gather(key="CheckItem", value="Choice",Sheep, Motorcycle) %>%
  mutate(choiceLabel = ifelse(Choice=="B","Biological",ifelse(Choice=="M","Mechanical","Empty")))
checkTbl <- table(dat.checkItems$CheckItem, dat.checkItems$Choice)
checkTbl

# Reorder so it goes empty/mechanical/biological bottom to top.
dat.graph <- dat.long %>%
  mutate(thisorder = ifelse(choiceLabel=="Emtpy",1,ifelse(choiceLabel=="Mechanical",2,3))) 

dat.graph$choiceLabel <- factor(dat.graph$choiceLabel, levels=c("Empty","Mechanical","Biological"))

plot1 <- ggplot(data=dat.graph, aes(x=Item, fill=choiceLabel)) +
  geom_bar(position = position_fill(reverse = TRUE), color = "black")+
  #coord_flip(expand = F)+
  labs(y = "proportion", fill = "")+
  scale_y_continuous(breaks = seq(0, 1, 0.25), labels = str_c(seq(0, 100, 25), "%"))+
  # scale_fill_brewer(type = "qual", palette = 4, direction = -1)+
  scale_fill_manual(values = c("ghostwhite", "#8C50F9", "darkgreen", "blue"))+
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill='white',color='white'),
        text=element_text(size=30)
  )+ 
  guides(fill = guide_legend(ncol = 2))
plot1

dat.checkGraph <- dat.checkItems
dat.checkGraph$choiceLabel <- factor(dat.checkGraph$choiceLabel, levels=c("Empty","Mechanical","Biological"))
plot2 <-  ggplot(data=dat.checkGraph, aes(x=CheckItem, fill=choiceLabel)) +
  geom_bar(position = position_fill(reverse = TRUE), color = "black")+
  #coord_flip(expand = F)+
  labs(y = "proportion", fill = "")+
  scale_y_continuous(breaks = seq(0, 1, 0.25), labels = str_c(seq(0, 100, 25), "%"))+
  # scale_fill_brewer(type = "qual", palette = 4, direction = -1)+
  scale_fill_manual(values = c("ghostwhite", "#8C50F9", "darkgreen", "blue"))+
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill='white',color='white'),
        text=element_text(size=30)
  )+ 
  guides(fill = guide_legend(ncol = 2)) 
plot2

## Analyises
study1.bin <- dat.long %>%
  mutate(insides=ifelse(Choice == "E", 0, 1))


bin.summ <- study1.bin %>%
  group_by(Item) %>%
  summarize(meanNotEmpty = mean(insides))

mod0 <- glmer(insides~(1|snum), data=study1.bin, 
              family=binomial(link="logit"))
mod1 <- glmer(insides~Item + (1|snum), data=study1.bin, 
              family=binomial(link="logit"))
mod2 <- glmer(insides~Item + Item*Cond + Item*SelfProp + (1|snum), data=study1.bin, 
              family=binomial(link="logit"))

anova(mod1, mod0) # Should indicate that item is a better predictor than the null.
# Yes, p = .04555.
anova(mod2, mod1)
# NS, adding interactions with condition and identity of self-propelled object doesn't produce
# a better fit.

# Now contrasting types of insides.
noEmpties <- dat.long %>%
  filter(Choice != "E") %>%
  mutate(bio = ifelse(Choice == "B", 1, 0))
mod0.b <- glmer(bio~ (1|snum), data=noEmpties, 
                family=binomial(link="logit"))
mod1.b <- glmer(bio~Item + (1|snum), data=noEmpties, 
                family=binomial(link="logit"))
anova(mod0.b, mod1.b)
# NS, no difference in preference for mechanical versus biological insides.

#We will also conduct simple binomial tests to examine whether there is an preference for one 
#or the other overall, and if the analyses reveal an effect of item we will conduct separate 
#binomial tests for each item. [There weren't]

# Simple binomial test of "Choice" for "noempties"
sum(noEmpties$bio) # 59 "bio" out of total (across both items) of 133
binom.test(59, 133)
# No overall preference for bio versus mechanical for those who did not say "empty", regardless of item.



# Now let's have a quick look at online.

dat.place <- study1.bin%>%
  mutate(Online = ifelse(Loc == "Online","Online","InPerson"))

mod0.online <- glmer(insides~(1|snum), data=dat.place, 
              family=binomial(link="logit"))
mod1.online <- glmer(insides~Item + (1|snum), data=dat.place, 
              family=binomial(link="logit"))
mod2.online <- glmer(insides~Item + Item*Online + (1|snum), data=dat.place, 
              family=binomial(link="logit"))
anova(mod1.online,mod2.online)  
# There is an interaction, but it is not a particularly meaningful one. 
# Many fewer biological responses, many mre empty responses.

# Graph online vs. in person.
plot3 <- ggplot(data=dat.place, aes(x=Item, fill=choiceLabel)) +
  facet_wrap(~Online)+
  geom_bar(position = position_fill(reverse = TRUE), color = "black")+
  #coord_flip(expand = F)+
  labs(y = "proportion", fill = "")+
  scale_y_continuous(breaks = seq(0, 1, 0.25), labels = str_c(seq(0, 100, 25), "%"))+
  # scale_fill_brewer(type = "qual", palette = 4, direction = -1)+
  scale_fill_manual(values = c("green", "white", "purple", "blue"))+
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill='white',color='white'),
        text=element_text(size=30)
  )+ 
  guides(fill = guide_legend(ncol = 2))
plot3
# The pattern is assuredly different but not actually worse. Basically many bio responses appear
# to have been replaced by empty responses, while the rate of "mechanical" responses remains stable.


## Post-hoc examination of the online and in person separately.

online.bin <- dat.place %>%
  filter(Online == "Online")

mod0.ononly <- glmer(insides~(1|snum), data=online.bin, 
                     family=binomial(link="logit"))
mod1.ononly <- glmer(insides~Item + (1|snum),data=online.bin, 
                     family=binomial(link="logit"))
anova(mod0.ononly, mod1.ononly)
# No effect in the online sample alone, but it's only ~30 participants.

offline.bin <- dat.place %>%
  filter(Online != "Online")

mod0.offonly <- glmer(insides~(1|snum), data=offline.bin, 
                      family=binomial(link="logit"))
mod1.offonly <- glmer(insides~Item + (1|snum),data=offline.bin, 
                     family=binomial(link="logit"))

anova(mod0.offonly, mod1.offonly)
# Nor is the effect significant in the offline participants alone.


# First item analysis
# Conditions B and D show the animate trial first, conditions A and C show the inert trial first.
dat.3 <- dat %>%
  filter(OT!="") %>%
  mutate(OT = as.character(OT), BT = as.character(BT)) %>%
  mutate(SelfProp = ifelse(Cond=="A","O",ifelse(Cond=="B","O","B"))) %>%
  mutate(FirstTrial = ifelse(Cond=="A", "SP", ifelse(Cond=="C", "SP","I"))) %>%
  mutate(SPResp = ifelse(SelfProp=="O",OT,BT)) %>%
  mutate(IResp = ifelse(SelfProp=="O",BT,OT)) %>%
  mutate(FirstResp = ifelse(FirstTrial == "SP", SPResp, IResp))

# chi-square test of independence
firsttbl <- table(dat.3$FirstTrial, dat.3$FirstResp)
firsttbl
chisq.test(firsttbl)
# Nope, p = .409.

#For the validation items (the sheep and motorcycle), we will recode children’s responses 
#into a binary variable, either “correct” or “incorrect”, yielding a score of 0, 1, or 2 
#for each child, and examine whether children are overall significantly better than chance 
#(1) using a two-tailed t- test. We expect children to be either at or slightly better than 
#chance, based on the results of Simons & Keil (1995).
dat.accuracy <- dat.2 %>%
  mutate(SheepAcc = ifelse(Sheep == "B", 1, 0)) %>%
  mutate(CycleAcc = ifelse(Motorcycle == "M", 1, 0)) %>%
  mutate(Accuracy = SheepAcc + CycleAcc)

t.test(dat.accuracy$Accuracy,mu=1)  
mean(dat.accuracy$Accuracy)
sd(dat.accuracy$Accuracy)
# Significantly better than chance, p = .009, mean = 1.21 out of 2

# Post-hoc: Full pattern of responses for check items
check.bin <- dat.checkItems %>%
  mutate(insides=ifelse(Choice == "E", 0, 1))

mod0.check <- glmer(insides~(1|snum), data=check.bin, 
                    family=binomial(link="logit"))
mod1.check <- glmer(insides~CheckItem + (1|snum), data=check.bin, 
                    family=binomial(link="logit"))
anova(mod0.check, mod1.check)

check.summ <- check.bin %>%
  group_by(CheckItem) %>%
  summarize(meanNotEmpty = mean(insides))
# Interesting. Sig. dif. in choice of "empty" by item, more for sheep.

# Did they choose the biological option more often than chance for the sheep?
# They chose the biological option 45/92 times, chance is .33
binom.test(45, 92, p=(1/3))
# Yes, p = .003

# Now for bio vs mech.
check.bio <- dat.checkItems %>%
  filter(Choice != "E") %>%
  mutate(bio = ifelse(Choice == "B", 1, 0))

mod0.check.b <-  glmer(bio~(1|snum), data=check.bio, 
                       family=binomial(link="logit"))
mod1.check.b <- glmer(bio~CheckItem + (1|snum), data=check.bio, 
                      family=binomial(link="logit"))
anova(mod0.check.b, mod1.check.b)
# Unsurprisingly a rather huge effect.

# Post-hoc: Filtering by whether they were correct on BOTH the sheep and motorcycle.

dat.insidesIntuitions <- dat.2 %>%
  filter(Sheep == "B" & Motorcycle == "M") %>% # 37/92
  group_by(snum) %>%
  gather(key="Item", value="Choice", SPResp, IResp) %>%
  mutate(choiceLabel = ifelse(Choice=="B","Biological",ifelse(Choice=="M","Mechanical","Empty"))) %>%
  mutate(Item = ifelse(Item=="SPResp", "Self-propelled","Inert"))

plot4 <- ggplot(data=dat.insidesIntuitions, aes(x=Item, fill=choiceLabel)) +
  geom_bar(position = position_fill(reverse = TRUE), color = "black")+
  #coord_flip(expand = F)+
  labs(y = "proportion", fill = "")+
  scale_y_continuous(breaks = seq(0, 1, 0.25), labels = str_c(seq(0, 100, 25), "%"))+
  # scale_fill_brewer(type = "qual", palette = 4, direction = -1)+
  scale_fill_manual(values = c("green", "white", "purple", "blue"))+
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill='white',color='white'),
        text=element_text(size=30)
  )+ 
  guides(fill = guide_legend(ncol = 2))
plot4


# Post-hoc, just out of curiosity, let's look at mech vs bio by item.
noEmpties.SP <- noEmpties %>%
  filter(Item=="Self-propelled")
sum(noEmpties.SP$bio) # 31/72
binom.test(31, 72) # Nope

noEmpties.Inert <- noEmpties %>%
  filter(Item == "Inert")
sum(noEmpties.Inert$bio) # 28/61
binom.test(28, 61) # nope.

# Post-hoc looking at individual patterns of responding across all items, test and check.
# Specifically, looking for kids who gave the same answer for all items, and seeing if there
# are a lot who just said "empty" for everything.

# Let's just start descriptively: How many kids gave the "empty" response for everything?
dat.allempty <- dat.2 %>%
  filter(OT == "E" & BT == "E" & Sheep == "E" & Motorcycle == "E")
# There's only 6 participants who did this.

dat.allmech <- dat.2 %>%
  filter(OT == "M" & BT == "M" & Sheep == "M" & Motorcycle == "M")
# 2 who did this

dat.allbio <- dat.2 %>%
  filter(OT == "B" & BT == "B" & Sheep == "B" & Motorcycle == "B")
# and 2 who did this. So a total of 10/92 gave the same response, 
# and only 6 of those responded "emtpy" to all

# More specifically, let's filter by those kids that said "empty" to the sheep,
# and see what they look like for the test items. There are 24 such kids
dat.emptysheep <- dat.2 %>%
  filter(Sheep == "E") %>%
  gather(key="Item", value="Choice", SPResp, IResp) %>%
  mutate(choiceLabel = ifelse(Choice=="B","Biological",ifelse(Choice=="M","Mechanical","Empty"))) %>%
  mutate(Item = ifelse(Item=="SPResp", "Self-propelled","Inert"))

plot.emptysheep <- ggplot(data=dat.emptysheep, aes(x=Item, fill=choiceLabel)) +
  geom_bar(position = position_fill(reverse = TRUE), color = "black")+
  #coord_flip(expand = F)+
  labs(y = "proportion", fill = "")+
  scale_y_continuous(breaks = seq(0, 1, 0.25), labels = str_c(seq(0, 100, 25), "%"))+
  # scale_fill_brewer(type = "qual", palette = 4, direction = -1)+
  scale_fill_manual(values = c("green", "white", "purple", "blue"))+
  theme(legend.position = "bottom",
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.background = element_rect(fill='white',color='white'),
        text=element_text(size=30)
  )+ 
  guides(fill = guide_legend(ncol = 2))
plot.emptysheep

# Definitely a tendency to answer "empty" on the test items too in these 24 kids.
tbl.emptysheep <- table(dat.emptysheep$Item, dat.emptysheep$Choice)
tbl.emptysheep
# Equal # of empty responses across items (11 each), and most common response for both. 
# Notably very few bio responses for self-propelled item (only 3), but 10 mech resp.