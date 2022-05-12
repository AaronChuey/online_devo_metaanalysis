library(tidyverse)
library(broom)
library(lme4)
library(lmerTest)
library(car)
library(broom.mixed)
library(stargazer)
library(ggpubr)
library(papaja)

all_stim <- read_csv("Data/TVSNewTalker_stimuli_characteristics.csv")

tvs_participants_all <- read_csv("Data/TVS_participant_demographics_questionnaires.csv") %>%
  filter(!Reason %in% c("cancelled"))

tvs_participants_final <- tvs_participants_all %>%
  filter(Exclude == "no") %>%
  mutate(SubjectID = Name)

tvs_participants_exp1 <- tvs_participants_final %>%
  filter(Experiment == "1")

tvs_participants_online <- tvs_participants_final %>%
  filter(Experiment == "online") %>%
  mutate(Monitor = as.numeric(Monitor))

tvs_participants_exp2 <- tvs_participants_final %>%
  filter(Experiment == "2") %>%
  mutate(Monitor = as.numeric(Monitor))

tvs_data_exp1 <- read_csv("Data/TVS_data_exp1_all.csv") %>%
  filter(SubjectID %in% tvs_participants_final$Name) %>%
  left_join(tvs_participants_final) %>%
  mutate(TVCondition = factor(TVCondition), 
         Order = factor(Order), 
         Pair = factor(Pair)) %>%
  select(-Trial, -X12) %>%
  mutate(Experiment = "1a")

tvs_habituation_exp1 <- tvs_data_exp1 %>%
  filter(Phase == "Habituation") %>%
  group_by(SubjectID, TVCondition, Pair, Order) %>%
  summarize(trials_to_habit = n(),
            sum = sum(TotalLook), 
            mean = mean(TotalLook)) %>%
  mutate(TVCondition = factor(TVCondition, levels = c("NoVariability", "WithinTalker", "BetweenTalker"))) %>%
  mutate(Experiment = "1a")

habit_trials_by_condition_exp1 <- lm(trials_to_habit ~ TVCondition, data = tvs_habituation_exp1)
habit_trials_by_condition_anova_exp1 <- anova(habit_trials_by_condition_exp1)

habit_no_within_exp1 <- t.test(tvs_habituation_exp1$trials_to_habit[tvs_habituation_exp1$TVCondition=="NoVariability"], tvs_habituation_exp1$trials_to_habit[tvs_habituation_exp1$TVCondition == "WithinTalker"])

habit_no_between_exp1 <- t.test(tvs_habituation_exp1$trials_to_habit[tvs_habituation_exp1$TVCondition=="NoVariability"], tvs_habituation_exp1$trials_to_habit[tvs_habituation_exp1$TVCondition == "BetweenTalker"])

habit_within_between_exp1 <- t.test(tvs_habituation_exp1$trials_to_habit[tvs_habituation_exp1$TVCondition=="BetweenTalker"], tvs_habituation_exp1$trials_to_habit[tvs_habituation_exp1$TVCondition == "WithinTalker"])

habit_trials_by_pair_exp1 <- tidy(anova(lm(trials_to_habit ~ Pair, data = tvs_habituation_exp1)))

tvs_test_exp1 <- tvs_data_exp1 %>%
  filter(Phase == "Test") %>%
  mutate(StimName = factor(StimName),
         TVCondition = factor(TVCondition)) %>%
  mutate(StimName = fct_recode(StimName,
                               "WordSwitch" = "AuditoryControl",
                               "PictureSwitch" = "VisualControl",
                               "Same" = "Same_test",
                               "TalkerSwitch" = "NT_test")) %>%
  mutate(StimName = factor(StimName, levels = c("TalkerSwitch", "WordSwitch", "PictureSwitch", "Same"))) %>%
  mutate(TVCondition = factor(TVCondition, levels = c("NoVariability", "WithinTalker", "BetweenTalker")))%>%
  mutate(dialect = ifelse(Parent1Dialect == Parent2Dialect, 0, 1))

tvs_test_graph_exp1 <- tvs_test_exp1 %>%
  mutate(StimName = factor(StimName, levels = c("Same", "TalkerSwitch", "WordSwitch", "PictureSwitch"))) %>%
  rename(Trial = StimName) %>%
  mutate(TotalLook = TotalLook/1000) %>%
  select(-exp2_lab)

experiment_results <- ggplot(data = tvs_test_graph_exp1, aes(x = Trial, y = TotalLook, color = Trial)) + stat_summary(fun="mean", geom="bar", position ="dodge", fill = "white") + labs(y = "Mean looking time (s)") + geom_point(shape = 1, color = "grey") + facet_wrap(~TVCondition) + theme_bw() + stat_summary(fun.data = mean_se, geom = "errorbar",width=.2, position=position_dodge(.9)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(legend.position ="none")

ggplot(data = tvs_test_graph_exp1, aes(x = Trial, y = TotalLook, color = Trial)) + stat_summary(fun="mean", geom="bar", position ="dodge", fill = "white") + labs(y = "Mean looking time (s)") + geom_point(shape = 1, color = "grey") + facet_wrap(~Order + TVCondition) + theme_bw() + stat_summary(fun.data = mean_se, geom = "errorbar",width=.2, position=position_dodge(.9)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(legend.position ="none")

habituation_graph <- ggplot(data = tvs_habituation_exp1, aes(x = TVCondition, y = trials_to_habit)) + stat_summary(fun="mean", geom = "bar", position = "dodge", fill = "white", color = "black") + labs(y = "Mean trials to habituate") + geom_point(shape = 1, color = "grey")+ theme_bw() + stat_summary(fun.data = mean_se, geom = "errorbar",width=.2, position=position_dodge(.9))+ theme(axis.title.x=element_blank()) + theme(legend.position ="none")+ geom_bracket(xmin = "NoVariability", xmax = "WithinTalker", y.position = 20, label = "*", bracket.shorten = 0.25) + geom_bracket(xmin = "WithinTalker", xmax = "BetweenTalker", y.position = 20, label = "*", bracket.shorten = 0.25) + geom_bracket(xmin = "NoVariability", xmax = "BetweenTalker", y.position = 25, label = "n.s")

options(contrasts=c("contr.sum","contr.poly"))

c1 <- c(1, -0.5, -0.5)
c2 <- c(0, -.5, .5)

contrasts <- rbind(constant=1/3, c1, c2)
contrasts
contrasts <-solve(contrasts)
contrasts <- contrasts[ , -1]

contrasts(tvs_test_exp1$TVCondition) <- contrasts

c3 <- c(1, 0, 0, -1)
c4 <- c(0, 1, 0, -1)
c5 <- c(0, 0, 1, -1)

contrasts_trial <- rbind(constant = 1/4, c3, c4, c5)
contrasts_trial <- solve(contrasts_trial)
contrasts_trial <- contrasts_trial[,-1]

contrasts(tvs_test_exp1$StimName) <- contrasts_trial

model_exp1 <- lmerTest::lmer(TotalLook ~ StimName*TVCondition + (1|Pair) + (1|SubjectID) + (1|Order), data = tvs_test_exp1)
summary(model_exp1)
#apa_model <- apa_print(model)
model_output_exp1 <- tidy(model_exp1)

model1_d <- EMAtools::lme.dscore(model_exp1, tvs_test_exp1, type = "lme4") %>% as_tibble() %>%
  add_column(term = c("StimNamec3", "StimNamec4", "StimNamec5", "TVConditionc1", "TVConditionc2", "StimNamec3:TVConditionc1", "StimNamec4:TVConditionc1", "StimNamec5:TVConditionc1", "StimNamec3:TVConditionc2", "StimNamec4:TVConditionc2", "StimNamec5:TVConditionc2")) %>%
  select(term, d)

model_exp1_table <- broom.mixed::tidy(model_exp1, effects = "fixed") %>%
  left_join(model1_d)

test_habit_trials_by_pair_exp1 <- tidy(anova(lm(TotalLook ~ Pair*StimName, data = tvs_test_exp1)))
test_habit_trials_by_order_exp1 <- tidy(anova(lm(TotalLook ~ Order, data = tvs_test_exp1)))

summary(lm(TotalLook ~ Pair*StimName, data = tvs_test_exp1))

same_switch_noVar_exp1 <- apa_print(t.test(tvs_test_exp1$TotalLook[tvs_test_exp1$StimName == "Same" & tvs_test_exp1$TVCondition=="NoVariability"], tvs_test_exp1$TotalLook[tvs_test_exp1$StimName == "TalkerSwitch" & tvs_test_exp1$TVCondition=="NoVariability"], paired = T))

same_switch_Var_exp1 <- apa_print(t.test(tvs_test_exp1$TotalLook[tvs_test_exp1$StimName == "Same" & tvs_test_exp1$TVCondition !="NoVariability"], tvs_test_exp1$TotalLook[tvs_test_exp1$StimName == "TalkerSwitch" & tvs_test_exp1$TVCondition !="NoVariability"], paired = T))

same_auditory_noVar_exp1 <- apa_print(t.test(tvs_test_exp1$TotalLook[tvs_test_exp1$StimName == "Same" & tvs_test_exp1$TVCondition=="NoVariability"], tvs_test_exp1$TotalLook[tvs_test_exp1$StimName == "WordSwitch" & tvs_test_exp1$TVCondition=="NoVariability"], paired = T))

same_auditory_Var_exp1 <- apa_print(t.test(tvs_test_exp1$TotalLook[tvs_test_exp1$StimName == "Same" & tvs_test_exp1$TVCondition !="NoVariability"], tvs_test_exp1$TotalLook[tvs_test_exp1$StimName == "WordSwitch" & tvs_test_exp1$TVCondition !="NoVariability"], paired = T))

same_visual_noVar_exp1 <- apa_print(t.test(tvs_test_exp1$TotalLook[tvs_test_exp1$StimName == "Same" & tvs_test_exp1$TVCondition=="NoVariability"], tvs_test_exp1$TotalLook[tvs_test_exp1$StimName == "PictureSwitch" & tvs_test_exp1$TVCondition=="NoVariability"], paired = T))

same_visual_Var_exp1 <- apa_print(t.test(tvs_test_exp1$TotalLook[tvs_test_exp1$StimName == "Same" & tvs_test_exp1$TVCondition !="NoVariability"], tvs_test_exp1$TotalLook[tvs_test_exp1$StimName == "PictureSwitch" & tvs_test_exp1$TVCondition !="NoVariability"], paired = T))

diff_scores_exp1 <- tvs_test_exp1 %>%
  select(-TotalLookAway) %>%
  group_by(SubjectID) %>%
  spread(StimName, TotalLook) %>%
  mutate(Talker_same = TalkerSwitch - Same,
         Word_same = WordSwitch -Same,
         Pitcure_same = PictureSwitch - Same) %>%
  mutate(Sex = factor(Sex))

contrasts(diff_scores_exp1$TVCondition) <- contrasts

diff_score_model_all_exp1 <- lm(Talker_same ~ Age_days + Sex + WordsUnderstood + TVCondition, data = diff_scores_exp1)
diff_score_model_tv_exp1 <- lm(Talker_same ~ TVCondition, data = diff_scores_exp1)
diff_score_model_all_glance_exp1 <- diff_score_model_all_exp1 %>% glance()
diff_score_model_all_summary_glance_exp1 <- tidy(diff_score_model_all_exp1)
diff_score_model_tv_glance_exp1 <- diff_score_model_tv_exp1 %>% glance()
summary(diff_score_model_tv_exp1)
model_comparison_exp1 <- tidy(anova(diff_score_model_tv_exp1, diff_score_model_all_exp1))

tvs_data_online <- read_csv("Data/tvs_online_final.csv") %>%
  filter(SubjectID %in% tvs_participants_final$Name) %>%
  left_join(tvs_participants_final) %>%
  mutate(TVCondition = factor(TVCondition), 
         Order = factor(Order), 
         Pair = factor(Pair)) %>%
  mutate(Experiment = "1b")

tvs_habituation_online <- tvs_data_online %>%
  filter(Phase == "Habituation") %>%
  group_by(SubjectID, TVCondition, Pair, Order, Experiment) %>%
  summarize(trials_to_habit = n(),
            sum = sum(TotalLook), 
            mean = mean(TotalLook))

tvs_habitation_lab_novariability <- tvs_habituation_exp1 %>%
  filter(TVCondition == "NoVariability")

tvs_habituation_online_lab <- rbind(tvs_habituation_online, tvs_habitation_lab_novariability) %>%
  mutate(Experiment = as.factor(Experiment))

tvs_habituation_online_lab_comparison <-  apa_print(t.test(tvs_habituation_online_lab$trials_to_habit[tvs_habituation_online_lab$Experiment=="1a"], tvs_habituation_online_lab$trials_to_habit[tvs_habituation_online_lab$Experiment == "1b"]))

tvs_test_online <- tvs_data_online %>%
  filter(Phase == "Test") %>%
  mutate(StimName = factor(StimName)) %>%
  mutate(StimName = fct_recode(StimName,
                               "WordSwitch" = "AuditoryControl",
                               "PictureSwitch" = "VisualControl",
                               "Same" = "Same_test",
                               "TalkerSwitch" = "NT_test")) %>%
  mutate(StimName = factor(StimName, levels = c("TalkerSwitch", "WordSwitch", "PictureSwitch", "Same"))) %>%
  select(SubjectID, Phase, StimName, TotalLook, TotalLookAway, Name, Age_days, Sex, Languages, Exclude, Reason, TVCondition, Pair, Order, Race, MatEducation, Parent1Dialect, Parent2Dialect, Experiment, WordsUnderstood, WordsProduced, Monitor) %>%
  mutate(dialect = ifelse(Parent1Dialect == Parent2Dialect, 0, 1)) %>%
  mutate(exp2_lab = 0)

tvs_test_graph_online <- tvs_test_online %>%
  mutate(StimName = factor(StimName, levels = c("Same", "TalkerSwitch", "WordSwitch", "PictureSwitch"))) %>%
  rename(Trial = StimName) %>%
  mutate(TotalLook = TotalLook/1000) %>%
  select(-exp2_lab)
  

experiment_results_online <- ggplot(data = tvs_test_graph_online, aes(x = Trial, y = TotalLook, color = Trial)) + stat_summary(fun="mean", geom="bar", position ="dodge", fill = "white") + labs(y = "Mean looking time (s)") + geom_point(shape = 1, color = "grey") + theme_bw() + stat_summary(fun.data = mean_se, geom = "errorbar",width=.2, position=position_dodge(.9)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(legend.position ="none")

exp1_online_all_graph <- rbind(tvs_test_graph_online, tvs_test_graph_exp1)

experiment1_online_graph <-ggplot(data = exp1_online_all_graph, aes(x = Trial, y = TotalLook, color = Trial)) + stat_summary(fun="mean", geom="bar", position ="dodge", fill = "white") + labs(y = "Mean looking time (s)") + geom_point(shape = 1, color = "grey") + theme_bw() + stat_summary(fun.data = mean_se, geom = "errorbar",width=.2, position=position_dodge(.9)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(legend.position ="none") + facet_grid(~Experiment + TVCondition)

tvs_test_exp1_novariability <- tvs_test_exp1 %>%
  filter(TVCondition == "NoVariability") %>%
  select(-Experiment) %>%
  mutate(Experiment = "1a")

tvs_test_online_lab <- rbind(tvs_test_exp1_novariability, tvs_test_online)

contrasts(tvs_test_online_lab$StimName) <- contrasts_trial

model_online_lab_comparison <- lmerTest::lmer(TotalLook ~ StimName*Experiment +  (1|SubjectID), data = tvs_test_online_lab)
summary(model_online_lab_comparison)
#apa_model <- apa_print(model)
online_lab_model_output <- tidy(model_online_lab_comparison)

test_habit_trials_by_pair_online <- tidy(anova(lm(TotalLook ~ Pair, data = tvs_test_online)))
test_habit_trials_by_order_online <- tidy(anova(lm(TotalLook ~ Order, data = tvs_test_online)))

model_online_d <- EMAtools::lme.dscore(model_online_lab_comparison, tvs_test_online_lab, type = "lme4") %>% as_tibble() %>%
  add_column(term = c("StimNamec3", "StimNamec4", "StimNamec5", "Experiment1", "StimNamec3:Experiment1", "StimNamec4:Experiment1", "StimNamec5:Experiment1")) %>%
  select(term, d)

model_online_table <- broom.mixed::tidy(model_online_lab_comparison, effects = "fixed") %>%
  left_join(model_online_d)

###Exp2-MPs
tvs_data_exp2 <- read_csv("Data/tvs_all_exp2.csv") %>%
  filter(SubjectID %in% tvs_participants_final$Name) %>%
  select(SubjectID, Phase, StimName, TotalLook, TotalLookAway) %>%
  left_join(tvs_participants_final) %>%
  mutate(TVCondition = factor(TVCondition), 
         Order = factor(Order), 
         Pair = factor(Pair))

n_distinct(tvs_data_exp2$SubjectID)
list(unique(tvs_data_exp2$SubjectID))

tvs_data_exp2_location_habit <- tvs_data_exp2 %>%
  filter(Phase == "Habituation") %>%
  group_by(SubjectID, TVCondition, Pair, Order, exp2_lab) %>%
  summarize(trials_to_habit = n(),
            sum = sum(TotalLook), 
            mean = mean(TotalLook))

mean(tvs_data_exp2_location_habit$trials_to_habit[tvs_data_exp2_location_habit$exp2_lab=="lab"])
mean(tvs_data_exp2_location_habit$trials_to_habit[tvs_data_exp2_location_habit$exp2_lab=="zoom"])

t.test(tvs_data_exp2_location_habit$trials_to_habit[tvs_data_exp2_location_habit$exp2_lab=="lab"], tvs_data_exp2_location_habit$trials_to_habit[tvs_data_exp2_location_habit$exp2_lab=="zoom"])

tvs_habituation_exp2 <- tvs_data_exp2 %>%
  filter(Phase == "Habituation") %>%
  group_by(SubjectID, TVCondition, Pair, Order) %>%
  summarize(trials_to_habit = n(),
            sum = sum(TotalLook), 
            mean = mean(TotalLook))%>%
  mutate(TVCondition = factor(TVCondition, levels = c("NoVariability", "WithinTalker", "BetweenTalker")))

habit_trials_by_condition_exp2 <- lm(trials_to_habit ~ TVCondition, data = tvs_habituation_exp2)
habit_trials_by_condition_anova_exp2 <- anova(habit_trials_by_condition_exp2)

habit_no_within_exp2 <- t.test(tvs_habituation_exp2$trials_to_habit[tvs_habituation_exp2$TVCondition=="NoVariability"], tvs_habituation_exp2$trials_to_habit[tvs_habituation_exp2$TVCondition == "WithinTalker"])

habit_no_between_exp2 <- t.test(tvs_habituation_exp2$trials_to_habit[tvs_habituation_exp2$TVCondition=="NoVariability"], tvs_habituation_exp2$trials_to_habit[tvs_habituation_exp2$TVCondition == "BetweenTalker"])

habit_within_between_exp2 <- t.test(tvs_habituation_exp2$trials_to_habit[tvs_habituation_exp2$TVCondition=="BetweenTalker"], tvs_habituation_exp2$trials_to_habit[tvs_habituation_exp2$TVCondition == "WithinTalker"])

habit_trials_by_pair_exp2 <- tidy(anova(lm(trials_to_habit ~ Pair, data = tvs_habituation_exp2)))

tvs_test_exp2 <- tvs_data_exp2 %>%
  select(-X12) %>%
  filter(Phase == "Test") %>%
  mutate(StimName = factor(StimName),
         TVCondition = factor(TVCondition)) %>%
  mutate(StimName = fct_recode(StimName,
                               "WordSwitch" = "AuditoryControl",
                               "PictureSwitch" = "VisualControl",
                               "Same" = "Same_test",
                               "MispronunciationSwitch" = "MP_test",
                               "MispronunciationSwitch" = "MP_noom",
                               "MispronunciationSwitch" = "MP_Lef",
                               "MispronunciationSwitch" = "MP_Noom",
                               "MispronunciationSwitch" = "MP_lef")) %>%
  mutate(StimName = factor(StimName, levels = c("MispronunciationSwitch", "WordSwitch", "PictureSwitch", "Same"))) %>%
  mutate(TVCondition = factor(TVCondition, levels = c("NoVariability", "WithinTalker", "BetweenTalker"))) %>%
  mutate(dialect = ifelse(Parent1Dialect == Parent2Dialect, 0, 1))

tvs_test_graph_exp2 <- tvs_test_exp2 %>%
  mutate(StimName = factor(StimName, levels = c("Same", "MispronunciationSwitch", "WordSwitch", "PictureSwitch"))) %>%
  rename(Trial = StimName) %>%
  mutate(TotalLook = TotalLook/1000) %>%
  mutate(dialect = ifelse(Parent1Dialect == Parent2Dialect, 0, 1))

experiment_results_exp2 <- ggplot(data = tvs_test_graph_exp2, aes(x = Trial, y = TotalLook, color = Trial)) + stat_summary(fun="mean", geom="bar", position ="dodge", fill = "white") + labs(y = "Mean looking time (s)") + geom_point(shape = 1, color = "grey") + facet_wrap(~TVCondition) + theme_bw() + stat_summary(fun.data = mean_se, geom = "errorbar",width=.2, position=position_dodge(.9)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(legend.position ="none")

ggplot(data = tvs_test_graph_exp2, aes(x = Trial, y = TotalLook, color = Trial)) + stat_summary(fun="mean", geom="bar", position ="dodge", fill = "white") + labs(y = "Mean looking time (s)") + geom_point(shape = 1, color = "grey") + facet_wrap(~exp2_lab + TVCondition) + theme_bw() + stat_summary(fun.data = mean_se, geom = "errorbar",width=.2, position=position_dodge(.9)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(legend.position ="none")

habituation_graph_exp2 <- ggplot(data = tvs_habituation_exp2, aes(x = TVCondition, y = trials_to_habit)) + stat_summary(fun="mean", geom = "bar", position = "dodge", fill = "white", color = "black") + labs(y = "Mean trials to habituate") + geom_point(shape = 1, color = "grey")+ theme_bw() + stat_summary(fun.data = mean_se, geom = "errorbar",width=.2, position=position_dodge(.9))+ theme(axis.title.x=element_blank()) + theme(legend.position ="none")+ geom_bracket(xmin = "NoVariability", xmax = "WithinTalker", y.position = 20, label = "n.s.", bracket.shorten = 0.25) + geom_bracket(xmin = "WithinTalker", xmax = "BetweenTalker", y.position = 20, label = "n.s.", bracket.shorten = 0.25) + geom_bracket(xmin = "NoVariability", xmax = "BetweenTalker", y.position = 25, label = "n.s.")

options(contrasts=c("contr.sum","contr.poly"))

c1 <- c(1, -0.5, -0.5)
c2 <- c(0, -.5, .5)

contrasts <- rbind(constant=1/3, c1, c2)
contrasts
contrasts <-solve(contrasts)
contrasts <- contrasts[ , -1]

contrasts(tvs_test_exp2$TVCondition) <- contrasts

c3 <- c(1, 0, 0, -1)
c4 <- c(0, 1, 0, -1)
c5 <- c(0, 0, 1, -1)

contrasts_trial <- rbind(constant = 1/4, c3, c4, c5)
contrasts_trial <- solve(contrasts_trial)
contrasts_trial <- contrasts_trial[,-1]

contrasts(tvs_test_exp2$StimName) <- contrasts_trial

model_exp2 <- lmerTest::lmer(TotalLook ~ StimName*TVCondition +  (1|Pair) + (1|SubjectID), data = tvs_test_exp2)
summary(model_exp2)
plot(resid(model_exp2))
#apa_model <- apa_print(model)
model_output_exp2 <- tidy(model_exp2)

model_exp2_lab_zoom <- lmerTest::lmer(TotalLook ~ StimName*TVCondition + exp2_lab + (1|Pair) + (1|SubjectID), data = tvs_test_exp2)
summary(model_exp2_lab_zoom)
plot(resid(model_exp2_lab_zoom))
#apa_model <- apa_print(model)
model_output_exp2_lab_zoom <- tidy(model_exp2_lab_zoom)

model_exp2_lab_zoom_d <- EMAtools::lme.dscore(model_exp2_lab_zoom, tvs_test_exp2, type = "lme4") %>% as_tibble() %>%
  add_column(term = c("StimNamec3", "StimNamec4", "StimNamec5", "TVConditionc1", "TVConditionc2", "exp2_lab1", "StimNamec3:TVConditionc1", "StimNamec4:TVConditionc1", "StimNamec5:TVConditionc1", "StimNamec3:TVConditionc2", "StimNamec4:TVConditionc2", "StimNamec5:TVConditionc2"))%>%
  select(term, d)

model_exp2_table_lab_zoom <- broom.mixed::tidy(model_exp2_lab_zoom, effects = "fixed") %>%
  left_join(model_exp2_lab_zoom_d)

t.test(tvs_test_exp2$TotalLook[tvs_test_exp2$dialect == 0 & tvs_test_exp2$StimName=="WordSwitch" & tvs_test_exp2$TVCondition == "WithinTalker"])

t.test(tvs_test_exp2$TotalLook[tvs_test_exp2$StimName=="Same"], tvs_test_exp2$TotalLook[tvs_test_exp2$StimName=="WordSwitch"], paired = T)

mean(tvs_test_exp2$TotalLook[tvs_test_exp2$StimName=="Same"])
mean(tvs_test_exp2$TotalLook[tvs_test_exp2$StimName=="WordSwitch"])

model_exp2_d <- EMAtools::lme.dscore(model_exp2, tvs_test_exp2, type = "lme4") %>% as_tibble() %>%
  add_column(term = c("StimNamec3", "StimNamec4", "StimNamec5", "TVConditionc1", "TVConditionc2", "StimNamec3:TVConditionc1", "StimNamec4:TVConditionc1", "StimNamec5:TVConditionc1", "StimNamec3:TVConditionc2", "StimNamec4:TVConditionc2", "StimNamec5:TVConditionc2"))%>%
  select(term, d)

model_exp2_table <- broom.mixed::tidy(model_exp2, effects = "fixed") %>%
  left_join(model_exp2_d)


test_habit_trials_by_pair_exp2 <- tidy(anova(lm(TotalLook ~ Pair*StimName, data = tvs_test_exp2)))
test_habit_trials_by_order_exp2 <- tidy(anova(lm(TotalLook ~ Order, data = tvs_test_exp2)))

summary(lm(TotalLook ~ Pair*StimName, data = tvs_test_exp2))

ggplot(data = tvs_test_graph_exp2, aes(x = Trial, y = TotalLook, color = Trial)) + stat_summary(fun="mean", geom="bar", position ="dodge", fill = "white") + labs(y = "Mean looking time (s)") + geom_point(shape = 1, color = "grey") + facet_wrap(~Pair +TVCondition) + theme_bw() + stat_summary(fun.data = mean_se, geom = "errorbar",width=.2, position=position_dodge(.9)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(legend.position ="none")

ggplot(data = tvs_test_graph_exp1, aes(x = Trial, y = TotalLook, color = Trial)) + stat_summary(fun="mean", geom="bar", position ="dodge", fill = "white") + labs(y = "Mean looking time (s)") + geom_point(shape = 1, color = "grey") + facet_wrap(~Pair + TVCondition) + theme_bw() + stat_summary(fun.data = mean_se, geom = "errorbar",width=.2, position=position_dodge(.9)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(legend.position ="none")

same_word_B_exp2 <- apa_print(t.test(tvs_test_exp2$TotalLook[tvs_test_exp2$StimName == "Same" & tvs_test_exp2$TVCondition=="BetweenTalker"], tvs_test_exp2$TotalLook[tvs_test_exp2$StimName == "WordSwitch" & tvs_test_exp2$TVCondition=="BetweenTalker"], paired = T))

same_word_W_exp2 <- apa_print(t.test(tvs_test_exp2$TotalLook[tvs_test_exp2$StimName == "Same" & tvs_test_exp2$TVCondition=="WithinTalker"], tvs_test_exp2$TotalLook[tvs_test_exp2$StimName == "WordSwitch" & tvs_test_exp2$TVCondition=="WithinTalker"], paired = T))

diff_scores_exp2 <- tvs_test_exp2 %>%
  group_by(SubjectID) %>%
  spread(StimName, TotalLook) %>%
  mutate(MP_same = MispronunciationSwitch - Same,
         ) %>%
  mutate(Sex = factor(Sex)) %>%
  filter(!is.na(WordsUnderstood))

contrasts(diff_scores_exp2$TVCondition) <- contrasts

diff_score_model_all_exp2 <- lm(MP_same ~ Age_days + Sex + WordsUnderstood + TVCondition, data = diff_scores_exp2)
diff_score_model_tv_exp2 <- lm(MP_same ~ TVCondition, data = diff_scores_exp2)
diff_score_model_all_glance_exp2 <- diff_score_model_all_exp2 %>% glance()
diff_score_model_all_summary_glance_exp2 <- tidy(diff_score_model_all_exp2)
diff_score_model_tv_glance_exp2 <- diff_score_model_tv_exp2 %>% glance()
summary(diff_score_model_tv_exp2)
summary(diff_score_model_all_exp2)
model_comparison_exp2 <- tidy(anova(diff_score_model_tv_exp2, diff_score_model_all_exp2))

tvs_habituation_exp12 <- tvs_habituation_exp1 %>%
  rbind(tvs_habituation_exp2) %>%
  rbind(tvs_habituation_online)

habit_trials_by_condition_exp12 <- lm(trials_to_habit ~ TVCondition, data = tvs_habituation_exp12)
habit_trials_by_condition_anova_exp12 <- anova(habit_trials_by_condition_exp12)

habit_no_within_exp12 <- t.test(tvs_habituation_exp12$trials_to_habit[tvs_habituation_exp12$TVCondition=="NoVariability"],tvs_habituation_exp12$trials_to_habit[tvs_habituation_exp12$TVCondition=="WithinTalker"], paired = F)

habit_no_between_exp12 <- t.test(tvs_habituation_exp12$trials_to_habit[tvs_habituation_exp12$TVCondition=="NoVariability"],tvs_habituation_exp12$trials_to_habit[tvs_habituation_exp12$TVCondition=="BetweenTalker"], paired = F)
habit_within_between_exp12 <- t.test(tvs_habituation_exp12$trials_to_habit[tvs_habituation_exp12$TVCondition=="BetweenTalker"],tvs_habituation_exp12$trials_to_habit[tvs_habituation_exp12$TVCondition=="WithinTalker"], paired = F)

habituation_across_exps <- ggplot(data = tvs_habituation_exp12, aes(x = TVCondition, y = trials_to_habit)) + stat_summary(fun="mean", geom = "bar", position = "dodge", fill = "white", color = "black") + labs(y = "Mean trials to habituate") + geom_point(shape = 1, color = "grey")+ theme_bw() + stat_summary(fun.data = mean_se, geom = "errorbar",width=.2, position=position_dodge(.9))+ theme(axis.title.x=element_blank()) + theme(legend.position ="none")+ geom_bracket(xmin = "NoVariability", xmax = "WithinTalker", y.position = 20, label = "p=.01", bracket.shorten = 0.25) + geom_bracket(xmin = "WithinTalker", xmax = "BetweenTalker", y.position = 20, label = "p=.04", bracket.shorten = 0.25) + geom_bracket(xmin = "NoVariability", xmax = "BetweenTalker", y.position = 25, label = "n.s.")

tvs_test_exp12 <- tvs_test_exp1 %>%
  rbind(tvs_test_exp2) %>%
  rbind(tvs_test_online)%>%
  filter(StimName %in% c("Same", "WordSwitch")) %>%
  mutate(StimName = factor(StimName, levels = c("WordSwitch", "Same"))) %>%
  droplevels() %>%
  mutate(Experiment_combined = ifelse(Experiment %in% c("1a", "1b"), 1, 2))

contrasts(tvs_test_exp12$TVCondition) <- contrasts

c6 <- c(1, 0, -1)
c7 <- c(0, 1, -1)

contrasts_trial_both <- rbind(constant = 1/3, c6, c7)
contrasts_trial_both <- solve(contrasts_trial_both)
contrasts_trial_both <- contrasts_trial_both[,-1]

contrasts <- rbind(constant=1/3, c1, c2)
contrasts
contrasts <-solve(contrasts)
contrasts <- contrasts[ , -1]

model_exp12 <- lmerTest::lmer(TotalLook ~ StimName*TVCondition + Experiment_combined + (1|Pair) + (1|SubjectID), data = tvs_test_exp12)
anova(model_exp12)
summary(model_exp12)
model_output_exp12 <- tidy(model_exp12)

model_exp12_d <- EMAtools::lme.dscore(model_exp12, tvs_test_exp12, type = "lme4") %>% as_tibble()%>%
  add_column(term = c("StimName1", "TVConditionc1", "TVConditionc2", "Experiment_combined", "StimName1:TVConditionc1", "StimName1:TVConditionc2"))%>%
  select(term, d)

model_exp12_table <- broom.mixed::tidy(model_exp12, effects = "fixed") %>%
  left_join(model_exp12_d)

ggplot(tvs_test_exp12, aes(y = TotalLook, x = TVCondition)) + stat_summary(fun="mean", geom="bar", position ="dodge", fill = "white") + labs(y = "Mean looking time (s)") + geom_point(shape = 1, color = "grey") + theme_bw() + stat_summary(fun.data = mean_se, geom = "errorbar",width=.2, position=position_dodge(.9)) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + theme(legend.position ="none")

same_word_novar_exp12 <- apa_print(t.test(tvs_test_exp12$TotalLook[tvs_test_exp12$StimName == "Same" & tvs_test_exp12$TVCondition=="NoVariability"], tvs_test_exp12$TotalLook[tvs_test_exp12$StimName == "WordSwitch" & tvs_test_exp12$TVCondition=="NoVariability"], paired = T))

same_word_var_exp12 <- apa_print(t.test(tvs_test_exp12$TotalLook[tvs_test_exp12$StimName == "Same" & tvs_test_exp12$TVCondition!="NoVariability"], tvs_test_exp12$TotalLook[tvs_test_exp12$StimName == "WordSwitch" & tvs_test_exp12$TVCondition!="NoVariability"], paired = T))

test_habit_trials_by_pair_exp2 <- tidy(anova(lm(trials_to_habit ~ Pair, data = tvs_habituation_exp2)))
test_habit_trials_by_pair_exp12 <- tidy(anova(lm(trials_to_habit ~ Pair, data = tvs_habituation_exp12)))

test_habit_trials_by_pair_exp12 <- tidy(anova(lm(TotalLook ~ Pair*StimName*Experiment, data = tvs_test_exp12)))
test_habit_trials_by_order_exp12 <- tidy(anova(lm(TotalLook ~ Order, data = tvs_test_exp12)))

