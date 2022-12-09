# load packages -----
library(lme4)
library(lmerTest)
library(psych)
library(plyr)
library(sjPlot)
library(ggplot2)
library(performance)
library(see)
library(patchwork)
library(jtools)
library(interactions)
library(tidyverse)

# read in data ------
data <- read.csv("data.csv")
# View(data) # view dataset

# Change value of the variable "stim_chose"; probability to chose the stimuli with the higher prob to sit vs stand
data$stim_chose_fact <- revalue(data$stim_chose, c("prob_sit"="sit", "prob_stnd"="stand"))
data$stim_chose_fact <- as.factor(as.character(data$stim_chose_fact))
table(data$stim_chose)
table(data$stim_chose_fact) # to check correct conversion

# Change value of the variable "reward"; if the trial result in a reward or not.
data$reward_fact <- as.factor(as.character(data$reward))
data$reward_fact <- revalue(data$reward_fact, c("TRUE"= "reward", "FALSE"=" no reward"))
data$reward_fact <- as.factor(as.character(data$reward_fact))
table(data$reward)
table(data$reward_fact) # to check correct conversion

# Change value of the variable "type"; if the trial result in a sit vs a stand trial
data$type_fact <- revalue(data$type, c("stand"="stand", "sit"="sit"))
data$type_fact <- as.factor(as.character(data$type_fact))
table(data$type)
table(data$type_fact) # to check correct conversion

# Descriptive statistics
hist(data$rewp_230)
describe(data$rewp_230)

hist(data$rewp_250)
describe(data$rewp_250)

hist(data$rewp_cz_cpz_pz)
describe(data$rewp_cz_cpz_pz)

##### Centering and scaling (z score)
hist(data$trial)
describe(data$trial)
data$trial_z <- scale (data$trial, scale = TRUE)
hist(data$trial_z)
describe(data$trial_z)

########################################
####### Registered Primary Analyses
########################################

#### Empty model
m_rewp_230_empty <- lmer(rewp_230 ~  1 + (1 | subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_empty)

#### Model including reward and type 
m_rewp_230_main <- lmer(rewp_230 ~  1 + reward_fact*type_fact + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_main)
check_model(m_rewp_230_main)
plot_model(m_rewp_230_main, type="pred", terms=c("reward_fact", "type_fact")) 
### thus, main effect of reward as expected (H1).However, no interaction with the type of trials 

############
####  sensitivity analyses (i.e., rewp at 250 and with cz, cpz, and pz)
##### 250
m_rewp_250_main <- lmer(rewp_250 ~  1 + reward_fact*type_fact + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_250_main)
check_model(m_rewp_250_main)
### Consistent with the main analysis. Main effect of reward as expected (H1). 
### However,no interaction with the type of trials (i.e., the main hypothesis)

###### rewp_cz_cpz_pz
# Including all random effects
m_rewp_cz_cpz_pz_main <- lmer(rewp_cz_cpz_pz ~  1 + reward_fact*type_fact + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_cz_cpz_pz_main)
check_model(m_rewp_cz_cpz_pz_main)
plot_model(m_rewp_cz_cpz_pz_main, type="pred", terms=c("reward_fact", "type_fact")) 
### results are sightly different. We observe a significant reward x trial type interaction. 
# The reward positivity for the rewarding trial is higher when the trial is stand rather than sit.

# change the reference for reward
data$ref_reward <- relevel(data$reward_fact, ref = "reward")
m_rewp_cz_cpz_pz_main_ref_reward <- lmer(rewp_cz_cpz_pz ~  1 + ref_reward*type_fact + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_cz_cpz_pz_main_ref_reward)

########################################
####### Registered Secondary analyses 
########################################

# H2.1: we hypothesize that the effect is larger in participants who are typically less physically active
# H2.2: we hypothesize that the effect is larger in participants who are physically active on the day of the experiment prior to the experiment
# H2.3: we hypothesize that the effect is larger after energetically demanding behavior (i.e., squatting) during the experiment
# H3: the probability of choosing the stimulus more likely to lead to sitting than standing will increase as the number of trials increases 
# H4: we hypothesize that a large positive reward-prediction error reinforces the decision that led to it (i.e., the participant should choose the same stimulus)

###############
# H2.1:
###############
# standardize the variable usual mvpa
hist(data$typical_mvpa)
data$typical_mvpa_z <- scale (data$typical_mvpa, scale = TRUE)
describe(data$typical_mvpa_z)

# transformation for mvpa
data$typical_mvpa_t <- data$typical_mvpa +1
hist(data$typical_mvpa_t)
qqnorm(data$typical_mvpa_t)     
boxcox(data$typical_mvpa_t~1)              
p_typical_mvpa_Trans<-powerTransform(data$typical_mvpa_t)  
data$p_typical_mvpa_Trans<-bcPower(data$typical_mvpa_t,p_typical_mvpa_Trans$lambda) 
qqnorm(data$p_typical_mvpa_Trans)  		
hist(data$p_typical_mvpa_Trans)
data$p_typical_mvpa_Trans_z <- scale (data$p_typical_mvpa_Trans, scale = TRUE)
describe(data$p_typical_mvpa_Trans_z)

# standardize the variable usual walking
hist(data$typical_walking)
data$typical_walking_z <- scale (data$typical_walking, scale = TRUE)
describe(data$typical_walking_z)

# transformation for usual walking
data$typical_walking_t <- data$typical_walking + 1
boxcox(data$typical_walking_t~1)              
p_typical_walking_Trans<-powerTransform(data$typical_walking_t)  
data$p_typical_walking_Trans<-bcPower(data$typical_walking_t,p_typical_walking_Trans$lambda) 
qqnorm(data$p_typical_walking_Trans)  		
hist(data$p_typical_walking_Trans)
data$p_typical_walking_Trans_z <- scale (data$p_typical_walking_Trans, scale = TRUE)
describe(data$p_typical_walking_Trans_z)

# standardize the variable usual sitting
hist(data$typical_sitting)
data$typical_sitting_z <- scale (data$typical_sitting, scale = TRUE)
describe(data$typical_sitting_z)

# transformation for usual sitting
data$typical_sitting_t <- data$typical_sitting + 1
boxcox(data$typical_sitting_t~1)              
p_typical_sitting_Trans<-powerTransform(data$typical_sitting_t)  
data$p_typical_sitting_Trans<-bcPower(data$typical_sitting_t,p_typical_sitting_Trans$lambda) 
qqnorm(data$p_typical_sitting_Trans)  		
hist(data$p_typical_sitting_Trans)
data$p_typical_sitting_Trans_z <- scale (data$p_typical_sitting_Trans, scale = TRUE)
describe(data$p_typical_sitting_Trans_z)

# with mvpa
m_rewp_230_ipaq.mvpa.typical_z <- lmer(rewp_230 ~  1 + reward_fact*type_fact*typical_mvpa_z  + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ipaq.mvpa.typical_z)

m_rewp_230_ipaq.mvpa.typical_trans_z <- lmer(rewp_230 ~  1 + reward_fact*type_fact*p_typical_mvpa_Trans_z  + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ipaq.mvpa.typical_trans_z)
plot_model(m_rewp_230_ipaq.mvpa.typical_trans_z, type="pred", terms=c("p_typical_mvpa_Trans_z", "type_fact")) 

# with walking
m_rewp_230_ipaq.walking.typical_z <- lmer(rewp_230 ~  1 + reward_fact*type_fact*typical_walking_z  + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ipaq.walking.typical_z)

m_rewp_230_ipaq.walking.typical_trans_z <- lmer(rewp_230 ~  1 + reward_fact*type_fact*p_typical_walking_Trans_z  + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ipaq.walking.typical_trans_z)

# with sitting
m_rewp_230_ipaq.sitting.typical_z <- lmer(rewp_230 ~  1 + reward_fact*type_fact*typical_sitting_z  + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ipaq.sitting.typical_z)

m_rewp_230_ipaq.sitting.typical_trans_z <- lmer(rewp_230 ~  1 + reward_fact*type_fact*p_typical_sitting_Trans_z  + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ipaq.sitting.typical_trans_z)
plot_model(m_rewp_230_ipaq.sitting.typical_trans_z, type="pred", terms=c("p_typical_sitting_Trans_z", "reward_fact")) 


###############
# H2.2:
################
# standardize the variable usual mvpa
hist(data$today_mvpa)
data$today_mvpa_z <- scale (data$today_mvpa, scale = TRUE)
describe(data$today_mvpa_z)

# transformation for today mvpa
data$today_mvpa_t <- data$today_mvpa +1
hist(data$today_mvpa_t)
qqnorm(data$today_mvpa_t)     
boxcox(data$today_mvpa_t~1)              
p_today_mvpa_Trans<-powerTransform(data$today_mvpa_t)  
data$p_today_mvpa_Trans<-bcPower(data$today_mvpa_t,p_today_mvpa_Trans$lambda) 
qqnorm(data$p_today_mvpa_Trans)  		
hist(data$p_today_mvpa_Trans)
data$p_today_mvpa_Trans_z <- scale (data$p_today_mvpa_Trans, scale = TRUE)
describe(data$p_today_mvpa_Trans_z)

# standardize the variable today walking
hist(data$today_walking)
data$today_walking_z <- scale (data$today_walking, scale = TRUE)
describe(data$today_walking_z)

# transformation for today walking
data$today_walking_t <- data$today_walking + 1
boxcox(data$today_walking_t~1)              
p_today_walking_Trans<-powerTransform(data$today_walking_t)  
data$p_today_walking_Trans<-bcPower(data$today_walking_t,p_today_walking_Trans$lambda) 
qqnorm(data$p_today_walking_Trans)  		
hist(data$p_today_walking_Trans)
data$p_today_walking_Trans_z <- scale (data$p_today_walking_Trans, scale = TRUE)
describe(data$p_today_walking_Trans_z)

# standardize the variable usual sitting
hist(data$today_sitting)
data$today_sitting_z <- scale (data$today_sitting, scale = TRUE)
describe(data$today_sitting_z)

# transformation for usual sitting
data$today_sitting_t <- data$today_sitting + 1
boxcox(data$today_sitting_t~1)              
p_today_sitting_Trans<-powerTransform(data$today_sitting_t)  
data$p_today_sitting_Trans<-bcPower(data$today_sitting_t,p_today_sitting_Trans$lambda) 
qqnorm(data$p_today_sitting_Trans)  		
hist(data$p_today_sitting_Trans)
data$p_today_sitting_Trans_z <- scale (data$p_today_sitting_Trans, scale = TRUE)
describe(data$p_today_sitting_Trans_z)

# with mvpa
m_rewp_230_ipaq.mvpa.today_z <- lmer(rewp_230 ~  1 + reward_fact*type_fact*today_mvpa_z  + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ipaq.mvpa.today_z)
# three-way interaction between reward, trials and today mvpa
plot_model(m_rewp_230_ipaq.mvpa.today_z, type="pred", terms=c("reward_fact", "type_fact", "today_mvpa_z")) 

m_rewp_230_ipaq.mvpa.today_trans_z <- lmer(rewp_230 ~  1 + reward_fact*type_fact*p_today_mvpa_Trans_z  + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ipaq.mvpa.today_trans_z)

# with walking
m_rewp_230_ipaq.walking.today_z <- lmer(rewp_230 ~  1 + reward_fact*type_fact*today_walking_z  + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ipaq.walking.today_z)

m_rewp_230_ipaq.walking.today_trans_z <- lmer(rewp_230 ~  1 + reward_fact*type_fact*p_today_walking_Trans_z  + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ipaq.walking.today_trans_z)

# with sitting
m_rewp_230_ipaq.sitting.today_z <- lmer(rewp_230 ~  1 + reward_fact*type_fact*today_sitting_z  + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ipaq.sitting.today_z)
# interactive effect between sitting and reward
plot_model(m_rewp_230_ipaq.sitting.today_z, type="pred", terms=c("today_sitting_z", "reward_fact")) 

m_rewp_230_ipaq.sitting.today_trans_z <- lmer(rewp_230 ~  1 + reward_fact*type_fact*p_today_sitting_Trans_z  + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ipaq.sitting.today_trans_z)

# Overall: the higher rewp for stand (vs sit) in the rewarding trial is more pronounced whentoday mvpa is high
# in contrast, the higher rewp for stand (vs sit) in the rewarding trial is more pronounced when today sitting is low


###############
# H2.3:
################
# standardize the variable energy expenditure
hist(data$stdy_enrg_exp)
data$stdy_enrg_exp_z <- scale (data$stdy_enrg_exp, scale = TRUE)
describe(data$stdy_enrg_exp_z)

# transformation energy expenditure
data$stdy_enrg_exp_t <- data$stdy_enrg_exp + 1
boxcox(data$stdy_enrg_exp_t~1)              
p_stdy_enrg_exp_Trans<-powerTransform(data$stdy_enrg_exp_t)  
data$p_stdy_enrg_exp_Trans<-bcPower(data$stdy_enrg_exp_t,p_stdy_enrg_exp_Trans$lambda) 
qqnorm(data$p_stdy_enrg_exp_Trans)  		
hist(data$p_stdy_enrg_exp_Trans)
data$p_stdy_enrg_exp_Trans_z <- scale (data$p_stdy_enrg_exp_Trans, scale = TRUE)
describe(data$p_stdy_enrg_exp_Trans_z)

## sensitivity analyses
# 230
m_rewp_230_ipaq.stdy_enrg_exp_z <- lmer(rewp_230 ~  1 + reward_fact*type_fact*stdy_enrg_exp_z  + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ipaq.stdy_enrg_exp_z)
# a main effect of energy expenditure: higher rewp when energy expenditure increases

m_rewp_230_ipaq.stdy_enrg_exp_trans_z <- lmer(rewp_230 ~  1 + reward_fact*type_fact*p_stdy_enrg_exp_Trans_z  + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ipaq.stdy_enrg_exp_trans_z)


###############
# H3:
################
data$stim_chose
data$stim.chosen_num <- revalue(data$stim_chose, c("prob_sit"="1", "prob_stnd"="0"))
data$stim.chosen_num <- as.numeric(as.character(data$stim.chosen_num))
table(data$stim_chose)
table(data$stim.chosen_num) # to check

m_stim <- glmer (stim.chosen_num ~ trial_z + (trial_z|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim)
# thus, as expected, the probability to chose the square with the higher sit (vs stand) probability increase across the trials. 

###############
# H4:
################
data$chng_respns
data$chng_respns_num <- revalue(data$chng_respns, c("yes"="1", "no"="0"))
data$chng_respns_num <- as.numeric(as.character(data$chng_respns_num))
table(data$chng_respns)
table(data$chng_respns_num) # to check

# Build the variable rewp at trial minus 1
toto <- data %>%
  group_by(subject) %>%
  mutate(rewp_230_lag = lag(rewp_230)) %>%
  ungroup()
totobis <- select(toto, c('rewp_230','rewp_230_lag')) # to check; everyting seems correct.

m_change <- glmer (chng_respns_num ~ rewp_230_lag + (rewp_230_lag|subject), family="binomial", data=toto, na.action=na.omit)
summary(m_change)
# thus, the rewp at the preceding wave did not predict the change response.


########################################
####### Secondary analyses #############
########################################
########### REWP
###################
# for models testing rewp, we add
# how frequently (1) a reward has been received up to the current trial "reward_prob"
# how frequently (2) a reward has been received when choosing a certain stimulus up to the current trial 

# standardize these variables
hist(data$reward_prob)
data$reward_prob_z <- scale (data$reward_prob, scale = TRUE)
describe(data$reward_prob_z)

hist(data$sit_stim_reward_prob)
data$sit_stim_reward_prob_z <- scale (data$sit_stim_reward_prob, scale = TRUE)
describe(data$sit_stim_reward_prob_z)

hist(data$stnd_stim_reward_prob)
data$stnd_stim_reward_prob_z <- scale (data$stnd_stim_reward_prob, scale = TRUE)
describe(data$stnd_stim_reward_prob_z)

hist(data$sit_trial_reward_prob)
data$sit_trial_reward_prob_z <- scale (data$sit_trial_reward_prob, scale = TRUE)
describe(data$sit_trial_reward_prob_z)

hist(data$stnd_trial_reward_prob)
data$stnd_trial_reward_prob_z <- scale (data$stnd_trial_reward_prob, scale = TRUE)
describe(data$stnd_trial_reward_prob_z)

hist(data$stimulus_trial_type_prob)
data$stimulus_trial_type_prob_z <- scale (data$stimulus_trial_type_prob, scale = TRUE)
describe(data$stimulus_trial_type_prob_z)

# moderation by each variables separately
m_rewp_230_main_adjusted_1 <- lmer(rewp_230 ~  1 + reward_fact*type_fact*reward_prob_z + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_main_adjusted_1)

m_rewp_230_main_adjusted_2 <- lmer(rewp_230 ~  1 + reward_fact*type_fact*sit_stim_reward_prob_z + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_main_adjusted_2)

m_rewp_230_main_adjusted_3 <- lmer(rewp_230 ~  1 + reward_fact*type_fact*stnd_stim_reward_prob_z + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_main_adjusted_3)

m_rewp_230_main_adjusted_4 <- lmer(rewp_230 ~  1 + reward_fact*type_fact*sit_trial_reward_prob_z + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_main_adjusted_4)

m_rewp_230_main_adjusted_5 <- lmer(rewp_230 ~  1 + reward_fact*type_fact*stnd_trial_reward_prob_z + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_main_adjusted_5)

m_rewp_230_main_adjusted_6 <- lmer(rewp_230 ~  1 + reward_fact*type_fact*stnd_trial_reward_prob_z + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_main_adjusted_6)

m_rewp_230_main_adjusted_7 <- lmer(rewp_230 ~  1 + reward_fact*type_fact +
                                             reward_fact*sit_stim_reward_prob_z +   reward_fact*stnd_stim_reward_prob_z +
                                               type_fact*sit_trial_reward_prob_z +   type_fact*stnd_trial_reward_prob_z + trial_z+
                                     (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_main_adjusted_7)


# Based on the Matt M's suggestions
data$Proba_sit_stim_rel_stand <- data$sit_stim_reward_prob - data$stnd_stim_reward_prob
data[, c("Proba_sit_stim_rel_stand", "sit_stim_reward_prob", "stnd_stim_reward_prob")] # to check

data$Proba_sit_trial_rel_stand  <- data$sit_trial_reward_prob - data$stnd_trial_reward_prob
data[, c("Proba_sit_trial_rel_stand", "sit_trial_reward_prob", "stnd_trial_reward_prob")]# to check

# standardization of these variables
hist(data$Proba_sit_stim_rel_stand)
data$Proba_sit_stim_rel_stand_z <- scale (data$Proba_sit_stim_rel_stand, scale = TRUE)
describe(data$Proba_sit_stim_rel_stand_z)

hist(data$Proba_sit_trial_rel_stand)
data$Proba_sit_trial_rel_stand_z <- scale (data$Proba_sit_trial_rel_stand, scale = TRUE)
describe(data$Proba_sit_trial_rel_stand_z)

m_rewp_230_main_adjusted_8 <- lmer(rewp_230 ~  1 + reward_fact*type_fact*Proba_sit_stim_rel_stand_z + (1 | subject) + (1 | reward_fact:subject) + trial_z + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_main_adjusted_8)

m_rewp_230_main_adjusted_9 <- lmer(rewp_230 ~  1 + reward_fact*type_fact*Proba_sit_trial_rel_stand_z + (1 | subject) + (1 | reward_fact:subject) +  trial_z +(1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_main_adjusted_9)

m_rewp_230_main_adjusted_10 <- lmer(rewp_230 ~  1 + reward_fact*type_fact*Proba_sit_stim_rel_stand_z + reward_fact*type_fact*Proba_sit_trial_rel_stand_z + trial_z + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_main_adjusted_10)


#######################
########### Change stimuli chosen
###################
hist(data$stimulus_trial_type_prob)
data$stimulus_trial_type_prob_z <- scale (data$stimulus_trial_type_prob, scale = TRUE)
describe(data$stimulus_trial_type_prob_z)

m_stim_adjusted <- glmer (stim.chosen_num ~ trial_z*stimulus_trial_type_prob_z + (trial_z|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_adjusted)


#######################
########### rewp on the previous trials
###################
hist(toto$rewp_230_lag)
toto$rewp_230_lag_z <- scale (toto$rewp_230_lag, scale = TRUE)
describe(toto$rewp_230_lag_z)

# previous trials type (sit vs stand)
# previous reward (reward vs no reward)

m_change_adjusted_1 <- glmer (chng_respns_num ~ rewp_230_lag_z*previous_trial_type + (rewp_230_lag_z|subject), family="binomial", data=toto, na.action=na.omit)
summary(m_change_adjusted_1)
# thus, the rewp at the preceding wave did not predict the change response.

m_change_adjusted_2 <- glmer (chng_respns_num ~ rewp_230_lag_z*previous_reward + (rewp_230_lag_z|subject), family="binomial", data=toto, na.action=na.omit)
summary(m_change_adjusted_2)

m_change_adjusted_3 <- glmer (chng_respns_num ~ rewp_230_lag_z*previous_trial_type*previous_reward + (rewp_230_lag_z|subject), family="binomial", data=toto, na.action=na.omit)
summary(m_change_adjusted_3)


###################################################
####### Additional Exploratory analyses ###########
###################################################
########
##### Predicting rewp
# sex
m_rewp_230_sex <- lmer(rewp_230 ~  1 + reward_fact*type_fact*sex  + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_sex)

# age
hist(data$age)
data$age_z <- scale (data$age, scale = TRUE)
describe(data$age_z)
m_rewp_230_age <- lmer(rewp_230 ~  1 + reward_fact*type_fact*age_z  + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_age)

# bmi
hist(data$bmi)
data$bmi_z <- scale (data$bmi, scale = TRUE)
describe(data$bmi_z)
m_rewp_230_bmi <- lmer(rewp_230 ~  1 + reward_fact*type_fact*bmi_z  + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_bmi)

# ex_dep_avg
hist(data$ex_dep_avg)
data$ex_dep_avg_z <- scale (data$ex_dep_avg, scale = TRUE)
describe(data$ex_dep_avg_z)
m_rewp_230_ex_dep_avg <- lmer(rewp_230 ~  1 + reward_fact*type_fact*ex_dep_avg_z  + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ex_dep_avg)

# ex_att_inst_avg
hist(data$ex_att_inst_avg)
data$ex_att_inst_avg_z <- scale (data$ex_att_inst_avg, scale = TRUE)
describe(data$ex_att_inst_avg_z)
m_rewp_230_ex_att_inst_avg <- lmer(rewp_230 ~  1 + reward_fact*type_fact*ex_att_inst_avg_z  + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ex_att_inst_avg)

# ex_att_aff_avg
hist(data$ex_att_aff_avg)
data$ex_att_aff_avg_z <- scale (data$ex_att_aff_avg, scale = TRUE)
describe(data$ex_att_aff_avg_z)
m_rewp_230_ex_att_aff_avg <- lmer(rewp_230 ~  1 + reward_fact*type_fact*ex_att_aff_avg_z  + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ex_att_aff_avg)

# pre_cust_fat_avg
hist(data$pre_cust_fat_avg)
data$pre_cust_fat_avg_z <- scale (data$pre_cust_fat_avg, scale = TRUE)
describe(data$pre_cust_fat_avg_z)
m_rewp_230_pre_cust_fat_avg <- lmer(rewp_230 ~  1 + reward_fact*type_fact*pre_cust_fat_avg_z  + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_pre_cust_fat_avg)

# post_cust_fat_avg
hist(data$post_cust_fat_avg)
data$post_cust_fat_avg_z <- scale (data$post_cust_fat_avg, scale = TRUE)
describe(data$post_cust_fat_avg_z)
m_rewp_230_post_cust_fat_avg <- lmer(rewp_230 ~  1 + reward_fact*type_fact*post_cust_fat_avg_z  + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_post_cust_fat_avg)

# mfi_avg
hist(data$mfi_avg)
data$mfi_avg_z <- scale (data$mfi_avg, scale = TRUE)
describe(data$mfi_avg_z)
m_rewp_230_mfi_avg <- lmer(rewp_230 ~  1 + reward_fact*type_fact*mfi_avg_z  + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_mfi_avg)

# rpe_sit
hist(data$rpe_sit)
data$rpe_sit_z <- scale (data$rpe_sit, scale = TRUE)
describe(data$rpe_sit_z)
m_rewp_230_rpe_sit <- lmer(rewp_230 ~  1 + reward_fact*type_fact*rpe_sit_z  + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_rpe_sit)

# rep_stnd
hist(data$rep_stnd)
data$rep_stnd_z <- scale (data$rep_stnd, scale = TRUE)
describe(data$rep_stnd_z)
m_rewp_230_rep_stnd <- lmer(rewp_230 ~  1 + reward_fact*type_fact*rep_stnd_z  + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_rep_stnd)

# prefer
m_rewp_230_prefer <- lmer(rewp_230 ~  1 + reward_fact*type_fact*prefer  + (1 | subject) + (1 | reward_fact:subject) + (1 | type_fact:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_prefer)


########
##### Stimuli chosen
m_stim_sex <- glmer (stim.chosen_num ~ trial_z*sex + (trial_z|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_sex)

m_stim_age <- glmer (stim.chosen_num ~ trial_z*age_z + (trial_z|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_age)

m_stim_bmi <- glmer (stim.chosen_num ~ trial_z*bmi_z + (trial_z|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_bmi)

m_stim_ex_dep_avg <- glmer (stim.chosen_num ~ trial_z*ex_dep_avg_z + (trial_z|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_ex_dep_avg)

m_stim_ex_att_inst_avg <- glmer (stim.chosen_num ~ trial_z*ex_att_inst_avg_z + (trial_z|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_ex_att_inst_avg)

m_stim_ex_att_aff_avg <- glmer (stim.chosen_num ~ trial_z*ex_att_aff_avg_z + (trial_z|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_ex_att_aff_avg)

m_stim_pre_cust_fat_avg <- glmer (stim.chosen_num ~ trial_z*pre_cust_fat_avg_z + (trial_z|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_pre_cust_fat_avg)

m_stim_post_cust_fat_avg <- glmer (stim.chosen_num ~ trial_z*post_cust_fat_avg_z + (trial_z|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_post_cust_fat_avg)

m_stim_mfi_avg <- glmer (stim.chosen_num ~ trial_z*mfi_avg_z + (trial_z|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_mfi_avg)

m_stim_rpe_sit <- glmer (stim.chosen_num ~ trial_z*rpe_sit_z + (trial_z|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_rpe_sit)

m_stim_rep_stnd <- glmer (stim.chosen_num ~ trial_z*rep_stnd_z + (trial_z|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_rep_stnd)

m_stim_prefer <- glmer (stim.chosen_num ~ trial_z*prefer + (trial_z|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_prefer)

########
##### Change Stimuli chosen
m_change_sex <- glmer (chng_respns_num ~ rewp_230_lag_z*sex + (rewp_230_lag_z|subject), family="binomial", data=toto, na.action=na.omit)
summary(m_change_sex)

toto$age_z <- scale (toto$age, scale = TRUE)
m_change_age <- glmer (chng_respns_num ~ rewp_230_lag_z*age_z + (rewp_230_lag_z|subject), family="binomial", data=toto, na.action=na.omit)
summary(m_change_age)

toto$bmi_z <- scale (toto$bmi, scale = TRUE)
m_change_bmi <- glmer (chng_respns_num ~ rewp_230_lag_z*bmi_z + (rewp_230_lag_z|subject), family="binomial", data=toto, na.action=na.omit)
summary(m_change_bmi)

toto$ex_dep_avg_z <- scale (toto$ex_dep_avg, scale = TRUE)
m_change_ex_dep_avg <- glmer (chng_respns_num ~ rewp_230_lag_z*ex_dep_avg_z + (rewp_230_lag_z|subject), family="binomial", data=toto, na.action=na.omit)
summary(m_change_ex_dep_avg)

toto$ex_att_inst_avg_z <- scale (toto$ex_att_inst_avg, scale = TRUE)
m_change_ex_att_inst_avg <- glmer (chng_respns_num ~ rewp_230_lag_z*ex_att_inst_avg_z + (rewp_230_lag_z|subject), family="binomial", data=toto, na.action=na.omit)
summary(m_change_ex_att_inst_avg)

toto$ex_att_inst_avg_z <- scale (toto$ex_att_inst_avg, scale = TRUE)
m_change_ex_att_inst_avg <- glmer (chng_respns_num ~ rewp_230_lag_z*ex_att_inst_avg_z + (rewp_230_lag_z|subject), family="binomial", data=toto, na.action=na.omit)
summary(m_change_ex_att_inst_avg)

toto$pre_cust_fat_avg_z <- scale (toto$pre_cust_fat_avg, scale = TRUE)
m_change_pre_cust_fat_avg <- glmer (chng_respns_num ~ rewp_230_lag_z*pre_cust_fat_avg_z + (rewp_230_lag_z|subject), family="binomial", data=toto, na.action=na.omit)
summary(m_change_pre_cust_fat_avg)

toto$post_cust_fat_avg_z <- scale (toto$post_cust_fat_avg, scale = TRUE)
m_change_post_cust_fat_avg <- glmer (chng_respns_num ~ rewp_230_lag_z*post_cust_fat_avg_z + (rewp_230_lag_z|subject), family="binomial", data=toto, na.action=na.omit)
summary(m_change_post_cust_fat_avg)

toto$mfi_avg_z <- scale (toto$mfi_avg, scale = TRUE)
m_change_mfi_avg <- glmer (chng_respns_num ~ rewp_230_lag_z*mfi_avg_z + (rewp_230_lag_z|subject), family="binomial", data=toto, na.action=na.omit)
summary(m_change_mfi_avg)

toto$rpe_sit_z <- scale (toto$rpe_sit, scale = TRUE)
m_change_rpe_sit <- glmer (chng_respns_num ~ rewp_230_lag_z*rpe_sit_z + (rewp_230_lag_z|subject), family="binomial", data=toto, na.action=na.omit)
summary(m_change_rpe_sit)

toto$rep_stnd_z <- scale (toto$rep_stnd, scale = TRUE)
m_change_rep_stnd <- glmer (chng_respns_num ~ rewp_230_lag_z*rep_stnd_z + (rewp_230_lag_z|subject), family="binomial", data=toto, na.action=na.omit)
summary(m_change_rep_stnd)

m_change_prefer <- glmer (chng_respns_num ~ rewp_230_lag_z*prefer + (rewp_230_lag_z|subject), family="binomial", data=toto, na.action=na.omit)
summary(m_change_prefer)

