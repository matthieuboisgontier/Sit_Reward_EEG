#load packages -----
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
library(car)
library(MASS)

### read in data ------
data <- read.csv("data.csv")
#View(data) #view dataset

# tidying data  ------
### Change value of the variable "stim_chose"; probability to chose the stimuli with the higher prob to sit vs stand
data$stim_chose_fact <- revalue(data$stim_chose, c("prob_sit"="sit", "prob_stnd"="stand"))
data$stim_chose_fact <- as.factor(as.character(data$stim_chose_fact))
table(data$stim_chose)
table(data$stim_chose_fact) # to check correct conversion

###Change value of the variable "reward"; if the trial result in a reward or not.
data$reward_fact <- as.factor(as.character(data$reward))
data$reward_fact <- revalue(data$reward_fact, c("TRUE"= "reward", "FALSE"="no reward"))
data$reward_fact <- as.factor(as.character(data$reward_fact))
table(data$reward)
table(data$reward_fact) # to check correct conversion

### Remove 9 lines for which there is NO type (and no answers (rewp_230=NA)=
data = data[data$type!="", ]

### Change value of the variable "type"; if the trial result in a sit vs a stand trial
data$type_fact <- revalue(data$type, c("stand"="stand", "sit"="sit"))
data$type_fact <- as.factor(as.character(data$type_fact))
table(data$type)
table(data$type_fact) # to check correct conversion

##### Centering and scaling (z score)
hist(data$trial)
describe(data$trial)
data$trial_z <- scale (data$trial, scale = TRUE)
hist(data$trial_z)
describe(data$trial_z)


#########################
## DESCRIPTIVE STATISTICS
#########################

# Creating binary variable for sex
data$sex01 <- NA
data$sex01[is.element(data$sex ,c("male"))] <- "1"
data$sex01[is.element(data$sex ,c("female"))] <- "0"
unique(data$sex01)
class(data$sex01)
data$sex01_num <- as.numeric(as.character(data$sex01))

# aggrefate data per subject
DataAggreg = aggregate (cbind(age, sex01_num, bmi, typical_mvpa, today_mvpa, stdy_enrg_exp,typical_sitting, rpe_sit, rep_stnd, rewp_250, rewp_230, pre_cust_fat_avg, post_cust_fat_avg, mfi_avg, ex_dep_avg, ex_att_aff_avg, ex_att_inst_avg, aware)
                        ~subject, data=data, FUN=mean, na.rm=TRUE, na.action=na.pass)
describe(DataAggreg)
sum(with(DataAggreg,sex01_num == "0"))

# Reward Positivity (µV) in Reward trials
DataAggreg_rewp_reward = aggregate (cbind(rewp_230)
                        ~subject, data=data, FUN=mean, na.rm=TRUE, na.action=na.pass, subset = reward == "TRUE")
describe(DataAggreg_rewp_reward)

# Reward Positivity (µV) in No Reward trials
DataAggreg_rewp_noreward = aggregate (cbind(rewp_230)
                                    ~subject, data=data, FUN=mean, na.rm=TRUE, na.action=na.pass, subset = reward == "FALSE")
describe(DataAggreg_rewp_noreward)

# Probability to choose the sit stimulus relative to the stand stimulus
data$stim_chose_num <- revalue(data$stim_chose_fact,
                               c("sit" = "1",
                                 "stand" = "0"))
data$stim_chose_num <- as.numeric(as.character(data$stim_chose_num)) # as numeri

DataAggreg_choice = aggregate (cbind(stim_chose_num)
                               ~subject, data=data, FUN=mean, na.rm=TRUE, na.action=na.pass)
describe(DataAggreg_choice)
SD(DataAggreg_choice)

# Probability to change the stimulus choosen
data$chng_respns_num <- revalue(data$chng_respns,
                                c("yes" = "1",
                                  "no" = "0"))
data$chng_respns_num <- as.numeric(as.character(data$chng_respns_num)) # as numeri

DataAggreg_chng_respns = aggregate (cbind(chng_respns_num)
                                    ~subject, data=data, FUN=mean, na.rm=TRUE, na.action=na.pass)
describe(DataAggreg_chng_respns)
SD(DataAggreg_chng_respns)

########################################
####### Registered Primary Analyses
########################################

### Change values assigned to no reward (-1) and reward (1)
data$reward_fact_order <- revalue(data$reward_fact,
                                  c("reward" = "1",
                                    "no reward" = "-1"))
data[,c("reward_fact","reward_fact_order")]
data$reward_num <- as.numeric(as.character(data$reward_fact_order))
class(data$reward_num)

### Change values assigned to sit (-1) and stand (1)
data$type_fact_order <- revalue(data$type_fact,
                                  c("stand" = "1",
                                    "sit" = "-1"))
data[,c("type_fact","type_fact_order")]
data$type_num <- as.numeric(as.character(data$type_fact_order))
class(data$type_num)

#### Empty model
m_rewp_230_empty <- lmer(rewp_230 ~  1 + (1 | subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_empty)

#### Model including reward and type 
m_rewp_230_main <- lmer(rewp_230 ~  1 + reward_num*type_num + (1 | subject) + (1 | reward_num:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_main)
check_model(m_rewp_230_main)
plot_model(m_rewp_230_main, type="pred", terms=c("reward_num", "type_num")) 
confint(m_rewp_230_main)

##################################################
####  sensitivity analyses for reward positivity
##################################################

##### Reward positivity centered on a peak between 250 ms
m_rewp_250_main <- lmer(rewp_250 ~  1 + reward_num*type_num + (1 | subject) + (1 | reward_num:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_250_main)
check_model(m_rewp_250_main)
confint(m_rewp_250_main)

###### rewp with cz cpz pz
## Including all random effects
m_rewp_cz_cpz_pz_main <- lmer(rewp_cz_cpz_pz ~  1 + reward_num*type_num + (1 | subject) + (1 | reward_num:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_cz_cpz_pz_main)
check_model(m_rewp_cz_cpz_pz_main)
plot_model(m_rewp_cz_cpz_pz_main, type="pred", terms=c("reward_num", "type_num")) 
confint(m_rewp_cz_cpz_pz_main)

## Simple effects of reward
data$reward_factor <- as.factor(data$reward_num)
m_rewp_cz_cpz_pz_simple_noreward <- lmer(rewp_cz_cpz_pz ~  1 + reward_factor*type_num + (1 | subject) + (1 | reward_factor:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_cz_cpz_pz_simple_noreward)
confint(m_rewp_cz_cpz_pz_simple_noreward)

data$ref_reward <- relevel(data$reward_factor, ref = "1") #change the reference of the reward variable: no reward to reward
m_rewp_cz_cpz_pz_simple_reward  <- lmer(rewp_cz_cpz_pz ~  1 + ref_reward*type_num + (1 | subject) + (1 | ref_reward:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_cz_cpz_pz_simple_reward)
confint(m_rewp_cz_cpz_pz_simple_reward)

########################################
####### Registered Secondary analyses 
########################################

# H2.1: we hypothesize that the effect is larger in participants who are typically less physically active
# H2.2: we hypothesize that the effect is larger in participants who are physically active on the day of the experiment prior to the experiment
# H2.3: we hypothesize that the effect is larger after energetically demanding behavior (i.e., squatting) during the experiment
# H3: the probability of choosing the stimulus more likely to lead to sitting than standing will increase as the number of trials increases 
# H4: we hypothesize that a large positive reward-prediction error reinforces the decision that led to it (i.e., the participant should choose the same stimulus)

###############
# H2.1: Typical MVPA
###############

## standardize the variable typical mvpa
hist(data$typical_mvpa)
data$typical_mvpa_z <- scale (data$typical_mvpa, scale = TRUE)
describe(data$typical_mvpa_z)

## transformation for mvpa
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

## model with untransformed typical mvpa
m_rewp_230_ipaq.mvpa.typical_z <- lmer(rewp_230 ~  1 + reward_num*type_num*typical_mvpa_z  + (1 | subject) + (1 | reward_num:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ipaq.mvpa.typical_z)
confint(m_rewp_230_ipaq.mvpa.typical_z)

## model with transformed typical mvpa
m_rewp_230_ipaq.mvpa.typical_trans_z <- lmer(rewp_230 ~  1 + reward_num*type_num*p_typical_mvpa_Trans_z  + (1 | subject) + (1 | reward_num:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ipaq.mvpa.typical_trans_z)
confint(m_rewp_230_ipaq.mvpa.typical_trans_z)
plot_model(m_rewp_230_ipaq.mvpa.typical_trans_z, type="pred", terms=c("p_typical_mvpa_Trans_z", "type_num")) 


###############
# H2.2: Today MVPA
################

## standardize the variable usual mvpa
hist(data$today_mvpa)
data$today_mvpa_z <- scale (data$today_mvpa, scale = TRUE)
describe(data$today_mvpa_z)

## BOXCOX transformation for today mvpa
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

## LOG1000 transformation for today mvpa
data$today_mvpa_TransLog<- log(data$today_mvpa+1000)
hist(log(data$today_mvpa+1000))
data$today_mvpa_TransLog_z <- scale (data$today_mvpa_TransLog, scale = TRUE)

## model with non-transformed today mvpa
m_rewp_230_ipaq.mvpa.today_z <- lmer(rewp_230 ~  1 + reward_num*type_num*today_mvpa_z  + (1 | subject) + (1 | reward_num:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ipaq.mvpa.today_z)
plot_model(m_rewp_230_ipaq.mvpa.today_z, type="pred", terms=c("reward_num", "type_num", "today_mvpa_z")) 
confint(m_rewp_230_ipaq.mvpa.today_z)

# simple effects of the 3-way interaction
data$today_mvpa_z_low <- data$today_mvpa_z + 1
m_rewp_230_ipaq.mvpa.today_z_low <- lmer(rewp_230 ~  1 + reward_num*type_num*today_mvpa_z_low  + (1 | subject) + (1 | reward_num:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ipaq.mvpa.today_z_low)
confint(m_rewp_230_ipaq.mvpa.today_z_low)

data$today_mvpa_z_high <- data$today_mvpa_z -1
m_rewp_230_ipaq.mvpa.today_z_high <- lmer(rewp_230 ~  1 + reward_num*type_num*today_mvpa_z_high  + (1 | subject) + (1 | reward_num:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ipaq.mvpa.today_z_high)
confint(m_rewp_230_ipaq.mvpa.today_z_high)

# sit as reference
data$sit_ref <- ifelse(data$type_num == -1, 0,1)
data[,c("type_num","sit_ref")]
m_rewp_230_ipaq.mvpa.today_z_high_sit_ref <- lmer(rewp_230 ~  1 + reward_num*sit_ref*today_mvpa_z_high  + (1 | subject) + (1 | reward_num:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ipaq.mvpa.today_z_high_sit_ref)
confint(m_rewp_230_ipaq.mvpa.today_z_high_sit_ref)

# stand as reference
data$stand_ref <- ifelse(data$type_num == -1, 1,0)
data[,c("type_num","stand_ref")]
m_rewp_230_ipaq.mvpa.today_z_high_stand_ref <- lmer(rewp_230 ~  1 + reward_num*stand_ref*today_mvpa_z_high  + (1 | subject) + (1 | reward_num:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ipaq.mvpa.today_z_high_stand_ref)
confint(m_rewp_230_ipaq.mvpa.today_z_high_stand_ref)

## model with boxcox transformed today mvpa
m_rewp_230_ipaq.mvpa.today_trans_z <- lmer(rewp_230 ~  1 + reward_num*type_num*p_today_mvpa_Trans_z  + (1 | subject) + (1 | reward_num:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ipaq.mvpa.today_trans_z)
confint(m_rewp_230_ipaq.mvpa.today_trans_z)

## model with log1000 transformed today mvpa
m_rewp_230_ipaq.mvpa.today_translog_z <- lmer(rewp_230 ~  1 + reward_num*type_num*today_mvpa_TransLog_z  + (1 | subject) + (1 | reward_num:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ipaq.mvpa.today_translog_z)
confint(m_rewp_230_ipaq.mvpa.today_translog_z)


###############
# H2.3: Study-related energy expenditure
################

## standardize the variable study-related energy expenditure
hist(data$stdy_enrg_exp)
data$stdy_enrg_exp_z <- scale (data$stdy_enrg_exp, scale = TRUE)
describe(data$stdy_enrg_exp_z)

## transformation of study-related energy expenditure
data$stdy_enrg_exp_t <- data$stdy_enrg_exp + 1
boxcox(data$stdy_enrg_exp_t~1)              
p_stdy_enrg_exp_Trans<-powerTransform(data$stdy_enrg_exp_t)  
data$p_stdy_enrg_exp_Trans<-bcPower(data$stdy_enrg_exp_t,p_stdy_enrg_exp_Trans$lambda) 
qqnorm(data$p_stdy_enrg_exp_Trans)  		
hist(data$p_stdy_enrg_exp_Trans)
data$p_stdy_enrg_exp_Trans_z <- scale (data$p_stdy_enrg_exp_Trans, scale = TRUE)
describe(data$p_stdy_enrg_exp_Trans_z)

## model with non-transformed study-related energy expenditure
m_rewp_230_ipaq.stdy_enrg_exp_z <- lmer(rewp_230 ~  1 + reward_num*type_num*stdy_enrg_exp_z  + (1 | subject) + (1 | reward_num:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ipaq.stdy_enrg_exp_z)
confint(m_rewp_230_ipaq.stdy_enrg_exp_z)

## model with transformed study-related energy expenditure
m_rewp_230_ipaq.stdy_enrg_exp_trans_z <- lmer(rewp_230 ~  1 + reward_num*type_num*p_stdy_enrg_exp_Trans_z  + (1 | subject) + (1 | reward_num:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ipaq.stdy_enrg_exp_trans_z)
confint(m_rewp_230_ipaq.stdy_enrg_exp_trans_z)

###############
# H3: Probability of choosing the stimulus with the higher sitting likelihood
################
data$stim_chose
data$stim.chosen_num <- revalue(data$stim_chose, c("prob_sit"="1", "prob_stnd"="0"))
data$stim.chosen_num <- as.numeric(as.character(data$stim.chosen_num))
table(data$stim_chose)
table(data$stim.chosen_num) # to check

m_stim <- glmer (stim.chosen_num ~ trial_z + (trial_z|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim)
confint(m_stim)

## EXploratory analysis of the quadratic effect of trial number
m_stim_quadratic <- glmer (stim.chosen_num ~ trial_z+I(trial_z^2) + (trial_z|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_quadratic)
confint(m_stim_quadratic)


###############
# H4: Influence of reward positivity on the subsequent decision to choose the same (vs. different) stimulus.
################
data$chng_respns
data$chng_respns_num <- revalue(data$chng_respns, c("yes"="1", "no"="0"))
data$chng_respns_num <- as.numeric(as.character(data$chng_respns_num))
table(data$chng_respns)
table(data$chng_respns_num, useNA = "ifany")

## builds the reward positivity variable at trial minus 1
toto <- data %>%
      group_by(subject) %>%
      mutate(rewp_230_lag = lag(rewp_230)) %>%
      ungroup()
totobis <- select(toto, c('rewp_230','rewp_230_lag')) # to check; everytHing seems correct.

m_change <- glmer (chng_respns_num ~ rewp_230_lag + (rewp_230_lag|subject), family="binomial", data=toto, na.action=na.omit)
summary(m_change)
confint(m_change)


########################################
####### Secondary analyses #############
########################################

#####################
##REWP
###################
# for models testing reward positivity (rewp), we add
# how frequently (1) a reward has been received up to the current trial "reward_prob"
# how frequently (2) a reward has been received when choosing a certain stimulus up to the current trial 


# Indicators.of the frequency related to the type of stimulus or the type of trial
data$Proba_sit_stim_rel_stand <- data$sit_stim_reward_prob - data$stnd_stim_reward_prob
data[, c("Proba_sit_stim_rel_stand", "sit_stim_reward_prob", "stnd_stim_reward_prob")] # to check

data$Proba_sit_trial_rel_stand  <- data$sit_trial_reward_prob - data$stnd_trial_reward_prob
data[, c("Proba_sit_trial_rel_stand", "sit_trial_reward_prob", "stnd_trial_reward_prob")]# to check

# standardization of these variables
hist(data$reward_prob)
data$reward_prob_z <- scale (data$reward_prob, scale = TRUE)
describe(data$reward_prob_z)

hist(data$Proba_sit_stim_rel_stand)
data$Proba_sit_stim_rel_stand_z <- scale (data$Proba_sit_stim_rel_stand, scale = TRUE)
describe(data$Proba_sit_stim_rel_stand_z)

hist(data$Proba_sit_trial_rel_stand)
data$Proba_sit_trial_rel_stand_z <- scale (data$Proba_sit_trial_rel_stand, scale = TRUE)
describe(data$Proba_sit_trial_rel_stand_z)

# moderation by reward probability
m_rewp_230_main_adjusted_1 <- lmer(rewp_230 ~  1 + reward_num*type_num*reward_prob_z + (1 | subject) + (1 | reward_num:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_main_adjusted_1)
confint(m_rewp_230_main_adjusted_1)

# moderation by type of stimulus
m_rewp_230_main_adjusted_2 <- lmer(rewp_230 ~  1 + reward_num*type_num*Proba_sit_stim_rel_stand_z + (1 | subject) + (1 | reward_num:subject) + trial_z + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_main_adjusted_2)
confint(m_rewp_230_main_adjusted_2)

# moderation by type of trial
m_rewp_230_main_adjusted_3 <- lmer(rewp_230 ~  1 + reward_num*type_num*Proba_sit_trial_rel_stand_z + (1 | subject) + (1 | reward_num:subject) +  trial_z +(1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_main_adjusted_3)
confint(m_rewp_230_main_adjusted_3)


#######################
## Choice of the stimulus
###################
hist(data$stimulus_trial_type_prob)
data$stimulus_trial_type_prob_z <- scale (data$stimulus_trial_type_prob, scale = TRUE)
describe(data$stimulus_trial_type_prob_z)

m_stim_adjusted <- glmer (stim.chosen_num ~ trial_z*stimulus_trial_type_prob_z + (trial_z|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_adjusted)
confint(m_stim_adjusted)


#######################
### Influence of the reward positivity on the subsequent stimulus choice
###################
hist(toto$rewp_230_lag)
toto$rewp_230_lag_z <- scale (toto$rewp_230_lag, scale = TRUE)
describe(toto$rewp_230_lag_z)

# model adjusting for the previous trial type (sit vs stand)
m_change_adjusted_1 <- glmer (chng_respns_num ~ rewp_230_lag_z*previous_trial_type + (rewp_230_lag_z|subject), family="binomial", data=toto, na.action=na.omit)
summary(m_change_adjusted_1)
confint(m_change_adjusted_1)

# model adjusting for the previous stimulus type (reward vs no reward)
m_change_adjusted_2 <- glmer (chng_respns_num ~ rewp_230_lag_z*previous_reward + (rewp_230_lag_z|subject), family="binomial", data=toto, na.action=na.omit)
summary(m_change_adjusted_2)


###################################################
####### Additional Exploratory analyses ###########
###################################################

#######################
##### Predicting rewp
#######################

# age
hist(data$age)
data$age_z <- scale (data$age, scale = TRUE)
describe(data$age_z)
m_rewp_230_age <- lmer(rewp_230 ~  1 + reward_num*type_num*age_z  + (1 | subject) + (1 | reward_num:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_age)

# sex
m_rewp_230_sex <- lmer(rewp_230 ~  1 + reward_num*type_num*sex  + (1 | subject) + (1 | reward_num:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_sex)

# bmi
hist(data$bmi)
data$bmi_z <- scale (data$bmi, scale = TRUE)
describe(data$bmi_z)
m_rewp_230_bmi <- lmer(rewp_230 ~  1 + reward_num*type_num*bmi_z  + (1 | subject) + (1 | reward_num:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_bmi)

# typical sitting time
hist(data$typical_sitting)
data$typical_sitting_z <- scale (data$typical_sitting, scale = TRUE)
describe(data$typical_sitting_z)
m_rewp_230_typical_sitting <- lmer(rewp_230 ~  1 + reward_num*type_num*typical_sitting_z  + (1 | subject) + (1 | reward_num:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_typical_sitting)

# exercise dependence 
hist(data$ex_dep_avg)
data$ex_dep_avg_z <- scale (data$ex_dep_avg, scale = TRUE)
describe(data$ex_dep_avg_z)
m_rewp_230_ex_dep_avg <- lmer(rewp_230 ~  1 + reward_num*type_num*ex_dep_avg_z  + (1 | subject) + (1 | reward_num:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ex_dep_avg)

# affective attitudes 
hist(data$ex_att_aff_avg)
data$ex_att_aff_avg_z <- scale (data$ex_att_aff_avg, scale = TRUE)
describe(data$ex_att_aff_avg_z)
m_rewp_230_ex_att_aff_avg <- lmer(rewp_230 ~  1 + reward_num*type_num*ex_att_aff_avg_z  + (1 | subject) + (1 | reward_num:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ex_att_aff_avg)

# instrumental attitudes 
hist(data$ex_att_inst_avg)
data$ex_att_inst_avg_z <- scale (data$ex_att_inst_avg, scale = TRUE)
describe(data$ex_att_inst_avg_z)
m_rewp_230_ex_att_inst_avg <- lmer(rewp_230 ~  1 + reward_num*type_num*ex_att_inst_avg_z  + (1 | subject) + (1 | reward_num:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_ex_att_inst_avg)

# custom items fatigue (see Appendix B) before the task
hist(data$pre_cust_fat_avg)
data$pre_cust_fat_avg_z <- scale (data$pre_cust_fat_avg, scale = TRUE)
describe(data$pre_cust_fat_avg_z)
m_rewp_230_pre_cust_fat_avg <- lmer(rewp_230 ~  1 + reward_num*type_num*pre_cust_fat_avg_z  + (1 | subject) + (1 | reward_num:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_pre_cust_fat_avg)

# custom items fatigue (see Appendix B) after the task
hist(data$post_cust_fat_avg)
data$post_cust_fat_avg_z <- scale (data$post_cust_fat_avg, scale = TRUE)
describe(data$post_cust_fat_avg_z)
m_rewp_230_post_cust_fat_avg <- lmer(rewp_230 ~  1 + reward_num*type_num*post_cust_fat_avg_z  + (1 | subject) + (1 | reward_num:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_post_cust_fat_avg)

# Fatigue (multidimensional fatigue inventory)
hist(data$mfi_avg)
data$mfi_avg_z <- scale (data$mfi_avg, scale = TRUE)
describe(data$mfi_avg_z)
m_rewp_230_mfi_avg <- lmer(rewp_230 ~  1 + reward_num*type_num*mfi_avg_z  + (1 | subject) + (1 | reward_num:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_mfi_avg)

# rating of perceived exertion associated with retrieving coins on SIT reward trials
hist(data$rpe_sit)
data$rpe_sit_z <- scale (data$rpe_sit, scale = TRUE)
describe(data$rpe_sit_z)
m_rewp_230_rpe_sit <- lmer(rewp_230 ~  1 + reward_num*type_num*rpe_sit_z  + (1 | subject) + (1 | reward_num:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_rpe_sit)

# rating of perceived exertion associated with retrieving coins on STAND reward trials
hist(data$rep_stnd)
data$rep_stnd_z <- scale (data$rep_stnd, scale = TRUE)
describe(data$rep_stnd_z)
m_rewp_230_rpe_stnd <- lmer(rewp_230 ~  1 + reward_num*type_num*rep_stnd_z  + (1 | subject) + (1 | reward_num:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_rpe_stnd)

# preference for the sit vs. stand trials 
m_rewp_230_prefer <- lmer(rewp_230 ~  1 + reward_num*type_num*prefer  + (1 | subject) + (1 | reward_num:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_prefer)

# ranking IPAQ
quantile(data$typical_mvpa, na.rm= TRUE)
data$quartile_PA <- ntile(data$typical_mvpa, 4)
data[,c("typical_mvpa", "quartile_PA")]
table(data$quartile_PA)
m_rewp_230_quart_PA <- lmer(rewp_230 ~  1 + reward_num*type_num*quartile_PA  + (1 | subject) + (1 | reward_num:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_quart_PA)

#awareness about the fact that one stimulus led to a higher probability of stand (vs. sit) trials relative to the other stimulus
hist(data$aware)
data$aware_z <- scale (data$aware, scale = TRUE)
m_rewp_230_awareness <- lmer(rewp_230 ~  1 + reward_num*type_num*aware_z  + (1 | subject) + (1 | reward_num:subject) + (1 | type_num:subject), data=data, REML=FALSE, na.action=na.omit)
summary(m_rewp_230_awareness)


###################################################
##### Predicting the probability of choosing the stimulus with the higher probability of sitting
###################################################
m_stim_sex <- glmer (stim.chosen_num ~ trial_z*sex + (trial_z|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_sex)

m_stim_age <- glmer (stim.chosen_num ~ trial_z*age_z + (trial_z|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_age)

m_stim_bmi <- glmer (stim.chosen_num ~ trial_z*bmi_z + (trial_z|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_bmi)

m_stim_sitting <- glmer (stim.chosen_num ~ trial_z*typical_sitting_z + (trial_z|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_sitting)

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

m_stim_rpe_stnd <- glmer (stim.chosen_num ~ trial_z*rep_stnd_z + (trial_z|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_rpe_stnd)

m_stim_prefer <- glmer (stim.chosen_num ~ trial_z*prefer + (trial_z|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_prefer)

m_stim_quartPA <- glmer (stim.chosen_num ~ trial_z*quartile_PA + (trial_z|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_quartPA)
confint(m_stim_quartPA)

m_stim_awareness <- glmer (stim.chosen_num ~ trial_z*aware_z + (trial_z|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_awareness)
confint(m_stim_awareness)


###################################################
##### Predicting change in the chosen stimulus
###################################################

m_change_sex <- glmer (chng_respns_num ~ rewp_230_lag_z*sex + (rewp_230_lag_z|subject), family="binomial", data=toto, na.action=na.omit)
summary(m_change_sex)

toto$age_z <- scale (toto$age, scale = TRUE)
m_change_age <- glmer (chng_respns_num ~ rewp_230_lag_z*age_z + (rewp_230_lag_z|subject), family="binomial", data=toto, na.action=na.omit)
summary(m_change_age)

toto$bmi_z <- scale (toto$bmi, scale = TRUE)
m_change_bmi <- glmer (chng_respns_num ~ rewp_230_lag_z*bmi_z + (rewp_230_lag_z|subject), family="binomial", data=toto, na.action=na.omit)
summary(m_change_bmi)

toto$typical_sitting_z <- scale (toto$typical_sitting, scale = TRUE)
m_change_sitting <- glmer (chng_respns_num ~ rewp_230_lag_z*typical_sitting_z + (rewp_230_lag_z|subject), family="binomial", data=toto, na.action=na.omit)
summary(m_change_sitting)

toto$ex_dep_avg_z <- scale (toto$ex_dep_avg, scale = TRUE)
m_change_ex_dep_avg <- glmer (chng_respns_num ~ rewp_230_lag_z*ex_dep_avg_z + (rewp_230_lag_z|subject), family="binomial", data=toto, na.action=na.omit)
summary(m_change_ex_dep_avg)

toto$ex_att_inst_avg_z <- scale (toto$ex_att_inst_avg, scale = TRUE)
m_change_ex_att_inst_avg <- glmer (chng_respns_num ~ rewp_230_lag_z*ex_att_inst_avg_z + (rewp_230_lag_z|subject), family="binomial", data=toto, na.action=na.omit)
summary(m_change_ex_att_inst_avg)

toto$ex_att_aff_avg_z <- scale (toto$ex_att_aff_avg, scale = TRUE)
m_change_ex_att_aff_avg <- glmer (chng_respns_num ~ rewp_230_lag_z*ex_att_aff_avg_z + (rewp_230_lag_z|subject), family="binomial", data=toto, na.action=na.omit)
summary(m_change_ex_att_aff_avg)

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
confint(m_change_prefer)

toto$quartile_PA <- scale (toto$typical_mvpa, scale = TRUE)
quantile(toto$typical_mvpa, na.rm= TRUE)
toto$quartile_PA <- ntile(toto$typical_mvpa, 4)
m_change_quartile_PA <- glmer (chng_respns_num ~ rewp_230_lag_z*quartile_PA + (rewp_230_lag_z|subject), family="binomial", data=toto, na.action=na.omit)
summary(m_change_quartile_PA)
confint(m_change_quartile_PA)

toto$aware_z <- scale (toto$aware, scale = TRUE)
m_change_awareness <- glmer (chng_respns_num ~ rewp_230_lag_z*aware_z + (rewp_230_lag_z|subject), family="binomial", data=toto, na.action=na.omit)
summary(m_change_awareness)
confint(m_change_awareness)



######################################################################
### Estimates to create the Figures for the behavioral results
#######################################################################

######
# Figure for the choice of stimulus as a function of the trial
######

# main model
m_stim <- glmer (stim.chosen_num ~ trial_z + (trial_z|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim)

# estimation all the ten trials
table(data$trial)
describe(data$trial)

data$trial_10 <- data$trial - 10
describe(data$trial_10)

data$trial_20 <- data$trial - 20
describe(data$trial_20)

data$trial_30 <- data$trial - 30
describe(data$trial_30)

data$trial_40 <- data$trial - 40
describe(data$trial_40)

data$trial_50 <- data$trial - 50
describe(data$trial_50)

data$trial_60 <- data$trial - 60
describe(data$trial_60)

data$trial_70 <- data$trial - 70
describe(data$trial_70)

data$trial_80 <- data$trial - 80
describe(data$trial_80)

data$trial_90 <- data$trial - 90
describe(data$trial_90)

data$trial_100 <- data$trial - 100
describe(data$trial_100)

data$trial_110 <- data$trial - 110
describe(data$trial_110)

data$trial_120 <- data$trial - 120
describe(data$trial_120)

data$trial_130 <- data$trial - 130
describe(data$trial_130)

data$trial_140 <- data$trial - 140
describe(data$trial_140)

data$trial_150 <- data$trial - 150
describe(data$trial_150)

data$trial_160 <- data$trial - 160
describe(data$trial_160)

m_stim_10 <- glmer (stim.chosen_num ~ trial_10 + (trial_10|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_10)
exp(summary(m_stim_10)$coef[,1])
exp(summary(m_stim_10)$coef[,2])
exp(confint(m_stim_10))

m_stim_20 <- glmer (stim.chosen_num ~ trial_20 + (trial_20|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_20)
exp(summary(m_stim_20)$coef[,1])
exp(summary(m_stim_20)$coef[,2])
exp(confint(m_stim_20))

m_stim_30 <- glmer (stim.chosen_num ~ trial_30 + (trial_30|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_30)
exp(summary(m_stim_30)$coef[,1])
exp(confint(m_stim_30))

m_stim_40 <- glmer (stim.chosen_num ~ trial_40 + (trial_40|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_40)
exp(summary(m_stim_40)$coef[,1])
exp(confint(m_stim_40))

m_stim_50 <- glmer (stim.chosen_num ~ trial_50 + (trial_50|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_50)
exp(summary(m_stim_50)$coef[,1])
exp(confint(m_stim_50))

m_stim_60 <- glmer (stim.chosen_num ~ trial_60 + (trial_60|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_60)
exp(summary(m_stim_60)$coef[,1])
exp(confint(m_stim_60))
Low_IC <- exp((summary(m_stim_60)$coef[,1]) - (summary(m_stim_60)$coef[,2])* 1.96) 
Low_IC
High_IC <- exp((summary(m_stim_60)$coef[,1]) + (summary(m_stim_60)$coef[,2])* 1.96) 
High_IC

m_stim_70 <- glmer (stim.chosen_num ~ trial_70 + (trial_70|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_70)
exp(summary(m_stim_70)$coef[,1])
exp(confint(m_stim_70))

m_stim_80 <- glmer (stim.chosen_num ~ trial_80 + (trial_80|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_80)
exp(summary(m_stim_80)$coef[,1])
exp(confint(m_stim_80))

m_stim_90 <- glmer (stim.chosen_num ~ trial_90 + (trial_90|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_90)
exp(summary(m_stim_90)$coef[,1])
exp(confint(m_stim_90))

m_stim_100 <- glmer (stim.chosen_num ~ trial_100 + (trial_100|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_100)
exp(summary(m_stim_100)$coef[,1])
exp(confint(m_stim_100))

m_stim_110 <- glmer (stim.chosen_num ~ trial_110 + (trial_110|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_110)
exp(summary(m_stim_110)$coef[,1])
exp(confint(m_stim_110))

m_stim_120 <- glmer (stim.chosen_num ~ trial_120 + (trial_120|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_120)
exp(summary(m_stim_120)$coef[,1])
exp(confint(m_stim_120))

m_stim_130 <- glmer (stim.chosen_num ~ trial_130 + (trial_130|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_130)
exp(summary(m_stim_130)$coef[,1])
exp(confint(m_stim_130))

m_stim_140 <- glmer (stim.chosen_num ~ trial_140 + (trial_140|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_140)
exp(summary(m_stim_140)$coef[,1])
exp(confint(m_stim_140))

m_stim_150 <- glmer (stim.chosen_num ~ trial_150 + (trial_150|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_150)
exp(summary(m_stim_150)$coef[,1])
exp(confint(m_stim_150))

m_stim_160 <- glmer (stim.chosen_num ~ trial_160 + (trial_160|subject), family="binomial", data=data, na.action=na.omit)
summary(m_stim_160)
exp(summary(m_stim_160)$coef[,1])
exp(confint(m_stim_160))

####################
# FIGURES
##################

# Figure 4. Odds Ratio for choosing the stimulus that was more likely to lead to sitting (vs. standing) as a function of trial number
p <- ggplot() + geom_jitter(position=position_jitter(0.15),
                            color="black") + ylab("Odds Ratio") + theme_classic() +
      geom_point(aes(x= 2.69, y =1,  yend = 1), fill ="lightgrey",
                 color="black", ,size = 3) + geom_segment(aes(x= 1.78  , xend= 4.11 , y =1,  yend = 1),
                                                          colour = "black")  + geom_point(aes(x =2.51, y =2,  yend = 2),
                                                                                          color = "black", size =3) + geom_segment(aes(x=  1.71  , xend=  3.71 , y =2,  yend = 2),
                                                                                                                                   colour = "black") + geom_point(aes(x =2.34  , y =3,  yend = 3),
                                                                                                                                                                  colour = "black", size =3)+  geom_segment(aes(x=  1.64 , xend= 3.35 , y =3,  yend = 3),
                                                                                                                                                                                                            colour ="black") +  geom_point(aes(x = 2.18 , y =4,  yend = 4),
                                                                                                                                                                                                                                           colour = "black", size =3)+   geom_segment(aes(x= 1.57 , xend= 3.04  , y =4,  yend = 4),
                                                                                                                                                                                                                                                                                      colour ="black") + geom_point(aes(x = 2.03 , y =5,  yend = 5),
                                                                                                                                                                                                                                                                                                                    colour = "black", size =3)+   geom_segment(aes(x= 1.50 , xend= 2.75  , y =5,  yend = 5),
                                                                                                                                                                                                                                                                                                                                                               colour ="black") + geom_point(aes(x = 1.89 , y =6,  yend = 6),
                                                                                                                                                                                                                                                                                                                                                                                             colour = "black", size =3) +   geom_segment(aes(x= 1.43 , xend= 2.50  , y =6,  yend = 6),
                                                                                                                                                                                                                                                                                                                                                                                                                                         colour ="black") + geom_point(aes(x = 1.76 , y =7,  yend = 7),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                       colour = "black", size =3) +   geom_segment(aes(x= 1.36 , xend= 2.28  , y =7,  yend = 7),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   colour ="black")+ geom_point(aes(x = 1.64 , y =8,  yend = 8),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                colour = "black", size =3)+   geom_segment(aes(x= 1.29 , xend= 2.09  , y =8,  yend = 8),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           colour ="black") + geom_point(aes(x = 1.52 , y =9,  yend = 9),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         colour = "black", size =3)+   geom_segment(aes(x= 1.22 , xend= 1.92  , y =9,  yend = 9),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    colour ="black") + geom_point(aes(x = 1.42 , y =10,  yend = 10),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  colour = "black", size =3)+   geom_segment(aes(x= 1.14 , xend= 1.77  , y =10,  yend = 10),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             colour ="black") + geom_point(aes(x = 1.33 , y =11,  yend = 11),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           colour = "black", size =3) +   geom_segment(aes(x= 1.07 , xend= 1.64  , y =11,  yend = 11),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       colour ="black")+ geom_point(aes(x = 1.23 , y =12,  yend = 12),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    colour = "black", size =3) +   geom_segment(aes(x= 0.99 , xend= 1.54  , y =12,  yend = 12),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                colour ="black") + geom_point(aes(x = 1.15 , y =13,  yend = 13),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              colour = "black", size =3)+   geom_segment(aes(x= 0.91 , xend= 1.45  , y =13,  yend = 13),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         colour ="black") + geom_point(aes(x = 1.07 , y =14,  yend = 14),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       colour = "black", size =3)+   geom_segment(aes(x= 0.84 , xend= 1.37  , y =14,  yend = 14),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  colour ="black") + geom_point(aes(x = 1.00 , y =15,  yend = 15),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                colour = "black", size =3)+   geom_segment(aes(x= 0.76 , xend= 1.30  , y =15,  yend = 15),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           colour ="black") + geom_point(aes(x = 0.93 , y =16,  yend = 16),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         colour = "black", size =3)+   geom_segment(aes(x= 0.70 , xend= 1.24  , y =16,  yend = 16),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    colour ="black") +
      coord_trans(x="log2") + geom_vline(xintercept = 1, colour = "black", linetype = 5) + xlim(0.60, 1)  + scale_x_continuous(breaks = c(0.50, 1.00,1.50,2.00, 4.00)) +
      theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
      annotation_logticks(base = 2) +   theme(axis.text=element_text(size=12))
p


# Figure 5. Odds for changing of stimulus as a function of choice on the previous trial
toto$previous_trial_type <- as.factor(as.character(toto$previous_trial_type))
toto$previous_trial_ref_sit <- relevel(toto$previous_trial_type,
                                       ref = "sit")
toto$previous_trial_ref_stand <- relevel(toto$previous_trial_type,
                                         ref = "stand")

m_change_adjusted_sit_ref <- glmer (chng_respns_num ~ rewp_230_lag_z*previous_trial_ref_sit + (rewp_230_lag_z|subject), family="binomial", data=toto, na.action=na.omit)
summary(m_change_adjusted_sit_ref)
exp(summary(m_change_adjusted_sit_ref)$coef[,1])
exp(confint(m_change_adjusted_sit_ref))

m_change_adjusted_stand_ref <- glmer (chng_respns_num ~ rewp_230_lag_z*previous_trial_ref_stand + (rewp_230_lag_z|subject), family="binomial", data=toto, na.action=na.omit)
summary(m_change_adjusted_stand_ref)
exp(summary(m_change_adjusted_stand_ref)$coef[,1])
exp(confint(m_change_adjusted_stand_ref))

p <- ggplot() + geom_jitter(position=position_jitter(0.15),
                            color="black") + ylab("Odds Ratio") + theme_classic() +
      geom_point(aes(x= 0.49, y =1,  yend = 1), fill ="lightgrey",
                 color="black", ,size = 3) + geom_segment(aes(x= 0.41  , xend= 0.60 , y =1,  yend = 1),
                                                          colour = "black")  + geom_point(aes(x =0.70, y =2,  yend = 2), color = "black", size =3) +
      geom_segment(aes(x=  0.58  , xend=  0.85 , y =2,  yend = 2), colour = "black") +
      coord_trans(x="log2") + geom_vline(xintercept = 1, colour = "black", linetype = 5) + xlim(0.60, 1)  + scale_x_continuous(breaks = c(0.25, 0.50,0.75,1.00)) +
      theme(axis.text.y=element_blank(),axis.ticks.y=element_blank()) + 
      annotation_logticks(base = 2) +   theme(axis.text=element_text(size=12))
p
