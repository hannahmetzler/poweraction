#models on reaction times: initiation time and movement duration

#LOAD PACKAGES ####
require(dplyr) #basic data handling
require(lme4);require(sjPlot); #for mixed effects models
require(lmerTest) #for t-tests and p-values for parameter estimates
require(broom); require(tibble) #for organizes model output into data frames, reading and writing tidy tables/data to excel
require(car) #for diagnostics plot, quantile comparison Plot qqp, and Anova() to print only significance of fixed effects without rest of model summary
require(cowplot) #for 2 plots next to each other

# read data ####
rm(list=ls())
load("data/behavior_poweraction.R")

#data 
mouse <- data_filt %>% #insert data_filt here to test effect of outliers
  #rename away to side (to make distinction between DV in choice model, which becomes the independent variable here)
  rename(side = away)

#log transfrom skewed initiation time and movement time distribution
mouse$ln_init_time <- log(mouse$init_time)
mouse$ln_mov_time<- log(mouse$mov_time)

#look at distributions of log transformed values
par(mfrow=c(2,2))
qqp(mouse$init_time, "norm"); hist(mouse$init_time)
qqp(mouse$ln_init_time, "norm") ; hist(mouse$ln_init_time)# much better, but not perfect at ends of distribution

par(mfrow=c(2,2))
qqp(mouse$mov_time, "norm"); hist(mouse$mov_time)
qqp(mouse$ln_mov_time, "norm") ; hist(mouse$ln_mov_time)# much better, but still thick tail at lower move times

# to test the main effect emo=treat vs neutral: create variable with Anger and Fear combined as Threat
mouse$threat<-mouse$emotion
mouse$threat<-relevel(mouse$threat, "Neutral") #put Neutral as first level
levels(mouse$threat) <- c("Neutral", "Threat", "Threat") #combine anger and fear into one level "threat"

#set baseline level for within-subject factors: 
mouse$emotion<-relevel(mouse$emotion, "Fear")
mouse$side<-relevel(mouse$side, "0") #toward =0 as baseline, away=1 as the difference

#deviation contrast coding for emotion, threat and side 
#treatment coding for session and posture see below, re-leveled for each model to describe effects in each group and session
contrasts(mouse$threat) <- c(-0.5,0.5) #neutral -0.5, threat 0.5
contrasts(mouse$side) <- c(-0.5,0.5) # toward -0.5, away 0.5 #for initiation time and movement duration

#default baselines for group and session (dummy/treatment coding)
mouse$group <- relevel(mouse$group, "Expansive")
mouse$session <-relevel(mouse$session, "1")

#data subset and contrast for emotion for models on only angry and fearful trials: 
mouse_emo <- droplevels(subset(mouse,emotion!="Neutral")) #exclude neutral trials and delete the neutral level
contrasts(mouse_emo$emotion) <- c(-0.5,0.5) # fear -0.5 anger 0.5 => effect of emotion reflects main effect (not simple effect)

# MAXIMAL RANDOM EFFECTS STRUCTURE FOR INITIATION TIME AND MOVEMENT DURATION ####
#I tried to include: 
# first all random effects for within-subject factors (intercepts and slopes)
# second only the highest-order within-subject interaction of interest, i.e. (threat:session:group) or (emotion*side*session*group)
# third only main effects for emotion, side & session or threat & session.
#Both when trying to include this interaction and only main effects, 
# - we first tried including intercept and slope, e.g. (1+emotion|subject) 
# - then uncorrelated intercept and slope, e.g. (1|emotion)+(0+emotion|subject) 
# - and finally tried including only the intercept, e.g. (1|emotion).
# Look at script 02_model_choice for an example for how this was done. 

#below is the max rand effects structure that works for each model

#models for which only inclusion of (1|subject) works
#neither intercept for stim_pair, nor intercept or slope for interaction or main effects of emo/threat, side or session work
maxr_it_emo <- lmer(ln_init_time ~ (1|subject)+ emotion*side*session*group, data=mouse_emo, REML = F)
maxr_it_threat <- lmer(ln_init_time ~ (1|subject) + threat*session*group, data=mouse, REML = F)

#stim_pair works, but intercept or slope for interaction or main effects of emo/threat, side or session don't work
maxr_mt_emo <- lmer(ln_mov_time ~ (1|subject) + (1|stim_pair)+ emotion*side*session*group, data=mouse_emo, REML = F)
maxr_mt_threat <- lmer(ln_mov_time ~ (1|subject) + (1|stim_pair)+ threat*session*group, data=mouse, REML = F)

# LINEAR MIXED MODEL INITIATION TIME with emo*side on angry and fearful trials ####
# null-model
m.0_emo <- lmer(ln_init_time ~ (1|subject) , data=mouse_emo, REML = F)
# REML = FALSE, i.e. Maximum Likelihood estimation will be used instead, this is needed when model comparison is used to determine p-values (determines how variances are calculated)

# full model with session 1 and expansive posture as baseline: 
#main effect emotion for expansive group in the first session, 
# and the interaction emotion:session in expansive group, which indicates whether emotion effect changed in this group
mouse_emo$group <- relevel(mouse_emo$group, "Expansive") #set other group as baseline here to look at simple effects side/emotion 
mouse_emo$session <-relevel(mouse_emo$session, "1") #set other session as baseline here
me1.full_emo <- update(m.0_emo, .~. + emotion*side*session*group) #add fixed effects and their interactions to null-model
tab_model(me1.full_emo, show.stat = T) #model parameters

anova(m.0_emo, me1.full_emo)

#export model results to table
tab_model(me1.full_emo, show.stat = T, string.stat = "t",string.est = "Beta",
          title = "Model of initiation time for angry and fearful trials - Baseline: Expansive posture and session 1",
          pred.labels = c(" (Intercept)", "Emotion","Side", "Session", "Posture",
                          "Emotion x Side","Emotion x Session", "Side x Session", "Emotion x Posture","Side x Posture","Session x Posture", 
                          "Emotion x Side x Session","Emotion x Side x Posture","Emotion x Session x Posture", "Side x Session x Posture", 
                          "Emotion x Side x Session x Posture"),
          file = "output/model_inittime_emoside_1expansive.doc")

# no effect of side, emotion and no interaction of these factors with session or posture 
# => neither effect is significantly different in other group or other session

# INITIATION TIME: Fit 2nd model with anger/fear combined to threat on all trials ####
#null model
m.0_threat <- lmer(ln_init_time ~ (1|subject), data=mouse, REML = F)

# full model with expansive & session 1 as baseline
mouse$group <- relevel(mouse$group, "Expansive")
mouse$session <- relevel(mouse$session, "1")
me1.full_threat <- update(m.0_threat, .~. + threat*session*group) #add fixed effects and their interactions to null-model
tab_model(me1.full_threat,show.stat = T) # summary of model parameters

#compare to null model
anova(m.0_threat, me1.full_threat)

#constrictive session 1
mouse$group <- relevel(mouse$group, "Constrictive")
mouse$session <- relevel(mouse$session, "1")
mc1.full_threat <- update(m.0_threat, .~. + threat*session*group)  
tab_model(mc1.full_threat,show.stat = T)

#expansive session 2
mouse$group <- relevel(mouse$group, "Expansive")
mouse$session <- relevel(mouse$session, "2")
me2.full_threat <- update(m.0_threat, .~. + threat*session*group) 
tab_model(me2.full_threat,show.stat = T)

#constrictive session 2
mouse$group <- relevel(mouse$group, "Constrictive")
mouse$session <- relevel(mouse$session, "2")
mc2.full_threat <- update(m.0_threat, .~. + threat*session*group) 
tab_model(mc2.full_threat,show.stat = T)

#export model results to table for all baseline combinations
tab_model(me1.full_threat, show.stat = T, string.stat = "t",string.est = "Beta",
          title = "Model of initiation time for angry and fearful trials - Baseline: Expansive posture and session 1",
          pred.labels = c(" (Intercept)", "Threat", "Session", "Posture", "Threat x Session","Threat x Posture",  "Session x Posture", "Threat x Session x Posture"),
          file = "output/model_inittime_threat_1expansive.doc")
tab_model(mc1.full_threat, show.stat = T, string.stat = "t",string.est = "Beta",
          title = "Model of initiation time for angry and fearful trials - Baseline: Constrictive posture and session 1",
          pred.labels = c(" (Intercept)", "Threat", "Session", "Posture", "Threat x Session","Threat x Posture",  "Session x Posture", "Threat x Session x Posture"),
          file = "output/model_inittime_threat_1constrictive.doc")
tab_model(me2.full_threat, show.stat = T, string.stat = "t",string.est = "Beta",
          title = "Model of initiation time for angry and fearful trials - Baseline: Expansive posture and session 2",
          pred.labels = c(" (Intercept)", "Threat", "Session", "Posture", "Threat x Session","Threat x Posture",  "Session x Posture", "Threat x Session x Posture"),
          file = "output/model_inittime_threat_2expansive.doc")
tab_model(mc2.full_threat, show.stat = T, string.stat = "t",string.est = "Beta",
          title = "Model of initiation time for angry and fearful trials - Baseline: Constrictive posture and session 2",
          pred.labels = c(" (Intercept)", "Threat", "Session", "Posture", "Threat x Session","Threat x Posture",  "Session x Posture", "Threat x Session x Posture"),
          file = "output/model_inittime_threat_2constrictive.doc")

# LINEAR MIXED MODEL MOVEMENT DURATION: angry and fearful trials ####
# null-model
m.0_emo <- lmer(ln_mov_time ~ (1|subject) + (1|stim_pair), data=mouse_emo, REML = F)
# REML = FALSE, i.e. Maximum Likelihood estimation will be used instead, this is needed when model comparison is used to determine p-values (determines how variances are calculated)

# full model with session 1 and expansive posture as baseline: 
#main effect emotion for expansive group in the first session, 
# and the interaction emotion:session in expansive group, which indicates whether emotion effect changed in this group
mouse_emo$group <- relevel(mouse_emo$group, "Expansive")
mouse_emo$session <-relevel(mouse_emo$session, "1")
me1.full_emo <- update(m.0_emo, .~. + emotion*side*session*group) #add fixed effects and their interactions to null-model
tab_model(me1.full_emo, show.stat = T, show.std="std") #model parameters
anova(m.0_emo, me1.full_emo)

#export model results to table
tab_model(me1.full_emo, show.stat = T, string.stat = "t",string.est = "Beta",
          title = "Model of movement duration for angry and fearful trials - Baseline: Expansive posture and session 1",
          pred.labels = c(" (Intercept)", "Emotion","Side", "Session", "Posture",
                          "Emotion x Side","Emotion x Session", "Side x Session", "Emotion x Posture","Side x Posture","Session x Posture", 
                          "Emotion x Side x Session","Emotion x Side x Posture","Emotion x Session x Posture", "Side x Session x Posture", 
                          "Emotion x Side x Session x Posture"),
          file = "output/model_movtime_emoside_1expansive.doc")

# no effect of side, emotion and no interaction of these factors with session or posture 
# => neither effect is significantly different in other group or other session

# fit 2nd model with anger/fear combined to threat ####
#null model
m.0_threat <- lmer(ln_mov_time ~ (1|subject) +  (1|stim_pair) , data=mouse, REML = F)

# full model with expansive & session 1 as baseline
mouse$group <- relevel(mouse$group, "Expansive")
mouse$session <- relevel(mouse$session, "1")
me1.full_threat <- update(m.0_threat, .~. + threat*session*group) #add fixed effects and their interactions to null-model
tab_model(me1.full_threat,show.stat = T) # summary of model parameters

#compare to null model
anova(m.0_threat, me1.full_threat)

#constrictive session 1
mouse$group <- relevel(mouse$group, "Constrictive")
mouse$session <- relevel(mouse$session, "1")
mc1.full_threat <- update(m.0_threat, .~. + threat*session*group)  
tab_model(mc1.full_threat,show.stat = T)

#expansive session 2
mouse$group <- relevel(mouse$group, "Expansive")
mouse$session <- relevel(mouse$session, "2")
me2.full_threat <- update(m.0_threat, .~. + threat*session*group) 
tab_model(me2.full_threat,show.stat = T)

#constrictive session 2
mouse$group <- relevel(mouse$group, "Constrictive")
mouse$session <- relevel(mouse$session, "2")
mc2.full_threat <- update(m.0_threat, .~. + threat*session*group) 
tab_model(mc2.full_threat,show.stat = T)


#export model results to table for all baseline combinations
tab_model(me1.full_threat, show.stat = T, string.stat = "t",string.est = "Beta",
          title = "Model of initiation time for angry and fearful trials - Baseline: Expansive posture and session 1",
          pred.labels = c(" (Intercept)", "Threat", "Session", "Posture", "Threat x Session","Threat x Posture",  "Session x Posture", "Threat x Session x Posture"),
          file = "output/model_movtime_threat_1expansive.doc")
tab_model(mc1.full_threat, show.stat = T, string.stat = "t",string.est = "Beta",
          title = "Model of initiation time for angry and fearful trials - Baseline: Constrictive posture and session 1",
          pred.labels = c(" (Intercept)", "Threat", "Session", "Posture", "Threat x Session","Threat x Posture",  "Session x Posture", "Threat x Session x Posture"),
          file = "output/model_movtime_threat_1constrictive.doc")
tab_model(me2.full_threat, show.stat = T, string.stat = "t",string.est = "Beta",
          title = "Model of initiation time for angry and fearful trials - Baseline: Expansive posture and session 2",
          pred.labels = c(" (Intercept)", "Threat", "Session", "Posture", "Threat x Session","Threat x Posture",  "Session x Posture", "Threat x Session x Posture"),
          file = "output/model_movtime_threat_2expansive.doc")
tab_model(mc2.full_threat, show.stat = T, string.stat = "t",string.est = "Beta",
          title = "Model of initiation time for angry and fearful trials - Baseline: Constrictive posture and session 2",
          pred.labels = c(" (Intercept)", "Threat", "Session", "Posture", "Threat x Session","Threat x Posture",  "Session x Posture", "Threat x Session x Posture"),
          file = "output/model_movtime_threat_2constrictive.doc")