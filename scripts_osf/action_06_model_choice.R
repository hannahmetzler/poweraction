#generalized linear mixed model on choice behavior paway

#choice behavior: paway = proportion of choosing the chair away from vs. next to the emotional actor

# LOAD PACKAGES ####
require(lme4);require(dplyr); require(sjPlot); require(lmerTest); #for mixed effects models
require(broom); require(tibble) #for reading and writing tidy tables/data to excel
require(car) #for diagnostics plot, and Anova() to print only significance of fixed effects without rest of model summary

# READ AND PREPARE DATA ####
rm(list=ls())
load("data/behavior_poweraction.R")

#filter neutral trials to run models on proportion of choice
# to check if outlier exclusion effects results, use data_filt instead of data_clean here
data_stats <- data_filt %>% #switch to data_filt here
  filter(emotion != "Neutral") %>% #filter neutral trials
  droplevels()

summary(data_stats)

# Define contrasts:  
#treatment (=dummy) coding for posture and session, first with expansive as 0 (baseline) and constrictive 1, to describe effects in the expansive posture group
data_stats$group<-relevel(data_stats$group, "Expansive")
data_stats$session<-relevel(data_stats$session, "1")
  
#set baseline level for within-subject factor contrasts: 
data_stats$emotion<-relevel(data_stats$emotion, "Fear")
data_stats$away<-relevel(data_stats$away, "0")#make Toward the baseline, so change is expressed as difference to Away 

# deviation contrast coding for emotion, side and session to look at main effects (i.e. compare mean of each condition to grand mean of all other conditions)
#https://stats.idre.ucla.edu/spss/faq/coding-systems-for-categorical-variables-in-regression-analysis/
contrasts(data_stats$emotion) <- c(-0.5,0.5) # fear -0.5 anger 0.5 => effect of emotion reflects main effect (not simple effect)

# comments for model specifications
# family = binomial("logit") => logistic mixed effects regression
# optimizer bobyqa  and nAGQ=10: bobyqa is the default optimizer used by  glmer
# nAGY=10 increases integration points to 10, without default value of 1, model does not converge 
#(from documentation: Values greater than 1 produce greater accuracy in the evaluation of the log-likelihood at the expense of speed.) 

# NULL MODEL and MODEL WITH ALL FIXED EFFECTS OF INTEREST (emotion, session, posture) ####

#null-model
m.0 <- glmer(away ~ (1|subject) + (1|stim_pair) , data= data_stats, family = binomial("logit"),
             control = glmerControl(optimizer = "bobyqa", nAGQ = 10))

# full model with session 1 and expansive posture as baseline: 
#main effect of emotion for expansive posture group in the first session, interactions: changes from these baseline levels to the other level
data_stats$session<-relevel(data_stats$session, "1")
data_stats$group<-relevel(data_stats$group, "Expansive")
me1.full <- update(m.0, .~. + emotion*session*group) #add fixed effects and their interactions to null-model
summary(me1.full) # summary of model parameters

# assess model fit: model comparison to null-model (likehood ratio test)
anova(m.0, me1.full)

# SEARCHING THE MAXIMAL RANDOM EFFECTS STRUCTURE ####
#ideally add slopes for emotion by session interaction (highest order within-subject factor interaction - see Barr 2013) 

# full random effects structure: intecept and slope for main effects and interaction of emotion x session
m.rand.full <- update(me1.full, .~. + (emotion|subject) + (session|subject)+(emotion:session|subject)) 
#results in singular fit => reduce random effects structure
#info on singular fit: https://stats.stackexchange.com/questions/378939/dealing-with-singular-fit-in-mixed-models

# interaction: intercept and slope - try including only the highest within-sub interaction of interest
m.rand.emoXses <- update(me1.full, .~. + (emotion:session|subject)) #results in singular fit

# interaction: uncorrelating intercept and slope. For more info on uncorrelating see: https://pagepiccinini.com/r-course/lesson-6-part-1-linear-mixed-effects-models/
m.rand.emoXses.unc <- update(me1.full, .~. + (1|emotion:session) + (0+emotion:session|subject)) #results in singular fit

# interaction: only intercept
m.rand.emoXses.ic <- update(me1.full, .~. + (1|emotion:session)) # results in singular fit

# main effects: intercepts and slopes
m.rand.mains <- update(me1.full, .~. + (emotion|subject) + (session|subject)) #results in singular fit 

#main effects: uncorrelate intercepts and slopes
#first try uncorrelating only int and slope of emotion
m.rand.mains.unc1 <- update(me1.full, .~. + (1|emotion) + (0+emotion|subject) + (session|subject)) #results in singular fit 
# also uncorrelate also for session
m.rand.mains.unc2 <- update(me1.full, .~. + (1|emotion) + (0+emotion|subject) + (1|session) + (0+session|subject)) #results in singular fit 

# main effects: only intercepts
m.rand.mains.ic <- update(me1.full, .~. + (1|emotion) + (1|session)) #also singular fit 

#show model summary for any of above models
summary(me.rand.emoXses.ic) #enter model name here

# => adding random slopes and random intercepts other than subject is not possible - models don't converge without a singular fit. 

# REPEAT MAIN MODEL WITH POSTURE OR SESSION RE-LEVELED to look at effects at different baselines ####

#session 1 and constrictive as baseline
#main effect of emotion for constrictive posture group in the first session, interactions: changes from these baseline levels to the other level
data_stats$session<-relevel(data_stats$session, "1")
data_stats$group<-relevel(data_stats$group, "Constrictive")
mc1.full <- update(m.0, .~. + emotion*session*group) 
summary(mc1.full)

#session 2 and expansive as baseline
#main effect of emotion for expansive posture group in the second session, interactions: changes from these baseline levels to the other level
data_stats$session<-relevel(data_stats$session, "2")
data_stats$group<-relevel(data_stats$group, "Expansive")
me2.full <- update(m.0, .~. + emotion*session*group) 
summary(me2.full)

#session 2 and constrictive as baseline
data_stats$session<-relevel(data_stats$session, "2")
data_stats$group<-relevel(data_stats$group, "Constrictive")
mc2.full <- update(m.0, .~. + emotion*session*group) 
summary(mc2.full)

# is the effect driven by anger or fear? 
data_stats$session<-relevel(data_stats$session, "1")
data_stats$group<-relevel(data_stats$group, "Expansive")
data_stats$emotion <- relevel (data_stats$emotion, "Fear")
memo.full <- update(m.0, .~. + emotion*session*group) 
summary(memo.full)

# ASSESS SIGNIFICANCE OF EFFECTS AND MODEL FIT IN ALTERNATIVE WAYS AND COMPARE ####

#Anova function from car package: Wald chisquare tests
ae1 <- Anova(me1.full,  type="III"); ae1
ac1 <- Anova(mc1.full, type="III"); ac1
ae2 <- Anova(me2.full,  type="III"); ae2 
ac2 <- Anova(mc2.full, type="III"); ac2

#note: the p-values of these chi-square tests are identical with p-values for z-tests in the model summary, see summary(me1.full)

# model comparison with single predictor deletion
#done in 3 steps: model with main effects only, model 2nd order interactions only, full model with 3rd-order interaction
#reason: "The hierarchy is respected when considering terms to be added or dropped: all main effects contained in a second-order interaction must remain, and so on" (from package documentation)

#baselines: session 1 and expansive posture - compare to output of me1.full
data_stats$session<-relevel(data_stats$session, "1")
data_stats$group<-relevel(data_stats$group, "Expansive")

#first for model with only main effects
me1.main <- update (m.0, .~. + emotion+session+group) 
main <- drop1(me1.main, test="Chisq"); main

#significance 
me1.int2 <- update(me1.0, .~. + emotion*session*group - emotion:session:group) #model without 3-way interaction
int2 <- drop1(me1.int2, test="Chisq"); int2

#significance of the 3way interaction 
int3 <- drop1(me1.full, test="Chisq") ; int3

#show results in one table and compare to output of Anova (analysis of deviance)
rbind(tidy(main), tidy(int2), tidy(int3)) #warnings only mean that this is not typical anova output
ae

#conclusion from this: the resulting p-values show the same significance/non-significance pattern, 
# emotion effect is always largest (highest chisq), and 3way interaction second largest => posture effect is smaller than emotion effect (this was expected)

# EXPORT RESULTS TO TABLES ####

#save model parameters for the model, once with each posture and session as baseline
tab_model(me1.full,
          show.aic = F, show.r2 = T, show.stat = T, show.dev = T, string.stat = "z",show.df =T, 
          title = "Model of choice away vs. toward - Baseline: Expansive posture and session 1",
          #string.est = "OR", show.obs = F,show.ngroups = F,
          pred.labels = c(" (Intercept)", "Emotion", "Session", "Posture", "Emotion x Session","Emotion x Posture",  "Session x Posture", "Emotion x Session x Posture"),
          dv.labels = "P(away)", file = "output/model_choice_1expansive.doc")
tab_model(mc1.full,
          show.aic = F, show.icc = F, show.r2 = F,  show.stat = T,show.dev = T, string.stat = "z",
          title = "Model of choice away vs. toward - Baseline: Constrictive posture and session 1",
          #string.est = "OR", show.obs = F,show.ngroups = F,
          pred.labels = c(" (Intercept)", "Emotion", "Session", "Posture", "Emotion x Session","Emotion x Posture",  "Session x Posture", "Emotion x Session x Posture"),
          dv.labels = "P(away)", file = "output/model_choice_1constrictive.doc")
tab_model(me2.full,
          show.aic = F, show.icc = F, show.r2 = F,  show.stat = T,show.dev = T, string.stat = "z",
          title = "Model of choice away vs. toward - Baseline: Expansive posture and session 2",
          #string.est = "OR", show.obs = F,show.ngroups = F,
          pred.labels = c(" (Intercept)", "Emotion", "Session", "Posture", "Emotion x Session","Emotion x Posture",  "Session x Posture", "Emotion x Session x Posture"),
          dv.labels = "P(away)", file = "output/model_choice_2expansive.doc")
tab_model(mc2.full,
          show.aic = F, show.icc = F, show.r2 = F,  show.stat = T,show.dev = T, string.stat = "z",
          title = "Model of choice away vs. toward - Baseline: Constrictive posture and session 2",
          #string.est = "OR", show.obs = F,show.ngroups = F,
          pred.labels = c(" (Intercept)", "Emotion", "Session", "Posture", "Emotion x Session","Emotion x Posture",  "Session x Posture", "Emotion x Session x Posture"),
          dv.labels = "P(away)", file = "output/model_choice_2constrictive.doc")


# MODEL DIAGNOSTICS ####
vif(me1.full)#collinearity - no values >10 => seems ok

#binned residual plot to assess model fit for logistic regression
# lines indicate ±2 standard-error bounds,i.e. about 95% of the binned residuals should fall inside for a good model fit
require(arm)
par(bg="white", cex=1.2, las=1)
binnedplot(fitted(me1.full), residuals(me1.full), cex.pts=0.8, col.int="black")
#outliers for probabilities above 0.56, all in positive direction (4 clear outliers): 
# for these bins, model predicts lower probability than is actually the case (because residuals are Residuals are observed minus fitted values, and result is positive for these outliers.)
# try find out which subjects these are: 
# data_stats$residuals <- residuals(me1.full) 
# avg_residuals <- data_stats %>%
#   group_by(subject)%>% 
#   summarise(avg_residuals = mean(residuals))
# plot( avg_residuals$subject, avg_residuals$avg_residuals)

#are random intercepts for subject and item_pair normally distributed?
lme4::qqmath(ranef(me1.full))

#it looks like same 4 data points are visible for subject intercept 
#stim pair looks alright, line seems straight enough for so few data points
#for stim pair, also use a shapiro wilk test (sample small enough)
shapiro.test(ranef(me1.full)$stim_pair[,1]) #NV ok


# PLOTS TO INTERPRET INTERACTIONS ####
load('./data/paway_79sub_filt5perc.R')
#no session*posture interaction in the expansive posture, but in the constrictive posture. 
ggplot(paway)+geom_boxplot(aes(x = group, y= proportion, colour = session))


# IMPACT OF PERSONALITY TRAITS AND ANXIETY STATE ####
# based on equivalence testing (see script 03): groups are not significantly different, but also not equivalent on any questionnaire measure (threshold 0.4)
# check impact of these variables: "anxstate" "anxtrait" "self_esteem"    "bis" "bas"  "dominance" "affiliation" 

#are there any empty values?
unique(data_stats$subject[(is.na(data_stats$anxstate))])#subject 54 has no state anxiety score 
unique(data_stats$subject[(is.na(data_stats$anxtrait))])
unique(data_stats$subject[(is.na(data_stats$self_esteem))])
unique(data_stats$subject[(is.na(data_stats$bis))])
unique(data_stats$subject[(is.na(data_stats$bas))])
unique(data_stats$subject[(is.na(data_stats$dominance))])
unique(data_stats$subject[(is.na(data_stats$affiliation))])
#all other questionnaires are complete

# => because models estimated on different sample size cannot be compared, replace S54 state anxiety scroe with mean
data_stats$anxstate[is.na(data_stats$anxstate)] <- mean(data_stats$anxstate, na.rm=T) #replace with mean

#model including all questionnaire main effects (covariates)
data_stats$group <- relevel(data_stats$group, "Expansive")
me1.full.quest <- glmer(away ~ (1|subject) + (1|stim_pair) +emotion*session*group +
                      anxstate + anxtrait + bis + bas + self_esteem + dominance + affiliation, 
                      data= data_stats, family = binomial("logit"), control = glmerControl(optimizer = "bobyqa", nAGQ = 10))

#any effect significant? 
Anova(me1.full.quest,  type="III")

#compare to effects without questionnaires:
Anova(me1.full,  type="III")

#conclusion: none of the questionnaires has a significant impact on p(away), 
#and the pattern of significance of all other effects remains the same with questionnaires added to the model

# IMPACT OF EXCLUDING CERTAIN SUBJECTS ####

##### EXCLUDE subjects who understood experiment: 28 and 69

data_ex2869 <- data_stats%>%
  filter(!(subject %in% c("28", "69")))

me1.full.ex2869 <- glmer(away ~ (1|subject) + (1|stim_pair) +emotion*session*group,
                        data=data_ex2869, family = binomial("logit"), control = glmerControl(optimizer = "bobyqa", nAGQ = 10))

Anova(me1.full.ex2869, type="III") 
