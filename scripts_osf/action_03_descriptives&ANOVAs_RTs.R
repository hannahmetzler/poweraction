# Statistics for RTs: Descriptive statistics, ANOVAS

#LOAD LIBRARIES ####
require(dplyr) #for basic data handling
require(ez) #for Anovas
require(car) #for qqplots

require(psych) #for describeBy
require(tidyr) #for gather and spread
require(pastecs)

#define function to calculate partial eta squared: F*dfn/F*dfn+dfd see Lakens 2013 page 6
peta <- function(x) {a$ANOVA[x,"F"]*a$ANOVA[x, "DFn"]/(a$ANOVA[x,"F"]*a$ANOVA[x, "DFn"]+a$ANOVA[x, "DFd"])}

# LOAD AND FORMAT DATA ####
load("data/behavior_poweraction.R")

#data for threat vs. neutral effect (called mouse because clicking and releasing the mouse cursor define the two RTs in this task)
mouse <- data_clean %>%
  #rename away to side (to make distinction between DV in choice model, which becomes the independent variable here)
  rename(side = away) %>%
  mutate(threat = ifelse(emotion=="Neutral", 0, 1))%>%
  mutate(init_time = round(init_time*1000))%>%
  mutate(mov_time = round(mov_time*1000))

#data for emotion and side effects
mouse_es <- mouse %>% #es for emo side
  filter(emotion != 'Neutral')

# calculate subject means per condition ####

#subject mean for init and move time by condition for emotion by side per session, 
mouse_aov_es <- mouse_es %>% #es for emo side, aov for analysis of variance, since data will be used for ANOVAs
  group_by(subject, group, session, emotion, side)%>%
  summarise(submean_initt=mean(init_time), 
            submean_movt = mean(mov_time))%>%
  droplevels()%>% #drop the level neutral from emotion
  ungroup()

#subject mean for init and move time by condition for threat per session
mouse_aov_threat <- mouse %>%
  mutate(threat = factor(ifelse(emotion=="Neutral", 0 , 1)))%>%
  group_by(subject, group, session, threat)%>%
  summarise(submean_initt=mean(init_time), 
            submean_movt = mean(mov_time),
            # log transformation (to check if it improves the distribution)
            ln_submean_initt=log(mean(init_time)), 
            ln_submean_movt = log(mean(mov_time)))%>%
  ungroup()

#normal distribution of subject means per condition?  ####
# initiation time: NV ok - logged values are worse
qqp(mouse_aov_threat$submean_initt, "norm")
qqp(log(mouse_aov_threat$submean_initt), "norm") #worse, do not use logged values
shapiro.test(mouse_aov_threat$submean_initt)
shapiro.test(log(mouse_aov_threat$submean_initt)) #test significant - do not use logged values

#movement duration: 
qqp(mouse_aov_threat$submean_movt, "norm")
qqp(log(mouse_aov_threat$submean_movt), "norm") #worse, do not use logged values
shapiro.test(mouse_aov_threat$submean_movt) # test significant - not perfectly normally distributed, 
#but no robust alternative for 2-way within sub anova, t-test: used wilcox ranked tests instead, simply rely on result of linear mixed effect model, this is just a check
shapiro.test(log(mouse_aov_threat$submean_movt)) #test significant - do not use logged values

# means + SD for boths session or threat vs. neutral####
#intitiation time
with(mouse, describeBy(x=init_time, group=session)) #mean for session 1 vs. session 2
with(mouse, describeBy(x=init_time, group=threat)) #mean for threat vs. neutral
#movement duration
with(mouse, describeBy(x=mov_time, group=session)) #mean for session 1 vs. session 2
with(mouse, describeBy(x=mov_time, group=threat)) #mean for threat vs. neutral

# descriptive statistics for threat x session x posture group ####

#within-subject confidence intervals: Cousineau method with Morey correction (Baguley, 2012)
#to remove within-participant variance, subtract participant mean from their raw scores in each condition, then add the grand mean to these normalised participant means 
#because normalised data has lower variability (induces positive correlation): correct for number of within-conditions sqrt(c / (c-1)) to make CIs slightly larger


#initiation time
desc_initt <- mouse_aov_threat %>%
  dplyr::select(group, subject, session, threat, submean_initt)%>%
  #grand mean per group
  group_by(group)%>% 
  mutate(grandmean = mean(submean_initt)) %>%
  # n subjects per group
  group_by(group) %>%
  mutate(n = length(unique(subject))) %>%
  #normalized difference for each subject: raw level minus subject mean plus grand mean for each group
  group_by(subject)%>% 
  mutate(normdiff = submean_initt - mean(submean_initt) + grandmean)%>%
  #mean, sd, normed sd per group and condition
  group_by(group, session, threat, n)%>%
  summarise(mean=mean(submean_initt),
            sd = sd(submean_initt), 
            normsd = sd(normdiff))%>%
  #within-subject CIs according to Cousineau Morey method
  mutate(ci = 1.96*normsd/sqrt(n)*sqrt(2/(2-1)), 
         wci_low = mean-ci, 
         wci_up = mean+ci)%>% #2 conditions: threat/neutral
  ungroup()%>%
  select(session, everything(), -normsd, -ci)%>%
  mutate(threat = factor(threat, levels=c(0,1), labels=c("Neutral", "Threatening")))%>%
  mutate(session = factor(session, levels=c(1,2), labels=c("1 - Baseline", "2 - Posture")))%>%
  arrange(session)

#write to excel
desc_initt[,c("mean", "sd","wci_low","wci_up")] <- round(desc_initt[,c("mean", "sd","wci_low","wci_up")], digits=0)
desc_initt$CI <- paste(desc_initt$wci_low, desc_initt$wci_up, sep="-") #combine low and upper ci limit into 1 CI
desc_initt <- dplyr::select(desc_initt, -wci_low, -wci_up) #delete limits, keep only entire CI
write.csv2(desc_initt, file="output/TableS5_means_sds_cis_initt.csv")
  
#movement time
desc_movt <- mouse_aov_threat %>%
  dplyr::select(group, subject, session, threat, submean_movt)%>%
  #grand mean per group
  group_by(group)%>% 
  mutate(grandmean = mean(submean_movt)) %>%
  # n subjects per group
  group_by(group) %>%
  mutate(n = length(unique(subject))) %>%
  #normalized difference for each subject: raw level minus subject mean plus grand mean for each group
  group_by(subject)%>% 
  mutate(normdiff = submean_movt - mean(submean_movt) + grandmean)%>%
  #mean, sd, normed sd per group and condition
  group_by(group, session, threat, n)%>%
  summarise(mean=mean(submean_movt),
            sd = sd(submean_movt), 
            normsd = sd(normdiff))%>%
  #within-subject CIs according to Cousineau Morey method
  mutate(ci = 1.96*normsd/sqrt(n)*sqrt(2/(2-1)), 
         wci_low = mean-ci, 
         wci_up = mean+ci)%>% #2 conditions: threat/neutral
  ungroup()%>%
  select(everything(), -normsd, -ci)%>%
  mutate(threat = factor(threat, levels=c(0,1), labels=c("Neutral", "Threatening")))%>%
  mutate(session = factor(session, levels=c(1,2), labels=c("1 - Baseline", "2 - Posture")))%>%
  arrange(session)

#write to excel
desc_movt[,c("mean", "sd","wci_low","wci_up")] <- round(desc_movt[,c("mean", "sd","wci_low","wci_up")], digits=0)
desc_movt$CI <- paste(desc_movt$wci_low, desc_movt$wci_up, sep="-") #combine low and upper ci limit into 1 CI
desc_movt <- dplyr::select(desc_movt, -wci_low, -wci_up) #delete limits, keep only entire CI
write.csv2(desc_movt, file="output/TableS8_means_sds_cis_movt.csv")


#ANOVAs ####
# ANOVA initiation time emo by side
a<- ezANOVA(data=subset(mouse_aov_es, emotion!=3), dv=submean_initt, wid=subject, within=c(emotion,side, session), between=group, type = 3)
#partial eta-squared for all lines: apply function peta (defined at beginning of script) to each line in the a$ANOVA data frame
a$ANOVA[,"peta"] <- round(sapply(seq(1,nrow(a$ANOVA),1), peta), digits=3) #calculate peta (with function defined at start of script)
a$ANOVA[c("F", "p", "ges")]<- round(a$ANOVA[c("F", "p", "ges")], 3); a #round F, p and general eta squared to 3 digits
write.csv2(a, file="output/initt_anova_emoside.csv")

#ANOVA initiation time threat
a<- ezANOVA(data=mouse_aov_threat, dv=submean_initt, wid=subject, within=c(threat,session), between=group, type = 3)
a$ANOVA[,"peta"] <- round(sapply(seq(1,nrow(a$ANOVA),1), peta), digits=3) #calculate peta (with function defined at start of script)
a$ANOVA[c("F", "p", "ges")]<- round(a$ANOVA[c("F", "p", "ges")], 3); a #round F, p and general eta squared to 3 digits
write.csv2(a, file="output/initt_anova_threat.csv")

#ANOVA movement duration emo by side
a<- ezANOVA(data=subset(mouse_aov_es, emotion!=3), dv=submean_movt, wid=subject, within=c(emotion,side, session), between=group, type = 3)
a$ANOVA[,"peta"] <- round(sapply(seq(1,nrow(a$ANOVA),1), peta), digits=3) #calculate peta (with function defined at start of script)
a$ANOVA[c("F", "p", "ges")]<- round(a$ANOVA[c("F", "p", "ges")], 3); a #round F, p and general eta squared to 3 digits
write.csv2(a, file="output/movt_anova_emoside.csv")

#ANOVA movement duration threat
a<- ezANOVA(data=mouse_aov_threat, dv=submean_movt, wid=subject, within=c(threat,session), between=group, type = 3)
a$ANOVA[,"peta"] <- round(sapply(seq(1,nrow(a$ANOVA),1), peta), digits=3) #calculate peta (with function defined at start of script)
a$ANOVA[c("F", "p", "ges")]<- round(a$ANOVA[c("F", "p", "ges")], 3); a #round F, p and general eta squared to 3 digits
write.csv2(a, file="output/movt_anova_threat.csv")

# Asterisks for Figure 4: are RTs different between posture in the neutral and threat condition? ####
#wide data format: one column per threat x session cell
initt_w = mouse_aov_threat %>% # for initiation time
  mutate(s = paste(session, threat, sep="_threat."))%>% #variable s will define each new column, one per session x threat condition combination
  select(-submean_movt, -ln_submean_initt, -ln_submean_movt, -threat, -session)%>% #delete unnessary variables
  spread(key = s, value = submean_initt, sep=".") # spread according to variable s, insert mean_innitt in the different columns
movt_w = mouse_aov_threat %>% # for click time, same as above for init time
  mutate(s = paste(session, threat, sep="_threat."))%>%
  select(-submean_initt, -ln_submean_initt, -ln_submean_movt, -threat, -session)%>% 
  spread(key = s, value = submean_movt, sep=".")

#levene-Tests homogeneity of variances initiation time - all variances equal
with(initt_w, leveneTest(s.1_threat.0, group))
with(initt_w, leveneTest(s.1_threat.1, group))
with(initt_w, leveneTest(s.2_threat.0, group))
with(initt_w, leveneTest(s.2_threat.1, group))

#levene Test movement duration - all variances equal
with(movt_w, leveneTest(s.1_threat.0, group))
with(movt_w, leveneTest(s.1_threat.1, group))
with(movt_w, leveneTest(s.2_threat.0, group))
with(movt_w, leveneTest(s.2_threat.1, group))

#t-tests on initiation time between expansive vs. constrictive group in each session by threat condition
t_initt_EvsC <- as.data.frame(t(sapply(initt_w[,c("s.1_threat.0", "s.1_threat.1", "s.2_threat.0", "s.2_threat.1")], function(x)
  round(unlist(t.test(x~initt_w$group, paired=F, var.equal=T)[c("estimate","parameter", "p.value","statistic", "conf.int")]),digits=3))))
t.test(s.1_threat.0 ~group, paired=F, var.equal=T, data=initt_w) #what are these confidence intervals? /10 smaller than means?
#compare with wilcox ranked sum test in each threatxsession condition: similar p-values, slightly more conservative than t-tests
wilcox.test(s.1_threat.0 ~ group, data=initt_w)
wilcox.test(s.1_threat.1 ~ group, data=initt_w)
wilcox.test(s.2_threat.0 ~ group, data=initt_w)
wilcox.test(s.2_threat.1 ~ group, data=initt_w)

#t-tests on movement time between expansive vs. constrictive group in each session by threat condition
t_movt_EvsC <- as.data.frame(t(sapply(movt_w[,c("s.1_threat.0", "s.1_threat.1", "s.2_threat.0", "s.2_threat.1")], function(x)
  round(unlist(t.test(x~movt_w$group, paired=F, var.equal=T)[c("estimate","parameter", "p.value","statistic", "conf.int")]),digits=3))))

#cohen's d from t-value and n in each group
n <-as.data.frame(xtabs(~group, initt_w))$Freq #calculate n per posture group for cohen's d
#calculate d: t/sqrt(1/n1+1/n2)
t_initt_EvsC$cohens.d <- (t_movt_EvsC[,"statistic.t"]*sqrt(1/n[1]+1/n[2]))
t_movt_EvsC$cohens.d <- (t_movt_EvsC[,"statistic.t"]*sqrt(1/n[1]+1/n[2]))
# all effect sizes equal or smaller than .21, all negative, i.e. expansive always quicker than constrictive

with(initt_w, describeBy(x=s.1_threat.0, group=group))

