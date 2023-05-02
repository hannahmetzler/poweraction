# Statistics for propotion of moving away vs toward: Descriptive statistics, ANOVAS, t-tests, effect-sizes

#LOAD LIBRARIES ####
require(dplyr) #for basic data handling
require(pastecs)
require(ez) #for Anovas
require(car) #for levene Test
require(tidyr) #for gather and spread

#delete all variables in environment
rm(list=ls())

#define function to calculate partial eta squared: F*dfn/F*dfn+dfd see Lakens 2013 page 6
peta <- function(x) {a$ANOVA[x,"F"]*a$ANOVA[x, "DFn"]/(a$ANOVA[x,"F"]*a$ANOVA[x, "DFn"]+a$ANOVA[x, "DFd"])}

# LOAD DATA AND CALCULATE PROPORTION AWAY VS. TOWARD ####
load("data/behavior_poweraction.R")

#data without neutral, because there is no away/toward for neutral trials: data emotion x side
d_es <- data_clean %>%
  filter(emotion != "Neutral")%>%
  droplevels()

#calculate propotions 
paway <- d_es %>%
  #group by subject, session, emotion, side
  group_by(subject, group, session, emotion, away) %>%
  #sum of responses per session, emotion, side for each subject
  summarise(nresp = n()) %>% 
  #calculate the proportion
  mutate(proportion = nresp/sum(nresp))%>%
  mutate(proportion = round(proportion, digits=4))%>%
  ungroup()%>%
  filter(away == 1) %>% #keep only proportion for away, since toward is simply the opposite
  dplyr::select(-nresp)

#add questionnaires to paway dataframe
addtopaway <- data_clean %>%
  dplyr::select(subject, anxstate, anxtrait, self_esteem, bis, bas, dominance, affiliation) %>%
  group_by(subject) %>%
  filter(row_number() <=1)
paway <- inner_join(paway, addtopaway)#, by = c('subject'))
paway

# relevel posture with Expansive first for the new dataframe "paway"
paway <- paway %>%
  mutate(group = relevel(group, "Expansive")) #group Expansive first in graphs and tables

#calculate change from session 1 to 2
pawaydiff <- paway %>%
  group_by(subject,emotion) %>%
  mutate(diff2vs1 = proportion[session==2]-proportion[session==1])%>%
  filter(row_number()<=1) %>% #only keep first line for each subject (otherwise sample size is double, 2 identical lines for each subject)
  dplyr::select(-session, -away, -proportion)%>%
  ungroup()

#wide data format: one column per emotion
paway_w = spread(paway, key = emotion, value = proportion)
pawaydiff_w = spread(pawaydiff, key = emotion, value = diff2vs1)

#save data for use in other scripts (e.g. the questionnaires script)
save(paway, pawaydiff, paway_w, pawaydiff_w, file = "./data/paway_79sub_filt5perc.R")

# formulas to calculate cohen's d (used throughout the script) ####
#dz: round(with(t_EvsC1, statistic.t/sqrt(parameter.df+1)), digits=2);
#to calculate cohen's d=2*t/sqrt(1/n1 + 1/n2): round(with(ttestdata,statistic.t*sqrt(1/n1+1/n2)), digits=2)


# descriptive statistics ####

# n participants per emotion and session in each group
with(paway, xtabs(~group+session+emotion)) 
n <- 79; n1 <- 40; n2 <- 39

#define baseline level of factors posture
paway$group <- relevel(paway$group, "Expansive")
pawaydiff$group <- relevel(pawaydiff$group, "Expansive")

#create subsets of data per posture
paway_e <- paway %>%
  filter(group == "Expansive")%>%
  droplevels()
paway_c <- paway %>%
  filter(group == "Constrictive")%>%
  droplevels()

# create Table 1 with means, sd, CIs
desc <- paway %>%
  dplyr::select(group, subject,session, emotion, proportion)%>%
  mutate(proportion = round(proportion, 4)*100)%>%#transform proportion to percent 0-100
  #grand mean per group
  group_by(group)%>% 
  mutate(grandmean = mean(proportion)) %>%
  # n in each group (conditions included to count each subject only once)
  group_by(group, session, emotion)%>% 
  mutate(n=n())%>% 
  #normalized mean for each subject: raw level minus subject mean plus grand mean for each group
  group_by(subject)%>% 
  mutate(normdiff = proportion - mean(proportion) + grandmean)%>% 
  #mean, sd and normed sd per group and condition
  group_by(group, session, emotion, n)%>%
  summarise(mean=mean(proportion),
            sd = sd(proportion), 
            normsd = sd(normdiff))%>%
  # # Cohen's d and betwen subject CIs
  # mutate(dvs50 = (mean-0.5)/sd, #cohen's d against 0.50
  #        bci_low = mean - 1.96*sd/sqrt(n),
  #        bci_up = mean + 1.96*sd/sqrt(n))%>%
  # within-subject CIs with Morey correction for 4 conditions: session x emotion (see below for more details)
   mutate(wci_low = mean - 1.96*normsd/sqrt(n)*sqrt(4/(4-1)), 
         wci_up = mean + 1.96*normsd/sqrt(n)*sqrt(4/(4-1)))#,
         #wci=1.96*normsd/sqrt(n)*sqrt(4/(4-1))) #only ci without mean
desc$normsd <- NULL
desc

#write to excel
desc[,c("mean", "sd","wci_low","wci_up")] <- round(desc[,c("mean", "sd","wci_low","wci_up")], digits=2)
desc$CI <- paste(desc$wci_low, desc$wci_up, sep="-")
desc <- dplyr::select(desc, -wci_low, -wci_up)
write.csv2(desc, file="output/Table1_means_sd_cis.csv")

#within-subject confidence intervals: Cousineau method with Morey correction (Baguley, 2012)
#to remove within-participant variance, subtract participant mean from their raw scores in each condition, then add the grand mean to these normalised participant means 
#because normalised data has lower variability (induces positive correlation): correct for number of within-conditions sqrt(c / (c-1)) to make CIs slightly larger

# ANOVAS on proportion of choice (paway) ####

#these ANOVAS all produce a warning because of unequal number of participants (39 vs. 40) in the 2 posture groups. 
#This is one of the reasons the analysis in the main manuscript was run as a LMEM. 

#entire data set
a <- ezANOVA(data = paway, dv = proportion, wid = subject, between=group, within = c(emotion, session), type = 3); 
#partial eta-squared for all lines: apply function peta (defined at beginning of script) to each line in the a$ANOVA data frame
a$ANOVA[,"peta"] <- round(sapply(seq(1,nrow(a$ANOVA),1), peta), digits=3); a

#entire data set on change S1-S2
a <- ezANOVA(data = pawaydiff, dv = diff2vs1, wid = subject, between=group, within = c(emotion), type = 3)
a$ANOVA[,"peta"] <- round(sapply(seq(1,nrow(a$ANOVA),1), peta), digits=3); a

#expansive group
a <- ezANOVA(data = paway_e, dv = proportion, wid = subject, within = c(emotion, session), type = 3) 
a$ANOVA[,"peta"] <- round(sapply(seq(1,nrow(a$ANOVA),1), peta), digits=3); a

#constrictive group
a <- ezANOVA(data = paway_c, dv = proportion, wid = subject, within = c(emotion, session), type = 3)
a$ANOVA[,"peta"] <- round(sapply(seq(1,nrow(a$ANOVA),1), peta), digits=3); a

# A) t-tests on change paway from S1 to S2 ####

# A1) Change in paway S1 - S2 against zero separately for each posture and emotion - in SI and Figure 3a ####
with(subset(pawaydiff_w, group == "Constrictive"), t.test(x = Anger, mu = 0))
with(subset(pawaydiff_w, group == "Constrictive"), t.test(x = Fear, mu = 0))
with(subset(pawaydiff_w, group == "Expansive"), t.test(x = Anger, mu = 0)) 
with(subset(pawaydiff_w, group == "Expansive"), t.test(x = Fear, mu = 0))

#cohen's d
change_vs_0 <- pawaydiff %>%
  group_by(group,emotion)%>%
  summarise(mean =mean(diff2vs1), sd = sd(diff2vs1))%>%
  mutate(dvs0 = (mean)/sd)%>%
  ungroup()
change_vs_0

# A2) expansive vs constrictive: is change in one group bigger than in the other? - in SI and Figure 3b####

#levene-Tests homogeneity of variances
with(pawaydiff_w, leveneTest(Anger, group))#variance equal
with(pawaydiff_w, leveneTest(Fear, group))

# t-tests between posture groups
with(pawaydiff_w, t.test(Anger~group, paired = F, var.equal=T ))
with(pawaydiff_w, t.test(Fear~group, paired = F, var.equal=T))

#cohen's d between-sub
change_vs_0$posediff=c(1:2,1:2) #things to compare get the same number
posediff_change <- change_vs_0 %>%
  group_by(posediff)%>%
  summarize(mean=mean[2]-mean[1],  #C-E difference per emotion
            sdpooled = (sd[2]+sd[1])/2,
            dEvsC = mean/sdpooled) %>%
  ungroup()
posediff_change$emo <- c("Anger", "Fear")
posediff_change

#Anger vs Fear - within subject change in response to 2 different signals (threat vs affiliation) - not in paper
with(pawaydiff, t.test(diff2vs1 ~ emotion, paired = T, subset=group=="Expansive"))#Lakes spreadsheet: d=-0.19
with(pawaydiff, t.test(diff2vs1 ~ emotion, paired = T, subset=group=="Constrictive"))# d=.40

#cohen's d
emodiff_change <- pawaydiff_w %>%
  dplyr::select(subject,group,Anger,Fear) %>%
  group_by(subject)%>%
  mutate(AdiffvsFdiff=Anger-Fear) %>%
  ungroup()%>%
  group_by(group)%>%
  summarize(mean=mean(AdiffvsFdiff),
            sd = sd(AdiffvsFdiff),
            dAvsF = mean/sd) %>%
  ungroup()
emodiff_change

# B t-tests on paway - not all reported in the paper ####
# B1) expansive vs constrictive: independent-tests & cohen's d at baseline and session 2 for each emotion separately - in SI and Figure 3a#### 

#split wide data (one column per emotion )
paway_w1 <- subset(paway_w, session==1)
paway_w2 <- subset(paway_w, session==2)

#levene-Tests homogeneity of variances - all variances equal
with(paway_w1, leveneTest(Anger, group))
with(paway_w1, leveneTest(Fear, group))
with(paway_w2, leveneTest(Anger, group))
with(paway_w2, leveneTest(Fear, group))

#t-test session 1
t_EvsC1 <- as.data.frame(t(sapply(paway_w1[,c("Anger","Fear")], function(x)
  round(unlist(t.test(x~ paway_w1$group, paired=F, var.equal=T)[c("estimate","parameter", "p.value","statistic")]),digits=3))))
#t-test session 2
t_EvsC2 <- as.data.frame(t(sapply(subset(paway_w2, session == 2)[,c("Anger","Fear")], function(x)
  round(unlist(t.test(x~paway_w2$group, paired=F, var.equal=T)[c("estimate","parameter", "p.value","statistic")]),digits=3))))
#bind sessions into one dataframe
session = factor(c(1,1,2,2),levels = c(1,2)) #create factor session
t_EvsC <- cbind(session, rbind(t_EvsC1,t_EvsC2))

#cohen's d from t-value and n
t_EvsC$d <- round(with(t_EvsC,statistic.t*sqrt(1/n1+1/n2)), digits=3); t_EvsC

# B2) paway against 0.5: one sample t-tests & cohen's dz in each condition (emotion x session x posture) - not in paper ####
t_vs_50_e1 <- as.data.frame(t(sapply(subset(paway_w, group =="Expansive"& session == 1)[,c("Anger","Fear")], function(x)
  round(unlist(t.test(x, mu=0.5)[c("estimate","parameter", "p.value","statistic","conf.int")]),digits=3))))
t_vs_50_e2 <- as.data.frame(t(sapply(subset(paway_w, group =="Expansive"& session == 2)[,c("Anger","Fear")], function(x)
  round(unlist(t.test(x, mu=0.5)[c("estimate","parameter", "p.value","statistic","conf.int")]),digits=3))))
t_vs_50_c1 <- as.data.frame(t(sapply(subset(paway_w, group =="Constrictive"& session == 1)[,c("Anger","Fear")], function(x)
  round(unlist(t.test(x, mu=0.5)[c("estimate","parameter", "p.value","statistic","conf.int")]),digits=3))))
t_vs_50_c2 <- as.data.frame(t(sapply(subset(paway_w, group =="Constrictive"& session == 2)[,c("Anger","Fear")], function(x)
  round(unlist(t.test(x, mu=0.5)[c("estimate","parameter", "p.value","statistic","conf.int")]),digits=3))))
t_vs_50 <- rbind(t_vs_50_e1, t_vs_50_e2,t_vs_50_c1,t_vs_50_c2);
names(t_vs_50) <- c("estimate_mean_diff", "df", "p-value", "t-value", "CI_low", "CI_up")
t_vs_50$emotion <- rep(c("Anger", "Fear"), times=4)
t_vs_50$group <- as.factor(c(rep("Expansive", times=4), rep("Constrictive", times=4)))
t_vs_50$session <- as.factor(rep(c(1,1,2,2), times = 2))
t_vs_50 <- t_vs_50[c("session","group", "emotion", "estimate_mean_diff", "df", "p-value", "t-value", "CI_low", "CI_up")]

#paway against 0.5: calculate mean, sd and cohen's dz against 50 (dependent t-test) 
d_vs_50 <- paway %>%
  group_by(session, group,emotion)%>%
  summarise(mean=mean(proportion), sd = sd(proportion))%>%
  mutate(dvs50 = (mean-0.5)/sd)%>%
  ungroup()%>%
  arrange(group,session)

#display t-test, mean, sd and cohen's d for paway against 0.5
paway_vs_50 <- as.data.frame(d_vs_50)%>%
  inner_join(t_vs_50)%>% #inner_join only adds columns not already in d_vs_50
  arrange(session) #order by session
paway_vs_50

# B3) Anger vs Fear: dependent t-tests in each session and each posture separately - not in paper ####
with(paway_e, t.test(proportion ~ emotion, paired = T, subset=session==1)) #S1 expansive
with(paway_e, t.test(proportion ~ emotion, paired = T, subset=session==2)) #S2 expansive
with(paway_c, t.test(proportion ~ emotion, paired = T, subset=session==1)) #S1 constrictive
with(paway_c, t.test(proportion ~ emotion, paired = T, subset=session==2)) #S2 constrictive

# B4) session 1 vs 2: cohen's dz in each emotion x posture condition - not in paper ####
# assign number to each emotion x posture condition, to calculate d of difference between sessions
d_vs_50$sessiondiff = c(1,2,1,2,3,4,3,4)
#calculate dz 
d_session <- d_vs_50 %>% 
  group_by(sessiondiff) %>% 
  summarize(meandiff=mean[2]-mean[1], #mean of the difference scores
            sdpooled = (sd[2]+sd[1])/2, #standard deviation of the difference scores
            dS1vsS2 = meandiff/sdpooled)
d_session$pose_emo <- c("E Anger", "E Fear", "C Anger", "C Fear")
d_session

