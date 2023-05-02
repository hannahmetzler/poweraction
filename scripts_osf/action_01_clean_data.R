# SCRIPT to clean behavioural data poweraction ####

# VARIABLE NAMES ####
#pair = index of pair (pair of actors depicted in stimuli) (1-10)
# gend = pair gender (1:Male 2:Female)
# gind =  gender index for left actor
# gind2 =  gender index for right actor
# emotpair =  emotion of the pair (1 = 1 actor angry, 2= 1 actor fearful 3 = both actors neutral)
# emot =  emotion for the left actor (1:Anger 2:Fear 3:Neutral)
# emot2 =  emotion for the right actor (1:Anger 2:Fear 3:Neutral)
# elevpair = emotion level of the pair (i.e. the one actor expressing an emotion) (0=neutral,3=low intensity,5,7=high intensity)
# elev =  emotion level for the left actor (0,3,5,7)
# elev2 =  emotion level for the right actor (0,3,5,7)
# sidemotpair =  emotional actor side (1:Left  2:Right 3:Neutral pair)
# sidemot =  is the left actor emotional (0: No 1: Yes 3:Neutral pair)
# sidemot2 =  is the right actor emotional (0: No 1: Yes 3:Neutral pair)
# response = 1 Left chair, 2 right chair, 3 no response, 4 outside chair
#kinematics of movement: 
# mov_time = time from click in the middle until release on the chair, called movement time in the paper
# init_time = time to click from appearance of scene, called initiation time in the paper

# LOAD PACKAGES ####
require(dplyr); require(tidyr); require(ggplot2); require(car)

#set working directory (fill in the link to the folder with subfolders data, scripts, figures, output here)
# setwd('~/Documents/PhD_lab/Projects/Poweraction/Posture_Action_OSF/')

# PRINT PDF FIGURES while running the script? ####
figures <- FALSE # TRUE or FALSE

# READ AND PREPARE DATA ####
data <- read.table("./data_osf/behav_poweraction.txt", sep = "\t", head = T)
names(data) <- tolower(names(data)) #remove capital letters
table(data$session, data$subject)#check number of trials per session

#delete unnecessary variables
#data$pair <- NULL; data$gend<- NULL; 
data$sidemot<- NULL; data$sidemot2<- NULL; data$gind<- NULL; data$gind2<- NULL; 
data$emot<- NULL; data$emot2<- NULL; data$elev<- NULL; data$elev2<- NULL

summary(data)

#simplify variable names: 
#instead of typing them manually, print them to Console with function as.vector()
#as.vector(names(data), mode="expression")
#copy-paste variable names from Console into this vector and rename columns of data: 
#copied: ("subject", "session", "posture", "pair", "gender", "emotpair", "elevpair", "sidemotpair", "response", "mov_time", "init_time", "trial")
names(data) <- c("subject", "session", "posture", "stim_pair", "stim_gender", "emotion", "level", "side_emo",   "response", "mov_time", "init_time", "trial")

#create variable "away" from variable side_emo as 1=away, 0=toward
data <- data %>%
  mutate(away = ifelse(response == side_emo, 0, 1)) %>%
  mutate(session = as.factor(session))%>% #transform variable session to factor
  mutate(ratio_init_mov  = init_time/ mov_time)

# ADD QUESTIONNAIRE DATA (STAI etc) ####
q <- read.table("data/questionnaires.txt", sep = "\t", head = T)
q[,2:length(q)] <- scale(q[,2:length(q)], center = TRUE, scale = T)#center to mean 0
#add questionnaire data to behavioural data
data <- data %>%
  inner_join(q)%>%
  arrange(subject,session,trial)

# ORGANIZE DATA ####

#create a variable group, that indicates posture condition across both sessions
#data from session 2 indicates the posture
d_s2 <- data %>%
  filter(session == 2) %>%
  mutate(group = factor(ifelse(posture == 1, "Expansive", "Constrictive")))
#data from session 1 has no posture yet
d_s1 <- data %>%
  filter(session == 1)
#copy variable group from session 2 to session 1 data
d_s1$group <- d_s2$group
#combine session 1 and 2 in one data frame
data <- rbind(d_s1, d_s2) %>%
  mutate(subject = as.factor(subject))

#all variables included now -  order them
col_order_b <- c("subject", "session", "trial", "emotion", "level", "response", "mov_time", "init_time", "away", 
                 "side_emo", "posture",  "group", "ratio_init_mov",
                 "anxstate", "anxtrait", "self_esteem", "bis", "bas", "dominance", "affiliation",
                 "stim_pair", "stim_gender")
data <- data[, col_order_b]

# FILTER INVALID TRIALS AND LOOK AT MOV_TIME AND INIT_TIME DISTRIBUTION (kinematics) ####

# exclude bad trials: 
# (1) response outside chair (wrong), no response (indices = 3 and 4) 
# (2) click/initiation on top of chair instead of square at bottom center (not as instructed)
# (3) starting to move before scene appears

# (1) filter wrong trials or no response (indices 3 and 4)
data_correct <- data %>%
  filter(response <=2)
# (2) and (3) follow after distribution figures, see below

# DISTRIBUTIONS PLOTS movement time and inititation time (time until click)
if(figures == TRUE){
  graphics.off()
  pdf("./figures/distribution_mov_init_time.pdf")
  #movement time Boxplot and histogram of all trials
  Boxplot(mov_time ~ session, data=data_correct) #labels are line numbers
  ggplot(data_correct, aes(x = mov_time, fill = session)) +  geom_histogram(breaks = seq(0,1,0.05))+ facet_grid(~session)+ggtitle("mov_time correct trials")
  #initiation time Boxplot and histogram of all trials
  Boxplot(init_time ~ session, data=data_correct)
  ggplot(data_correct, aes(x = init_time, fill = session)) +  geom_histogram(breaks = seq(0,1,0.05))+ facet_grid(~session)+ggtitle("init_time correct trials")
  #shows peak at zero
  
  #calculate mean and median mov_times and init_times (and their ratio) per subject and session to make plots of these central tendencies 
  #(ratio makes sense because total trial duration is fixed, if quicker initiation, there is more time for movement, and the reverse, 
  #so outliers in one should be outliers in the other in the reverse direction)
  stats_time <- data_correct %>%
    group_by(session,subject) %>%
    summarise(mean_mov_time = mean(mov_time), 
              median_mov_time = median(round(mov_time, digits = 2)),
              mean_init_time = mean(init_time),
              median_init_time = median(round(init_time, digits = 2)), 
              mean_ratio = mean(ratio_init_mov), 
              median_ratio = median(round(ratio_init_mov, digits=2))) %>%
    ungroup()
  
  #mov_time plots of mean and median per session
  #histogram of mean
  ggplot(stats_time, aes(x = mean_mov_time, fill = session)) +  geom_histogram(breaks = seq(0,1,0.05))+ facet_grid(~session)+ggtitle("Mean movement time per session")
  #Which subjects are outliers if we take the mean?
  stats_time[Boxplot(mean_mov_time~ session, data=stats_time,ylab = "Mean mov_time", main= "labels = rownumbers = sub 72 and 3"),]$subject #outlier: 3 and 72
  #same for the median
  ggplot(stats_time, aes(x = median_mov_time, fill = session)) +  geom_histogram(breaks = seq(0,1,0.05))+ facet_grid(~session)+ggtitle("Median movement time per session")
  stats_time[Boxplot(median_mov_time~ session, data=stats_time,main= "labels = rownumbers = sub 72", ylab = "Median mov_time"),]$subject #outlier: 3 and 72
  #72 and 3 are constantly too quick, with both mean and median
  
  #now do the same for init_time: histogram of mean and median per session
  #Means histogram
  ggplot(stats_time, aes(x = mean_init_time, fill = session)) +  geom_histogram(breaks = seq(0,1,0.05))+ facet_grid(~session)+ggtitle("Mean initiation time per session")
  #Means Boxplot
  Boxplot(mean_init_time ~ session, data=stats_time, main= "labels below CIs = rownum = sub 46")
  #which subjects are outliers below the lower limit of the CI?
  stats_time[c(129),]#subject 46 in sessino 2 - clicks without waiting that scene appears (mean initiation time after scene onset = 60ms)
  #Median
  ggplot(stats_time, aes(x = median_init_time, fill = session)) +  geom_histogram(breaks = seq(0,1,0.05))+ facet_grid(~session)+ggtitle("Median initiation time  per session")
  #which subjects are outliers?
  stats_time[Boxplot(median_init_time ~ session, data=stats_time, main ="sub 46 31 52 84"),]$subject#46 31 52 84 start anticipating scene in session 2
  
  # ratio init_time/mov_time plots (same code as above for mean and median)
  ggplot(stats_time, aes(x = mean_ratio, fill = session)) +  geom_histogram(breaks = seq(0,12,0.5))+ facet_grid(~session)+ggtitle("Mean Ratio init_time/mov_time per session")
  Boxplot(mean_ratio ~ session, data=stats_time)
  ggplot(stats_time, aes(x = median_ratio, fill = session)) +  geom_histogram(breaks = seq(0,12,0.5))+ facet_grid(~session)+ggtitle("Median Ratio init_time/mov_time per session")
  Boxplot(median_ratio ~ session, data=stats_time)
  dev.off()
}

# TRESHOLDS FOR FILTERING VALID MOVEMENT AND INITIATION TIMES ####
# distribution plots show that some subjects move quicker than possible if moving as instructed
# and others initiate movement quicker than possible if waiting until scene appears 
# we therefore filter for trials below 5th percentile of quickest move and init times

# (2) and (3) check 5% percentile in mov_time(movement) and init_time (init_time)
mtperc5 <- quantile(data_correct$ mov_time , probs = c(0.05))[[1]]# 0.1839603s
itperc5 <- quantile(data_correct$init_time, probs = c(0.05))[[1]]# 0.09459017 
# these  5th percentiles are taken as thresholds for correct (=valid) trials: 
# (2) movement time below 180ms is only possible if start location not in the square, but already on top of chair
# (3) initiation of response below 90ms is only possible if starting before scene is presented, anticipation possible because intertrial-interval was fixed

# PERCENTAGE OF TRIALS ABOVE THRESHOLS FOR MOVE AND INITIATION TIME - EXCLUDE SUBJECTS WITH LESS THAN 50% ####
#movement time
perc_mov_time <- data_correct %>%
  group_by(session,subject) %>%
  mutate(validmt = ifelse(mov_time < mtperc5, 0, 1)) %>% #if smaller than threshold for valid mov_times at 5th percentile, then 0, otherwise 1
  summarise(perc_mov_time = round(sum(validmt/480*100), digits = 1))  %>% # how many out of 480 trials in total?
  mutate(subject = as.factor(subject)) %>% #transform subject to a factor (needed for plots below)
  ungroup()
#initiation time
perc_init_time <- data_correct %>%
  group_by(session,subject) %>%
  mutate(validit = ifelse(init_time < itperc5, 0, 1)) %>% #if below threshold for valid initiation times at 5th percentile, than 0, otherwise 1
  summarise(perc_init_time = round(sum(validit/480*100), digits = 1))  %>%
  mutate(subject = as.factor(subject)) %>%
  ungroup()

# plot the percentage of  move and initiation time above thresholds for each subject, 1 dot per session
if(figures == TRUE){
  graphics.off()
  pdf("./figures/percent_above_5tquantile_time_correct_trials.pdf", width = 15)
  ggplot(perc_mov_time, aes(x=subject, y = perc_mov_time)) +  geom_count() + ggtitle("Percent trials> move time 184ms per session & subject") +
    scale_y_continuous(breaks=seq(0,100,10))
  #mov_time filter, below 50% of trials below threshold: subject 3, 38, 72
  ggplot(perc_init_time, aes(x=subject, y = perc_init_time)) +  geom_count() + ggtitle("Percent trials> initiation time 95ms per session & subject") +
    scale_y_continuous(breaks=seq(0,100,10))
  #init_time filter, below 50%: subject 31, 46, 52
  dev.off()
}

#subjects below the 50% valid trials threshold for move or initiation time will be excluded (not enough valid trials outliers) 
excluded <- unique(c(as.vector(subset(perc_mov_time, perc_mov_time < 50)$subject), as.vector(subset(perc_init_time, perc_init_time < 50)$subject)))

# CLEAN DATA: FILTER TRIALS BELOW AND INIT TIME THRESHOLDS ####
data_filt <- data_correct %>%
  filter(mov_time >= mtperc5) %>% #filters trials in which participants clicked on top of chair instead of bottom center
  filter(init_time >= itperc5) #%>% #filters trials in which participants started to move before scene appeared

#verify if filtered
#ggplot(data_filt, aes(x = mov_time, fill = session)) +  geom_histogram(breaks = seq(0,1,0.05))+ facet_grid(~session)+ggtitle("mov_time correct trials after filter")
#ggplot(data_filt, aes(x = init_time, fill = session)) +  geom_histogram(breaks = seq(0,1,0.05))+ facet_grid(~session)+ggtitle("init_time correct trials after filter")

# NUMBER OF CORRECT TRIALS LEFT AFTER FILTERING ####
ntrials <- count(data_filt,session,subject)

if(figures == TRUE){
  pdf("./figures/number_valid_trials_session_filt5perc.pdf")
  
  #show boxplot and number of trials 
  ntrials[Boxplot(n ~ session, data=ntrials,ylim = c(0,520), main="total valid trials per session"),]
  #the outliers below the boxes are subjects identified with less than 50% of valid trials, will be excluded from analyses
  
  #dotplot of number of trials per subject
  ggplot(ntrials, aes(x=paste(session), y=n)) + geom_point() + ggtitle("total valid trials per session")
  #subjects with lowest numbers of trials in session 1: 3, 31, 37, 38; in session 2: 31, 38, 46, 52
  
  dev.off()
}

# count correct trials after filtering per condition (session by emotion)
ntrials_cat <- data_filt%>%
  count(session,subject, emotion)
#subjects who have less than 80 trials per condition
as.data.frame(subset(ntrials_cat, n<80))
as.data.frame(subset(ntrials_cat, n<60))

#number of trials in remaining subjects (those who have less than 50% trials above mov_time and init_time thresholds)
summary(subset(ntrials_cat, !(subject %in% c(3, 31, 38, 46, 52, 72)))) #mean = 132, median = 138 (of 160 total presented trials per emo x session)
with(subset(ntrials_cat, !(subject %in% c(3, 31, 38, 46, 52, 72))), sd(n)) #standard deviation

#plot number of trials after filtering per condition
if(figures == TRUE){
  pdf("./figures/number_valid_trials_condition_filt5perc.pdf", width = 10)
  ntrials_cat[Boxplot(n ~ paste(session, emotion), data=ntrials_cat, main="n trials per subject and condition, max: 160"),]
  ntrials_cat[Boxplot(n ~ paste(session, emotion), data=ntrials_cat, ylim=c(0,80),main="n trials < 80 per subject and condition"),]
  #subject 38 gas below 10 trials in most conditions - has to be excluded
  dev.off()
}

# TOTAL ACCURACY CHECKS ####

# Accuracy describes whether participants performed the task correctly as instructed, and not the variables of interest analysed in the models (choice, kinematics)
# a correct movement is: clicking in gray area at bottom center of screen, hold mouse button, release it on top of a chair area.
# Accuracy is the percentage of trials with the correct movement (irrespective of which chair was chosen)
# Accuracy simply allows to screen for participants who did not perform the movement carefully and as instructed

#Calculate accuracy (percentage of correct trials) after filtering trials below 5th percentiles
Accuracy <- data %>% #start from data with all trials (since we need number of correct/incorrect trials to calculate accuracy)
  #since data including all trials are unfiltered, filter for 5th percentiles of move and initiation time (these trials are not actually the accurate)
  filter(mov_time >= mtperc5) %>% 
  filter(init_time >= itperc5)%>% 
  mutate(subject = as.integer(subject))%>%
  #if response >=3, trial is wrong= 0, otherwise correct =1
  mutate(correct = ifelse(response >= 3, 0, 1)) %>%
  #calculate percentage correct per session in each subject
  group_by(session, subject) %>%
  mutate(ntrials = length(correct))%>% 
  summarise(accuracy = sum(correct/ntrials*100)) %>% 
  ungroup()

#display accuracy scores
as.data.frame(Accuracy[,c("subject", "accuracy")])

#mean accuracy score : 91% (measures if you correctly click at the start location and release witin a chair area, i.e. you manage to do the movement as instructed)
mean_acc <- mean(Accuracy$accuracy) #88.27 => movement was easy to perform correctly, they usually managed
#subjects below mean minus 3 SD
mean_acc_min3sd <- mean_acc - 3*sd(Accuracy$accuracy) #less than 72.55%
Accuracy$subject[Accuracy$accuracy<mean_acc_min3sd] #subjects below 3sd: 29, 36, but even below 3 SD is still a good enough performance (we required at least 60% of trials in training)

#accuracy boxplot (excluding subjects who are excluded based on less than 50% trials above 5th percentiles of kinematics, 
#(reason for excluding: they have many trials which seem correct (i.e. release inside chair area) but weren't performed correctly (click also in chair area))
if(figures == TRUE){
  graphics.off()
  pdf("./figures/accuracy_boxplot_filtered_no_time_outliers.pdf")
  acc_no_out <- Accuracy %>%
    filter(!(subject %in% c("3","31","38", "46","52","72"))) ##filter mov_time and init_time >50% below 5th percentile subjects
  d<- acc_no_out[c(Boxplot(accuracy ~ session, data = acc_no_out,main = "% accurate trials after mt and it filter") ),]; i<- nrow(d); j=c(1:i);
  text(x=1.5, y=85, labels = paste("subid:", d$subject[j[1]], d$subject[j[2]],d$subject[j[3]], d$subject[j[4]]))
  dev.off()
  
  #Plot accurracy per subject across all trials (no kinematic filters, i.e. no subjects excluded)
  pdf("./figures/accuracy_per_subject_all.pdf", width = 15)
  Accuracy <- data %>% #take unfiltered data since we need number of correct/incorrect trials 
    mutate(subject = as.integer(subject))%>%
    #if response >=3, trial is wrong= 0, otherwise correct =1
    mutate(correct = ifelse(response >= 3, 0, 1)) %>%
    #calculate percentage correct per session in each subject
    group_by(session, subject) %>%
    mutate(ntrials = length(correct))%>% 
    summarise(accuracy = sum(correct/ntrials*100)) %>%
    ungroup()
  Accuracy$subject <- as.factor(Accuracy$subject) #ggplot needs subject as factor for the x-axis
  ggplot(Accuracy, aes(x=subject, y=accuracy))+geom_point()+ggtitle("% accuracy in all trials (no filter)")
  dev.off()
}

# EXCLUDE SUBJECTS WITH LESS THAN 50% VALID TRIALS (outliers outside boxplot fences of number of trials) ####

data_clean <- data_filt %>%
  filter(!(subject %in% excluded)) %>%
  droplevels()%>%
  #transform variables to type needed for all other analyses
  mutate(subject = as.factor(subject)) %>%
  mutate(session = factor(session))%>%
  mutate(emotion = factor(emotion, levels = c(1,2,3), labels = c("Anger", "Fear", "Neutral")))%>% #label levels of emotion
  mutate(away = factor(away, levels = c(1,0)))#label levels of away

#same column formats for data_filt
data_filt <- data_filt %>%
  #transform variables to type needed for all other analyses
  mutate(subject = as.factor(subject)) %>%
  mutate(session = factor(session))%>%
  mutate(emotion = factor(emotion, levels = c(1,2,3), labels = c("Anger", "Fear", "Neutral")))%>% #label levels of emotion
  mutate(away = factor(away, levels = c(1,0)))#label levels of away

# SUMMARY OF PROBLEMATIC SUBJECTS -  we need verify how exclusion effects results ####
#stai values above 60: "15","17","25","27" 
#undestood posture was the purpose of experiment : "28" keep for know, see if anxiety impacts model or effects different with 28
#mov_time: 3, 38, 72 have at least 1 session below 50% => exclude on basis of mov_time
#init_time: 31,46,52 have at least 1 session below 50% => exclude on basis of init_time

# which conditions are the movement and initiation time outliers in? 
data %>% 
  filter(subject %in% excluded) %>% 
  select(subject, group) %>% 
  group_by(subject) %>% 
  slice(1)

# SAVE DIFFERENT DATA VERSIONS ####
#all trials and subjects, correct trials only, filtered data, clean data (filtered and subjects with not enough clean trials excluded)
save(data, data_correct, data_filt, data_clean, file = "data/behavior_poweraction.R")

# SUBJECTS IN EACH POSTURE GROUP ####
#which subjects adopted which posture
subject_all_posture<- data[,c("subject","posture")] %>%
  filter(posture > 0) %>% #only posture codes from session 2 (no posture = 0 in session 1)
  group_by(subject) %>%
  filter(row_number() <=1) %>% #only keep first row of each subject
  ungroup()
#export to excel
write.csv(subject_all_posture, file = "./output/subjects_all_posture.csv")

#posture of included subjects
included_subjects_posture<- data_clean[,c("subject","posture")] %>%
  filter(posture > 0) %>%
  group_by(subject) %>%
  filter(row_number() <=1) %>%
  ungroup()
#how many subjects per posture? 
xtabs(~posture, included_subjects_posture)#39 constricted, 40 Expansive
#export to excel
write.csv(included_subjects_posture, file = "./output/included_subjects_posture_50perc_above5perc_mt_it.csv")