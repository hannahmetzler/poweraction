#figures for paper power posture and action decision in response to social threat

#packages
require(dplyr); require(scales);require(ggplot2);require(cowplot)
require(ggsignif)#for significance asterisks in plots
# theme_set(theme_cowplot())

# short script to calculate proportion from variable away coded as 0 and 1 for each trial
source('scripts/stat_proportion.R')

#data
load("data/behavior_poweraction.R")

# prepare data for plots
plotData <- data_clean %>%
  # label the different levels of factors
  mutate(session = factor(session, levels = c(1,2), labels = c("Baseline", "With Posture"))) %>%
  #transform initiation and move time into miliseconds 
  mutate(init_time = round(init_time*1000))%>%
  mutate(mov_time = round(mov_time*1000))

# colour for all plots
colpose <- c("magenta4", "mediumseagreen") #colours for posture group: expansive red, constrictive green

# formatting for Figure 3 
s = 30 #text size
pd = position_dodge(width = 0) #distance between dots with CIs of the two posture groups

# Prepare data for Figure 3: calculate proportion of choice (paway) ####
#calculate proportion of away responses per condition and group for each subject
paway_fig <- plotData %>%
  filter(!emotion == "Neutral")%>% # exclude neutral trials because there is no away/toward emotion for neutral pairs, both actors neutral
  group_by(subject, session, group, emotion) %>%
  summarise(proportion = mean.proportion(away)) %>% #calculate mean proportion with function in script stat_proportion.R
  ungroup()

#calculate difference in proportions of away responses per condition and group for each subject
pawayd_fig <- paway_fig %>%
  group_by(subject,emotion) %>%
  mutate(diff2vs1 = proportion[session=="With Posture"]-proportion[session=="Baseline"])%>% #calculate difference between sessions
  filter(row_number()<=1) %>% #keep only first line per diff2vs1 (otherwise sample size is double)
  dplyr::select(-session, -proportion)%>%
  ungroup()%>%
  # #create a factor with positive/none/negative change - to see how many participants show the effect in which direction
  mutate(change_direction = factor(ifelse(diff2vs1 > 0.01, "1", 
                                           ifelse(diff2vs1<(-0.01), "-1", "0")), 
                                    levels=c(-1,0, 1), labels = c("negative", "change<1%","positive")))

# reorder factor levels of posture group so that expansive always comes first in graphs in each data set ####
plotData$group <- relevel(plotData$group, "Expansive")
paway_fig$group = relevel(paway_fig$group,"Expansive")
pawayd_fig$group = relevel(pawayd_fig$group,"Expansive")

# Figure 3a ####

# calculate within-subject confidence intervals for paway (in Figure 3b) Cousineau method with Morey correction (Baguley, 2012)
statsp <- paway_fig %>%
  dplyr::select(group, subject,session, emotion, proportion)%>%
  #grand mean per group
  group_by(group)%>% 
  mutate(grandmean = mean(proportion)) %>%
  # n in each group (conditions included to count each subject only once)
  group_by(group, session, emotion)%>% 
  mutate(n=n())%>% 
  #normalized difference for each subject: raw level minuse subject mean plus grand mean for each group
  group_by(subject)%>% 
  mutate(normdiff = proportion - mean(proportion) + grandmean)%>% 
  #mean, sd and normed sd per group and condition
  group_by(group, session, emotion, n)%>%
  summarise(mean=mean(proportion),
            sd = sd(proportion), 
            normsd = sd(normdiff))%>%
  # within-subject CIs with Morey correction for 4 conditions: session x emotion
  mutate(ci=1.96*normsd/sqrt(n)*sqrt(4/(4-1)),
         ci_low = mean-ci,
         ci_up = mean+ci)

#plot effect of posture (emotion x session x posture)

#dot plot 4 cells with within-subject lines
dots.choice <- ggplot(data=statsp, aes(x=session, y=mean, colour=group, shape=group))+
  facet_grid(~emotion) + #split plot per emotion
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width = 0.3, size = 1.5, position=pd)+ #CIs
  geom_line(aes(group=group), size=1.25, position=pd) + #connecting lines from session 1 to 2
  geom_point(size=7, position=pd) + #means per posture/session
  scale_colour_manual(values = colpose, name="Group")+ #colours for posture
  scale_shape_discrete(solid=T, name="Group") + #shape for posture
  geom_abline(intercept = 0.5, slope = 0, colour = "dimgray", linetype = 2)+ #line at 50
  ylab("p(away)") + #y-axis label
  #figure design
  theme_bw() +theme(text = element_text(size=s), #text size
                    axis.title.x=element_blank(), # no x axis title
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank(), #no grid lines
                    legend.position = c(0.75, 0.8))+ #legend position in plot
  #coordinates of y axis
  coord_cartesian(ylim=c(0.48,0.57))+scale_y_continuous(breaks=seq(0.48,0.58,0.02))+
  #add asterisk for significant change in constrictive
  geom_text(data=data.frame(emotion = "Anger", group="Constrictive"), #specify part of plot and colour for group
            mapping = aes(x = 1.5, y = 0.538, label = "*"), size=10, show.legend=F, fontface="bold")+
  #add asterisk for significant difference between postures Anger in session 2
  geom_text(data=data.frame(emotion = "Anger", group="Constrictive", session="With Posture"), #part of plot, on top of which session (x position), group needed for default colour of asterisk)
            mapping = aes(y = 0.565, label = "*"), size=10, show.legend=F, colour="black", fontface="bold")

# Figure 3b bar plot pf paway difference/change from session 1 to 2 ####
# calculate within-subject confidence intervals for difference S2-S1 in paway (Figure 3b)
statsd <- pawayd_fig %>%
  dplyr::select(group, subject,emotion, diff2vs1)%>%
  #grand mean per group
  group_by(group)%>% 
  mutate(grandmean = mean(diff2vs1)) %>%
  # n in each group (conditions included to count each subject only once)
  group_by(group, emotion)%>% 
  mutate(n=n())%>% 
  #normalized difference for each subject: raw level minuse subject mean plus grand mean for each group
  group_by(subject)%>% 
  mutate(normdiff = diff2vs1 - mean(diff2vs1) + grandmean)%>% 
  #mean, sd and normed sd per group and condition
  group_by(group, emotion,n)%>%
  summarise(mean=mean(diff2vs1),
            sd = sd(diff2vs1), 
            normsd = sd(normdiff))%>%
  #calculate wthin-subject CIs with Morey correction
  mutate(ci = 1.96*normsd/sqrt(n)*sqrt(2/(2-1)),#2 within-subject conditions: emotion
         ci_low = mean-ci,
         ci_up = mean+ci)%>%
  ungroup()

# individual dots posture as columns ####
dots.change <- ggplot(statsd, aes(x=group, y=mean, shape=group)) +
  facet_grid(~emotion) + #split plot by emotion
  #participant data points
  geom_dotplot(data=pawayd_fig, aes(y=diff2vs1), fill="darkgrey", binwidth = 0.01, dotsize=0.5, 
               binaxis = "y", stackdir = "centerwhole")+
  geom_errorbar(aes(colour=group, ymin=mean-ci, ymax=mean+ci), width = 0.2, size = 1.5, position=pd, colour=rep(colpose, 2))+ #CIs
  geom_point(aes(colour=group), size=7, position=pd)+ #mean change
  scale_colour_manual(values = colpose,name="Group")+
  #scale_shape_discrete(solid=T, name="Group") + #shape for posture
  # limits and breaks of y coordinate
  coord_cartesian(ylim=c(-0.14,0.17))+ scale_y_continuous(breaks=c(-0.1,-0.05,0,0.05, 0.10, 0.15))+
  geom_abline(intercept = 0, slope = 0, colour = "dimgray", linetype = 2)+
  ylab("change p(away)")+
  #identical design parameters as figure 3a 
  theme_bw() +theme(text = element_text(size=s),
                    axis.title.x=element_blank(),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                    legend.position ="none")+
  #add asterisk for significant difference between postures for change anger
  geom_text(data=data.frame(emotion = "Anger", group="Constrictive"), #specify part of plot and group (group needed because it is the colour variable in the dataframe, default position of asterisk)
            mapping = aes(x = 1.5, y = 0.14, label = "*"), size=10, show.legend=F, fontface="bold")+
  #add line between postures
  geom_segment(data=data.frame(emotion="Anger", group=c("Expansive", "Constrictive")),
               y=0.135, yend=0.135, x=1, xend=2, size=1)+
  #add asterisk for significant difference between postures for change anger 
  geom_text(data=data.frame(emotion = "Anger", group="Constrictive"), #part of plot, on top of which group
            mapping = aes(y = 0.058, label = "*",colour=group), size=10, show.legend=F, fontface="bold")


#how many participants show the effect in the group direction? uncomment lines above where change_direction is created, to use this
with(pawayd_fig, table(change_direction, group, emotion))

#plot proportion per session and change between sessions next to each other: 
plot_grid(dots.choice, dots.change, labels = c("a", "b"), label_size = 30)

#saving a pdf with the plots in high resolution
ggsave("./figures/Figure3_choice_dotplot.pdf", plot = last_plot(), device = NULL, path = NULL,
       scale = 1, width = 18, height = 7, units = c("in", "cm", "mm"), #15 statt 14?
       dpi = 300, limitsize = TRUE)

# Reaction times plot Figure 4 ####

#text size for reaction time plots
s=30

plot_mouse<- plotData %>%
  mutate(away = factor(away, levels = c(1,0), labels = c("Away", "Toward")))%>%
  mutate(threat = factor(ifelse(emotion=="Neutral", 0,1), levels=c(0,1), labels=c("Neutral", "Threat"))) %>% #factor with anger and fear combined to threat (level 1) vs. neutral (level 0)
  #subject means per threat condition and session - sd, CI etc calculated based on variation between participants (not within each participant)
  group_by(subject, session, group, threat) %>%
  summarise(submean_initt = mean(init_time),
            submean_movt = mean(mov_time))
  
#data frame for making asterisks, with only one row for each condition, because star always compares 2 conditions or groups
asterisks <- data.frame(
  threat=rep(c("Neutral", "Threat"), 2),
  session=rep(c("Baseline", "With Posture"), each=2), 
  group = rep(c("Constrictive", "Expansive"), 2)
)

# Figure 4a: plot initiation time by facial expression: threat vs. neutral ####
#for explanations/comments: see Figure 3a, same commands used, only variables changed

#calculate means and within-subject confidence intervals for initiation time by emotion (Figure 4b)
statsce<- plot_mouse %>%
  dplyr::select(group, subject,session, threat, submean_initt)%>%
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
  mutate(ci = 1.96*normsd/sqrt(n)*sqrt(2/(2-1)))%>% #2 conditions: threat/neutral
  ungroup


#make the plot: initiation time by threat
init_threat <- ggplot(data = statsce, aes(x = threat, y = mean, colour=group, shape=group))+
  facet_grid(~session)+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width = 0.2, size = 1.5)+
  geom_line(aes(group=group), size=1.25) + #connecting lines from session 1 to 2
  geom_point(size=7) + #means per posture/session
  labs(y = "Initiation time in ms") +  
  scale_colour_manual(values=colpose, name="Group")+#c("dimgray", "gray"))+
  scale_shape_discrete(solid=T, name="Group") + #shape for posture
  theme_bw() +theme(legend.position =c(0.25,0.25), text = element_text(size=s),#c(0.75,0.8)
                    axis.title.x=element_blank(),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(320,440))+scale_y_continuous(breaks=seq(320, 440, 20))+
  #asterisks for threat effect in each group/session (p-value of threat effect of model with each group/session as baseline)
  geom_text(data=asterisks, size=10,show.legend=F, 
            mapping=aes(x=c(1.5, 1.4, 1.5, 1.5), #order: C-1, E-1, C-2, E-2 #
                        y=c(420, 390, 385, 359), #c(420, 395, 383, 359),
                        label="***"))
# # no asterisks for each threat x group cell: t-test/wilcox tests between postures not significant (script action_03)
#  geom_text(asterisks, mapping=aes(y=c(400, 396, 370, 360),
#                                   label=c("", "*", "*", "*")), 
#                                   colour="black", size=10)

asterisks2 <- data.frame(session=c("Baseline", "With Posture"), threat=c("Neutral", "Threat"))

# Figure 4b: movement time by facial expression: threat vs. neutral ####
#calculate means and within-subject confidence intervals for movement time by threat
statsm <- plot_mouse %>%
  dplyr::select(group, subject,session, threat, submean_movt)%>%
  group_by(group)%>% 
  mutate(grandmean = mean(submean_movt)) %>%
  group_by(group, session, threat)%>% 
  mutate(n=n())%>% 
  group_by(subject)%>% 
  mutate(normdiff = submean_movt - mean(submean_movt) + grandmean)%>% 
  group_by(group, session, threat, n)%>%
  summarise(mean=mean(submean_movt),
            sd = sd(submean_movt), 
            normsd = sd(normdiff))%>%
  mutate(ci = 1.96*normsd/sqrt(n)*sqrt(2/(2-1)))%>%#2 conditions: threat/neutral
  ungroup()

#make the plot: movement time by threat
mov_threat <- ggplot(data = statsm , aes(x = threat, y = mean, colour=group, shape=group))+
  facet_grid(~session)+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width = 0.2, size = 1.5)+
  geom_line(aes(group=group), size=1.25) + #connecting lines from session 1 to 2
  geom_point(size=7) + #means per posture/session
  labs(y = "Movement duration in ms") +  
  scale_colour_manual(values=colpose, name="Group")+
  scale_shape_discrete(solid=T, name="Group") + #shape for posture
  theme_bw() +theme(legend.position ="none", text = element_text(size=s),
                    axis.title.x=element_blank(),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(420,540))+scale_y_continuous(breaks=seq(420,540, 20))+
  #asterisks for threat effect in each group/session 
  geom_text(data=asterisks, size=10,show.legend=F, 
            mapping=aes(x=1.5,
                        y=c(470, 481, 468, 495), #order: C-1, E-1, C-2, E-2
                        label=c("***", "***", "*", "**"))) #order: C-1, E-1, C-2, E-2


# new Figure 4: add threat effect of intitiation time and movement duration together in one plot
plot_grid(init_threat, mov_threat, labels = c("a", "b"), label_size = 30)

ggsave("./figures/Figure4_kinematics_threat_dotplot.pdf", plot = last_plot(), device = NULL, path = NULL,
       scale = 1, width = 18, height = 7, units = c("in", "cm", "mm"),
       dpi = 300, limitsize = TRUE)

