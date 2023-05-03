#poweraction figures of proportion choices away from emotional actor
library(dplyr); library(scales);library(ggplot2);library(cowplot)

rm(list=ls())
source('scripts/stat_proportion.R')
load("data/behavior_poweraction.R")

plotData <- data_clean %>%
  mutate(session = factor(session, levels = c(1,2), labels = c("Session 1", "Session 2")))

plotData$group <- relevel(plotData$group, "Expansive") #expansive first in graphs

#split into sessions and posture groups
plotData1 <- plotData %>%
  filter(session == "Session 1")
plotData2 <- plotData %>%
  filter(session == "Session 2")
plotDataE <- plotData %>%
  filter(group == "Expansive")
plotDataC <- plotData %>%
  filter(group == "Constrictive") 

#plot parameters ####
colpose_s1 <- c(alpha("chocolate2",0.6),alpha("springgreen4",0.6))
colpose_s2 <- c("chocolate2","springgreen4")
colsession_e <- c(alpha("chocolate2",0.6),"chocolate2")#try out this colours when I have time
colsession_c <- c(alpha("springgreen4",0.6),"springgreen4")#try out this colours when I have time
#colsession <- c("dodgerblue3", "firebrick3")#colours previously used for session - but they don't have the usual group colour code
colemo <- c("firebrick3","dodgerblue3", 'darkgrey')
colemoAF <- c("firebrick3","dodgerblue3") #only anger fear
colemoalpha = alpha(colemoAF, 0.4) #transparent emotion colors
colemolev <- c("firebrick4","firebrick3","firebrick2","firebrick1","dodgerblue4","dodgerblue3","dodgerblue2","dodgerblue1")
s = 20 #axis text size
st = 35 #title text size

# barplots/dotplots between subject confidence intervals - with Rocco's function CI.proportion ####
# pdf("figures/paway_point_bar_plots_btwCIs.pdf")
#bar plot without level
ggplot(data = plotData[plotData$emotion !="Neutral",] , aes(x = as.factor(emotion), y = away, fill= as.factor(emotion)))+
  facet_grid(group~session) + stat_summary(fun.y = mean, geom = "bar", position= position_dodge(width = 0.5),size = 5)+
  stat_summary(fun.data = CI.proportion, geom = "errorbar", position= position_dodge(width = 0.5),width = 0.1,size = 2) + 
  labs(x = "", y = "p(away)")  +   
  theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + 
  scale_fill_manual(values = colemoAF,name="")+
  coord_cartesian(ylim=c(0.48,0.58))+scale_y_continuous(breaks=seq(0.50, 0.58,0.04))+
  geom_abline(intercept = 0.50, slope = 0, colour = "dimgray", linetype = 2)
# on the whole database with level
ggplot(data = plotData[plotData$emotion !="Neutral",] , aes(x = as.factor(emotion), y = away, col= as.factor(level)))+
  facet_grid(group~session) + stat_summary(fun.y = mean, geom = "point", position= position_dodge(width = 0.5),size = 5)+
  stat_summary(fun.data = CI.proportion, geom = "errorbar", position= position_dodge(width = 0.5),width = 0.2,size = 2) + 
  labs(x = "Emotion", y = "p(away)")  + 
  scale_color_manual(labels = c("L1", "L2", "L3", "L4"), values = c("orangered4", "orangered3", 'orangered2', 'orangered1')) + 
  guides(color=guide_legend("Intensity")) +
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + 
  coord_cartesian(ylim=c(0.44,0.63))+#scale_y_continuous(breaks=seq(0.45, 0.63,0.02))+
  geom_abline(intercept = 0.50, slope = 0, colour = "dimgray", linetype = 2)
#with level grouped as 1-3 and 5-7
plotDatag <- plotData %>%
  mutate(level = ifelse(level==c(1,3), 1, 2))%>%
  mutate(level = factor(level, levels = c(1,2), labels = c("L1-3", "L5-7")))
ggplot(data = plotDatag[plotDatag$emotion !="Neutral",] , aes(x = as.factor(emotion), y = away, col= as.factor(level)))+
  facet_grid(group~session) + stat_summary(fun.y = mean, geom = "point", position= position_dodge(width = 0.5),size = 5)+
  stat_summary(fun.data = CI.proportion, geom = "errorbar", position= position_dodge(width = 0.5),width = 0.2,size = 2) + 
  labs(x = "Emotion", y = "p(away)")  + 
  scale_color_manual(values = c("orangered4", 'orangered1')) + 
  guides(color=guide_legend("Intensity")) +
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + 
  coord_cartesian(ylim=c(0.46,0.62))+scale_y_continuous(breaks=seq(0.46, 0.62,0.04))+
  geom_abline(intercept = 0.50, slope = 0, colour = "dimgray", linetype = 2)

dev.off()

# barplots choice per session or per posture, between CIs  ####

# prepare data for plots with within-subject CIs - calculate paway per condition/group ####
paway <- plotData %>%
  filter(!emotion == "Neutral")%>% # there is no away/toward emotion for neutral pairs, both actors neutral
  group_by(subject, session, group, emotion) %>%
  summarise(proportion = mean.proportion(away)) %>%
  ungroup()

pawaydiff <- paway %>%
  group_by(subject,emotion) %>%
  mutate(diff2vs1 = proportion[session=="Session 2"]-proportion[session=="Session 1"])%>%
  filter(row_number()<=1) %>% #only one line per diff2vs1 (otherwise sample size is double)
  select(-session, -proportion)%>%
  ungroup()

paway_fig <- paway
pawayd_fig <- pawaydiff

#reorder factor levels group if necessary
paway_fig$group = relevel(paway_fig$group,"Expansive")
pawayd_fig$group = relevel(pawayd_fig$group,"Expansive")
# print(levels(paway_fig$group)) #check

#subsets
paway_fig1 <- paway_fig %>%
  filter(session == "Session 1")
paway_fig2 <- paway_fig %>%
  filter(session == "Session 2")
paway_fige <- paway_fig %>%
  filter(group == "Expansive")
paway_figc <- paway_fig %>%
  filter(group == "Constrictive")

# PAPER choice in 4 cells (group x session): bar plot within sub CIs, optionally with individual dots ####
#only source this once all manipulations using dplyr above are done - it loads plyr and afterwards dplyr does not work anymore
#within subject error bars method http://www.cookbook-r.com/Graphs/Plotting_means_and_error_bars_(ggplot2)/
s = 35
# calculate within-subject confidence intervals for paway (in Figure 3b) Cousineau method with Morey correction (Baguley, 2012)
statsp <- paway_fig %>%
  select(group, subject,session, emotion, proportion)%>%
  #grand mean per group
  group_by(group)%>% 
  mutate(grandmean = mean(proportion)) %>%
  # n in each group (conditions included to count each subject only once)
  group_by(group, session, emotion)%>% 
  mutate(n=n())%>% 
  #normalized mean for each subject: raw level minuse subject mean plus grand mean for each group
  group_by(subject)%>% 
  mutate(normdiff = proportion - mean(proportion) + grandmean)%>% 
  #mean, sd and normed sd per group and condition
  group_by(group, session, emotion, n)%>%
  summarise(mean=mean(proportion),
            sd = sd(proportion), 
            normsd = sd(normdiff))%>%
  # wthin-subject CIs with Morey correction for 4 conditions: session x emotion
  mutate(ci=1.96*normsd/sqrt(n)*sqrt(4/(4-1)),
         ci_low = mean-ci,
         ci_up = mean+ci)

bars_within <- ggplot(data = statsp, aes(x = emotion, y = mean, colour=emotion, fill=emotion))+
  facet_grid(group~session) +
  #means
  geom_bar(stat="identity", position=position_dodge(), size=2) + 
  #individual data points: randomly jittered
   geom_point(data = paway_fig, aes(x=emotion, y=proportion), shape=16, size = 2, colour="black", show.legend=F,
              position=position_jitterdodge(jitter.width=1.5, dodge.width =0.95))+
  #individual data points: organized / distribution visible
  #geom_dotplot(data=paway_fig, aes(x=emotion, y=proportion), binwidth = 0.01, dotsize=0.8, 
  #            fill="black", colour="black", binaxis = "y", stackdir = "centerwhole")+
  #error bars: within-sub CIs
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), position= position_dodge(width = 0.9),width = 0.2,size = 2) +
  labs(x = "", y = "p(away)")  +   
  theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + 
  scale_colour_manual(values = colemoAF,name="")+scale_fill_manual(values=colemoalpha)+
  #coord_cartesian(ylim=c(0.485,0.575))+scale_y_continuous(breaks=seq(0.48, 0.56,0.02))+ #if only means + CI
  coord_cartesian(ylim=c(0.40,0.72))+scale_y_continuous(breaks=seq(0.40, 0.70,0.1))+#if with individual dots
  geom_abline(intercept = 0.50, slope = 0, colour = "dimgray", linetype = 2)
bars_within

# PAPER choice change session 1 - 2: bar plots within-sub CIs, optionally with individual dots ####
statsd <- pawayd_fig %>%
  select(group, subject,emotion, diff2vs1)%>%
  #grand mean per group
  group_by(group)%>% 
  mutate(grandmean = mean(diff2vs1)) %>%
  # n in each group (conditions included to count each subject only once)
  group_by(group, emotion)%>% 
  mutate(n=n())%>% 
  #normalized mean for each subject: raw level minuse subject mean plus grand mean for each group
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

#factor session with 2x same level, to plot postures below each other as in 4cell figure session x posture
statsd$session = factor(rep(1, dim(statsd)[1]), levels=1, labels="Change")

bar.2vs1 <-  ggplot(statsd, aes(x=emotion, y=mean, fill = emotion)) +
  facet_grid(group~session) +
  geom_bar(stat="identity", position=position_dodge(), size=1.5) +
  geom_errorbar(ymin=statsd$ci_low, ymax=statsd$ci_up, position= position_dodge(width = 0.9),width = 0.15,size = 1.25) +
  scale_fill_manual(values = colemoAF,name="")+
  coord_cartesian(ylim=c(-0.025,0.05))+scale_y_continuous(breaks=seq(-0.02, 0.04,0.02))+
  geom_abline(intercept = 0, slope = 0, colour = "dimgray", linetype = 2)+
  ylab("")+#keep empty space to previous plot instead of label
  theme_bw() +theme(text = element_text(size=s),
                    axis.title.x=element_blank(),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
                    legend.position ="none")

bar.2vs1

pdf('figures/paper_proportion_ind_dots.pdf', width=15); 
plot_grid(bars_within, bar.2vs1, rel_widths=c(1.6, 1)); 
dev.off()
#boxplots 4 cells ####
box_paway <- ggplot(pawad_fig, aes(x = emotion, y = proportion,fill=emotion)) + 
  geom_boxplot(size=1)+
  facet_grid(group~session) + 
  labs(x = "", y = "p(away)") + coord_cartesian(ylim=c(0.38, 0.72)) + #axes
  theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + 
  scale_fill_manual(values = colemoAF,name="")+
  geom_abline(intercept = 0.5, slope = 0, colour = "dimgray", linetype = 2)
pawayd_box <- data_clean
pawayd_box$session = factor(rep(1, dim(pawayd_box)[1]), levels=1, labels="Change")
box_paway_diff <- ggplot(pawayd_box, aes(x = emotion, y = diff2vs1,fill=emotion)) + 
  geom_boxplot(size=1)+
  facet_grid(group~session) + 
  labs(x = "", y = "p(away) S2-S1") + #coord_cartesian(ylim=c(0.38, 0.72)) + #axes
  theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + 
  scale_fill_manual(values = colemoAF,name="")+
  geom_abline(intercept = 0, slope = 0, colour = "dimgray", linetype = 2)
pdf('figures/paper_proportion_boxplots.pdf', width=15); 
plot_grid(box_paway, box_paway_diff, rel_widths=c(1.6, 1)); 
dev.off()


# change direction of choice per subject ####
pd = position_dodge(width=0.4)
change <- ggplot(subset(pawayd_fig, emotion!="Neutral"), aes(x=group, y=diff2vs1, group = factor(subject))) + 
  geom_point(position=pd) +facet_grid(~emotion)+
  labs(x = "", y = "p(away) S2-S1")  + 
  theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + 
  geom_abline(intercept = 0, slope = 0, colour = "dimgray", linetype = 2)

changedir <- paway_fig %>%
  group_by(group, subject, emotion)%>%
    mutate(mean_change = proportion[session=="Session 2"]-proportion[session=="Session 1"])%>%
  ungroup()%>%
  mutate(change_direction = factor(ifelse(mean_change > 0.01, "1", 
                                     ifelse(mean_change<(-0.01), "-1", "0")), 
                                     levels=c(-1,0, 1), labels = c("negative", "change<1%","positive")))
changes1s2 <- ggplot(subset(changedir,emotion=="Anger"), aes(x=session, y=proportion, colour = change_direction, group=factor(subject))) + 
  geom_point(position=pd) + geom_line(position=pd)+facet_grid(~group)+
  labs(x = "", y = "Change anger") +  
  scale_colour_manual(values=c("red", "black", "blue"), name = "Change direction", guide = guide_legend(byrow=T, ncol=1))+ 
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())

## choice change per participant: all data points with connecting lines from session 1 to 2 ####
changedir <- paway_fig %>%
  group_by(group, subject, emotion)%>%
  mutate(mean_change = proportion[session=="With Posture"]-proportion[session=="Baseline"])%>%
  ungroup()%>%
  mutate(change_direction = factor(ifelse(mean_change > 0.01, "1",
                                          ifelse(mean_change<(-0.01), "-1", "0")),
                                   levels=c(-1,0, 1), labels = c("negative", "change<1%","positive")))
ggplot(data=changedir, aes(x=session, y=proportion, group=factor(subject), colour = change_direction))+
  facet_grid(emotion~ group) + #ggtitle("Anger") +
  geom_point()+ geom_line()+ #individual lines
  scale_colour_manual(values = c("coral2", "gray68", "cyan3"))+
  coord_cartesian(ylim=c(0.41,0.71))+scale_y_continuous(breaks=seq(0.40, 0.70,0.05))+
  geom_abline(intercept = .50, slope = 0, colour = "dimgray", linetype = 2)+
  theme_bw() +theme(text = element_text(size=20), axis.title.x=element_blank(),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())

pdf('figures/paway_change_per_subject.pdf'); change; changes1s2; dev.off()

# histogram: distribution of proportion of choices####
paway_ind <- plotData %>%
  filter(!emotion == "Neutral")%>% # there is no away/toward emotion for neutral pairs, both actors neutral
  group_by(session, group, emotion, subject) %>%
  summarise(proportion = mean.proportion(away)) %>%
  ungroup()

pdf('figures/histogram_proportion_choice.pdf')
ggplot(paway_ind, aes(x = proportion))+
  facet_grid(emotion~group~session)+
  geom_histogram(aes(y=..density..), colour="black", fill="white")+  geom_density(alpha=.2, fill="blue", size=1) +
  geom_vline(aes(xintercept=0.50), color="blue", linetype="dashed", size=1) + 
  theme_bw() + theme(text=element_text( face="bold", size=18)) + 
  xlab("Proportion")
dev.off()

# prepare plots mouse and time ####
#prepare data
load('data/mouse_clean.R')
mouse_plot <- mouse_clean %>%
  mutate(click_time = round(click_time*1000))%>%
  mutate(mov_time = round(mov_time*1000)) %>%
  mutate(emotion = factor(emotion, levels = c(1,2,3), labels = c("Anger", "Fear", "Neutral"))) %>%
  mutate(session = factor(session, levels = c(1,2), labels = c("Session 1", "Session 2"))) %>%
  mutate(away = factor(away, levels = c(1,0), labels = c("Away", "Toward")))%>%
  mutate(md_pos = md>0)
mouse_plot$ln_click_time <- log(mouse_plot$click_time)
mouse_plot$ln_mov_time<- log(mouse_plot$mov_time)
mouse_plot$ln_peak_acc<- log(mouse_plot$peak_acc)
mouse_plot$ln_md_abs<- log(mouse_plot$md_abs)
mouse_plot$ln_xflip <- log(mouse_plot$x.flip)
mouse_plot$ln_yflip <- log(mouse_plot$y.flip)
mouse_plot$group <- relevel(mouse_plot$group, "Expansive") #expansive first in graphs

mouse_plot1 <- mouse_plot %>%
  filter(session == "Session 1")
mouse_plot2 <- mouse_plot %>%
  filter(session == "Session 2")
mouse_plote <- mouse_plot %>%
 filter(group == "Expansive")
mouse_plotc <- mouse_plot %>%
   filter(group == "Constrictive")

# Figure 4 for PAPER click time within-subject error bars ####
#code probably has to be adapted to work in this script, names of variables different?
s=30
# #calculate means and within-subject confidence intervals for initiation time by side (away-toward) (Figure 4a)
statscs <- mouse_plot %>%
  dplyr::select(group, subject,session, away, init_time)%>%
  group_by(group)%>%
  mutate(grandmean = mean(init_time)) %>%
  group_by(group, session, away)%>%
  mutate(n=n())%>%
  group_by(subject)%>%
  mutate(normdiff = init_time - mean(init_time) + grandmean)%>%
  group_by(group, session, away, n)%>%
  summarise(mean=mean(init_time),
            sd = sd(init_time),
            normsd = sd(normdiff))%>%
  mutate(ci = 1.96*normsd/sqrt(n)*sqrt(2/(2-1)),#2 conditions: session
         ci_low = mean-ci,
         ci_up = mean+ci)%>%
  ungroup()

# # Make Figure 4a: plot initiation time by side
init_side <- ggplot(data = statscs , aes(x = away, y = mean, colour=group, shape=group))+
  facet_grid(~session)+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width = 0.2, size = 1.5)+
  geom_line(aes(group=group), size=1.25) + #connecting lines from session 1 to 2
  geom_point(size=7) + #means per posture/session
  labs(y = "initiation time in ms") +
  scale_colour_manual(values=colpose, name="Group")+#c("dimgray", "gray"))+
  scale_shape_discrete(solid=T, name="Group") + #shape for posture
  theme_bw() +theme(legend.position =c(0.25,0.25), text = element_text(size=s),
                    axis.title.x=element_blank(),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(335,440))

# Figure 4b: plot initiation time by facial expression: threat vs. neutral: also in scripts for paper

#calculate means and within-subject confidence intervals for initiation time by emotion (Figure 4b)
statsce <- mouse_plot %>%
  dplyr::select(group, subject,session, threat, init_time)%>%
  group_by(group)%>% 
  mutate(grandmean = mean(init_time)) %>%
  group_by(group, session, threat)%>% 
  mutate(n=n())%>% 
  group_by(subject)%>% 
  mutate(normdiff = init_time - mean(init_time) + grandmean)%>% 
  group_by(group, session, threat, n)%>%
  summarise(mean=mean(init_time),
            sd = sd(init_time), 
            normsd = sd(normdiff))%>%
  mutate(ci = 1.96*normsd/sqrt(n)*sqrt(2/(2-1)),#2 conditions: threat/neutral
         ci_low = mean-ci,
         ci_up = mean+ci)%>%
  ungroup()

#Make Figure 4b: plot initiation time by threat vs. neutral as barplot
init_threat <- ggplot(data = statsce , aes(x = threat, y = mean, fill=threat))+
  facet_grid(group~session)+
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), position= position_dodge(width = 0.9),width = 0.15, size = 1.25) +
  labs(y = "") +  #keep free space between previous plot instead of repeating label
  scale_fill_manual(values=colemo, name="Group")+
  theme_bw() +theme(legend.position ="none", text = element_text(size=s),
                    axis.title.x=element_blank(),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(335,440))
# initiation time by threat as lineplot
init_threat <- ggplot(data = statsce , aes(x = threat, y = mean, colour=group, shape=group))+
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
  coord_cartesian(ylim=c(335,440))

# Figure 4: plot initiation time by side and by threat next to each other: 
plot_grid(init_side, init_threat, labels = c("a", "b"), label_size = 30)

#saving a pdf with the initiation time plots in high resolution
ggsave("./figures/Figure4_initiation_time_dotplot.pdf", plot = last_plot(), device = NULL, path = NULL,
       scale = 1, width = 18, height = 7, units = c("in", "cm", "mm"),
       dpi = 300, limitsize = TRUE)


# Figure 5 for PAPER movement duration within-subject error bars####

#figure 5a: movement time by side 
#calculate means and within-subject confidence intervals for movement time by side (away-toward) (Figure 5a)
statsms <- plot_mouse %>%
  dplyr::select(group, subject,session, away, mov_time)%>%
  group_by(group)%>%
  mutate(grandmean = mean(mov_time)) %>%
  group_by(group, session, away)%>%
  mutate(n=n())%>%
  group_by(subject)%>%
  mutate(normdiff = mov_time - mean(mov_time) + grandmean)%>%
  group_by(group, session, away, n)%>%
  summarise(mean=mean(mov_time),
            sd = sd(mov_time),
            normsd = sd(normdiff))%>%
  mutate(ci = 1.96*normsd/sqrt(n)*sqrt(2/(2-1)),#2 conditions: session
         ci_low = mean-ci,
         ci_up = mean+ci)%>%
  ungroup()

# Make Figure 5a: plot movement time by side
mov_side <- ggplot(data = statsms , aes(x = away, y = mean, colour=group, shape=group))+
  facet_grid(~session)+
  geom_errorbar(aes(ymin=mean-ci, ymax=mean+ci), width = 0.2, size = 1.5)+
  geom_line(aes(group=group), size=1.25) + #connecting lines from session 1 to 2
  geom_point(size=7) + #means per posture/session
  labs(y = "movement duration in ms") +
  scale_colour_manual(values=colpose, name="Group")+#c("dimgray", "gray"))+
  scale_shape_discrete(solid=T, name="Group") + #shape for posture
  theme_bw() +theme(legend.position =c(0.25,0.82), text = element_text(size=s),
                    axis.title.x=element_blank(),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(460,510))


# Figure 5b: movement time by threat
#calculate means and within-subject confidence intervals for movement time by threat
statsm <- plot_mouse %>%
  dplyr::select(group, subject,session, threat, mov_time)%>%
  group_by(group)%>% 
  mutate(grandmean = mean(mov_time)) %>%
  group_by(group, session, threat)%>% 
  mutate(n=n())%>% 
  group_by(subject)%>% 
  mutate(normdiff = mov_time - mean(mov_time) + grandmean)%>% 
  group_by(group, session, threat, n)%>%
  summarise(mean=mean(mov_time),
            sd = sd(mov_time), 
            normsd = sd(normdiff))%>%
  mutate(ci = 1.96*normsd/sqrt(n)*sqrt(2/(2-1)),#2 conditions: threat/neutral
         ci_low = mean-ci,
         ci_up = mean+ci)%>%
  ungroup()

#Figure 5b for movement time by threat
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
  coord_cartesian(ylim=c(460,510))

#add Figure 5a and 5b together
plot_grid(mov_side, mov_threat, labels = c("a", "b"), label_size = 30)

#print to pdf
ggsave("./figures/Figure5_movement_time_dotplot.pdf", plot = last_plot(), device = NULL, path = NULL,
       scale = 1, width = 18, height = 7, units = c("in", "cm", "mm"),
       dpi = 300, limitsize = TRUE)

# click time all plots ####
graphics.off()
pdf('figures/click_time.pdf')
#histograms
hist.s1 <- ggplot(mouse_plot1, aes(x = click_time))+
  geom_histogram(aes(y=..density..), binwidth=30, colour="black", fill="white")+geom_density(alpha=.2, fill="blue", size=1) + 
  geom_vline(aes(xintercept=median(click_time, na.rm=T)), color="red", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=mean(click_time, na.rm=T)), color="blue", linetype="dashed", size=1) + 
  theme_bw() + theme(text=element_text( face="bold", size=18)) + 
  ggtitle("Session 1") +xlab("Click time") +
  scale_x_continuous(breaks=c(95,300,median(mouse_plot1$click_time),450,600,750,900,1050))+
  scale_fill_manual(values=colpose_s2, name="")+theme(legend.position="bottom")
hist.s2 <- ggplot(mouse_plot2, aes(x = click_time))+
  geom_histogram(aes(y=..density..), binwidth=30, colour="black", fill="white")+ geom_density(alpha=.2, fill="blue", size=1) +
  geom_vline(aes(xintercept=median(click_time, na.rm=T)), color="red", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=mean(click_time, na.rm=T)), color="blue", linetype="dashed", size=1) + 
  theme_bw() + theme(text=element_text( face="bold", size=18)) + 
  ggtitle("Session 2") +xlab("Click time") +ylab("")+
  scale_x_continuous(breaks=c(95,300,median(mouse_plot2$click_time),450,600,750,900,1050))+
  scale_fill_manual(values=colpose_s2, name="")+theme(legend.position="bottom")
cowplot::plot_grid(hist.s1, hist.s2, ncol=2)
#boxplot main effect emotion
ggplot(mouse_plot) +  geom_boxplot(aes(x = emotion, y = click_time, fill=emotion))+facet_grid(group~session)+
  labs(x = "", y = "Click time") + scale_fill_manual(values=colemo)+ 
  theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())
#bar plot average per emotion
ggplot(data = mouse_plot , aes(x = emotion, y = click_time, fill=emotion))+facet_grid(group~session)+
  stat_summary(fun.y = mean, geom = "bar", position=position_dodge(width = 0.9))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.2, size = 1) + 
  labs(x = "", y = "Click time") +  scale_fill_manual(values=colemo)+ 
  theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(325,425))#+scale_y_continuous(breaks=c(3,300))
#point plot level 
ggplot(data = subset(mouse_plot, emotion!="Neutral") , aes(x = emotion, y = click_time, col=paste(emotion,level))) +
  facet_grid(group~session) + 
  stat_summary(fun.y = mean, geom = "point", position=position_dodge(width = 0.9), size=4)+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.5, size = 2) + 
  labs(x = "Emotion", y = "Click time") + ggtitle('Main effect emotion per level')+
  coord_cartesian(ylim=c(300,450))+
  scale_colour_manual(labels = rep(c("L1", "L2", "L3", "L4"),2), values = colemolev, name="Intensity", guide = guide_legend(byrow=T))+ 
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())
#boxplot emo side
ggplot(mouse_plot[mouse_plot$emotion!="Neutral",]) +geom_boxplot(aes(x = away, y = click_time, fill=emotion))+
  facet_grid(group~session)+  ylab("Click time") + xlab("")+
  scale_fill_manual(values=colemo, name="", guide = guide_legend(byrow=T))+
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())
#barplot emo-side
ggplot(data = mouse_plot[mouse_plot$emotion!="Neutral",] , aes(x = away, y = click_time, fill=emotion))+
  stat_summary(fun.y = mean, geom = "bar", position=position_dodge(width = 0.9))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.2, size = 1) + 
  coord_cartesian(ylim=c(325,425))+coord_cartesian(ylim=c(325,425))+
  facet_grid(group~session)+  ylab("Click time") + xlab("")+
  scale_fill_manual(values=colemo, name="", guide = guide_legend(byrow=T))+
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())
#pointplot emo-side level
ggplot(data = subset(mouse_plot, emotion!="Neutral") , aes(x = away, y = click_time, col= paste(emotion,level)))+
  stat_summary(fun.y = mean, geom = "point", position=position_dodge(width = 0.9), size=4)+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.5, size = 2) + 
  facet_grid(group~session) + 
  labs(x = "", y = "Click time")+
  scale_colour_manual(values = colemolev, name = "Emotion intensity", guide = guide_legend(byrow=T)) +
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank()) + 
  coord_cartesian(ylim=c(300,450))
dev.off()




#move time all plots ####
graphics.off()
pdf('figures/mov_time.pdf')
hist.s1 <- ggplot(mouse_plot1, aes(x = mov_time))+
  geom_histogram(aes(y=..density..), binwidth=30, colour="black",fill="white")+ geom_density(alpha=.2, fill="blue", size=1) +
  geom_vline(aes(xintercept=median(mov_time, na.rm=T)), color="red", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=mean(mov_time, na.rm=T)), color="blue", linetype="dashed", size=1) + 
  theme_bw() + theme(text=element_text( face="bold", size=18)) + 
  ggtitle("Session 1") +xlab("Movement time") +
  scale_x_continuous(breaks=c(184,300,median(mouse_plot1$mov_time),600,750,900,1050))
hist.s2 <- ggplot(mouse_plot2, aes(x = mov_time))+
  geom_histogram(aes(y=..density..), binwidth=30, colour="black",fill="white")+ geom_density(alpha=.2, fill="blue", size=1) +
  geom_vline(aes(xintercept=median(mov_time, na.rm=T)),color="red", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=mean(mov_time, na.rm=T)), color="blue", linetype="dashed", size=1) + 
  theme_bw() + theme(text=element_text( face="bold", size=18)) + 
  ggtitle("Session 2") +xlab("Movement time") +ylab("")+
  scale_x_continuous(breaks=c(184,300,median(mouse_plot2$mov_time),600,750,900,1050))
cowplot::plot_grid(hist.s1, hist.s2, ncol=2)
#boxplot main effect emotion
ggplot(mouse_plot) +  geom_boxplot(aes(x = emotion, y = mov_time, fill=emotion))+facet_grid(group~session)+
  labs(x = "", y = "Movement time") + scale_fill_manual(values=colemo)+ 
  theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())
#bar plot average per emotion
ggplot(data = mouse_plot , aes(x = emotion, y = mov_time, fill=emotion))+facet_grid(group~session)+
  stat_summary(fun.y = mean, geom = "bar", position=position_dodge(width = 0.9))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.2, size = 1) + 
  labs(x = "", y = "Movement time") +  scale_fill_manual(values=colemo)+ 
  theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(440,520))
#point plot level 
ggplot(data = subset(mouse_plot, emotion!="Neutral") , aes(x = emotion, y = mov_time, col=paste(emotion,level))) +
  facet_grid(group~session) + 
  stat_summary(fun.y = mean, geom = "point", position=position_dodge(width = 0.9), size=4)+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.5, size = 2) + 
  labs(x = "", y = "Movement time") + ggtitle('Main effect emotion per level')+
  scale_colour_manual(labels = rep(c("L1", "L2", "L3", "L4"),2), values = colemolev, name="Intensity", guide = guide_legend(byrow=T))+ 
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(440,520))
#boxplot emo side
ggplot(mouse_plot[mouse_plot$emotion!="Neutral",]) +geom_boxplot(aes(x = away, y = mov_time, fill=emotion))+
  facet_grid(group~session)+  ylab("Movement time") + xlab("")+
  scale_fill_manual(values=colemo, name="", guide = guide_legend(byrow=T))+
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())
#barplot emo-side
ggplot(data = mouse_plot[mouse_plot$emotion!="Neutral",] , aes(x = away, y = mov_time, fill=emotion))+
  stat_summary(fun.y = mean, geom = "bar", position=position_dodge(width = 0.9))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.2, size = 1) + 
  facet_grid(group~session)+  ylab("Movement time") + xlab("")+
  scale_fill_manual(values=colemo, name="", guide = guide_legend(byrow=T))+
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(440,520))
#pointplot emo-side level
ggplot(data = subset(mouse_plot, emotion!="Neutral") , aes(x = away, y = mov_time, col= paste(emotion,level)))+
  stat_summary(fun.y = mean, geom = "point", position=position_dodge(width = 0.9), size=4)+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.5, size = 2) + 
  facet_grid(group~session) + 
  labs(x = "", y = "Movement time")+
  scale_colour_manual(values = colemolev, name = "Emotion intensity", guide = guide_legend(byrow=T)) +
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  coord_cartesian(ylim=c(440,520))

ggplot(data = mouse_plot[mouse_plot$emotion!="Neutral",] , aes(x = away, y = mov_time, fill=away))+facet_grid(group~session)+
  stat_summary(fun.y = mean, geom = "bar", position=position_dodge(width = 0.9))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.2, size = 1) +   labs(x = "", y = "Movement duration in ms") +  scale_fill_manual(values=c("dimgray", "gray"))+ 
  theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(460,515))
dev.off()

#peak velocity ####
graphics.off()
pdf('figures/peak_velocity.pdf')
hist.s1 <- ggplot(mouse_plot1, aes(x = peak_veloc))+
  geom_histogram(aes(y=..density..), colour="black",fill="white")+ geom_density(alpha=.2, fill="blue", size=1) +
  geom_vline(aes(xintercept=median(peak_veloc, na.rm=T)), color="red", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=mean(peak_veloc, na.rm=T)), color="blue", linetype="dashed", size=1) + 
  theme_bw() + theme(text=element_text( face="bold", size=18)) + 
  ggtitle("Session 1") +xlab("Peak velocity")
hist.s2 <- ggplot(mouse_plot2, aes(x = peak_veloc))+
  geom_histogram(aes(y=..density..), colour="black", fill="white")+  geom_density(alpha=.2, fill="blue", size=1) +
  geom_vline(aes(xintercept=median(peak_veloc, na.rm=T)),color="red", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=mean(peak_veloc, na.rm=T)), color="blue", linetype="dashed", size=1) + 
  theme_bw() + theme(text=element_text( face="bold", size=18)) + 
  ggtitle("Session 2") +xlab("Peak velocity") + ylab("")
cowplot::plot_grid(hist.s1, hist.s2, ncol=2)
#boxplot main effect emotion
ggplot(mouse_plot) +  geom_boxplot(aes(x = emotion, y = peak_veloc, fill=emotion))+facet_grid(group~session)+
  labs(x = "", y = "Peak velocity") + scale_fill_manual(values=colemo)+ 
  theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())
#bar plot average per emotion
ggplot(data = mouse_plot , aes(x = emotion, y = peak_veloc, fill=emotion))+facet_grid(group~session)+
  stat_summary(fun.y = mean, geom = "bar", position=position_dodge(width = 0.9))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.2, size = 1) + 
  labs(x = "", y = "Peak velocity") +  scale_fill_manual(values=colemo)+ 
  theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(0.10,0.13))
#point plot level 
ggplot(data = subset(mouse_plot, emotion!="Neutral") , aes(x = emotion, y = peak_veloc, col=paste(emotion,level))) +
  facet_grid(group~session) + 
  stat_summary(fun.y = mean, geom = "point", position=position_dodge(width = 0.9), size=4)+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.5, size = 2) + 
  labs(x = "", y = "Peak velocity") + ggtitle('Main effect emotion per level')+
  scale_colour_manual(labels = rep(c("L1", "L2", "L3", "L4"),2), values = colemolev, name="Intensity", guide = guide_legend(byrow=T))+ 
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(0.10,0.13))
#boxplot emo side
ggplot(mouse_plot[mouse_plot$emotion!="Neutral",]) +geom_boxplot(aes(x = away, y = peak_veloc, fill=emotion))+
  facet_grid(group~session)+  ylab("Peak velocity") + xlab("")+
  scale_fill_manual(values=colemo, name="", guide = guide_legend(byrow=T))+
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())
#barplot emo-side
ggplot(data = mouse_plot[mouse_plot$emotion!="Neutral",] , aes(x = away, y = peak_veloc, fill=emotion))+
  stat_summary(fun.y = mean, geom = "bar", position=position_dodge(width = 0.9))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.2, size = 1) + 
  facet_grid(group~session)+  ylab("Peak velocity") + xlab("")+
  scale_fill_manual(values=colemo, name="", guide = guide_legend(byrow=T))+
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(0.10,0.13))
#pointplot emo-side level
ggplot(data = subset(mouse_plot, emotion!="Neutral") , aes(x = away, y = peak_veloc, col= paste(emotion,level)))+
  stat_summary(fun.y = mean, geom = "point", position=position_dodge(width = 0.9), size=4)+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.5, size = 2) + 
  facet_grid(group~session) + 
  labs(x = "", y = "Peak velocity")+
  scale_colour_manual(values = colemolev, name = "Emotion intensity", guide = guide_legend(byrow=T)) +
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  coord_cartesian(ylim=c(0.10,0.13))
dev.off()



#acceleration ####
graphics.off()
pdf('figures/peak_acceleration.pdf')
hist.s1 <- ggplot(mouse_plot1, aes(x = peak_acc))+
  geom_histogram(aes(y=..density..), colour="black", fill="white")+  geom_density(alpha=.2, fill="blue", size=1) +
  geom_vline(aes(xintercept=median(peak_acc, na.rm=T)), color="red", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=mean(peak_acc, na.rm=T)), color="blue", linetype="dashed", size=1) + 
  theme_bw() + theme(text=element_text( face="bold", size=18)) + 
  ggtitle("Session 1") +xlab("Peak acceleration")+xlim(c(0,0.2))
hist.s2 <- ggplot(mouse_plot2, aes(x = peak_acc, fill=group))+
  geom_histogram(aes(y=..density..), colour="black", fill="white")+  geom_density(alpha=.2, fill="blue", size=1) +
  geom_vline(aes(xintercept=median(peak_acc, na.rm=T)), color="red", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=mean(peak_acc, na.rm=T)), color="blue", linetype="dashed", size=1) + 
  theme_bw() + theme(text=element_text( face="bold", size=18)) + 
  ggtitle("Session 2") +xlab("Peak acceleration")+ylab("")+xlim(c(0,0.2))
cowplot::plot_grid(hist.s1, hist.s2, ncol=2)
#boxplot main effect emotion
ggplot(mouse_plot) +  geom_boxplot(aes(x = emotion, y = peak_acc, fill=emotion))+facet_grid(group~session)+
  labs(x = "", y = "Peak acceleration") + scale_fill_manual(values=colemo)+ 
  theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())
#bar plot average per emotion
ggplot(data = mouse_plot , aes(x = emotion, y = peak_acc, fill=emotion))+facet_grid(group~session)+
  stat_summary(fun.y = mean, geom = "bar", position=position_dodge(width = 0.9))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.2, size = 1) + 
  labs(x = "", y = "Peak acceleration") +  scale_fill_manual(values=colemo)+ 
  theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(0.04,0.05))+scale_y_continuous(breaks=c(0.04,0.045,0.05))
#point plot level 
ggplot(data = subset(mouse_plot, emotion!="Neutral") , aes(x = emotion, y = peak_acc, col=paste(emotion,level))) +
  facet_grid(group~session) + 
  stat_summary(fun.y = mean, geom = "point", position=position_dodge(width = 0.9), size=4)+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.5, size = 2) + 
  labs(x = "", y = "Peak acceleration") + ggtitle('Main effect emotion per level')+
  scale_colour_manual(labels = rep(c("L1", "L2", "L3", "L4"),2), values = colemolev, name="Intensity", guide = guide_legend(byrow=T))+ 
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(0.035,0.05))
#boxplot emo side
ggplot(mouse_plot[mouse_plot$emotion!="Neutral",]) +geom_boxplot(aes(x = away, y = peak_acc, fill=emotion))+
  facet_grid(group~session)+  ylab("Peak acceleration") + xlab("")+
  scale_fill_manual(values=colemo, name="", guide = guide_legend(byrow=T))+
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())
#barplot emo-side
ggplot(data = mouse_plot[mouse_plot$emotion!="Neutral",] , aes(x = away, y = peak_acc, fill=emotion))+
  stat_summary(fun.y = mean, geom = "bar", position=position_dodge(width = 0.9))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.2, size = 1) + 
  facet_grid(group~session)+  ylab("Peak acceleration") + xlab("")+
  scale_fill_manual(values=colemo, name="", guide = guide_legend(byrow=T))+
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(0.035,0.05))
#pointplot emo-side level
ggplot(data = subset(mouse_plot, emotion!="Neutral") , aes(x = away, y = peak_acc, col= paste(emotion,level)))+
  stat_summary(fun.y = mean, geom = "point", position=position_dodge(width = 0.9), size=4)+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.5, size = 2) + 
  facet_grid(group~session) + 
  labs(x = "", y = "Peak acceleration")+
  scale_colour_manual(values = colemolev, name = "Emotion intensity", guide = guide_legend(byrow=T)) +
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  coord_cartesian(ylim=c(0.035,0.05))
dev.off()


#max deviation ####
graphics.off()
pdf('figures/max_deviation_hist.pdf', width = 16)
hist.s1 <- ggplot(mouse_plot1, aes(x = md))+
  geom_histogram(aes(y=..density..), binwidth=0.02, colour="black", fill="white")+geom_density(alpha=.2, fill="blue", size=1) +
  geom_vline(aes(xintercept=median(md, na.rm=T)), color="red", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=mean(md, na.rm=T)), color="blue", linetype="dashed", size=1) + 
  theme_bw() + theme(text=element_text( face="bold", size=18)) + 
  ggtitle("Session 1") +xlab("Max Deviation")+xlim(-0.25,0.25)
hist.s2 <- ggplot(mouse_plot2, aes(x = md))+
  geom_histogram(aes(y=..density..), binwidth=0.01,colour="black", fill="white")+ geom_density(alpha=.2, fill="blue", size=1) +
  geom_vline(aes(xintercept=median(md, na.rm=T)),color="red", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=mean(md, na.rm=T)), color="blue", linetype="dashed", size=1) + 
  theme_bw() + theme(text=element_text(face="bold", size=18)) + 
  ggtitle("Session 2") +xlab("Max deviation") + ylab("")+xlim(-0.25,0.25)
plot_grid(hist.s1, hist.s2); dev.off()
pdf('figures/max_deviation_pos_neg_separate.pdf')
#boxplot main effect emotion
ggplot(subset(mouse_plot, md_pos==T)) +  geom_boxplot(aes(x = emotion, y = md, fill=emotion))+facet_grid(group~session)+
  labs(x = "", y = "Positive MD") + scale_fill_manual(values=colemo)+
  theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+ylim(c(0,0.2))
ggplot(subset(mouse_plot, md_pos==F)) +  geom_boxplot(aes(x = emotion, y = md, fill=emotion))+facet_grid(group~session)+
  labs(x = "", y = "Negative MD") + scale_fill_manual(values=colemo)+
  theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+ylim(c(0,-0.2))
#bar plot average per emotion
ggplot(data = subset(mouse_plot, md_pos==T) , aes(x = emotion, y = md, fill=emotion))+facet_grid(group~session)+
  stat_summary(fun.y = mean, geom = "bar", position=position_dodge(width = 0.9))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.2, size = 1) + 
  labs(x = "", y = "Positive MD") +  scale_fill_manual(values=colemo)+ 
  theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(0.03,0.05))
ggplot(data = subset(mouse_plot, md_pos==F) , aes(x = emotion, y = -md, fill=emotion))+facet_grid(group~session)+
  stat_summary(fun.y = mean, geom = "bar", position=position_dodge(width = 0.9))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.2, size = 1) + 
  labs(x = "", y = "Negative MD") +  scale_fill_manual(values=colemo)+ 
  theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(0.03,0.05))
#point plot level 
ggplot(data = subset(mouse_plot, emotion!="Neutral"& md_pos==T) , aes(x = emotion, y = md, col=paste(emotion,level))) +
  facet_grid(group~session) + 
  stat_summary(fun.y = mean, geom = "point", position=position_dodge(width = 0.9), size=4)+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.5, size = 2) + 
  labs(x = "", y = "Positive MD") + ggtitle('Main effect emotion per level')+
  scale_colour_manual(labels = rep(c("L1", "L2", "L3", "L4"),2), values = colemolev, name="Intensity", guide = guide_legend(byrow=T))+ 
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(0.03,0.06))
ggplot(data = subset(mouse_plot, emotion!="Neutral"& md_pos==F) , aes(x = emotion, y = -md, col=paste(emotion,level))) +
  facet_grid(group~session) + 
  stat_summary(fun.y = mean, geom = "point", position=position_dodge(width = 0.9), size=4)+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.5, size = 2) + 
  labs(x = "", y = "Negative MD") + ggtitle('Main effect emotion per level')+
  scale_colour_manual(labels = rep(c("L1", "L2", "L3", "L4"),2), values = colemolev, name="Intensity", guide = guide_legend(byrow=T))+ 
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(0.02,0.05))
#boxplot emo side
ggplot(subset(mouse_plot, emotion!="Neutral"& md_pos==T)) +geom_boxplot(aes(x = away, y = md, fill=emotion))+
  facet_grid(group~session)+  ylab("Positive MD") + xlab("")+
  scale_fill_manual(values=colemo, name="", guide = guide_legend(byrow=T))+
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+ylim(c(0,0.2))
ggplot(subset(mouse_plot, emotion!="Neutral"& md_pos==F)) +geom_boxplot(aes(x = away, y = -md, fill=emotion))+
  facet_grid(group~session)+  ylab("Negative MD") + xlab("")+
  scale_fill_manual(values=colemo, name="", guide = guide_legend(byrow=T))+
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+ylim(c(0,0.2))
#barplot emo-side
ggplot(data = subset(mouse_plot, emotion!="Neutral"& md_pos==T), aes(x = away, y = md, fill=emotion))+
  stat_summary(fun.y = mean, geom = "bar", position=position_dodge(width = 0.9))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.2, size = 1) + 
  facet_grid(group~session)+  ylab("Positive MD") + xlab("")+
  scale_fill_manual(values=colemo, name="", guide = guide_legend(byrow=T))+
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(0.03,0.05))
ggplot(data = subset(mouse_plot, emotion!="Neutral"& md_pos==F), aes(x = away, y = -md, fill=emotion))+
  stat_summary(fun.y = mean, geom = "bar", position=position_dodge(width = 0.9))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.2, size = 1) + 
  facet_grid(group~session)+  ylab("Negative MD") + xlab("")+
  scale_fill_manual(values=colemo, name="", guide = guide_legend(byrow=T))+
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(0.03,0.05))
#pointplot emo-side level
ggplot(data = subset(mouse_plot, emotion!="Neutral"& md_pos==T), aes(x = away, y = md, col= paste(emotion,level)))+
  stat_summary(fun.y = mean, geom = "point", position=position_dodge(width = 0.9), size=4)+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.5, size = 2) + 
  facet_grid(group~session) + 
  labs(x = "", y = "Positive MD")+
  scale_colour_manual(values = colemolev, name = "Emotion intensity", guide = guide_legend(byrow=T)) +
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  coord_cartesian(ylim=c(0.03,0.06))
ggplot(data = subset(mouse_plot, emotion!="Neutral"& md_pos==F), aes(x = away, y = -md, col= paste(emotion,level)))+
  stat_summary(fun.y = mean, geom = "point", position=position_dodge(width = 0.9), size=4)+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.5, size = 2) + 
  facet_grid(group~session) + 
  labs(x = "", y = "Negative MD")+
  scale_colour_manual(values = colemolev, name = "Emotion intensity", guide = guide_legend(byrow=T)) +
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank()) +
  coord_cartesian(ylim=c(0.02,0.05))
dev.off()



#area under the curve ####
graphics.off()
pdf('figures/area_under_curve.pdf')
hist.s1 <- ggplot(mouse_plot1, aes(x = auc))+
  geom_histogram(aes(y=..density..), binwidth=0.01, colour="black", fill="white")+  geom_density(alpha=.2, fill="blue", size=1) +
  geom_vline(aes(xintercept=median(auc, na.rm=T)), color="red", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=mean(auc, na.rm=T)), color="blue", linetype="dashed", size=1) + 
  theme_bw() + theme(text=element_text( face="bold", size=18)) + 
  ggtitle("Session 1") +xlab("AUC")+ xlim(-0.15,0.15)
hist.s2 <- ggplot(mouse_plot2, aes(x = auc))+
  geom_histogram(aes(y=..density..), binwidth=0.01,colour="black", fill="white")+  geom_density(alpha=.2, fill="blue", size=1) +
  geom_vline(aes(xintercept=median(auc, na.rm=T)),color="red", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=mean(auc, na.rm=T)), color="blue", linetype="dashed", size=1) + 
  theme_bw() + theme(text=element_text(face="bold", size=18)) + 
  ggtitle("Session 2") +xlab("AUC") + ylab("")+ xlim(-0.15,0.15)
cowplot::plot_grid(hist.s1, hist.s2, ncol=2)

ggplot(mouse_plot) +  geom_boxplot(aes(x = emotion, y = auc, fill=emotion))+facet_grid(group~session)+
  labs(x = "", y = "AUC") + scale_fill_manual(values=colemo)+ 
  theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(-0.1,0.1))
#bar plot average per emotion
ggplot(data = mouse_plot , aes(x = emotion, y = auc, fill=emotion))+facet_grid(group~session)+
  stat_summary(fun.y = mean, geom = "bar", position=position_dodge(width = 0.9))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.2, size = 1) + 
  labs(x = "", y = "AUC") +  scale_fill_manual(values=colemo)+ 
  theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(0,0.008))
#point plot level 
ggplot(data = subset(mouse_plot, emotion!="Neutral") , aes(x = emotion, y = auc, col=paste(emotion,level))) +
  facet_grid(group~session) + 
  stat_summary(fun.y = mean, geom = "point", position=position_dodge(width = 0.9), size=4)+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.5, size = 2) + 
  labs(x = "", y = "AUC") + ggtitle('Main effect emotion per level')+
  scale_colour_manual(labels = rep(c("L1", "L2", "L3", "L4"),2), values = colemolev, name="Intensity", guide = guide_legend(byrow=T))+ 
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(0,0.008))
#boxplot emo side
ggplot(mouse_plot[mouse_plot$emotion!="Neutral",]) +geom_boxplot(aes(x = away, y = auc, fill=emotion))+
  facet_grid(group~session)+  ylab("AUC") + xlab("")+
  scale_fill_manual(values=colemo, name="", guide = guide_legend(byrow=T))+
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(-0.1,0.1))
#barplot emo-side
ggplot(data = mouse_plot[mouse_plot$emotion!="Neutral",] , aes(x = away, y = auc, fill=emotion))+
  stat_summary(fun.y = mean, geom = "bar", position=position_dodge(width = 0.9))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.2, size = 1) + 
  facet_grid(group~session)+  ylab("AUC") + xlab("")+
  scale_fill_manual(values=colemo, name="", guide = guide_legend(byrow=T))+
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(0,0.008))
#pointplot emo-side level
ggplot(data = subset(mouse_plot, emotion!="Neutral") , aes(x = away, y = auc, col= paste(emotion,level)))+
  stat_summary(fun.y = mean, geom = "point", position=position_dodge(width = 0.9), size=4)+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.5, size = 2) + 
  facet_grid(group~session) + 
  labs(x = "", y = "AUC")+
  scale_colour_manual(values = colemolev, name = "Emotion intensity", guide = guide_legend(byrow=T)) +
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())
  coord_cartesian(ylim=c(0,0.008))
dev.off()

# x flip ####
graphics.off()
pdf('figures/xflip.pdf')
hist.s1 <- ggplot(mouse_plot1, aes(x = x.flip))+
  geom_histogram(aes(y=..density..), binwidth=1, colour="black", fill="white")+  geom_density(alpha=.2, fill="blue", size=1) +
  geom_vline(aes(xintercept=median(x.flip, na.rm=T)), color="red", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=mean(x.flip, na.rm=T)), color="blue", linetype="dashed", size=1) + 
  theme_bw() + theme(text=element_text( face="bold", size=18)) + 
  ggtitle("Session 1") +xlab("x flip")#+ xlim(0,15)
hist.s2 <- ggplot(mouse_plot2, aes(x = x.flip))+
  geom_histogram(aes(y=..density..), binwidth=1,colour="black", fill="white")+ geom_density(alpha=.2, fill="blue", size=1) +
  geom_vline(aes(xintercept=median(x.flip, na.rm=T)),color="red", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=mean(x.flip, na.rm=T)), color="blue", linetype="dashed", size=1) + 
  theme_bw() + theme(text=element_text(face="bold", size=18)) + 
  ggtitle("Session 2") +xlab("x flip") + ylab("")#+ xlim(0,15)
cowplot::plot_grid(hist.s1, hist.s2, ncol=2)
#boxplot
ggplot(mouse_plot) +  geom_boxplot(aes(x = emotion, y = x.flip, fill=emotion))+facet_grid(group~session)+
  labs(x = "", y = "x.flip") + scale_fill_manual(values=colemo)+ 
  theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())
#bar plot average per emotion
ggplot(data = mouse_plot , aes(x = emotion, y = x.flip, fill=emotion))+facet_grid(group~session)+
  stat_summary(fun.y = mean, geom = "bar", position=position_dodge(width = 0.9))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.2, size = 1) + 
  labs(x = "", y = "x.flip") +  scale_fill_manual(values=colemo)+ 
  theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(3,5))
#point plot level 
ggplot(data = subset(mouse_plot, emotion!="Neutral") , aes(x = emotion, y = x.flip, col=paste(emotion,level))) +
  facet_grid(group~session) + 
  stat_summary(fun.y = mean, geom = "point", position=position_dodge(width = 0.9), size=4)+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.5, size = 2) + 
  labs(x = "", y = "x.flip") + ggtitle('Main effect emotion per level')+
  scale_colour_manual(labels = rep(c("L1", "L2", "L3", "L4"),2), values = colemolev, name="Intensity", guide = guide_legend(byrow=T))+ 
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(3,5))
#boxplot emo side
ggplot(mouse_plot[mouse_plot$emotion!="Neutral",]) +geom_boxplot(aes(x = away, y = x.flip, fill=emotion))+
  facet_grid(group~session)+  ylab("x.flip") + xlab("")+
  scale_fill_manual(values=colemo, name="", guide = guide_legend(byrow=T))+
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())
#barplot emo-side
ggplot(data = mouse_plot[mouse_plot$emotion!="Neutral",] , aes(x = away, y = x.flip, fill=emotion))+
  stat_summary(fun.y = mean, geom = "bar", position=position_dodge(width = 0.9))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.2, size = 1) + 
  facet_grid(group~session)+  ylab("x.flip") + xlab("")+
  scale_fill_manual(values=colemo, name="", guide = guide_legend(byrow=T))+
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(3,5))
#pointplot emo-side level
ggplot(data = subset(mouse_plot, emotion!="Neutral") , aes(x = away, y = x.flip, col= paste(emotion,level)))+
  stat_summary(fun.y = mean, geom = "point", position=position_dodge(width = 0.9), size=4)+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.5, size = 2) + 
  facet_grid(group~session) + 
  labs(x = "", y = "x.flip")+
  scale_colour_manual(values = colemolev, name = "Emotion intensity", guide = guide_legend(byrow=T)) +
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(3,5))
dev.off()


# y flip ####
graphics.off()
pdf('figures/yflip_hist.pdf',width=13)
hist.s1 <- ggplot(mouse_plot1, aes(x = y.flip))+
  geom_histogram(aes(y=..density..), binwidth=1, colour="black", fill="white")+  geom_density(alpha=.2, fill="blue", size=1) +
  geom_vline(aes(xintercept=median(y.flip, na.rm=T)), color="red", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=mean(y.flip, na.rm=T)), color="blue", linetype="dashed", size=1) + 
  theme_bw() + theme(text=element_text( face="bold", size=18)) + 
  ggtitle("Session 1") +xlab("x flip")#+ xlim(0,15)
hist.s2 <- ggplot(mouse_plot2, aes(x = y.flip))+
  geom_histogram(aes(y=..density..), binwidth=1,colour="black", fill="white")+ geom_density(alpha=.2, fill="blue", size=1) +
  geom_vline(aes(xintercept=median(y.flip, na.rm=T)),color="red", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=mean(y.flip, na.rm=T)), color="blue", linetype="dashed", size=1) + 
  theme_bw() + theme(text=element_text(face="bold", size=18)) + 
  ggtitle("Session 2") +xlab("y flip") + ylab("")#+ xlim(0,15)
cowplot::plot_grid(hist.s1, hist.s2, ncol=2)
dev.off()
graphics.off()
pdf('figures/yflip.pdf')
#boxplot
ggplot(mouse_plot) +  geom_boxplot(aes(x = emotion, y = y.flip, fill=emotion))+facet_grid(group~session)+
  labs(x = "", y = "y flip") + scale_fill_manual(values=colemo)+ 
  theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())
#bar plot average per emotion
ggplot(data = mouse_plot , aes(x = emotion, y = y.flip, fill=emotion))+facet_grid(group~session)+
  stat_summary(fun.y = mean, geom = "bar", position=position_dodge(width = 0.9))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.2, size = 1) + 
  labs(x = "", y = "y flip") +  scale_fill_manual(values=colemo)+ 
  theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(3,5))
#point plot level 
ggplot(data = subset(mouse_plot, emotion!="Neutral") , aes(x = emotion, y = y.flip, col=paste(emotion,level))) +
  facet_grid(group~session) + 
  stat_summary(fun.y = mean, geom = "point", position=position_dodge(width = 0.9), size=4)+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.5, size = 2) + 
  labs(x = "", y = "y flip") + ggtitle('Main effect emotion per level')+
  scale_colour_manual(labels = rep(c("L1", "L2", "L3", "L4"),2), values = colemolev, name="Intensity", guide = guide_legend(byrow=T))+ 
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(3,5))
#boxplot emo side
ggplot(mouse_plot[mouse_plot$emotion!="Neutral",]) +geom_boxplot(aes(x = away, y = y.flip, fill=emotion))+
  facet_grid(group~session)+  ylab("y.flip") + xlab("")+
  scale_fill_manual(values=colemo, name="", guide = guide_legend(byrow=T))+
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())
#barplot emo-side
ggplot(data = mouse_plot[mouse_plot$emotion!="Neutral",] , aes(x = away, y = y.flip, fill=emotion))+
  stat_summary(fun.y = mean, geom = "bar", position=position_dodge(width = 0.9))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.2, size = 1) + 
  facet_grid(group~session)+  ylab("y.flip") + xlab("")+
  scale_fill_manual(values=colemo, name="", guide = guide_legend(byrow=T))+
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(3,5))
#pointplot emo-side level
ggplot(data = subset(mouse_plot, emotion!="Neutral") , aes(x = away, y = y.flip, col= paste(emotion,level)))+
  stat_summary(fun.y = mean, geom = "point", position=position_dodge(width = 0.9), size=4)+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.5, size = 2) + 
  facet_grid(group~session) + 
  labs(x = "", y = "y flip")+
  scale_colour_manual(values = colemolev, name = "Emotion intensity", guide = guide_legend(byrow=T)) +
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(3,5))
dev.off()

# choice confidence ####
load('data/confidence.R')

conf_plot <- c_clean %>%
  mutate(session = factor(session, levels = c(1,2), labels = c("Session 1", "Session 2")))

graphics.off()
pdf('figures/confidence.pdf')
#histograms
hist.s1 <- ggplot(subset(conf_plot, session!="Session 1"), aes(x = confidence))+
  geom_histogram(aes(y=..density..), colour="black", fill="white")+geom_density(alpha=.2, fill="blue", size=1) + 
  geom_vline(aes(xintercept=median(confidence, na.rm=T)), color="red", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=mean(confidence, na.rm=T)), color="blue", linetype="dashed", size=1) + 
  theme_bw() + theme(text=element_text( face="bold", size=18)) + 
  ggtitle("Session 1") +xlab("Choice confidence") +
  #scale_x_continuous(breaks=c(95,300,median(mouse_plot1$confidence),450,600,750,900,1050))+
  scale_fill_manual(values=colpose_s2, name="")+theme(legend.position="bottom")
hist.s2 <- ggplot(subset(conf_plot, session!="Session 2"), aes(x = confidence))+
  geom_histogram(aes(y=..density..), colour="black", fill="white")+ geom_density(alpha=.2, fill="blue", size=1) +
  geom_vline(aes(xintercept=median(confidence, na.rm=T)), color="red", linetype="dashed", size=1) + 
  geom_vline(aes(xintercept=mean(confidence, na.rm=T)), color="blue", linetype="dashed", size=1) + 
  theme_bw() + theme(text=element_text( face="bold", size=18)) + 
  ggtitle("Session 2") +xlab("Choice confidence") +ylab("")+
  #scale_x_continuous(breaks=c(95,300,median(mouse_plot2$confidence),450,600,750,900,1050))+
  scale_fill_manual(values=colpose_s2, name="")+theme(legend.position="bottom")
cowplot::plot_grid(hist.s1, hist.s2, ncol=2)
ggplot(conf_plot) +  geom_histogram(aes(x = confidence, y=..density..))+facet_grid(group~session)+xlab("Choice confidence")

#barplot interaction session x posture
ggplot(data = conf_plot , aes(x = session, y = confidence, fill=group))+facet_grid(~group)+
  stat_summary(fun.y = mean, geom = "bar", position=position_dodge(width = 0.9))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.2, size = 1) + 
  labs(x = "", y = "Choice confidence") +  scale_fill_manual(values=colpose_s2)+ 
  theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(60,80))
#boxplot interaction session x posture
ggplot(conf_plot) +  geom_boxplot(aes(x = session, y = confidence, fill=session))+facet_grid(~group)+
   labs(x = "", y = "Choice confidence") + scale_fill_manual(values=colpose_s2)+ 
   theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                     panel.grid.major=element_blank(), panel.grid.minor=element_blank())
#bar plot average per emotion
ggplot(data = conf_plot , aes(x = emotion, y = confidence, fill=emotion))+facet_grid(group~session)+
  stat_summary(fun.y = mean, geom = "bar", position=position_dodge(width = 0.9))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.2, size = 1) + 
  labs(x = "", y = "Choice confidence") +  scale_fill_manual(values=colemo)+ 
  theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(60,80))#+scale_y_continuous(breaks=c(3,300))
#barplot emo-side per session/posture
ggplot(data = conf_plot[conf_plot$emotion!="Neutral",] , aes(x = away, y = confidence, fill=emotion))+
  stat_summary(fun.y = mean, geom = "bar", position=position_dodge(width = 0.9))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.2, size = 1) + 
  coord_cartesian(ylim=c(60,80))+
  facet_grid(group~session)+  ylab("Choice confidence") + xlab("")+
  scale_fill_manual(values=colemo, name="", guide = guide_legend(byrow=T))+
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())
#barplot emo-side overall
ggplot(data = conf_plot[conf_plot$emotion!="Neutral",] , aes(x = away, y = confidence, fill=emotion))+
  stat_summary(fun.y = mean, geom = "bar", position=position_dodge(width = 0.9))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.2, size = 1) + 
  coord_cartesian(ylim=c(60,80))+
  ylab("Choice confidence") + xlab("")+
  scale_fill_manual(values=colemo, name="", guide = guide_legend(byrow=T))+
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())

ggplot(data=conf_plot[conf_plot$emotion!="Neutral",], aes(x = away, y = confidence, fill=emotion)) +  
  geom_boxplot()+
  labs(x = "", y = "Choice confidence") + scale_fill_manual(values=colemoAF, name="", guide = guide_legend(byrow=T))+
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
#plot all individual point in addition
  # geom_boxplot(notch = F, outlier.colour = "red", outlier.shape = NA, position=position_dodge(1), size = 1, width=0.4)+
  # geom_jitter(shape=16, position=position_jitter(0.2), size = 2)

#barplot side main effect
ggplot(data = conf_plot[conf_plot$emotion!="Neutral",] , aes(x = away, y = confidence, fill=away))+
  stat_summary(fun.y = mean, geom = "bar", position=position_dodge(width = 0.9))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.2, size = 1) + 
  coord_cartesian(ylim=c(60,80))+
  facet_grid(group~session)+  ylab("Choice confidence") + xlab("")+
  scale_fill_manual(values=c("dimgray", "gray"), name="", guide = guide_legend(byrow=T))+
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())
dev.off()

#interaction session x posture - better size format and bigger letters in relation
pdf("figures/confidence_sessionXposture.pdf", width=12); 
ggplot(data = conf_plot , aes(x = session, y = confidence, fill=session))+facet_grid(~group)+
  stat_summary(fun.y = mean, geom = "bar", position=position_dodge(width = 0.9))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position=position_dodge(width = 0.9), width = 0.2, size = 1) + 
  labs(x = "", y = "Choice confidence") +  scale_fill_manual(values=colpose_s2)+ 
  theme_bw() +theme(legend.position ="none", text = element_text(size=30), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(60,80))
dev.off()

#mean change plots
c_mean_plot <- c_mean %>%
  mutate(session = factor(session, levels = c(1,2), labels = c("Session 1", "Session 2")))
c_change_plot <- c_change %>%
  mutate(session = factor(session, levels = c(1,2), labels = c("Session 1", "Session 2")))

graphics.off()
pdf('figures/confidence_change.pdf')
#plot change 
ggplot(c_change) +  geom_histogram(aes(x = mean_change, y=..density..))+facet_grid(~group)+xlab("Mean confidence change")
ggplot(c_change) +  geom_boxplot(aes(x = group, y=mean_change, fill=group))+
  labs(x = "", y = "Mean confidence change") +  scale_fill_manual(values=colpose_s2)+ 
  theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())
ggplot(c_change) +  geom_boxplot(aes(x = group, y=mean_change, fill=group))+
  labs(x = "", y = "Zoom in: Mean confidence change") +  scale_fill_manual(values=colpose_s2)+ 
  theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
 coord_cartesian(ylim=c(-10,10))
ggplot(c_change) +  geom_boxplot(aes(x = group, y=median_change, fill=group))+
  labs(x = "", y = "Median confidence change") +  scale_fill_manual(values=colpose_s2)+ 
  theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())
ggplot(c_change) +  geom_boxplot(aes(x = group, y=median_change, fill=group))+
  labs(x = "", y = "Zoom in: Median confidence change") +  scale_fill_manual(values=colpose_s2)+ 
  theme_bw() +theme(legend.position ="none", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(-10,10))
#confidence change per participant
pd = position_dodge(width=0.4)
ggplot(c_change_plot, aes(x=session, y=mean, color=change_direction, group=factor(subject))) + 
  geom_point(position=pd) + geom_line(position=pd)+facet_grid(~group)+
  labs(x = "", y = "Mean confidence") +  
  scale_colour_manual(values=c("red", "black", "blue"), name = "Change direction", guide = guide_legend(byrow=T, ncol=1))+ 
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())
ggplot(c_change_plot, aes(x=session, y=mean, color=change_direction, group=factor(subject))) + 
  geom_point(position=pd) + geom_line(position=pd)+facet_grid(~group)+
  labs(x = "", y = "Mean confidence") +  
  scale_colour_manual(values=c("red", "black", "blue"), name = "Change direction", guide = guide_legend(byrow=T, ncol=1))+ 
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(40,100))#exclude 5 subjects below 40
ggplot(c_change_plot, aes(x=session, y=median, color=change_direction_med, group=factor(subject))) + 
  geom_point(position=pd) + geom_line(position=pd)+facet_grid(~group)+
  labs(x = "", y = "Median confidence") + 
  scale_colour_manual(values=c("red", "black", "blue"), name = "Change direction", guide = guide_legend(byrow=T, ncol=1))+ 
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())
ggplot(c_change_plot, aes(x=session, y=median, color=change_direction_med, group=factor(subject))) + 
  geom_point(position=pd) + geom_line(position=pd)+facet_grid(~group)+
  labs(x = "", y = "Median confidence") +  
  scale_colour_manual(values=c("red", "black", "blue"), name = "Change direction", guide = guide_legend(byrow=T, ncol=1))+ 
  theme_bw() +theme(legend.position ="bottom", text = element_text(size=s), plot.title=element_text(hjust = 0.5),
                    panel.grid.major=element_blank(), panel.grid.minor=element_blank())+
  coord_cartesian(ylim=c(40,100))#exclude 5 subjects below 40

dev.off()

