#questionnaires analysis

#load packages
require(dplyr); require(ggplot2); require(car); require(psych); require(pastecs); require(TOSTER) 

#clear variables in environment
rm(list=ls())

#load data
load("data/paway_79sub_filt5perc.R")

#any empty values? (show only for columns with questionnaire scores)
is.na(pawaydiff_w[,c(1,3:9)])#line 47, column anxstate
#which subject?
unique(pawaydiff_w$subject[(is.na(pawaydiff_w$anxstate))])#subject 54 has no state anxiety score 

#n per posture
xtabs(~group, subset(paway_w, session==1))
n1 <- 40; n2 <- 39

#Levene's test: are variances heterogenous between posture groups? No. 
t(sapply(pawaydiff_w[,c("anxstate", "anxtrait", "self_esteem", "bis", "bas", "dominance", "affiliation")], 
                       function(x) round(unlist(leveneTest(x ~ pawaydiff_w$group))[c("Df1", "Df2", "F value1", "Pr(>F)1")], digits=3)))
#all variances equal

#t-tests between posture, with Welch correction and unequal variances
t_evsc <- as.data.frame(t(sapply(pawaydiff_w[,c("anxstate", "anxtrait", "self_esteem", "bis", "bas", "dominance", "affiliation")], function(x)
  round(unlist(t.test(x ~ pawaydiff_w$group, var.equal = T)[c("estimate","parameter", "p.value","statistic")]),digits=3))))
#calculate cohen's d: this is the WRONG formula - correct
t_evsc$d <- round(t_evsc$statistic.t*sqrt(1/n1+1/n2), digits=3)
names(t_evsc) <- c("mean_Expan", "mean_Constr", "df", "p-value", "t-value", "d")
t_evsc
#no significant differences between posture groups on these questionnaire scores

pdf('figures/questionnaires_by_posture.pdf')
s=30
ggplot(pawaydiff_w) + geom_boxplot(aes(x = group, y = bas))+theme_bw()+theme(text = element_text(size=s))
ggplot(pawaydiff_w) + geom_boxplot(aes(x = group, y = self_esteem))+theme_bw()+theme(text = element_text(size=s))
ggplot(data = pawaydiff_w , aes(x = group, y = self_esteem)) +theme_bw()+theme(text = element_text(size=s))+
  stat_summary(fun.y = mean, geom = "bar", position= position_dodge())+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position= position_dodge(width = 0.9),width = 0.1,size = 1)
ggplot(data = pawaydiff_w , aes(x = group, y = bas)) +theme_bw()+theme(text = element_text(size=s))+
  stat_summary(fun.y = mean, geom = "bar", position= position_dodge())+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position= position_dodge(width = 0.9),width = 0.1,size = 1)
dev.off()

#correlation with anger or fear choices? Does not look like there are any strong correlations - i.e. questionnaires should not predict responses
d_anger <- paway%>%
  filter(emotion=="Anger")%>%
  rename(paway_anger = proportion)%>%
  select(-emotion, -away)
plot(d_anger[,c("paway_anger", "bas", "self_esteem", "anxtrait")])

d_fear <- paway%>%
  filter(emotion=="Fear")%>%
  rename(paway_fear = proportion)%>%
  select(-emotion, -away)
plot(d_fear[,c("paway_fear", "bas", "self_esteem", "anxtrait")])+abline()

#test correlations (only those in the first column are relevant)
corr.test(subset(d_anger, session == 1)[,c(4:11)])#no significant correlation, all smaller r=.17, p>.170, bas r=-.02
corr.test(subset(d_anger, session == 2)[,c(4:11)])#no significant correlation, all smaller r=.22, bas: r=-.21, p=.06, all other p >.19
corr.test(subset(d_fear, session == 1)[,c(4:11)])# no significant correlation, all smaller r=.22, affiliation = r=-.21, p=.07, all other p>.30
corr.test(subset(d_fear, session == 2)[,c(4:11)])#no significant correlation, all smaller r=.22, affilaition r=+.21, p=.07, all other p>.12
#all correlations non-singificant, but some around .20, trait/state - could potentially predict paway for anger or fear
#take into account in model and see if it affects results

#equivalence test's: is the difference not just not significant, but also smaller than a meaningful difference of 0.4?
#see: Lakens, D. (2016). Equivalence tests: A practical primer for t-tests, correlations, and meta-analyses. PsyArXiv. https://doi.org/10.1177/1948550617697177

desc <- as.data.frame(t(sapply(pawaydiff_w[,c("anxstate", "self_esteem", "bas", "dominance", "affiliation", "anxtrait","bis")], function(x)
  round(unlist(by(x, INDICES = pawaydiff_w$group, stat.desc, basic = FALSE))
        [c("Expansive.mean","Expansive.std.dev", "Constrictive.mean","Constrictive.std.dev")], digits=3))))

x <- "self_esteem" #fill in trait here
TOSTtwo(m1=desc[x,"Expansive.mean"], m2=desc[x,"Constrictive.mean"], sd1=desc[x,"Expansive.std.dev"],sd2=desc[x,"Constrictive.std.dev"], 
        n1=40, n2=39, low_eqbound_d=-0.3, high_eqbound_d=0.3)

#None of the equivalence tests is significant: we cannot conclude that values of these questionnaires are similar in the two posture groups. 
#We need to test effect of these questionnaire scores as covariates in the main model on choice, to verify they do not have an impact. 

