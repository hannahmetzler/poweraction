library(haven)
library(dplyr)

setwd('/media/hannah/HANNAH2/Hannah_Backup 20181220/Documents/Projects/Poweraction/results/')

#processed data ready for analysis: ####
qualtrics_all_sub = read_sav('./questionnaires/Poweraction_questionnaires_all_tested_sub.sav', user_na = TRUE)
summary(qualtrics_all_sub$age)
#here the lowest age is 18




#raw data for questionnaire prior to testing day: contains only birth dates ####
qualtrics_raw_data = read_sav('./questionnaires/original_qualtrics_data/Poweraction_inscription_anx_screening_April+19%2C+2017_17.37.sav')

#variable names
names = qualtrics_raw_data %>% 
  sjlabelled::get_label() %>% 
  enframe() %>% #convert list to tibble
  rename(code = name, 
         varname = value)
as.data.frame(names)
#Q2.4 is birth date, needs to be combined with test date to determine age
qualtrics_raw_data$Q2.4

#the most recent birth dates: 09/08/1999, 08/01/1999, 13/10/2017 is definitely wrong

#labels: the levels of each variable
labels =   sjlabelled::get_labels(qualtrics_raw_data, values="as.prefix") %>% enframe() %>% unnest(cols=c(value)) %>% 
  rename(code = name, 
         labels = value)

#questionnaire on testing day ####
qualtrics_raw_data_state = read_sav('./questionnaires/original_qualtrics_data/Poweraction_state_April+18%2C+2017_18.01.sav')
#this does not contain any age data
# I must have calculated age based on a comparison of testing day and birth date, or it is inside the matlab data

#variable names
names = qualtrics_raw_data_state %>% 
  sjlabelled::get_label() %>% 
  enframe() %>% #convert list to tibble
  rename(code = name, 
         varname = value)
as.data.frame(names)

#there is a matlab script called sample_stats.m - age variable is in matlab, I looked at age within matlab and never saved a csv

