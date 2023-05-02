# Power pose effects on approachand avoidance decisions in response to social threat

All code written by Hannah Metzler in R version 3.4.4 (2018-03-15). 

Additional information: 
* OSF Repository with supplementary information and materials: https://osf.io/q8s3w/
* Pre-print:  https://psyarxiv.com/t8mhw

## Instructions for using the code

- data_osf and scripts_osf are folders for datasets and code
- "osf" indicates that data and scripts are the same as those on the project's OSF repository. The most up to date version of all files is in this github repository, but differences are minor. 

Before using the code, create these additional folders in the directory: 
- output (tables will be saved here)
- figures (pdf figures will be saved here) 

* Use script 01 before all others, it saves the data file “data_poweraction.R” used by all other scripts. 
* Use script 02 before scripts 05 and 06, it calculates the proportion of away choices (paway) used for certain plots, correlations, and tests in scripts 05 and 06. 
* To see only title of each sub-section in the scripts (useful overview): click on triangle after row number in lines that finish with #### 

## Meaning of variable names in the behav_poweraction.txt file 

It contains behavioral and stimulus data that were exported from Matlab:

- pair = index of pair (pair of actors depicted in stimuli) (1-10)
- gend = pair gender (1:Male 2:Female)- gind =  gender index for left actor
- gind2 =  gender index for right actor- emotpair =  emotion of the pair (1 = 1 actor angry, 2= 1 actor fearful 3 = both actors neutral)
- emot =  emotion for the left actor (1:Anger 2:Fear 3:Neutral)- emot2 =  emotion for the right actor (1:Anger 2:Fear 3:Neutral)
- elevpair = emotion level of the pair (i.e. the one actor expressing an emotion) (0=neutral,3=low intensity,5,7=high intensity)
- elev =  emotion level for the left actor (0,3,5,7)- elev2 =  emotion level for the right actor (0,3,5,7)
- sidemotpair =  emotional actor side (1:Left  2:Right 3:Neutral pair)- sidemot =  is the left actor emotional (0: No 1: Yes 3:Neutral pair)
- sidemot2 =  is the right actor emotional (0: No 1: Yes 3:Neutral pair)- response = 1 Left chair, 2 right chair, 3 no response, 4 outside chair
- kinematics of movement: 
  - mov_time = time from click in the middle until release on the chair 
  - init_time = time to click from appearance of scene, called initiation time in the paper

## Meaning of variable names in the questionnaires.txt file: 

All of these variables are self-report questionnaire total scores. For details, see the Supplementary Information of the manuscript. 

- subject: Subject ID
- anxstate: State anxiety from STAI (French version)- anxtrait: Trait anxiety from STAI (French version)
- self_esteem: Rosenberg self-esteem scale (French version)
- bis: Behavioural inhibition system from BIS BAS scales (French version)
- bas: Behavioural activation system from BIS BAS scales (French version)
- Interpersonal Adjective List - Short Version: 
  - dominance: self-ratings on 2 scales (PA, HI) à 4 adjectives, calculated as: (PA + reversed HI)/2
  - affiliation: self-ratings on 2 scales (LM, DE) à 4 adjectives, calculated as: (LM + reversed DE)/2
