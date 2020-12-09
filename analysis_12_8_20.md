---
title: "Class and QWL analysis"
author: "Jerzy Eisenberg-Guyot"
date: "12/08/2020"
output: 
  html_document:
    code_folding: hide
    toc: true
    toc_float: true
    keep_md: true
always_allow_html: true
---



**General notes**

* I think including ethnicity in the race variable makes the cell sizes too small - for example, there are only 1000 Hispanics total and only 36 Hispanic capitalists of any race.

* Note that "mntlhlth" wasn't asked of ballot A in 2018.

* I made the QWL variables binary for the regression analyses for simplicity and stability.

**Reminder**

* Make sure the column indices for the regression functions are correct, since they can change if the data changes.

**Codebook for non-self-explanatory variables**

* Class variables:
    + explained in IJHS (2020) paper

* Health variables:
    + srh_bin: poor/fair vs good/vgood/excellent self-rated health
    + depress: whether respondent has ever been diagnosed by a healthcare professional with depression
    + mntlhlth: days of poor mental health in past 30 days

* Potentially relevant QWL variables:
    + mustwork: Mandatory to work extra hours
    + chngtme: How often r allowed change schedule
    + famwkoff: How hard to take time off
    + wkvsfam: How often job interferes fam life
    + secondwk: R has job other than main
    + learnnew: Job requires r to learn new things
    + workfast: Job requires r to work fast
    + wrktime: R has enough time to get the job done
    + workdiff: R does numerous things on job
    + toofewwk: How often not enough staff
    + overwork: R has too much work to do well
    + myskills: Job allows r use of skills
    + trainops: R have the training opportunities
    + opdevel: Opportunity to develop my abilities
    + respect: R treated with respect at work
    + trustman: R trust management at work
    + manvsemp: Relations bw management and employees
    + suphelp: Supervisor helpful to r in getting job done
    + supcares: Supervisor concerned about welfare
    + wkfreedm: A lot of freedom to decide how to do job
    + lotofsay: R has lot of say in job
    + wkdecide: How often r take part in decisions
    + satjob1: 	Job satisfaction in general
    + fairearn: How fair is what r earn on the job
    + fringeok: Fringe benefits are good
    + rincblls: Income alone is enough
    + laidoff: R was laid off main job last year
    + jobsecok: The job security is good
    + any_disc_haras: Any discrimination or harassment on job (created by me from several more specific variables focused separately on racism, sexism, etc.)
    + safetywk: Worker safety priority at work
    + safehlth: Safety and health condition good at work
    + safefrst: No shortcuts on worker safety
    

```r
library(readstata13)
library(foreign)
library(tidyr)
library(dplyr)
library(survival)
library(knitr)
library(ggplot2)
library(kableExtra)
library(survey)
library(tableone)
library(RColorBrewer)
library(survey)
library(VIM)
library(mice)
library(parallel)
library(mitools)
library(viridis)
library(survminer)
library(gridExtra)
library(stringr)
library(rms)
library(patchwork) 
library(spatstat)
library(purrr)

#########load data, which is a subsetted version of the GSS cumulative file available at the GSS website that I converted from .dta to .csv using Stata 
#####
#
#drop survey-ballot d, which was a ballot in 2006 that lacked responses to QWL variables
#remove those who were not employed, as only employed respondents were asked the QWL variables - also remove those with NA wrkstat (n=9), since they weren't asked the QWL variables 
#remove ballot b in 2002, as class variables weren't asked of that ballot in that year
#remove 421 respondents who still have IAP in 2006 and 2014 for QWL variables, even after making appropriate sample restrictions - according to GSS helpdesk, they weren't administered the questions/didn't complete the survey ('breakoffs')
read.csv('GSS_02_06_10_14_18.csv') %>% 
  filter(ballot != "ballot d") %>%
  filter(wrkstat=="working fulltime" | wrkstat=="working parttime" | wrkstat=="temp not working") %>%
  filter(!(year==2002 & ballot=="ballot b")) %>%
  mutate_all(funs(ifelse(.=="DK" | .=="NO ANSWER" | .=="DONT KNOW" | .=="Not asked" | .=="IAP,DK,NA,uncodeable", NA, .))) %>%
  filter(respect!="IAP") %>%
  droplevels() -> dat

########potential variables of interest (dataset already subsetted, but useful if we need to do again and add some variables)
#####
#

#vars <- c("ballot", "id", "year", "wrkstat", "wrkslf", "wrkgovt", "health1", "health", "mntlhlth", 
#          "occ10", "indus10", "marital", "spwrkslf", "age", "degree", "padeg", "madeg", "spdeg", 
#          "wksup", "wksups", "union", "prestg10", "prestg105plus", "sex", "race", "income", "rincome",
#          "coninc", "conrinc", "region", "hispanic", "sample", "wtssall", "vstrat", "vpsu", #QWL vars start on next line
#          'cowrkint', 'laidoff', 'jobfind1', 'trynewjb', 'wkageism', 'wkracism', 'wksexism', 'wkharsex', 
#          'wkharoth', 'health1', 'rincblls', 'fairearn', 'wkbonus', 'jobsecok', 'suphelp', 'wrktime',
#          'cowrkhlp', 'trainops', 'manvsemp', 'hvylift', 'handmove', 'wkpraise', 'physhlth', 'mntlhlth',
#          'hyperten', 'depress', 'misswork', 'usetech', 'knowschd', 'satjob1', 'hlthdays', 'usedup',
#          'backpain', 'painarms', 'hurtatwk', 'spvtrfair', 'strredpg', 'phyeffrt', 'slpprblm', 'promtefr',
#          'year', 'famvswk', 'hrsrelax', 'secondwk', 'learnnew', 'workfast', 'workdiff', 'lotofsay', 'wktopsat', 
#          'overwork', 'wkvsfam', 'famwkoff', 'whywkhme', 'wrktype', 'yearsjob', 'waypaid', 'wrksched', 
#          'moredays', 'mustwork', 'chngtme', 'wrkhome', 'knowwhat', 'myskills', 'setthngs',
#          'toofewwk', 'promteok', 'opdevel', 'hlpequip', 'haveinfo', 'wkfreedm', 'fringeok', 'supcares',
#          'wkdecide', 'partteam', 'trdunion', 'respect', 'trustman', 'safetywk', 'safefrst', 'teamsafe',
#          'safehlth', 'proudemp', 'prodctiv', 'wksmooth', 'condemnd')

#subset of QWL variables we'll focus on for now
#qwl_vars <- c("mustwork", "chngtme", "famwkoff", "wkvsfam", "secondwk", "learnnew", "workfast",
#              "wrktime", "workdiff", "toofewwk", "overwork", "myskills", "trainops",  "opdevel",
#              "respect", "trustman", "manvsemp", "suphelp", "supcares", "wkfreedm", "lotofsay", "wkdecide",  "satjob1", 
#              "fairearn", "fringeok", "rincblls", "laidoff", "jobsecok", "any_disc_haras", "safetywk", "safehlth", "safefrst")

#########make variables
######
#

dat %>% 
  mutate(educ = factor(ifelse(degree == "bachelor" | degree == "graduate", "College +", 
                              ifelse(degree== "high school", "HS", 
                                     ifelse(degree == "junior college", "JuCo", 
                                            ifelse(degree == "lt high school", "<HS", NA)))),
                       levels=c("<HS", "HS", "JuCo", "College +")), 
         age=ifelse(age=="89 or older", 89, as.numeric(as.character(age))),
         mntlhlth=as.numeric(as.character(mntlhlth)),
         region = factor(ifelse(region == "new england" | region == "middle atlantic", "Northeast",
                                   ifelse(region == "e. sou. central" | region == "south atlantic" | region == "w. sou. central", "South",
                                          ifelse(region == "e. nor. central" | region == "w. nor. central", "Midwest", "West")))),
         marital_tri=ifelse(marital=="married", "married",
                            ifelse(marital=="never married", "never married",
                                   ifelse(marital=="widowed" | marital=="divorced" | marital=="separated", "wid/div/sep", marital))),
         srh_bin=ifelse(health1 == "fair" | health1 == "poor", "poor/fair", "good/vgood/exc"), 
         race_poc=ifelse(race=="white", "white", 
                         ifelse(race=="black" | race=="other", "poc", NA)),
         race=factor(race, levels=c("white", "black", "other")),
         disc_haras=ifelse(wkageism=="yes" | wkracism=="yes" | wksexism=="yes" | wkharsex=="yes" | wkharoth=="yes", 1, 0),
         respect_bin=ifelse(respect=="disagree" | respect=="strongly disagree", 1, 0),
         trustman_bin=ifelse(trustman=="disagree" | trustman=="strongly disagree", 1, 0),
         manvsemp_bin=ifelse(manvsemp=="quite bad" | manvsemp=="very bad", 1, 0),
         suphelp_bin=ifelse(suphelp=="not too true" | suphelp=="not at all true", 1, 0),
         supcares_bin=ifelse(supcares=="not too true" | supcares=="not at all true", 1, 0),
         wkfreedm_bin=ifelse(wkfreedm=="not too true" | wkfreedm=="not at all true", 1, 0),
         wkdecide_bin=ifelse(wkdecide=="rarely" | wkdecide=="never", 1, 0),
         safehlth_bin=ifelse(safehlth=="disagree" | safehlth=="strongly disagree", 1, 0),
         safefrst_bin=ifelse(safefrst=="disagree" | safefrst=="strongly disagree", 1, 0),
         income=as.numeric(as.character(coninc)) * 251.1 / 172.2, #adjust income to 2018 dollars from 2000 dollars
         prestg10_bin=ifelse(prestg10 < weighted.median(prestg10, w=wtssall, na.rm=T), "low prestige", "high prestige"),  #prestige below median
         #we could also create the class variable using numemps (number of employees employed by the self-employed), but it's not available in 2002 
         class=factor(ifelse((wksup == "yes" & wrkslf == "self-employed") | (wksup == "yes" & wrkslf == "someone else" & occ10 == 10), "Capitalists", 
                              ifelse((wksup == "no" & wrkslf == "self-employed") | (wksup == "no" & wrkslf == "someone else" & occ10 == 10), "Petit bourgeoisie", 
                                                 ifelse(wksup == "yes" & wrkslf == "someone else" & occ10 != "chief executives", "Managers", 
                                                        ifelse(wksup == "no" & wrkslf ==  "someone else" & occ10 != "chief executives", "Workers", NA)))), 
                                   levels=c("Workers", "Managers", "Petit bourgeoisie", "Capitalists")),
         class_gender=factor(ifelse(class=="Workers" & sex=="male", "Male workers",
                                    ifelse(class=="Workers" & sex=="female", "Female workers",
                                           ifelse(class=="Managers" & sex=="male", "Male managers",
                                                  ifelse(class=="Managers" & sex=="female", "Female managers",
                                                         ifelse(class=="Petit bourgeoisie" & sex=="male", "Male petit bourgeoisie",
                                                                ifelse(class=="Petit bourgeoisie" & sex=="female", "Female petit bourgeoisie",
                                                                       ifelse(class=="Capitalists" & sex=="male", "Male capitalists",
                                                                              ifelse(class=="Capitalists" & sex=="female", "Female capitalists", NA)))))))),
                             levels=c("Male workers", "Male managers", "Male petit bourgeoisie", "Male capitalists",
                                      "Female workers", "Female managers", "Female petit bourgeoisie", "Female capitalists")),
         class_poc=factor(ifelse(class=="Workers" & race_poc=="white", "white workers",
                                    ifelse(class=="Workers" & race_poc=="poc", "poc workers",
                                           ifelse(class=="Managers" & race_poc=="white", "white managers",
                                                  ifelse(class=="Managers" & race_poc=="poc", "poc managers",
                                                         ifelse(class=="Petit bourgeoisie" & race_poc=="white", "white petit bourgeoisie",
                                                                ifelse(class=="Petit bourgeoisie" & race_poc=="poc", "poc petit bourgeoisie",
                                                                       ifelse(class=="Capitalists" & race_poc=="white", "white capitalists",
                                                                              ifelse(class=="Capitalists" & race_poc=="poc", "poc capitalists", NA)))))))),
                             levels=c("white workers", "white managers", "white petit bourgeoisie", "white capitalists",
                                      "poc workers", "poc managers", "poc petit bourgeoisie", "poc capitalists")),
         class_race=factor(ifelse(class=="Workers" & race=="white", "white workers",
                                    ifelse(class=="Workers" & race=="black", "black workers",
                                           ifelse(class=="Workers" & race=="other", "other workers",
                                                  ifelse(class=="Managers" & race=="white", "white managers",
                                                         ifelse(class=="Managers" & race=="black", "black managers",
                                                                ifelse(class=="Managers" & race=="other", "other managers",
                                                         ifelse(class=="Petit bourgeoisie" & race=="white", "white petit bourgeoisie",
                                                                ifelse(class=="Petit bourgeoisie" & race=="black", "black petit bourgeoisie",
                                                                       ifelse(class=="Petit bourgeoisie" & race=="other", "other petit bourgeoisie",
                                                                       ifelse(class=="Capitalists" & race=="white", "white capitalists",
                                                                              ifelse(class=="Capitalists" & race=="black", "black capitalists", 
                                                                                     ifelse(class=="Capitalists" & race=="other", "other capitalists", NA)))))))))))),
                             levels=c("white workers", "white managers", "white petit bourgeoisie", "white capitalists",
                                      "black workers", "black managers", "black petit bourgeoisie", "black capitalists",
                                      "other workers", "other managers", "other petit bourgeoisie", "other capitalists")),
         race_gender=factor(ifelse(race=="white" & sex=="male", "white male",
                                   ifelse(race=="white" & sex=="female", "white female",
                                          ifelse(race=="black" & sex=="male", "black male",
                                                 ifelse(race=="black" & sex=="female", "black female",
                                                        ifelse(race=="other" & sex=="male", "other male",
                                                               ifelse(race=="other" & sex=="female", "other female", NA)))))),
                            levels=c("white male", "white female", "black male", "black female", "other male", "other female"))) -> dat
```

# Handling missing data

## Missingness patterns


```r
aggr(dat[,c("class", 'sex', 'race', 'educ', 'marital_tri', 'region', 'income', 'age', "srh_bin", "mntlhlth", "disc_haras", "respect_bin", "trustman_bin", "manvsemp_bin", "suphelp_bin", "supcares_bin", "wkfreedm_bin", "wkdecide_bin", "safehlth_bin", "safefrst_bin")], cex.axis = 0.7, numbers=FALSE, prop=TRUE)
```

![](analysis_12_8_20_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

## Multiple imputation

**TBD**


```r
#make imputation dataset
data_mice <- dat[,c("capital_chief_exec", "capital_det_skill_chief_exec", "wksup", "wksups", "wrkslf", 'wrkstat', 'prestg10', 'income', 'region', 'race', "non_white", 'age', 'sex', 'educ', 'year', 'decade', 'vpsu', 'vstrat', 'wtssall', 'srh_bin', "age90", "death", "yeardeath", "duration", "capital_chief_exec_non_white")]

##do imputation
initialize <- mice(data_mice, max=0, print=FALSE) # this creates an empty imputation object that you can manipulate and set options

method <- initialize$method #this creates an object that will contain the imputation method for each variable. let mice choose defaults based on variable class

method[c( "region", "race", "sex", "year", 'decade', "vpsu", 'vstrat', 'wtssall',  'srh_bin', "age90", "death", "yeardeath", "duration")] <- "" # this code tells mice not to impute these variables. not imputing variables with complete data or any of the outcome variables

method["capital_chief_exec_non_white"] <- "~I(as.numeric(capital_chief_exec)*as.numeric(non_white))" #passive imputation of variable that's a combo of other variables

prediction <- initialize$predictorMatrix  #this creates an object that will indicate which variables we actually want to include in predicting missing variables. 

prediction[,c("capital_det_skill_chief_exec", "capital_chief_exec_non_white", 'non_white', 'decade', 'srh_bin',  "age90", "death", "yeardeath", "duration", 'vstrat', 'vpsu')] <- 0 # don't use collinear variables, SRH, or death as predictors (don't use latter two variables because the class variables, SRH, and death are available in different years and on different ballots - to use them, I think we'd have to make three separate sets of imputed datasets, one for the descriptive analyses, one for the SRH analyses, and one for the mortality analyses). Also don't use vstrat, since it has too many levels (models won't fit) and vpsu because it's IAP until 1976 (plus the trace plots for imputations that included VPSU don't look as good as the trace plots for imputations that excluded VPSU)

cores_2_use <- 4 #4 cores times 5 imputations per core = 20 imputations
cl <- makeCluster(cores_2_use)
clusterSetRNGStream(cl, 9956)
clusterExport(cl, list("data_mice", "method", "prediction"), envir=environment())
clusterEvalQ(cl, library(mice))
imp_pars <- 
  parLapply(cl = cl, X = 1:cores_2_use, fun = function(no){
    mice(data_mice, m=5, meth=method, pred=prediction, print=FALSE, maxit = 20) #no seed because we don't want imputations to be identical across reps
  })
stopCluster(cl)

#combine the smaller imputation objects from each core into one imputation object
imp_merged <- imp_pars[[1]]
for (n in 2:length(imp_pars)){
  imp_merged <- 
    ibind(imp_merged,
          imp_pars[[n]])
}

#put data into useable format for survey package
imp.list <- lapply(1:20, function(x) mice::complete(imp_merged, x))
dat_mi <- imputationList(imp.list)
```

# Survey set the data


```r
#center data for the single-PSU stratum at the sample grand mean rather than the stratum mean (conservative)
options(survey.lonely.psu="adjust")

#survey set imputed data
#svy <- svydesign(ids = ~ vpsu,
#                 strata = ~ vstrat, 
#                 weights = ~ wtssall,
#                 nest=TRUE, 
#                 data=dat_mi)

#survey-set unimputed data for descriptives
svy_dat <- svydesign(ids = ~ vpsu,
                     strata = ~ vstrat,
                     weights = ~ wtssall, 
                     nest=TRUE, 
                     data=dat)
```

# Survey-weighted, unimputed descriptives

## Demographics and health stratifed by class 


```r
#vars of interest
vars <- c('sex', 'race', 'educ', 'marital_tri', 'region', 'income', 'age', "srh_bin", "mntlhlth")
nonorm <- c('income', 'age')

x <- svyCreateTableOne(data = svy_dat, vars = vars, strata='class')
x <- print(x, printToggle=FALSE, noSpaces=TRUE, nonnormal=nonorm)
kable(x[,1:4], caption="Simple") %>%
  kable_styling(c("striped", "condensed"))
```

<table class="table table-striped table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>Simple</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> Workers </th>
   <th style="text-align:left;"> Managers </th>
   <th style="text-align:left;"> Petit bourgeoisie </th>
   <th style="text-align:left;"> Capitalists </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> n </td>
   <td style="text-align:left;"> 3797.1 </td>
   <td style="text-align:left;"> 2104.5 </td>
   <td style="text-align:left;"> 525.1 </td>
   <td style="text-align:left;"> 382.4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sex = male (%) </td>
   <td style="text-align:left;"> 1695.5 (44.7) </td>
   <td style="text-align:left;"> 1089.9 (51.8) </td>
   <td style="text-align:left;"> 264.0 (50.3) </td>
   <td style="text-align:left;"> 294.8 (77.1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> race (%) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> white </td>
   <td style="text-align:left;"> 2763.9 (72.8) </td>
   <td style="text-align:left;"> 1618.9 (76.9) </td>
   <td style="text-align:left;"> 416.3 (79.3) </td>
   <td style="text-align:left;"> 316.1 (82.7) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> black </td>
   <td style="text-align:left;"> 614.7 (16.2) </td>
   <td style="text-align:left;"> 261.6 (12.4) </td>
   <td style="text-align:left;"> 36.7 (7.0) </td>
   <td style="text-align:left;"> 21.6 (5.6) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> other </td>
   <td style="text-align:left;"> 418.6 (11.0) </td>
   <td style="text-align:left;"> 223.9 (10.6) </td>
   <td style="text-align:left;"> 72.1 (13.7) </td>
   <td style="text-align:left;"> 44.7 (11.7) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> educ (%) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> &lt;HS </td>
   <td style="text-align:left;"> 359.4 (9.5) </td>
   <td style="text-align:left;"> 158.1 (7.5) </td>
   <td style="text-align:left;"> 63.2 (12.0) </td>
   <td style="text-align:left;"> 33.4 (8.7) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> HS </td>
   <td style="text-align:left;"> 2095.5 (55.2) </td>
   <td style="text-align:left;"> 918.9 (43.7) </td>
   <td style="text-align:left;"> 253.7 (48.3) </td>
   <td style="text-align:left;"> 160.3 (41.9) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> JuCo </td>
   <td style="text-align:left;"> 342.9 (9.0) </td>
   <td style="text-align:left;"> 220.9 (10.5) </td>
   <td style="text-align:left;"> 48.5 (9.2) </td>
   <td style="text-align:left;"> 27.3 (7.1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> College + </td>
   <td style="text-align:left;"> 999.3 (26.3) </td>
   <td style="text-align:left;"> 806.6 (38.3) </td>
   <td style="text-align:left;"> 159.7 (30.4) </td>
   <td style="text-align:left;"> 161.4 (42.2) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> marital_tri (%) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> married </td>
   <td style="text-align:left;"> 1938.3 (51.0) </td>
   <td style="text-align:left;"> 1176.2 (55.9) </td>
   <td style="text-align:left;"> 313.0 (59.6) </td>
   <td style="text-align:left;"> 270.9 (70.8) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> never married </td>
   <td style="text-align:left;"> 1186.0 (31.2) </td>
   <td style="text-align:left;"> 560.1 (26.6) </td>
   <td style="text-align:left;"> 104.7 (19.9) </td>
   <td style="text-align:left;"> 36.4 (9.5) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> wid/div/sep </td>
   <td style="text-align:left;"> 672.8 (17.7) </td>
   <td style="text-align:left;"> 368.2 (17.5) </td>
   <td style="text-align:left;"> 107.4 (20.4) </td>
   <td style="text-align:left;"> 75.1 (19.6) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> region (%) </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;">  </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Midwest </td>
   <td style="text-align:left;"> 926.6 (24.4) </td>
   <td style="text-align:left;"> 474.5 (22.5) </td>
   <td style="text-align:left;"> 99.5 (18.9) </td>
   <td style="text-align:left;"> 79.7 (20.8) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Northeast </td>
   <td style="text-align:left;"> 625.0 (16.5) </td>
   <td style="text-align:left;"> 387.9 (18.4) </td>
   <td style="text-align:left;"> 78.8 (15.0) </td>
   <td style="text-align:left;"> 46.0 (12.0) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> South </td>
   <td style="text-align:left;"> 1477.1 (38.9) </td>
   <td style="text-align:left;"> 734.6 (34.9) </td>
   <td style="text-align:left;"> 190.3 (36.2) </td>
   <td style="text-align:left;"> 134.5 (35.2) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> West </td>
   <td style="text-align:left;"> 768.5 (20.2) </td>
   <td style="text-align:left;"> 507.5 (24.1) </td>
   <td style="text-align:left;"> 156.5 (29.8) </td>
   <td style="text-align:left;"> 122.3 (32.0) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> income (median [IQR]) </td>
   <td style="text-align:left;"> 63838.02 [35022.03, 102218.99] </td>
   <td style="text-align:left;"> 84330.67 [48492.05, 128628.24] </td>
   <td style="text-align:left;"> 63838.02 [32196.43, 122662.79] </td>
   <td style="text-align:left;"> 117053.99 [64773.77, 230688.05] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> age (median [IQR]) </td>
   <td style="text-align:left;"> 40.00 [29.00, 51.00] </td>
   <td style="text-align:left;"> 41.00 [31.00, 51.00] </td>
   <td style="text-align:left;"> 49.00 [37.00, 57.00] </td>
   <td style="text-align:left;"> 50.00 [41.00, 58.01] </td>
  </tr>
  <tr>
   <td style="text-align:left;"> srh_bin = poor/fair (%) </td>
   <td style="text-align:left;"> 575.1 (15.2) </td>
   <td style="text-align:left;"> 269.4 (12.8) </td>
   <td style="text-align:left;"> 83.4 (15.9) </td>
   <td style="text-align:left;"> 41.9 (11.0) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> mntlhlth (mean (SD)) </td>
   <td style="text-align:left;"> 3.52 (7.09) </td>
   <td style="text-align:left;"> 3.34 (6.84) </td>
   <td style="text-align:left;"> 2.60 (6.18) </td>
   <td style="text-align:left;"> 3.02 (7.10) </td>
  </tr>
</tbody>
</table>

## QWL variables stratified by class 


```r
#vars of interest
qwl_vars <- c("disc_haras", "respect_bin", "trustman_bin", "manvsemp_bin", "suphelp_bin", "supcares_bin", "wkfreedm_bin", "wkdecide_bin", "safehlth_bin", "safefrst_bin")

x <- svyCreateTableOne(data = svy_dat, vars = qwl_vars, factorVars=qwl_vars, strata='class')
x <- print(x, printToggle=FALSE, noSpaces=TRUE)
kable(x[,1:4], caption="Simple") %>%
  kable_styling(c("striped", "condensed"))
```

<table class="table table-striped table-condensed" style="margin-left: auto; margin-right: auto;">
<caption>Simple</caption>
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:left;"> Workers </th>
   <th style="text-align:left;"> Managers </th>
   <th style="text-align:left;"> Petit bourgeoisie </th>
   <th style="text-align:left;"> Capitalists </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> n </td>
   <td style="text-align:left;"> 3797.1 </td>
   <td style="text-align:left;"> 2104.5 </td>
   <td style="text-align:left;"> 525.1 </td>
   <td style="text-align:left;"> 382.4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> disc_haras = 1 (%) </td>
   <td style="text-align:left;"> 786.6 (20.7) </td>
   <td style="text-align:left;"> 497.6 (23.7) </td>
   <td style="text-align:left;"> 72.6 (14.0) </td>
   <td style="text-align:left;"> 63.1 (16.7) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> respect_bin = 1 (%) </td>
   <td style="text-align:left;"> 334.9 (8.8) </td>
   <td style="text-align:left;"> 131.3 (6.2) </td>
   <td style="text-align:left;"> 22.4 (4.3) </td>
   <td style="text-align:left;"> 8.7 (2.3) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> trustman_bin = 1 (%) </td>
   <td style="text-align:left;"> 829.0 (21.9) </td>
   <td style="text-align:left;"> 396.3 (18.9) </td>
   <td style="text-align:left;"> 26.4 (5.2) </td>
   <td style="text-align:left;"> 16.0 (4.3) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> manvsemp_bin = 1 (%) </td>
   <td style="text-align:left;"> 317.2 (8.4) </td>
   <td style="text-align:left;"> 136.8 (6.5) </td>
   <td style="text-align:left;"> 10.2 (2.3) </td>
   <td style="text-align:left;"> 4.8 (1.3) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> suphelp_bin = 1 (%) </td>
   <td style="text-align:left;"> 545.0 (14.4) </td>
   <td style="text-align:left;"> 300.6 (14.4) </td>
   <td style="text-align:left;"> 77.6 (18.2) </td>
   <td style="text-align:left;"> 33.9 (11.1) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> supcares_bin = 1 (%) </td>
   <td style="text-align:left;"> 608.3 (16.2) </td>
   <td style="text-align:left;"> 297.0 (14.2) </td>
   <td style="text-align:left;"> 90.8 (21.7) </td>
   <td style="text-align:left;"> 40.3 (12.9) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> wkfreedm_bin = 1 (%) </td>
   <td style="text-align:left;"> 610.2 (16.1) </td>
   <td style="text-align:left;"> 199.7 (9.5) </td>
   <td style="text-align:left;"> 19.5 (3.8) </td>
   <td style="text-align:left;"> 5.3 (1.4) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> wkdecide_bin = 1 (%) </td>
   <td style="text-align:left;"> 1078.4 (28.4) </td>
   <td style="text-align:left;"> 231.1 (11.0) </td>
   <td style="text-align:left;"> 210.5 (40.3) </td>
   <td style="text-align:left;"> 62.3 (16.4) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> safehlth_bin = 1 (%) </td>
   <td style="text-align:left;"> 257.4 (6.8) </td>
   <td style="text-align:left;"> 110.3 (5.2) </td>
   <td style="text-align:left;"> 19.7 (3.9) </td>
   <td style="text-align:left;"> 14.1 (3.7) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> safefrst_bin = 1 (%) </td>
   <td style="text-align:left;"> 385.1 (10.3) </td>
   <td style="text-align:left;"> 188.7 (9.1) </td>
   <td style="text-align:left;"> 20.4 (4.1) </td>
   <td style="text-align:left;"> 27.0 (7.2) </td>
  </tr>
</tbody>
</table>

# Survey-weighted associations between QWL variables and class from Poisson regressions

Restricted to intrinsically relational variables with potentially interesting differences across classes or races.

## Functions


```r
#regression function
mysvy <- function(data, columns, adjvars, ...) {
  model <- lapply(as.list(columns), function(x) {
    svyglm(as.formula(paste0(names(data)[x], adjvars)), 
           data = data, family=poisson(), ...)
  })
  return(model)
}

#plot function
plotted <- function(xaxis, limitsvec, breaksvec, cols, shapes, xlabbed, nrow=1){
  xaxis <- enquo(xaxis)
  ggplot(binded, aes(x=!!xaxis, y=PR, ymin=Lower, ymax=Upper, shape=!!xaxis, color=!!xaxis)) +
    geom_hline(yintercept=1, lty=1, col='darkgrey') +
    geom_errorbar(width=0.5) +
    geom_point(size=2) +
    facet_wrap(.~V4, nrow=2) +
    scale_y_continuous(trans="log", limits=limitsvec, breaks=breaksvec, labels=scales::number_format(accuracy=0.01)) +
    theme_light() + 
    scale_color_manual(values=cols) +
    scale_shape_manual(values=shapes) +
    xlab(xlabbed) +
    ylab("Prevalence ratio") +
    theme(strip.background = element_rect(fill=NA, color='grey'), strip.text = element_text(colour = "black"), 
          legend.position='bottom', legend.title=element_blank(), axis.text.x = element_blank(), axis.ticks.x=element_blank()) +
    guides(color=guide_legend(nrow=nrow, byrow=T, label.position="bottom", keywidth=7, keyheight=0.5)) 
}
```

## Class and QWL 

Prevalence of bad category of each binary QWL variable among each class relative to the prevalence among (all, white, or male) workers adjusted for age and year with restricted cubic splines.

Excluded 'suphelp_bin' and 'supcares_bin'  for these regressions since PBs and caps shouldn't have supervisors (at least in theory).

### No gender interaction


```r
#run min. adjusted regression
less_adj <- mysvy(dat, c(125:128,131:134), "~class + rcs(age, 3) + rcs(year, 3)", design = svy_dat)

#pull into matrix
regs_less = NULL
for(i in 1:8){
  cbind(exp(less_adj[[i]]$coefficients[2:4]), exp(confint(less_adj[[i]])[2:4,])) -> bind
  rbind(regs_less, cbind(bind, names(dat)[c(125:128,131:134)][i])) -> regs_less
}

#format
as.data.frame(regs_less) %>%
  mutate(Class=rep(c("Managers", "Petite bourgeoisie", "Capitalists"), 8),
         PR=as.numeric(V1),
         Lower=as.numeric(`2.5 %`),
         Upper=as.numeric(`97.5 %`)) %>%
  bind_rows(data.frame(V4=c("disc_haras", "trustman_bin", "wkfreedm_bin", "safehlth_bin", "respect_bin", "manvsemp_bin", "wkdecide_bin", "safefrst_bin"), 
             Class=rep("Workers", 8), PR=rep(1, 8), Lower=rep(NA, 8), Upper=rep(NA, 8))) %>% #worker dummy row
  mutate(Class=factor(Class, levels=c("Workers", "Managers", "Petite bourgeoisie", "Capitalists")),
         V4=factor(V4, levels=c("disc_haras", "trustman_bin", "wkfreedm_bin", "safehlth_bin", 
                                "respect_bin", "manvsemp_bin", "wkdecide_bin", "safefrst_bin"))) -> binded

#plot
plotted(xaxis=Class, limitsvec=c(0.041, 1.81), breaksvec=c(0.125, 0.25, 0.5, 1, 2), 
        cols=brewer.pal(8, "Blues")[c(5,6,7,8)], 
        shapes=c(15,16,17,18), xlabbed="Class (ref: workers)")
```

![](analysis_12_8_20_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

### Gender interaction


```r
#run min. adjusted regression
less_adj <- mysvy(dat, c(125:128,131:134), "~class_gender + rcs(age, 3) + rcs(year, 3)", design = svy_dat)

#pull into matrix
regs_less_gender = NULL
for(i in 1:8){
  cbind(exp(less_adj[[i]]$coefficients[2:8]), exp(confint(less_adj[[i]])[2:8,])) -> bind
  rbind(regs_less_gender, cbind(bind, names(dat)[c(125:128,131:134)][i])) -> regs_less_gender
}

#format
as.data.frame(regs_less_gender) %>%
  mutate(Class=rep(c("Male managers", "Male petite bourgeoisie", "Male capitalists", "Female workers", "Female managers", "Female petite bourgeoisie", "Female capitalists"), 8),
         PR=as.numeric(V1),
         Lower=as.numeric(`2.5 %`),
         Upper=as.numeric(`97.5 %`)) %>%
  bind_rows(data.frame(V4=c("disc_haras", "trustman_bin", "wkfreedm_bin", "safehlth_bin", "respect_bin", "manvsemp_bin", "wkdecide_bin", "safefrst_bin"), 
             Class=rep("Male workers", 8), PR=rep(1, 8), Lower=rep(NA, 8), Upper=rep(NA, 8))) %>% #worker dummy row
  mutate(Class=factor(Class, levels=c("Male workers", "Male managers", "Male petite bourgeoisie", "Male capitalists",
                                      "Female workers", "Female managers", "Female petite bourgeoisie", "Female capitalists")),
         V4=factor(V4, levels=c("disc_haras", "trustman_bin", "wkfreedm_bin", "safehlth_bin", 
                                "respect_bin", "manvsemp_bin", "wkdecide_bin", "safefrst_bin"))) -> binded

#plot
plotted(xaxis=Class, limitsvec=c(0.017, 2.1), breaksvec=c(0.125, 0.25, 0.5, 1, 2), 
        cols=c(brewer.pal(8, "Blues")[c(5,6,7,8)], brewer.pal(8, "Purples")[c(5,6,7,8)]), 
        shapes=c(15,16,17,18,0,1,2,5), xlabbed="Class & gender (ref: male workers)", nrow=2)
```

![](analysis_12_8_20_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

### Race interaction (white/POC)

Cell sizes too small among Black capitalists to divide up race variable even further. 


```r
#run min. adjusted regression
less_adj <- mysvy(dat, c(125:128,131:134), "~class_poc + rcs(age, 3) + rcs(year, 3)", design = svy_dat)

#pull intro matrix
regs_less_poc = NULL
for(i in 1:8){
  cbind(exp(less_adj[[i]]$coefficients[2:8]), exp(confint(less_adj[[i]])[2:8,])) -> bind
  rbind(regs_less_poc, cbind(bind, names(dat)[c(125:128,131:134)][i])) -> regs_less_poc
}

#format
as.data.frame(regs_less_poc) %>%
  mutate(Class=rep(c("White managers", "White petite bourgeoisie", "White capitalists", "POC workers", "POC managers", "POC petite bourgeoisie", "POC capitalists"), 8),
         PR=as.numeric(V1),
         Lower=as.numeric(`2.5 %`),
         Upper=as.numeric(`97.5 %`)) %>%
  bind_rows(data.frame(V4=c("disc_haras", "trustman_bin", "wkfreedm_bin", "safehlth_bin", "respect_bin", "manvsemp_bin", "wkdecide_bin", "safefrst_bin"), 
             Class=rep("White workers", 8), PR=rep(1, 8), Lower=rep(NA, 8), Upper=rep(NA, 8))) %>% #worker dummy row
  mutate(Class=factor(Class, levels=c("White workers", "White managers", "White petite bourgeoisie", "White capitalists",
                                      "POC workers", "POC managers", "POC petite bourgeoisie", "POC capitalists")),
         V4=factor(V4, levels=c("disc_haras", "trustman_bin", "suphelp_bin", "wkfreedm_bin", "safehlth_bin",
                                "respect_bin", "manvsemp_bin", "supcares_bin", "wkdecide_bin", "safefrst_bin"))) -> binded

#plot
plotted(xaxis=Class, limitsvec=c(0.0165, 3.35), breaksvec=c(0.125, 0.25, 0.5, 1, 2), 
        cols=c(brewer.pal(8, "Blues")[c(5,6,7,8)], brewer.pal(8, "Purples")[c(5,6,7,8)]),
        shapes=c(15,16,17,18,0,1,2,5), xlabbed="Class & race (ref: white workers)", nrow=2)
```

![](analysis_12_8_20_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

## Race and QWL among workers

Prevalence of bad category of each binary QWL variable among Black or "other" workers relative to the prevalence among (all or male) white workers adjusted for age and year with restricted cubic splines.

### No gender interaction


```r
#less adjusted
less_adj <- mysvy(dat, 125:134, "~race + rcs(age, 3) + rcs(year, 3)", design = subset(svy_dat, class=="Workers"))

regs_less = NULL
for(i in 1:10){
  cbind(exp(less_adj[[i]]$coefficients[2:3]), exp(confint(less_adj[[i]])[2:3,])) -> bind
  rbind(regs_less, cbind(bind, names(dat)[125:134][i])) -> regs_less
}
  
#format
as.data.frame(regs_less) %>%
  mutate(Race=rep(c("Black", "Other"), 10),
         PR=as.numeric(V1),
         Lower=as.numeric(`2.5 %`),
         Upper=as.numeric(`97.5 %`)) %>%
  bind_rows(data.frame(V4=c("disc_haras", "trustman_bin", "suphelp_bin", "wkfreedm_bin", "safehlth_bin", "respect_bin", "manvsemp_bin", "supcares_bin", "wkdecide_bin", "safefrst_bin"), 
             Race=rep("White", 10), PR=rep(1, 10), Lower=rep(NA, 10), Upper=rep(NA, 10))) %>% #white dummy row
  mutate(Race=factor(Race, levels=c("White", "Black", "Other")),
         V4=factor(V4, levels=c("disc_haras", "trustman_bin", "suphelp_bin", "wkfreedm_bin", "safehlth_bin",
                                "respect_bin", "manvsemp_bin", "supcares_bin", "wkdecide_bin", "safefrst_bin"))) -> binded


#plot
plotted(xaxis=Race, limitsvec=c(0.3, 2.5), breaksvec=c(0.6, 1, 1.666667, 1.666667^2),
        cols=c(brewer.pal(8, "Blues")[c(6)], brewer.pal(8, "Purples")[c(6)], brewer.pal(8, "Greens")[c(6)]), 
        shapes=c(15, 0, 12), xlabbed="Race (ref: white)")
```

![](analysis_12_8_20_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

### Gender interaction


```r
#less adjusted
less_adj <- mysvy(dat, 125:134, "~race_gender + rcs(age, 3) + rcs(year, 3)", design = subset(svy_dat, class=="Workers"))

regs_less = NULL
for(i in 1:10){
  cbind(exp(less_adj[[i]]$coefficients[2:6]), exp(confint(less_adj[[i]])[2:6,])) -> bind
  rbind(regs_less, cbind(bind, names(dat)[125:134][i])) -> regs_less
}
  
#format
regs_less %>%
  as.data.frame() %>%
  mutate(race_gender=rep(c("White female", "Black male", "Black female", "Other male", "Other female"), 10),
         PR=as.numeric(V1),
         Lower=as.numeric(`2.5 %`),
         Upper=as.numeric(`97.5 %`)) %>%
  bind_rows(data.frame(V4=c("disc_haras", "trustman_bin", "suphelp_bin", "wkfreedm_bin", "safehlth_bin", "respect_bin", "manvsemp_bin", "supcares_bin", "wkdecide_bin", "safefrst_bin"), 
             race_gender=rep("White male", 10), PR=rep(1, 10), Lower=rep(NA, 10), Upper=rep(NA, 10))) %>% #white dummy row
  mutate(race_gender=factor(race_gender, levels=c("White male", "White female", "Black male", "Black female", "Other male", "Other female")),
         V4=factor(V4, levels=c("disc_haras", "trustman_bin", "suphelp_bin", "wkfreedm_bin", "safehlth_bin",
                                "respect_bin", "manvsemp_bin", "supcares_bin", "wkdecide_bin", "safefrst_bin"))) -> binded

#plot
plotted(xaxis=race_gender, limitsvec=c(0.1, 3.1), breaksvec=c(0.125, 0.25, 0.5, 1, 2), 
        cols=c(brewer.pal(8, "Blues")[c(6,8)], brewer.pal(8, "Purples")[c(6,8)], brewer.pal(8, "Greens")[c(6,8)]), 
        shapes=c(15, 16, 0, 1, 12, 10), xlabbed="Race & gender (ref: white male)", nrow=1)
```

![](analysis_12_8_20_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

# Appendix

## How many employees do PBs and capitalists typically identify as having? 

Note that the numemps variable wasn't asked in 2002, which is why we aren't using it for our primary analyses. Note also that numemps was only asked of people who identified as self-employed, which further supports our use of the variable to identify owners.


```r
dat %>%
  filter(year !=2002 & c(class=="Capitalists" | class=="Petit bourgeoisie")) %>%
  group_by(class) %>%
  summarise(mean_emps=round(mean(as.numeric(numemps), na.rm=T),1), 
            median_emps=round(median(as.numeric(numemps), na.rm=T),1),
            lower_IQR=round(quantile(as.numeric(numemps), 0.25, na.rm=T),1),
            upper_IQR=round(quantile(as.numeric(numemps), 0.75, na.rm=T),1))
```

```
## # A tibble: 2 x 5
##   class             mean_emps median_emps lower_IQR upper_IQR
##   <fct>                 <dbl>       <dbl>     <dbl>     <dbl>
## 1 Petit bourgeoisie       0.2           0         0         0
## 2 Capitalists             5.9           2         0         6
```