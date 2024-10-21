###############################################################################################################################
#### match fy25 1-N by survey name in PSA
####
################################################################################################################################

####clear environment
rm(list=ls())

#### set wds
wd<- "C:/Users/Derek.Bolser/Documents/"
data <- file.path(wd, "Survey-PSA/Data")
results <- file.path(wd, "Survey-PSA/Results")

#### load packages
library(tidyverse)

#### read in and combine datasheets
setwd(data)
fap<- read.csv("FY25 1-N Lists - All Rankings.csv")

setwd(results)
scores<- read.csv("survey_grouped_PSA_scores_extended_variables.csv")

#fix survey column in the fap
fap<-data.frame(lapply(fap, function(x) iconv(x, from = "latin1", to = "UTF-8", sub = " ")))

#join
alldata<-left_join(fap,scores,by = "survey")

#write new csv
setwd(results)
write.csv(alldata,"FY25 FAP plus scores.csv",row.names = F)
