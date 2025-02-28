###################################################################################################################
#### Get PSA scores
####
###################################################################################################################

####clear environment
rm(list=ls())

#get dev version of rphylopic
#remotes::install_github("palaeoverse/rphylopic")

#load packages
library(ggplot2)
library(tidyverse)
library(ggrepel)
library(rphylopic)
library(taxize)
library(fishualize)
library(ggimage)
library(httr)
library(png)
library(grid)

####set wds
#wd <- getwd() 
wd<- "C:/Users/Derek.Bolser/Documents"
data <- file.path(wd, "Survey-PSA/Data")
results<- file.path(wd, "Survey-PSA/Results")

#read in and edit species list
setwd(data)
alldata<-read.csv("raw_input_PSA_data_updated.csv")

##### Get productivity and suceptability scores; low values are low productivity/high suceptability. Following Patrick et al. 2009 tech memo where possible: #######################
#Patrick, W. S., P. Spencer, O. Ormseth, J. Cope, J. Field, D. Kobayashi, T. Gedamke, E. Cort?s, K. Bigelow, W. Overholtz,
#J. Link, and P. Lawson. 2009. Use of productivity and susceptibility indices to determine stock vulnerability,
#with example applications to six U.S. fisheries. U.S. Dep. Commer., NOAA Tech. Memo. NMFS-F/SPO-101, 90 p. 
scores<-alldata[,c(1:7,11,13,19,20,22:28,40:43,46:48,55,60,63)] 

####productivity scores ##################################################################################################################################################################
#Linf
scores<-scores %>%
  mutate(Loo = case_when(Loo < 60 ~ 3, 
                         Loo > 60 & Loo < 150 ~ 2, 
                         Loo > 150 ~ 1))

#max age
scores<-scores %>%
  mutate(tmax = case_when(tmax < 10 ~ 3, 
                          tmax > 10 & tmax < 30 ~ 2, 
                          tmax > 30 ~ 1))
#age at maturity
scores<-scores %>%
  mutate(tm = case_when(tm < 2 ~ 3, 
                        tm > 2 & tm < 4 ~ 2, 
                        tm > 4 ~ 1))

#natural mortality -- high NM = low vulnerability?? Yes, I think
scores<-scores %>%
  mutate(M = case_when(M > 0.4 ~ 3,
                       M > 0.2 & M < 0.4 ~ 2, 
                       M < 0.2 ~ 1))

#steepness
scores<-scores %>%
  mutate(h = case_when(h > 0.7 ~ 3,
                       h > 0.5 & h < 0.7 ~ 2, 
                       h < 0.5 ~ 1))


#population growth rate
scores<-scores %>%
  mutate(r = case_when(r > 0.5 ~ 3,
                       r > 0.16 & r < 0.5 ~ 2,
                       r < 0.16 ~ 1 ))

#sensitivity (1-4), low - medium - high - very high
scores<-scores %>%
  mutate(Sensitivity = case_when(Sensitivity >= 3 ~ 1, 
                                 Sensitivity < 3 & Sensitivity >=2 ~ 2, 
                                 Sensitivity < 2 & Sensitivity > 1 ~ 3)) 


####susceptibility (risk) scores ################################################################################################################################
# 3 = high risk, 1 = low

#Fmsy/M
scores<-scores %>%
  mutate(Fmsy_over_M = case_when(Fmsy_over_M > 1 ~ 3,
                                 Fmsy_over_M > 0.5 & Fmsy_over_M < 1 ~ 2, 
                                 Fmsy_over_M < 0.5 ~ 1))


#scale assessment level
scores<-scores %>%
  mutate(Assessment.Level_scored = case_when(Assessment.Level >= 4 ~ 1, 
                                             Assessment.Level == 3 ~ 2, 
                                             Assessment.Level <= 2 & Assessment.Level > 1 ~ 3)) 
#scale model category
scores<-scores %>%
  mutate(Model.Category_scored = case_when(Model.Category <= 2 ~ 3, 
                                           Model.Category > 2 & Model.Category <= 4 ~ 2, 
                                           Model.Category > 4 ~ 1)) 

#combine assessment level and model category; fill in NA model categories with assessment level
scores$Model.Category_scored[is.na(scores$Model.Category_scored)] <- scores$Assessment.Level_scored[is.na(scores$Model.Category_scored)]

#scale status
scores<-scores %>%
  mutate(status = case_when(overfishing == 'Yes' & overfished == 'Yes' ~ 3, 
                            overfishing == 'Unknown' & overfished == 'Yes' ~ 3, 
                            overfishing == 'Yes' & overfished == 'Unknown' ~ 3,
                            overfishing == 'No' & overfished == 'Yes' ~ 3,
                            overfishing == 'Yes' & overfished == 'No' ~ 3,
                            overfishing == 'Unknown' & overfished == 'Unknown' ~ NA,
                            overfishing == 'No' & overfished == 'Unknown' ~ 2,
                            overfishing == 'Unknown' & overfished == 'No' ~ 2,
                            is.na(overfishing) & is.na(overfished) ~ 2,
                            overfishing == 'No' & overfished == 'No' ~ 1,
                            overfishing == 'No' & overfished == 'No - Rebuilding' ~ 1))

#Exposure (1-4), low - medium - high - very high
scores<-scores %>%
  mutate(Exposure = case_when(Exposure >= 3 ~ 3, 
                              Exposure < 3 & Exposure >=2 ~ 2, 
                              Exposure < 2 ~ 1))

#stock assessment gaps; how?
scores<-scores %>%
  mutate(scored_gap = case_when(Mean.Gap < 2 ~ 1,
                                Mean.Gap < 3 & Mean.Gap >=2 ~ 2, 
                                Mean.Gap >= 3 ~ 3)) 


###### average productivity scores ###################################################################################################
scores$avg_p_score<-apply(scores[,c(21:27)],1, mean, na.rm = T)

#average susceptibility scores #####################################################################################
scores$avg_s_score<-apply(scores[,c(20,28,30:32)],1,mean, na.rm = T)

#### Euclidian distance; the vulnerability metric
# Target point
target <- c(3, 1)

# Calculate Euclidean distance
scores$distance <- sqrt((scores$avg_p_score - target[1])^2 + (scores$avg_s_score - target[2])^2)

#### write csv #########################################################################################
#examine duplicates
duplicated_rows <- scores[duplicated(scores) | duplicated(scores, fromLast = TRUE), ]

#exclude them
scores<-unique(scores)

setwd(results)
#write.csv(scores, "scores_updated.csv", row.names = F)
