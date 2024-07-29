###################################################################################################################
#### PSA for survey periodicity and prioritization
####
###################################################################################################################

####clear environment
rm(list=ls())

#load packages
library(tidyverse)
library(ggrepel)

####set wds
#wd <- getwd() 
wd<- "C:/Users/Derek.Bolser/Documents/"
data <- file.path(wd, "Survey-PSA/Data")
results<- file.path(wd, "Survey-PSA/Results")

#read in and edit species list
setwd(data)
alldata<-read.csv("raw_input_PSA_data_including_inverts.csv")
surveys<- read.csv('surveys_assessments_sis.csv')
surveys_2<- read.csv('surveys_assessments_sis_secondary.csv')

##### Get productivity and suceptability scores; low values are low productivity/high suceptability. Following Patrick et al. 2009 tech memo where possible: #######################
#Patrick, W. S., P. Spencer, O. Ormseth, J. Cope, J. Field, D. Kobayashi, T. Gedamke, E. Cort?s, K. Bigelow, W. Overholtz,
#J. Link, and P. Lawson. 2009. Use of productivity and susceptibility indices to determine stock vulnerability,
#with example applications to six U.S. fisheries. U.S. Dep. Commer., NOAA Tech. Memo. NMFS-F/SPO-101, 90 p. 
scores<-alldata[,c(1:8,10:14,16:21,23,24,28,38,40:45)] #removing model_category for now because it is the same type of info as assessment level, although specific values differ

#take the mean of CVA variables that were from different regional databases despite the region being listed as the same
scores<-scores%>%
  group_by(scientific_name,common_name, stock_name, stock_area, overfishing,overfished)%>%
  select(-Functional_group)%>%
  summarise_all(mean,na.rm = T)

#take NaNs and make NAs
NaN_replace <- function(x) {
  if (is.numeric(x)) {
    x[is.nan(x)] <- NA
  }
  return(x)
}

# Apply the function to each column in the data frame
scores <- scores %>%
  mutate(across(everything(), NaN_replace))

####productivity scores
#Linf
scores<-scores %>%
  mutate(Loo = case_when(Loo < 60 ~ 1, 
                           Loo > 60 & Loo < 150 ~ 2, 
                           Loo > 150 ~ 3))
#K
scores<-scores %>%
  mutate(K = case_when(K < 0.15 ~ 1, 
                         K > 0.15 & K < 0.25 ~ 2, 
                         K > 0.25 ~ 3))
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

#natural mortality
scores<-scores %>%
  mutate(M = case_when(M < 0.2 ~ 1, 
                         M > 0.2 & M < 0.4 ~ 2, 
                         M > 0.4 ~ 3))

#steepness; highly correlated M; excluding for now
#scores<-scores %>%
#  mutate(h = case_when(h < 0.5 ~ 1, 
#                       h > 0.5 & h < 0.7 ~ 2, 
#                       h > 0.7 ~ 3))


#maximum annual spawners per spawner; arbitrary cutoff based on data distribution
scores<-scores %>%
  mutate(MASPS = case_when(MASPS < 1 ~ 1, 
                       MASPS > 1 & MASPS < 5 ~ 2, 
                       MASPS > 5 ~ 3))


#recruitment SD
scores<-scores %>%
  mutate(margsd = case_when(margsd < 0.4 ~ 3, 
                       margsd > 0.4 & margsd < 0.6 ~ 2, 
                       margsd > 0.6 ~ 1))


#population growth rate
scores<-scores %>%
  mutate(r = case_when(r < 0.16 ~ 1, 
                         r > 0.16 & r < 0.5 ~ 2, 
                         r > 0.5 ~ 3))

#sensitivity (1-4), low - medium - high - very high
scores<-scores %>%
  mutate(Sensitivity = case_when(Sensitivity >= 3 ~ 1, 
                                 Sensitivity < 3 & Sensitivity >=2 ~ 2, 
                                 Sensitivity < 2 & Sensitivity > 1 ~ 3)) #this is fine because no values are exactly 1 naturally

#prey specificity (1-4), low - medium - high - very high
scores<-scores %>%
  mutate(Prey.Specificity = case_when(Prey.Specificity >= 3 ~ 1, 
                                      Prey.Specificity < 3 & Prey.Specificity >=2 ~ 2, 
                                      Prey.Specificity < 2 & Prey.Specificity > 1 ~ 3)) #this is fine because no values are exactly 1 naturally

#habitat specificity (1-4), low - medium - high - very high
scores<-scores %>%
  mutate(Habitat.Specificity= case_when(Habitat.Specificity >= 3 ~ 1, 
                                        Habitat.Specificity < 3 & Habitat.Specificity >=2 ~ 2, 
                                        Habitat.Specificity < 2 & Habitat.Specificity > 1 ~ 3)) #this is fine because no values are exactly 1 naturally

#complexity in reproductive strategy (1-4), low - medium - high - very high
scores<-scores %>%
  mutate(Complexity.in.Reproductive.Strategy = case_when(Complexity.in.Reproductive.Strategy >= 3 ~ 1, 
                                                         Complexity.in.Reproductive.Strategy < 3 & Complexity.in.Reproductive.Strategy >=2 ~ 2, 
                                                         Complexity.in.Reproductive.Strategy < 2 & Complexity.in.Reproductive.Strategy > 1 ~ 3)) #this is fine because no values are exactly 1 naturally

####susceptibility scores ################################################################################################################################
#score recruitment autocorrelation based on histogram (normally distributed around 0.6)
scores<-scores %>%
  mutate(rho = case_when(rho < 0.5 ~ 1, 
                       rho > 0.5 & rho < 0.7 ~ 2, 
                       rho > 0.7 ~ 3))

#Fmsy/M
scores<-scores %>%
  mutate(Fmsy_over_M = case_when(Fmsy_over_M < 0.5 ~ 3, 
                         Fmsy_over_M > 0.5 & Fmsy_over_M < 1 ~ 2, 
                         Fmsy_over_M > 1 ~ 1))


#scale model category
scores<-scores %>%
  mutate(assessment_level = case_when(assessment_level <= 2 ~ 1, 
                                      assessment_level > 2 & assessment_level <= 4 ~ 2, 
                                      assessment_level > 4 ~ 3)) #not many 3s
#scale status
scores$status<-NA

scores<-scores %>%
  mutate(status = case_when(overfishing == 'Yes' & overfished == 'Yes' ~ 1, 
                            overfishing == 'Unknown' & overfished == 'Yes' ~ 1, 
                            overfishing == 'Yes' & overfished == 'Unknown' ~ 1,
                            overfishing == 'No' & overfished == 'Yes' ~ 1,
                            overfishing == 'Yes' & overfished == 'No' ~ 1,
                            overfishing == 'Unknown' & overfished == 'Unknown' ~ 2,
                            overfishing == 'No' & overfished == 'Unknown' ~ 2,
                            overfishing == 'Unknown' & overfished == 'No' ~ 2,
                            is.na(overfishing) & is.na(overfished) ~ 2,
                            overfishing == 'No' & overfished == 'No' ~ 3,
                            overfishing == 'No' & overfished == 'No - Rebuilding' ~ 3))
                            
#Exposure (1-4), low - medium - high - very high
scores<-scores %>%
  mutate(Exposure = case_when(Exposure >= 3 ~ 1, 
                              Exposure < 3 & Exposure >=2 ~ 2, 
                              Exposure < 2 & Exposure > 1 ~ 3)) #this is fine because no values are exactly 1 naturally

#last assessment (2008 - 2023)
scores<-scores %>%
  mutate(last_assessment = case_when(last_assessment < 2015 ~ 1, 
                                     last_assessment >= 2015 & last_assessment < 2020 ~ 2, 
                                     last_assessment >= 2020 ~ 3))

#comp gap (0-4)
scores<-scores %>%
  mutate(comp_gap = case_when(comp_gap >= 3 ~ 10, #necessary to avoid sequential reassignment
                              comp_gap < 3 & comp_gap > 1 ~ 2, 
                              comp_gap <= 1 ~ 3))

scores$comp_gap[scores$comp_gap == 10]<- 1

#abundance gap (0-3)
scores<-scores %>%
  mutate(abundance_gap = case_when(abundance_gap >= 3 ~ 10, #necessary to avoid sequential reassignment
                                   abundance_gap < 3 & abundance_gap > 1 ~ 2, 
                                   abundance_gap <= 1 ~ 3))

scores$abundance_gap[scores$abundance_gap == 10]<- 1

#LH gap (0-3)
scores<-scores %>%
  mutate(LH_gap = case_when(LH_gap >= 3 ~ 10, #necessary to avoid sequential reassignment
                            LH_gap < 3 & LH_gap > 1 ~ 2, 
                            LH_gap <= 1 ~ 3))

scores$LH_gap[scores$LH_gap == 10]<- 1

#ecosystem gap (0-3)
scores<-scores %>%
  mutate(eco_gap = case_when(eco_gap >= 3 ~ 10, #necessary to avoid sequential reassignment
                             eco_gap < 3 & eco_gap > 1 ~ 2, 
                             eco_gap <= 1 ~ 3))

scores$eco_gap[scores$eco_gap == 10]<- 1

#### assign a value of 2 (midpoint) for missing data ##################################################################################
scores[is.na(scores)]<-2

#### copy columns to double their weight ##############################################################################################
#productivity
scores$tmax2<-scores$tmax
scores$margsd2<-scores$margsd

#susceptibility
scores$rho2<-scores$rho
scores$Fmsy_over_M2<-scores$Fmsy_over_M
scores$status2<-scores$status

###### average productivity scores ###################################################################################################
scores$avg_p_score<-apply(scores[,c(14:16, 18:20, 23:28, 30,31)],1, mean, na.rm = T)

#average susceptibility scores; FIX THE INDEXING #####################################################################################
scores$avg_s_score<-apply(scores[,c(7:13, 17, 21, 22, 29, 32, 34)],1,mean, na.rm = T)

##### avg score overall
scores$score<-apply(scores[,c(35,36)],1,mean)

#### write csv #########################################################################################
setwd(results)
#write.csv(scores, "scores_preliminary_extended_variables_including_inverts.csv", row.names = F)

##### plot ############################################################################################
#with scores; not much separation on x-axis
ggplot(scores, aes(x= avg_p_score, y= avg_s_score,label = stock_name)) + 
  geom_hline(yintercept=2, linetype="dashed", color = "red") +
  geom_vline(xintercept=2, linetype="dashed", color = "red")+
  geom_point(stat = "identity") + 
  theme_minimal() + labs(x = "Productivity", y = "Susceptibility") +
 geom_label_repel(aes(x = avg_p_score, 
                     y = avg_s_score, 
                      label = stock_name), 
                  max.overlaps = 200, angle = 45, hjust = 0.6, size = 1.5)

setwd(data)
ggsave(filename = 'PSA_NMFS_fish_spp_stock_name_FishLife_inverts_expanded_variables.png',plot = last_plot() , path = data, width = 18, height = 9, device = 'png', dpi = 300)
# not producing a png...

####group by quadrant
#export list of stocks by quadrant
scores_red<- scores[,c(1,3,35:37)]

#write csv of reduced scores DF
setwd(results)
#write.csv(scores_red, "avg_scores_df_including_inverts_expanded_variables.csv", row.names = F)

#back to quadrants
Q1<- as.data.frame(scores_red[scores_red$avg_p_score < 2 & scores_red$avg_s_score < 2,])
Q2<- as.data.frame(scores_red[scores_red$avg_p_score >= 2 & scores_red$avg_s_score < 2,])
Q3<- as.data.frame(scores_red[scores_red$avg_p_score < 2 & scores_red$avg_s_score >=2,])
Q4<- as.data.frame(scores_red[scores_red$avg_p_score >= 2 & scores_red$avg_s_score >=2,])

Q1[142:214,]<-NA
Q3[90:214,]<-NA
Q4[110:214,]<-NA

colnames(Q1)<-c('Q1','stock_name', 'p_score','s_score','score')
colnames(Q2)<-c('Q2','stock_name', 'p_score','s_score','score')
colnames(Q3)<-c('Q3','stock_name', 'p_score','s_score','score')
colnames(Q4)<-c('Q4','stock_name', 'p_score','s_score','score')

quadrants<-cbind.data.frame(c(Q1,Q2,Q3,Q4))

quadrants<-quadrants[,c('Q1','Q2','Q3','Q4')]

setwd(results)
#write.csv(quadrants, "fish_species_by_quadrant_PSA.csv", row.names = F)

#now for species
colnames(Q1)<-c('species_name','Q1', 'p_score','s_score','score')
colnames(Q2)<-c('species_name','Q2', 'p_score','s_score','score')
colnames(Q3)<-c('species_name','Q3', 'p_score','s_score','score')
colnames(Q4)<-c('species_name','Q4', 'p_score','s_score','score')

quadrants<-cbind.data.frame(c(Q1,Q2,Q3,Q4))

quadrants<-quadrants[,c('Q1','Q2','Q3','Q4')]

setwd(results)
#write.csv(quadrants, "fish_stocks_by_quadrant_PSA.csv", row.names = F)

##### link spp to their survey #########################################################################
#exclude non-FINSS surveys
surveys<-surveys[surveys[,2] == "FINSS Survey",] ####revist this to include F-D and state surveys. 
surveys_2<-surveys_2[surveys_2[,2] == "FINSS Survey",]

#fix formatting and merge primary and secondary
surveys<-surveys[,3:4]
colnames(surveys)<-c('survey','stock_name')

surveys_2<-surveys_2[,3:4]
colnames(surveys_2)<-c('survey','stock_name')

surveys<-rbind(surveys,surveys_2)

scores_survey<- left_join(scores,surveys,by = 'stock_name')

#summarize scores by survey; many NAs... go back here to check for misaligned stock names #############################################################################
scores_survey<-scores_survey[!is.na(scores_survey$survey),] 

str(scores_survey)

survey_grouped_scores<-scores_survey%>%
  group_by(survey)%>%
  summarise(p_score = mean(avg_p_score), s_score = mean(avg_s_score), n = n())

#classify n
survey_grouped_scores$spp_number<- survey_grouped_scores$n

survey_grouped_scores$spp_number[survey_grouped_scores$spp_number > 2]<-'3+'

#plot ######################################################################################################
#color by n in the future
ggplot(survey_grouped_scores, aes(x= p_score, y= s_score,label = survey)) + 
  geom_hline(yintercept=2, linetype="dashed", color = "red") +
  geom_vline(xintercept=2, linetype="dashed", color = "red")+
  geom_point(stat = "identity") + 
  theme_minimal() + labs(x = "Productivity", y = "Susceptibility") +
  geom_label_repel(aes(x = p_score, 
                       y = s_score, 
                       label = survey), 
                   max.overlaps = 200, angle = 45, hjust = 0.6, size = 1.5) +
  annotate(geom="text", x=1.25, y=1.15, label="Q1") + 
  annotate(geom="text", x=2.5, y=1.15, label="Q2") + 
  annotate(geom="text", x=1.25, y=2.5, label="Q3") + 
  annotate(geom="text", x=2.5, y=2.5, label="Q4")

setwd(data)
ggsave(filename = 'PSA_surveys_FishLife_inverts_extended_variables.png',plot = last_plot() , path = data, width = 18, height = 9, device = 'tiff', dpi = 300)

#get an average score
survey_grouped_scores$score<-apply(survey_grouped_scores[,c(2,3)],1,mean)

#write csv of survey scores
setwd(results)
#write.csv(survey_grouped_scores, "survey_grouped_PSA_scores_inverts_extended_variables.csv", row.names = F)

#export list of surveys by quadrant; some getting excluded here...
Q1<- as.data.frame(survey_grouped_scores[survey_grouped_scores$p_score < 2 & survey_grouped_scores$s_score < 2,])
Q2<- as.data.frame(survey_grouped_scores[survey_grouped_scores$p_score >= 2 & survey_grouped_scores$s_score < 2,])
Q3<- as.data.frame(survey_grouped_scores[survey_grouped_scores$p_score < 2 & survey_grouped_scores$s_score >=2,])
Q4<- as.data.frame(survey_grouped_scores[survey_grouped_scores$p_score >= 2 & survey_grouped_scores$s_score >=2,])

Q1[20:30,]<-NA
Q2[13:30,]<-NA
Q3[16:30,]<-NA

colnames(Q1)<-c('Q1','p_score','s_score','n','spp_num', 'score')
colnames(Q2)<-c('Q2','p_score','s_score','n','spp_num', 'score')
colnames(Q3)<-c('Q3','p_score','s_score','n','spp_num', 'score')
colnames(Q4)<-c('Q4','p_score','s_score','n','spp_num', 'score')

quadrants<-cbind.data.frame(c(Q1,Q2,Q3,Q4))

quadrants<-quadrants[,c('Q1','Q2','Q3','Q4')]

setwd(results)
#write.csv(quadrants, "surveys_by_quadrant_PSA_inverts_expanded_variables.csv", row.names = F)
