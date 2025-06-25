#############################################################################################################
####### reformat sheets to make final MS tables
#######
#############################################################################################################

####clear environment
rm(list=ls())

#get dev version of rphylopic
#remotes::install_github("palaeoverse/rphylopic")

#load packages
library(tidyverse)

#define wds
####set wds
#wd <- getwd() 
wd<- "C:/Users/Derek.Bolser/Documents"
data <- file.path(wd, "Survey-PSA/Data")
results<- file.path(wd, "Survey-PSA/Results")
tables<- file.path(wd, "Survey-PSA/Tables")

#read in data
setwd(results)
all_ids<- read.csv("PSA_data_with_uuids_updated_HMS_salmon_excluded.csv")
ggs<- read.csv("genus_grouped_vulnerability_scores_updated_HMS_salmon_excluded.csv")
fgs<-read.csv("family_grouped_vulnerability_scores_updated_HMS_salmon_excluded.csv") 
survey_grouped_scores<- read.csv("survey_grouped_scores_with_type_and_region_updated_HMS_salmon_excluded_other_surveys_included.csv")
no_survey_grouped_scores<-read.csv("unsurveyed_by_region_summary_updated_HMS_salmon_excluded_other_surveys_included.csv")
region_grouped_scores<- read.csv("surveyed_stocks_regional_summary_updated_HMS_salmon_excluded_other_surveys_included.csv")
scores_survey_type_region<- read.csv("survey_type_and_region_summary_updated_HMS_salmon_excluded_other_surveys_included.csv")
scores_survey_type<- read.csv("survey_type_summary_updated_HMS_salmon_excluded_other_surveys_included.csv")

####reformat main text tables #######################################################################################################################
#### Family ###########################################################################################################################################
#order first
fgs <- fgs %>% arrange(desc(distance_mean))

#format
fgs$`Vulnerability score`<- paste0(round(fgs$distance_mean,2), " +/- ", round(fgs$distance_se,2))
fgs$`Productivity score`<- paste0(round(fgs$avg_p_score_mean,2), " +/- ", round(fgs$avg_p_score_se,2))
fgs$`Risk score`<- paste0(round(fgs$avg_s_score_mean,2), " +/- ", round(fgs$avg_s_score_se,2))

#only keep needed columns
fgs<-fgs[,c(1,9:14)]

#replace NA SEMs
fgs[] <- lapply(fgs, function(x) gsub(" \\+/- NA", "", x))

#rename first few columns
colnames(fgs)<-c("Family", "n stocks", "n species", "n genera", "Vulnerability score", "Productivity score", "Susceptibility score")

#write csv
setwd(tables)
#write.csv(fgs,"family_score_table.csv",row.names = F)

#### type-region #######################################################################################################################################
#calculate weighted score
scores_survey_type_region$`n-weighted vulnerability score`<-round(scores_survey_type_region$distance_mean+(0.01*scores_survey_type_region$n_stocks),2)

#order
scores_survey_type_region <- scores_survey_type_region %>% arrange(desc(`n-weighted vulnerability score`))

#format
scores_survey_type_region$`Vulnerability score`<- paste0(round(scores_survey_type_region$distance_mean,2), " +/- ", round(scores_survey_type_region$distance_se,2))
scores_survey_type_region$`Productivity score`<- paste0(round(scores_survey_type_region$avg_p_score_mean,2), " +/- ", round(scores_survey_type_region$avg_p_score_se,2))
scores_survey_type_region$`Risk score`<- paste0(round(scores_survey_type_region$avg_s_score_mean,2), " +/- ", round(scores_survey_type_region$avg_s_score_se,2))

#only keep needed columns
scores_survey_type_region<-scores_survey_type_region[,c(1:4,11:14)]

#replace NA SEMs
scores_survey_type_region[] <- lapply(scores_survey_type_region, function(x) gsub(" \\+/- NA", "", x))

#rename first few columns
colnames(scores_survey_type_region)<-c("Area and method", "n stocks", "n species", "n surveys", "n stock-weighted vulnerability score",
                                       "Vulnerability score", "Productivity score", "Susceptibility score")

#write csv
setwd(tables)
#write.csv(scores_survey_type_region,"type_region_score_table.csv",row.names = F)

#### unsurveyed by jurisdiction #########################################################################################################################
no_survey_grouped_scores$`n-weighted vulnerability score`<-round(no_survey_grouped_scores$distance_mean+(0.01*no_survey_grouped_scores$n_stocks),2)

#order
no_survey_grouped_scores <- no_survey_grouped_scores %>% arrange(desc(`n-weighted vulnerability score`))

#format
no_survey_grouped_scores$`Vulnerability score`<- paste0(round(no_survey_grouped_scores$distance_mean,2), " +/- ", round(no_survey_grouped_scores$distance_se,2))
no_survey_grouped_scores$`Productivity score`<- paste0(round(no_survey_grouped_scores$avg_p_score_mean,2), " +/- ", round(no_survey_grouped_scores$avg_p_score_se,2))
no_survey_grouped_scores$`Risk score`<- paste0(round(no_survey_grouped_scores$avg_s_score_mean,2), " +/- ", round(no_survey_grouped_scores$avg_s_score_se,2))

#only keep needed columns
no_survey_grouped_scores<-no_survey_grouped_scores[,c(1:3,10:13)]

#replace NA SEMs
no_survey_grouped_scores[] <- lapply(no_survey_grouped_scores, function(x) gsub(" \\+/- NA", "", x))

#rename first few columns
colnames(no_survey_grouped_scores)<-c("Area", "n stocks", "n species", "n stock-weighted vulnerability score",
                                       "Vulnerability score", "Productivity score", "Susceptibility score")

#write csv
setwd(tables)
#write.csv(no_survey_grouped_scores,"unsurveyed_score_table.csv",row.names = F)

#### supplemental tables #################################################################################################################################
#### species
#order first
all_ids <- all_ids %>% arrange(desc(distance))

#only keep needed columns
all_ids<-all_ids[,c(18,1:5,33:35)]

#rename columns
colnames(all_ids)<-c("Scientific name","Stock name","Jurisdiction","FMP","Science Center","Area","Productivity score","Susceptibility score","Vulnerability score")
 
all_ids$`Productivity score`<-round(all_ids$`Productivity score`,2)
all_ids$`Susceptibility score`<-round(all_ids$`Susceptibility score`,2)
all_ids$`Vulnerability score`<-round(all_ids$`Vulnerability score`,2)

#write csv
setwd(tables)
#write.csv(all_ids,"stock_score_table.csv",row.names = F)

#### surveys
survey_grouped_scores$`n-weighted vulnerability score`<-round(survey_grouped_scores$distance+(0.01*survey_grouped_scores$n_stocks),2)

#order
survey_grouped_scores <- survey_grouped_scores %>% arrange(desc(`n-weighted vulnerability score`))

#only keep needed columns
survey_grouped_scores<-survey_grouped_scores[,c(1,9,7,5,6,10,4,2,3)]

#rename columns
colnames(survey_grouped_scores)<-c("Survey name","Area and method","method","n stocks","n species", "n stock-weighted vulnerability score","Vulnerability score","Productivity score","Susceptibility score")

survey_grouped_scores$`Productivity score`<-round(survey_grouped_scores$`Productivity score`,2)
survey_grouped_scores$`Susceptibility score`<-round(survey_grouped_scores$`Susceptibility score`,2)
survey_grouped_scores$`Vulnerability score`<-round(survey_grouped_scores$`Vulnerability score`,2)

#write csv
setwd(tables)
#write.csv(survey_grouped_scores,"survey_score_table.csv",row.names = F)

##### get avg number of stocks and species sampled by type
type_summary<- survey_grouped_scores%>%
  group_by(method)%>%
  summarize(avg_stocks_sampled = mean(`n stocks`,na.rm = T), avg_species_sampled = mean(`n species`,na.rm = T))

type_summary[,2:3]<-round(type_summary[,2:3],0)

setwd(tables)
#write.csv(type_summary,"sampling_by_type.csv",row.names = F)

####survey type 
scores_survey_type<- scores_survey_type %>% arrange(desc(distance_mean))

scores_survey_type$avg_stocks_sampled<- scores_survey_type$n_stocks/scores_survey_type$n_surveys
scores_survey_type$avg_species_sampled<- scores_survey_type$n_species/scores_survey_type$n_surveys

scores_survey_type[,5:12]<-round(scores_survey_type[,5:12],2)

#constrain and rename columns
scores_survey_type<-scores_survey_type[,c(1:4,11,12,9,5,7)]

colnames(scores_survey_type)<-c("Survey type","n stocks","n species", "n surveys","Average # stocks sampled","Average # species sampled", "Vulnerabiity score","Productivity score","Susceptibility score")

#write csv
setwd(tables)
#write.csv(scores_survey_type,"survey_type_score_table.csv",row.names = F)

