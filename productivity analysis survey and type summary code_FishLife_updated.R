###################################################################################################################
#### Link scores to surveys and summarize
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

#read in data #################################################################################################################################################################
setwd(data)
#surveys<- read.csv('surveys_assessments_sis.csv')
#surveys_2<- read.csv('surveys_assessments_sis_secondary.csv')
surveys<- read.csv('surveys_assessments_sis_all_influence.csv') #have a sheet with manual adds, but that's not vetted. 

setwd(results)
all_ids<- read.csv("PSA_data_with_uuids_updated_HMS_salmon_excluded.csv")

##### link spp to their survey #########################################################################
#exclude non-FINSS surveys
surveys<-surveys[!surveys$Degree.of.Influence == "Exploratory",] ####including 'other survey' would capture some state surveys, but how good is the data? 

#exclude exploratory degrees of influence
surveys<-surveys[surveys$Survey.Type == "FINSS Survey",]

#fix survey ecosystem classification
surveys<-surveys[,c(1,3:4)]
colnames(surveys)<-c('Survey.Ecosystem','survey','Stock.Name')

multiples<-surveys%>%
  group_by(survey)%>%
  summarise(n_ecosystems = nlevels(factor(Survey.Ecosystem)))

multiples<-multiples[multiples$n_ecosystems > 1,]

ne<- c("Bottom Trawl Survey_Fall","Massachusetts DMF Bottom Trawl_Fall")
se<- c("Florida Keys/Southeast Reef Fish Visual Census (RVC)", "MARMAP Reef Fish Long Bottom Longline Survey (SCDNR)", 
       "MARMAP Reef Fish Long Bottom Longline Survey (SCDNR)", "SEAMAP South Atlantic Coastal Trawl_Fall (SCDNR)",
       "SEAMAP South Atlantic Coastal Trawl_Summer (SCDNR)","SEAMAP South Atlantic Trawl_Summer", "SEAMAP South Atlantic Trawl_Winter", 
       "MARMAP/SEAMAP South Atlantic Reef Fish (SCDNR)")
gom<- c("Gulf of Mexico Pelagic Longline", "Panama City Laboratory Reef Fish Trap/Video", "SEAMAP-GOM Plankton (ADCNR)", 
        "SEAMAP-GOM Shrimp/Groundfish Trawl_Fall", "SEAMAP-GOM Shrimp/Groundfish Trawl_Summer")

surveys$Survey.Ecosystem[surveys$survey %in% ne ]<-"Northeast Shelf"
surveys$Survey.Ecosystem[surveys$survey %in% se ]<-"Southeast Shelf"
surveys$Survey.Ecosystem[surveys$survey %in% gom ]<-"Gulf of Mexico"
surveys$Survey.Ecosystem[surveys$survey == "Reef Fish Visual Census Survey - U.S. Caribbean"]<-"Caribbean Sea"

#merge
scores_survey<- left_join(all_ids,surveys,by = c('Stock.Name')) #many to many; ok for now

#write.csv
setwd(results)
#write.csv(scores_survey, "stock_scores_with_survey_links_updated_HMS_salmon_excluded.csv",row.names = F)

#summarize scores by survey #############################################################################
scores_no_survey<-scores_survey[is.na(scores_survey$survey),]
scores_survey<-scores_survey[!is.na(scores_survey$survey),]

#fix combination ecosystem designations for unsurveyed stocks by duplicating them
scores_no_survey <- scores_no_survey %>%
  mutate(has_slash = grepl("/", Regional.Ecosystem)) %>%  # Identify rows with "/"
  mutate(Regional.Ecosystem = ifelse(has_slash, strsplit(Regional.Ecosystem, "/"), Regional.Ecosystem)) %>%  
  unnest_longer(Regional.Ecosystem) %>%  # Expand the list into separate rows
  mutate(Regional.Ecosystem = trimws(Regional.Ecosystem)) %>%  # Remove leading/trailing spaces
  select(-has_slash)  # Remove temporary column

scores_no_survey<-unique(scores_no_survey)

#fix combination ecosystem designations for surveyed stocks by matching the ecosystem to the survey jurisdiction
scores_survey$Survey.Ecosystem[scores_survey$scientific_name == "Mycteroperca bonaci" &
                                   scores_survey$survey == "Dry Tortugas Reef Fish Visual Census (RVC)"]<- "Gulf of Mexico"

scores_survey$Survey.Ecosystem[scores_survey$scientific_name == "Lutjanus analis" &
                                   scores_survey$survey == "Gulf Video Surveys"]<- "Gulf of Mexico"

#remove ecosystem complex text and rename california current to west coast
#no survey
scores_no_survey$Regional.Ecosystem<-gsub(" Ecosystem Complex","",scores_no_survey$Regional.Ecosystem)
scores_no_survey$Regional.Ecosystem<-gsub("California Current","West Coast",scores_no_survey$Regional.Ecosystem)

#survey
scores_survey$Survey.Ecosystem<-gsub(" Ecosystem Complex","",scores_survey$Survey.Ecosystem)
scores_survey$Survey.Ecosystem<-gsub("California Current","West Coast",scores_survey$Survey.Ecosystem)

#create summary by ecosystem
no_survey_grouped_scores<- scores_no_survey%>%
  group_by(Regional.Ecosystem)%>%
  summarize(n_stocks = n(),
            n_species = n_distinct(scientific_name),
            across(
              where(is.numeric),
              list(mean = ~ mean(.x, na.rm = TRUE), 
                   se = ~ sd(.x, na.rm = TRUE) / sqrt(n()))
            ))

no_survey_grouped_scores<-no_survey_grouped_scores[,c(1:3,44:49)]

#write csv
setwd(results)
#write.csv(no_survey_grouped_scores, "unsurveyed_by_region_summary_updated_HMS_salmon_excluded.csv", row.names = F)
#write.csv(scores_no_survey, "unsurveyed_stock_scores_updated_HMS_salmon_excluded.csv", row.names = F)

####analyze regions in surveyed data ################################################################################################################
#create summary by ecosystem
region_grouped_scores<- scores_survey%>%
  group_by(Survey.Ecosystem)%>%
  summarize(n_stocks = n(),
            n_species = n_distinct(scientific_name),
            n_surveys = n_distinct(survey),
            across(
              where(is.numeric),
              list(mean = ~ mean(.x, na.rm = TRUE), 
                   se = ~ sd(.x, na.rm = TRUE) / sqrt(n()))
            ))

region_grouped_scores<-region_grouped_scores[,c(1:4,45:50)]

#write.csv
setwd(results)
#write.csv(region_grouped_scores,"surveyed_stocks_regional_summary_updated_HMS_salmon_excluded.csv",row.names = F)

###### summarize by survey type
scores_survey$type<- ""

#bottom trawl
btp <- c("Bottom Trawl", "Large-mesh", "inshore trawl", "NEAMAP", "Coastal trawl", "Groundfish trawl", "South Atlantic Trawl", "Shrimp")
btp <- paste(btp, collapse = "|")

scores_survey <- scores_survey %>%
  mutate(type = if_else(
    grepl(btp, survey, ignore.case = TRUE) & (is.na(type) | type == ""),
    "bottom trawl",
    type
  ))

#midwater trawl
mtp <- c("CalCOFI", "Juvenile Rockfish", "EcoMon", "Plankton")
mtp <- paste(mtp, collapse = "|")

scores_survey <- scores_survey %>%
  mutate(type = if_else(
    grepl(mtp, survey, ignore.case = TRUE) & (is.na(type) | type == ""),
    "midwater trawl",
    type
  ))

#hook and line
hlp <- c("hook", "line", "longline", "COASTSPAN", "Bottomfish survey", "Cowcod")
hlp <- paste(hlp, collapse = "|")

scores_survey <- scores_survey %>%
  mutate(type = if_else(
    grepl(hlp, survey, ignore.case = TRUE) & (is.na(type) | type == ""),
    "hook and line",
    type
  ))

#visual & trap-video
vp <- c("visual", "camera", "uvc", "video", "MARMAP/SEAMAP", "Benthic habitat", "Coral Reef", "SEAMAP Gulf of Mexico Reef Fish", "SEFIS", "Epifaunal")
vp <- paste(vp, collapse = "|")

scores_survey <- scores_survey %>%
  mutate(type = if_else(
    grepl(vp, survey, ignore.case = TRUE) & (is.na(type) | type == ""),
    "visual",
    type
  ))

#acoustic-trawl
atp <- c("acoustic", "accoustic", "CPS", "Small pelagics", "CCES")
atp <- paste(atp, collapse = "|")

scores_survey <- scores_survey %>%
  mutate(type = if_else(
    grepl(atp, survey, ignore.case = TRUE) & (is.na(type) | type == ""),
    "acoustic-trawl",
    type
  ))

#create summary by survey type
scores_survey_type<- scores_survey%>%
  group_by(type)%>%
  summarize(n_stocks = n(),
            n_species = n_distinct(scientific_name),
    n_surveys = n_distinct(survey),
    across(
      where(is.numeric),
      list(mean = ~ mean(.x, na.rm = TRUE), 
           se = ~ sd(.x, na.rm = TRUE) / sqrt(n()))
  ))

scores_survey_type<-scores_survey_type[,c(1:4, 45:50)]

#write csvs
setwd(results)
#write.csv(scores_survey_type,"survey_type_summary_updated_HMS_salmon_excluded.csv",row.names = F)

#combine the region and type scores
scores_survey$type_and_region<- paste0(scores_survey$Survey.Ecosystem, " ", scores_survey$type)

#summarize by type and region
scores_survey_type_region<- scores_survey%>%
  group_by(type_and_region)%>%
  summarize(n_stocks = n(),
            n_species = n_distinct(scientific_name),
            n_surveys = n_distinct(survey),
            across(
              where(is.numeric),
              list(mean = ~ mean(.x, na.rm = TRUE), 
                   se = ~ sd(.x, na.rm = TRUE) / sqrt(n()))
    
  ))

scores_survey_type_region<-scores_survey_type_region[,c(1:4,45:50)]

#write csvs
setwd(results)
#write.csv(scores_survey_type_region,"survey_type_and_region_summary_updated_HMS_salmon_excluded.csv",row.names = F)
#write.csv(scores_survey,"stock_scores_with_type_and_region_updated_HMS_salmon_excluded.csv",row.names = F)

#get an average score for each survey
survey_grouped_scores<-scores_survey%>%
  group_by(survey)%>%
  summarise(p_score = mean(avg_p_score), s_score = mean(avg_s_score), distance = mean(distance), n_stocks = n(), n_species = n_distinct(scientific_name), type = type, region = Survey.Ecosystem, type_and_region = type_and_region)

survey_grouped_scores<-unique(survey_grouped_scores)

#determine if regions are duplicated
multiples<-survey_grouped_scores%>%
  group_by(survey)%>%
  summarise(n_ecosystems = nlevels(factor(region)))

multiples<-multiples[multiples$n_ecosystems > 1,]

#write csv
#write.csv(survey_grouped_scores,"survey_grouped_scores_with_type_and_region_updated_HMS_salmon_excluded.csv",row.names = F)
