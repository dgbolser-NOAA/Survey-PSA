###################################################################################################################
#### PSA for survey periodicity and prioritization
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
wd<- "C:/Users/Derek.Bolser/Documents/"
data <- file.path(wd, "Survey-PSA/Data")
results<- file.path(wd, "Survey-PSA/Results")

#read in and edit species list
setwd(data)
alldata<-read.csv("raw_input_PSA_data.csv")
surveys<- read.csv('surveys_assessments_sis.csv')
surveys_2<- read.csv('surveys_assessments_sis_secondary.csv')

#filter out inverts for now
alldata<-as.data.frame(alldata[!grepl("coral", alldata[,1]),])
alldata<-as.data.frame(alldata[!grepl("Habitat", alldata[,1]),])
alldata<-as.data.frame(alldata[!grepl("crab", alldata[,1]),])
alldata<-as.data.frame(alldata[!grepl("shrimp", alldata[,1]),])
alldata<-as.data.frame(alldata[!grepl("scallop", alldata[,1]),])
alldata<-as.data.frame(alldata[!grepl("lobster", alldata[,1]),])
alldata<-as.data.frame(alldata[!grepl("Krill", alldata[,1]),])
alldata<-as.data.frame(alldata[!grepl("Sargassum", alldata[,1]),])
alldata<-as.data.frame(alldata[!grepl("fans", alldata[,1]),])
alldata<-as.data.frame(alldata[!grepl("Refugium", alldata[,1]),])
alldata<-as.data.frame(alldata[!grepl("squid", alldata[,1]),])
alldata<-as.data.frame(alldata[!grepl("Squid", alldata[,1]),])
alldata<-as.data.frame(alldata[!grepl("quahog", alldata[,1]),])
alldata<-as.data.frame(alldata[!grepl("octopus", alldata[,1]),])

##### Get productivity and suceptability scores; low values are low productivity/high suceptability. Following Patrick et al. 2009 tech memo where possible: #######################
#Patrick, W. S., P. Spencer, O. Ormseth, J. Cope, J. Field, D. Kobayashi, T. Gedamke, E. Cort?s, K. Bigelow, W. Overholtz,
#J. Link, and P. Lawson. 2009. Use of productivity and susceptibility indices to determine stock vulnerability,
#with example applications to six U.S. fisheries. U.S. Dep. Commer., NOAA Tech. Memo. NMFS-F/SPO-101, 90 p. 
scores<-alldata[,c(1:11,13:15,18:20,27,32,35)] #leaving out K...

#take the mean of CVA variables that were from different regional databases despite the region being listed as the same
cva<-scores%>%
  group_by(scientific_name,stock_name,common_name,stock_area)%>%
  select(c(Exposure,Sensitivity))%>%
  summarise_all(mean,na.rm = T)

#merge with scores
#remove sensitivity and exposure from scores
scores<-scores%>%
  select(-c(Exposure,Sensitivity))

scores<-merge(scores,cva,by = c("scientific_name","stock_name","common_name","stock_area"))

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

####productivity scores ##################################################################################################################################################################
#Linf
scores<-scores %>%
  mutate(Loo = case_when(Loo < 60 ~ 1, 
                           Loo > 60 & Loo < 150 ~ 2, 
                           Loo > 150 ~ 3))
#K
#scores<-scores %>%
#  mutate(K = case_when(K < 0.15 ~ 1, 
#                         K > 0.15 & K < 0.25 ~ 2, 
#                         K > 0.25 ~ 3))
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

#steepness
scores<-scores %>%
  mutate(h = case_when(h < 0.5 ~ 1, 
                       h > 0.5 & h < 0.7 ~ 2, 
                       h > 0.7 ~ 3))


#maximum annual spawners per spawner; arbitrary cutoff based on data distribution
#scores<-scores %>%
#  mutate(MASPS = case_when(MASPS < 1 ~ 1, 
#                       MASPS > 1 & MASPS < 5 ~ 2, 
#                       MASPS > 5 ~ 3))


#recruitment SD
#scores<-scores %>%
#  mutate(margsd = case_when(margsd < 0.4 ~ 3, 
#                       margsd > 0.4 & margsd < 0.6 ~ 2, 
#                       margsd > 0.6 ~ 1))


#population growth rate
scores<-scores %>%
  mutate(r = case_when(r < 0.16 ~ 1, 
                         r > 0.16 & r < 0.5 ~ 2, 
                         r > 0.5 ~ 3))

#sensitivity (1-4), low - medium - high - very high
scores<-scores %>%
  mutate(Sensitivity = case_when(Sensitivity >= 3 ~ 1, 
                                 Sensitivity < 3 & Sensitivity >=2 ~ 2, 
                                 Sensitivity < 2 & Sensitivity > 1 ~ 3)) 


####susceptibility scores ################################################################################################################################
#score recruitment autocorrelation based on histogram (normally distributed around 0.6)
#scores<-scores %>%
#  mutate(rho = case_when(rho < 0.5 ~ 1, 
#                       rho > 0.5 & rho < 0.7 ~ 2, 
#                       rho > 0.7 ~ 3))

#Fmsy/M
scores<-scores %>%
  mutate(Fmsy_over_M = case_when(Fmsy_over_M < 0.5 ~ 3, 
                         Fmsy_over_M > 0.5 & Fmsy_over_M < 1 ~ 2, 
                         Fmsy_over_M > 1 ~ 1))


#scale assessment level
scores<-scores %>%
  mutate(assessment_level = case_when(assessment_level <= 2 ~ 1, 
                                      assessment_level == 3 ~ 2, 
                                      assessment_level >= 4 ~ 3)) 
#scale model category
scores<-scores %>%
  mutate(model_category = case_when(model_category <= 2 ~ 1, 
                                      model_category > 2 & model_category <= 4 ~ 2, 
                                      model_category > 4 ~ 3)) 

#combine assessment level and model category; fill in NA model categories with assessment level
scores$model_category[is.na(scores$model_category)] <- scores$assessment_level[is.na(scores$model_category)]

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


#### assign a value of 2 (midpoint) for missing data ##################################################################################
scores[is.na(scores)]<-2

###### average productivity scores ###################################################################################################
scores$avg_p_score<-apply(scores[,c(12:17,20)],1, mean, na.rm = T)

#average susceptibility scores; FIX THE INDEXING #####################################################################################
scores$avg_s_score<-apply(scores[,c(8,10,18,19,21)],1,mean, na.rm = T)

##### avg score overall
scores$score<-apply(scores[,c(22,23)],1,mean)

#### euclidian distance; the vulnerabiltiy metric
scores$distance <- sqrt(scores$avg_p_score^2 + scores$avg_s_score^2)

#### write csv #########################################################################################
setwd(results)
scores<-unique(scores)
scores<-scores[!scores$scientific_name == 2,]
#write.csv(scores, "scores_preliminary_extended_variables.csv", row.names = F)

#write csv of reduced scores DF; this removes all duplicates resulting from different region designations that don't correspond to different stock names.
scores[scores == ""] <- NA
scores[,c(1:4,6,7,9,11)][scores[,c(1:4,6,7,9,11)] == "2"] <- NA

scores$stock_area[is.na(scores$stock_area)] <- scores$Region[is.na(scores$stock_area)]
scores_red<- scores[,c(1:4,22:25)]

setwd(results)
scores_red<-unique(scores_red)
#write.csv(scores_red, "avg_scores_df_expanded_variables.csv", row.names = F)

##### plot ############################################################################################
#match scientific names with images using phylopic
#first, fix sci names
scores_red$scientific_name<-gsub(" _ "," ",scores_red$scientific_name)

#get something for NAs
# Function to extract the genus from a scientific name
extract_genus <- function(scientific_name) {
  strsplit(scientific_name, " ")[[1]][1]
}

# Add a column with the genus for each species
scores_red <- scores_red %>%
  mutate(genus = sapply(scientific_name, extract_genus))

#### now go back to family
extract_family <- function(name) {
  # Split the input string into genus and species
  parts <- strsplit(name, " ")[[1]]
  if (length(parts) != 2) {
    return(NA)  # Return NA if input is not in the expected format
  }
  
  genus <- parts[1]
  species <- parts[2]
  
  # Fetch the taxonomic name using ITIS
  family_info <- tryCatch({
    tax_name(paste(genus, species), get = "family", db = "itis")
  }, error = function(e) {
    return(NA)  # Return NA if any error occurs
  })
  
  # Check if the result is valid and extract the family name
  if (!is.null(family_info) && nrow(family_info) > 0 && !is.na(family_info$family[1])) {
    return(family_info$family[1])
  }
  
  return(NA)
}

# Add a column with the family for each species 
# Extract families into a separate vector
families <- sapply(scores_red$scientific_name, extract_family)

# Add the families as a new column to the dataframe
scores_red$family <- families

#get a species column
scores_red <- scores_red %>%
  mutate(
    genus = sapply(scientific_name, function(x) strsplit(x, " ")[[1]][1]), # Extract genus
    species = sapply(scientific_name, function(x) strsplit(x, " ")[[1]][2]) # Extract species
  )

#write csvs
setwd(results)
#write.csv(scores_red,"scores_reduced_inc_taxonomic_info.csv",row.names = F)

#try rphylopic functions directly
#get the images
get_phylopic_uuid <- function(scientific_name) {
  tryCatch({
    uuid <- rphylopic::get_uuid(scientific_name)
    if (length(uuid) > 0) {
      return(uuid)
    } else {
      return(NA)
    }
  }, error = function(e) {
    return(NA)
  })
}

# Add a column with PhyloPic UUIDs to the dataframe
scores_red <- scores_red %>%
  rowwise() %>%
  mutate(phylopic_uuid = get_phylopic_uuid(scientific_name)) %>%
  ungroup()

#separate for genus matching
uuids<-scores_red[!is.na(scores_red$phylopic_uuid),]

genus_uuids<- scores_red[is.na(scores_red$phylopic_uuid),] 

#match genera
genus_uuids <- genus_uuids %>%
  rowwise() %>%
  mutate(phylopic_uuid = get_phylopic_uuid(genus)) %>%
  ungroup()

#separate for family matching
family_uuids<- genus_uuids[is.na(genus_uuids$phylopic_uuid),]

genus_uuids<- genus_uuids[!is.na(genus_uuids$phylopic_uuid),]

#match families
family_uuids <- family_uuids %>%
  rowwise() %>%
  mutate(phylopic_uuid = get_phylopic_uuid(family)) %>%
  ungroup()

#bind all
all_ids<-rbind(uuids,genus_uuids)
all_ids<-rbind(all_ids, family_uuids)

#make URLs from uuids
get_phylopic_url <- function(uuid) {
  if (is.na(uuid) || uuid == "") {
    message("UUID is missing or NA.")
    return(NA)
  }
  
  # Construct the URL from the UUID
  url <- paste0("https://phylopic.org/images/", uuid)
  
  # Log the constructed URL
  message("Checking URL: ", url)
  
  # Check if the URL returns a valid image
  response <- HEAD(url)
  
  # Log the status code
  message("Status code: ", status_code(response))
  
  if (status_code(response) == 200) {
    return(url)
  } else {
    return(NA)
  }
}

all_ids$silhouette_url <- sapply(all_ids$phylopic_uuid, get_phylopic_url)

#these aren't the final URLs; those have a species name at the end but aren't predictable. get the final ones
fetch_final_url <- function(url) {
  if (!is.na(url) && nzchar(url)) {
    tryCatch({
      response <- httr::GET(url)
      if (httr::status_code(response) == 200) {
        return(response$url)
      } else {
        return(NA)
      }
    }, error = function(e) {
      return(NA)  # Return NA if any error occurs
    })
  } else {
    return(NA)
  }
}

all_ids$final_url<-sapply(all_ids$silhouette_url,fetch_final_url)

setwd(results)
write.csv(all_ids,"PSA_data_with_uuids_and_urls.cvs",row.names = F)

#with scores; not much separation on x-axis
# ggplot(scores_red, aes(x= avg_p_score, y= avg_s_score,label = stock_name)) + 
#   geom_hline(yintercept=2, linetype="dashed", color = "red") +
#   geom_vline(xintercept=2, linetype="dashed", color = "red")+
#   geom_point(stat = "identity") + 
#   theme_minimal() + labs(x = "Productivity", y = "Susceptibility") +
#  geom_label_repel(aes(x = avg_p_score, 
#                      y = avg_s_score, 
#                       label = stock_name), 
#                   max.overlaps = 200, angle = 45, hjust = 0.6, size = 1.5)

ggplot(all_ids) + 
  geom_hline(yintercept = 2, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 2, linetype = "dashed", color = "red") +
  #geom_point(stat = "identity") + 
  theme_minimal() + 
  labs(x = "Productivity", y = "Risk") +
  xlim(2, 3) +  
  ylim(2, 3) +
  add_phylopic(x = all_ids$avg_p_score, y = all_ids$avg_s_score, uuid = as.character(all_ids$phylopic_uuid),alpha = 0.1, ysize = 0.05)  # Adjust size as needed 

setwd(results)
#ggsave(filename = 'PSA_NMFS_fish_images.png',plot = last_plot() , path = data, width = 18, height = 9, device = 'png', dpi = 300)
#ggsave(filename = 'PSA_NMFS_blank.png',plot = last_plot() , path = data, width = 18, height = 9, device = 'png', dpi = 300)
#ggsave(filename = 'PSA_NMFS_fish_images_q1.png',plot = last_plot() , path = data, width = 18, height = 9, device = 'png', dpi = 300)
#ggsave(filename = 'PSA_NMFS_fish_images_q2.png',plot = last_plot() , path = data, width = 18, height = 9, device = 'png', dpi = 300)
#ggsave(filename = 'PSA_NMFS_fish_images_q3.png',plot = last_plot() , path = data, width = 18, height = 9, device = 'png', dpi = 300)
#ggsave(filename = 'PSA_NMFS_fish_images_q4.png',plot = last_plot() , path = data, width = 18, height = 9, device = 'png', dpi = 300)
# not producing a png...

####group by quadrant
Q1<- as.data.frame(scores_red[scores_red$avg_p_score < 2 & scores_red$avg_s_score < 2,])
Q2<- as.data.frame(scores_red[scores_red$avg_p_score >= 2 & scores_red$avg_s_score < 2,])
Q3<- as.data.frame(scores_red[scores_red$avg_p_score < 2 & scores_red$avg_s_score >=2,])
Q4<- as.data.frame(scores_red[scores_red$avg_p_score >= 2 & scores_red$avg_s_score >=2,])

Q1[154:214,]<-NA
Q3[91:214,]<-NA
Q4[73:214,]<-NA

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

scores_survey<- left_join(scores_red,surveys,by = 'stock_name') #many to many; ok for now

#summarize scores by survey; many NAs... go back here to check for misaligned stock names #############################################################################
scores_no_survey<-scores_survey[is.na(scores_survey$survey),]
scores_survey<-scores_survey[!is.na(scores_survey$survey),]

scores_no_survey <- scores_no_survey %>%
  mutate(survey = ifelse(is.na(survey), 
                          str_extract(stock_area, "^\\w+(?:\\s+\\w+){0,3}"), 
                          survey))

scores_no_survey <- scores_no_survey %>%
  mutate(survey = ifelse(is.na(survey), 
                         str_extract(stock_name, "^\\w+(?:\\s+\\w+){0,3}"), 
                         survey))

#classify by science center if possible, region/taxa if unsure
scores_no_survey <- scores_no_survey %>%
  mutate(survey = ifelse(str_detect(survey, "River|Creek|Lake|Basin|Hatchery|Production|Natural|Migrating|Far|Fall|Spring|Summer|Puget|Valley|Washington Coast|Oregon Coast|Northern California Coast"), "Salmon", survey))

scores_no_survey <- scores_no_survey %>%
  mutate(survey = ifelse(str_detect(survey, "Alaska|alaska|bering_sea|Aleutian|Bering"), "AFSC not surveyed", survey))

scores_no_survey <- scores_no_survey %>%
  mutate(survey = ifelse(str_detect(survey, "Hawaiian|Samoa|Guam|Mariana|pacific"), "PIFSC not surveyed", survey))

scores_no_survey <- scores_no_survey %>%
  mutate(survey = ifelse(str_detect(survey, "Caribbean|gulf|Gulf of Mexico|sava|Atlantic and Gulf of|Southern Atlantic|Keys|Mid|Carolinas|Atlantic Coast"), "SEFSC not surveyed", survey))

scores_no_survey <- scores_no_survey %>%
  mutate(survey = ifelse(str_detect(survey, "Maine|Georges|Northwestern Atlantic|New England"), "NEFSC not surveyed", survey))

scores_no_survey <- scores_no_survey %>%
  mutate(survey = ifelse(str_detect(survey, "North Atlantic"), "North Atlantic High Seas", survey))

scores_no_survey <- scores_no_survey %>%
  mutate(survey = ifelse(str_detect(survey, "Western Atlantic"), "Western Atlantic High Seas", survey))

scores_no_survey <- scores_no_survey %>%
  mutate(survey = ifelse(str_detect(survey, "Eastern Pacific"), "Eastern Pacific High Seas", survey))

scores_no_survey <- scores_no_survey %>%
  mutate(survey = ifelse(str_detect(survey, "North Pacific"), "North Pacific High Seas", survey)) 

scores_no_survey <- scores_no_survey %>%
  mutate(survey = ifelse(str_detect(survey, "South Pacific"), "South Pacific High Seas", survey)) 

scores_no_survey <- scores_no_survey %>%
  mutate(survey = ifelse(str_detect(survey, "Western and Central Pacific"), "Western and Central Pacific High Seas", survey))

scores_no_survey <- scores_no_survey %>%
  mutate(survey = ifelse(str_detect(survey, "Western and Central North"), "Western and Central North Pacific High Seas", survey))

scores_no_survey <- scores_no_survey %>%
  mutate(survey = ifelse(str_detect(survey, "California|Oregon|Washington|Pacific Coast"), "Nearshore West Coast", survey))

scores_no_survey <- scores_no_survey %>%
  mutate(survey = ifelse(str_detect(stock_name, "Rex sole - Western / Central Gulf of Alaska"), "AFSC not surveyed", survey))

scores_no_survey <- scores_no_survey %>%
  mutate(survey = ifelse(str_detect(stock_name, "Gulf of Alaska Shallow Water Flatfish Complex"), "AFSC not surveyed", survey))

scores_no_survey <- scores_no_survey %>%
  mutate(survey = ifelse(str_detect(stock_name, "Pacific Coast Blackspotted and Rougheye Rockfish Complex"), "Nearshore West Coast", survey))

scores_no_survey<-unique(scores_no_survey)

str(scores_no_survey)

survey_grouped_scores<-scores_survey%>%
  group_by(survey)%>%
  summarise(p_score = mean(avg_p_score), s_score = mean(avg_s_score), n = n())

no_survey_grouped_scores<-scores_no_survey%>%
  group_by(survey)%>%
  summarise(p_score = mean(avg_p_score), s_score = mean(avg_s_score), n = n())

#get an average score
no_survey_grouped_scores$score<-apply(no_survey_grouped_scores[,c(2,3)],1,mean)
survey_grouped_scores$score<-apply(survey_grouped_scores[,c(2,3)],1,mean)

#calculate distance
no_survey_grouped_scores$distance <- sqrt(no_survey_grouped_scores$p_score^2 + no_survey_grouped_scores$s_score^2)
survey_grouped_scores$distance <- sqrt(survey_grouped_scores$p_score^2 + survey_grouped_scores$s_score^2)

#fix some names
no_survey_grouped_scores[2,1]<- "Unspecified Atlantic High Seas"
no_survey_grouped_scores[9,1]<- "Unspecified Pacific High Seas"
no_survey_grouped_scores[11,1]<- "Pacific Salmon"

colnames(no_survey_grouped_scores)<-c("jurisdiction","p_score","s_score","n","score","distance")

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
#ggsave(filename = 'PSA_surveys_FishLife_extended_variables.png',plot = last_plot() , path = data, width = 18, height = 9, device = 'tiff', dpi = 300)

#write csv of survey scores
setwd(results)
#write.csv(survey_grouped_scores, "survey_grouped_PSA_scores_extended_variables.csv", row.names = F)
#write.csv(no_survey_grouped_scores, "no_survey_grouped_PSA_scores_extended_variables.csv", row.names = F)

#export list of surveys by quadrant; some getting excluded here...
Q1<- as.data.frame(survey_id_grouped_scores[survey_id_grouped_scores$p_score < 2 & survey_id_grouped_scores$s_score < 2,])
Q2<- as.data.frame(survey_id_grouped_scores[survey_id_grouped_scores$p_score >= 2 & survey_id_grouped_scores$s_score < 2,])
Q3<- as.data.frame(survey_id_grouped_scores[survey_id_grouped_scores$p_score < 2 & survey_id_grouped_scores$s_score >=2,])
Q4<- as.data.frame(survey_id_grouped_scores[survey_id_grouped_scores$p_score >= 2 & survey_id_grouped_scores$s_score >=2,])

Q1[19:23,]<-NA
Q3[21:23,]<-NA
Q4[12:23,]<-NA

colnames(Q1)<-c('Q1','p_score','s_score','n','score')
colnames(Q2)<-c('Q2','p_score','s_score','n','score')
colnames(Q3)<-c('Q3','p_score','s_score','n','score')
colnames(Q4)<-c('Q4','p_score','s_score','n','score')

quadrants<-cbind.data.frame(c(Q1,Q2,Q3,Q4))

quadrants<-quadrants[,c('Q1','Q2','Q3','Q4')]

setwd(results)
#write.csv(quadrants, "surveys_by_quadrant_PSA_expanded_variables.csv", row.names = F)
