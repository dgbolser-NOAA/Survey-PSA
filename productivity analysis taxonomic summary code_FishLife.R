###################################################################################################################
#### Get family and genus grouped scores
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
setwd(results)
all_ids<- read.csv("PSA_data_with_uuids_updated.csv")

#### exclude HMS and salmon ####################################################################################################
exclude<-c('Salmon','Highly Migratory')

excluded_stocks <- all_ids[grepl(paste(exclude, collapse = "|"), all_ids$FMP) | 
                             grepl(paste(exclude, collapse = "|"), all_ids$Regional.Ecosystem), ]

write.csv(excluded_stocks,"HMS_and_Salmon_excluded_stocks.csv",row.names = F)

all_ids <- all_ids[!grepl(paste(exclude, collapse = "|"), all_ids$FMP) & 
                             !grepl(paste(exclude, collapse = "|"), all_ids$Regional.Ecosystem), ]

#write.csv(all_ids, "PSA_data_with_uuids_updated_HMS_salmon_excluded.csv",row.names = F)

####group by genus
ggs<- all_ids[,c(1,14,15,33:35,37)]

#get the number of stocks and species
ggs_components <- ggs %>%
  group_by(Genus) %>%
  summarise(
    n_stocks = n_distinct(Stock.Name),
    n_species = n_distinct(Species)
  )
   
#get means by species
ggs<-ggs%>%
  group_by(Genus,Species)%>%
  summarise(
  across(
    where(is.numeric),
    list(mean = ~ mean(.x, na.rm = TRUE))), # Calculate mean and standard error for numeric columns
    phylopic_uuid = first(na.omit(phylopic_uuid))
    )

#calculate summary statistics based on species in the group
ggs<- ggs %>%
  group_by(Genus) %>%
  summarise(
    across(
      where(is.numeric),
      list(mean = ~ mean(.x, na.rm = TRUE), 
           se = ~ sd(.x, na.rm = TRUE)/sqrt(n()) )), # Calculate mean and sd for numeric columns
    phylopic_uuid = first(na.omit(phylopic_uuid)) # Select first non-NA value for 'silhouette_url'
  )

####merge components with summary stats
ggs<- merge(ggs,ggs_components, by = "Genus")

colnames(ggs)<-c("Genus","avg_p_score_mean","avg_p_score_se","avg_s_score_mean","avg_s_score_se","distance_mean","distance_se","phylopic_uuid","n_stocks","n_species")

setwd(results)
write.csv(ggs, "genus_grouped_vulnerability_scores_updated_HMS_salmon_excluded.csv", row.names = F)

####group by family; there are some NA families, will need to manually enter. 
fgs<- all_ids[,c(1,14,15,33:37)]

#get the number of stocks and species
fgs_components <- fgs %>%
  group_by(family) %>%
  summarise(
    n_stocks = n_distinct(Stock.Name),
    n_species = n_distinct(Species),
    n_genera = n_distinct(Genus)
  )

#get means by species
fgs<-fgs%>%
  group_by(family,Genus,Species)%>%
  summarise(
    across(
      where(is.numeric),
      list(mean = ~ mean(.x, na.rm = TRUE))), # Calculate mean and standard error for numeric columns
    phylopic_uuid = first(na.omit(phylopic_uuid))
  )

#calculate summary statistics based on species in the group
fgs<- fgs %>%
  group_by(family) %>%
  summarise(
    across(
      where(is.numeric),
      list(mean = ~ mean(.x, na.rm = TRUE), 
           se = ~ sd(.x, na.rm = TRUE)/sqrt(n()))), # Calculate mean and standard error for numeric columns
    phylopic_uuid = first(na.omit(phylopic_uuid)) # Select first non-NA value for 'silhouette_url'
  )

####merge components with summary stats
fgs<- merge(fgs,fgs_components, by = "family")

colnames(fgs)<-c("Family","avg_p_score_mean","avg_p_score_se","avg_s_score_mean","avg_s_score_se","distance_mean","distance_se","phylopic_uuid","n_stocks","n_species","n_genera")

setwd(results)
write.csv(fgs, "family_grouped_vulnerability_scores_updated_HMS_salmon_excluded.csv", row.names = F)
