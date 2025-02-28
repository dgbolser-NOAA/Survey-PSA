###################################################################################################################
#### Extract UUIDs and URLs from phylopic
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

#match scientific names with images using phylopic
setwd(results)
scores<-read.csv("scores_updated.csv")
#scores_red<-read.csv("avg_scores_df_expanded_variables.csv")

#first, fix sci names
scores$scientific_name<-gsub(" _ "," ",scores$scientific_name)

#### get taxonomic family
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
families <- sapply(scores$scientific_name, extract_family)

# Add the families as a new column to the dataframe
scores$family <- families

#write csvs
setwd(results)
#write.csv(scores,"scores_updated_family.csv",row.names = F)

#MANUAL EDITING NEEDED -- fill in missing ones and read back in ########################################################################################
scores<-read.csv("scores_updated_family_complete.csv")

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
scores <- scores %>%
  rowwise() %>%
  mutate(phylopic_uuid = get_phylopic_uuid(scientific_name)) %>%
  ungroup()

#separate for genus matching
uuids<-scores[!is.na(scores$phylopic_uuid),]

genus_uuids<- scores[is.na(scores$phylopic_uuid),] 

#match genera
genus_uuids <- genus_uuids %>%
  rowwise() %>%
  mutate(phylopic_uuid = get_phylopic_uuid(Genus)) %>%
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

setwd(results)
#write.csv(all_ids,"PSA_data_with_uuids_updated.csv",row.names = F)
