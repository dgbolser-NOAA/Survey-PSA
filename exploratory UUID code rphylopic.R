###### get taxonomic infor and make plots for PSA project
#read in scores_red
##### plot ############################################################################################
#match scientific names with images using phylopic


library(rphylopic)
library(fishualize)

species<-scores_red[c(1,12,10,11,2)]
setwd(results)
#write.csv(species,"taxonomic_info.csv",row.names = F)



#this works
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

# Function to find an image of a species in the same genus
find_genus_image <- function(genus, all_species) {
  genus_species <- all_species[grepl(paste0("^", genus, " "), all_species)]
  
  # Return the first species with an image, if available
  for (s in genus_species) {
    uuid <- get_phylopic_uuid(s)
    if (!is.na(uuid)) {
      return(uuid)
    }
  }
  return(NA)
}

# Replace NA UUIDs with UUIDs from species in the same genus
all_species <- scores_red$scientific_name
scores_red <- scores_red %>%
  rowwise() %>%
  mutate(phylopic_uuid = ifelse(is.na(phylopic_uuid), 
                                find_genus_image(genus, all_species), 
                                phylopic_uuid)) %>%
  ungroup()


# Replace remaining NA UUIDs with UUIDs from species in the same family
# Function to fetch PhyloPic image UUIDs by name
get_phylopic_uuid <- function(name) {
  tryCatch({
    uuid <- rphylopic::get_uuid(name)
    if (!is.null(uuid)) {
      return(uuid[1])
    } else {
      return(NA)
    }
  }, error = function(e) {
    return(NA)
  })
}

# Function to fetch Fishualize image
get_fishualize_image <- function(name) {
  tryCatch({
    image <- fishualize::fishimg(name, database = "fishbase")
    if (!is.null(image)) {
      return(image)
    } else {
      return(NA)
    }
  }, error = function(e) {
    return(NA)
  })
}

# Main function to match images for species, genera, or families
get_image <- function(species_name, genus_name, family_name) {
  # Try to get PhyloPic UUID by species
  species_image <- get_phylopic_uuid(species_name)
  
  if (!is.na(species_image)) return(species_image)
  
  # Try to get PhyloPic UUID by genus if species fails
  genus_image <- get_phylopic_uuid(genus_name)
  
  if (!is.na(genus_image)) return(genus_image)
  
  # Try to get PhyloPic UUID by family if genus fails
  family_image <- get_phylopic_uuid(family_name)
  
  if (!is.na(family_image)) return(family_image)
  
  # Fall back to Fishualize image if PhyloPic fails
  species_fish_image <- get_fishualize_image(species_name)
  
  if (!is.na(species_fish_image)) return(species_fish_image)
  
  genus_fish_image <- get_fishualize_image(genus_name)
  
  if (!is.na(genus_fish_image)) return(genus_fish_image)
  
  family_fish_image <- get_fishualize_image(family_name)
  
  if (!is.na(family_fish_image)) return(family_fish_image)
  
  return(NA)  # Return NA if no image found
}

###apply functions
# Fetch images based on species, genus, and family
scores_red$image <- mapply(get_image, scores_red$species, scores_red$genus, scores_red$family)

# Convert PhyloPic UUIDs to image URLs (if needed)
# Retrieve information for each ID
phylopic_info <- lapply(scores_red$image, function(id) {
  tryCatch({
    phylopic(id)
  }, error = function(e) {
    return(NA) # Handle errors (e.g., if ID not found)
  })
})

# Extract URLs
phylopic_urls <- sapply(phylopic_info, function(info) {
  if (!is.na(info)) {
    return(info$url)
  } else {
    return(NA)
  }
})

# Print the URLs
print(phylopic_urls)

#### trial stuff ###########################################################################################################################################################################



# Load necessary libraries
library(rphylopic)
library(dplyr)
library(taxize)

# Function to get a PhyloPic image URL
get_phylopic_url <- function(genus, species, family) {
  
  # Try species-level search first
  uuid <- phylopic_uid(paste(genus, species), allow_na = TRUE)
  
  if (is.na(uuid)) {
    # Try genus-level search
    uuid <- phylopic_uid(genus, allow_na = TRUE)
  }
  
  if (is.na(uuid)) {
    # Try family-level search
    uuid <- phylopic_uid(family, allow_na = TRUE)
  }
  
  if (!is.na(uuid)) {
    # Get the image URL using the UUID
    url <- paste0("https://phylopic.org/assets/images/submissions/", uuid, ".png")
  } else {
    url <- NA
  }
  
  return(url)
}

# Apply the function to your dataset
df_with_urls <- df %>%
  rowwise() %>%
  mutate(silhouette_url = get_phylopic_url(genus, species, family))

# View the dataframe with URLs
print(df_with_urls)