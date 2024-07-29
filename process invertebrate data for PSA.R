##########################################################################################################################################################
#### process invert data for PSA
####
##########################################################################################################################################################

#clear environment
rm(list=ls())

#load packages
#devtools::install_github("cfree14/freeR")
#remotes::install_github("cfree14/freeR")

library(rfishbase)
library(freeR) 
library(ramlegacy)

#define wd 
pawd<- "C:/Users/Derek.Bolser/Documents/Productivity analysis/SurveyPSA"
setwd(pawd)

#read in data
alldata<- read.csv("raw_input_PSA_data.csv")

#filter data to remove fishes
inverts <- alldata %>%
  filter(is.na(get("Functional_group")) | get("Functional_group") == "Invertebrate")

inverts<- inverts %>%
  filter(is.na(get("Loo")))
  
inverts<- inverts %>%
  filter(!is.na(get("scientific_name")))

#modify species name to get the pull functions to work
inverts$scientific_name <- gsub(" _ ", " ", inverts$scientific_name)

# #use freeR  to query sealifebase
species<-inverts$scientific_name
species<-unique(species)

inv_spp <-fishbase(dataset = "species", species = species, level = "species", cleaned = T, add_taxa = F)
inv_mat <-fishbase(dataset = "maturity", species = species, level = "species", cleaned = T, add_taxa = F)
inv_fec <- fishbase(dataset = "fecundity", species = species, level = "species", cleaned = T, add_taxa = F)
inv_lw <-fishbase(dataset = "lw", species = species, level = "species", cleaned = T, add_taxa = F)
inv_vb<- fishbase(dataset = "vonb", species = species, level = "species", cleaned = T, add_taxa = F)

# #make one df
inv_df<-merge(inv_spp,inv_mat,by = c("species","database"))
inv_df<-merge(inv_df,inv_fec,by = c("species","database"))
inv_df<-merge(inv_df,inv_lw,by = c("species","database"))
inv_df<-merge(inv_df,inv_vb,by = c("species","database"))

# #remove fishbase NAs
inv_df<-inv_df[!inv_df$database == "FishBase",]

inv_df_red<-inv_df[,c("species","comm_name","location.x","location.y","sex.x","sex.y",
                      "linf_cm", "tl_linf_cm","length_cm_max",
                      "k",
                      "tmax_wild_yr","tmax_yr",
                      "tmat_yr.x","tmat_yr.y","tmat_yr_lo", "tmat_yr_hi",
                      "m",
                      "fecundity_rel_avg","fecundity_abs_min","fecundity_abs_max", "fecundity_rel_min", "fecundity_rel_max")]

# #coalesce columns
inv_df_red<- inv_df_red %>%
  mutate(species = species,
         comm_name = comm_name,
         location = coalesce(location.x, location.y),
         sex = coalesce(sex.x, sex.y),
         linf_cm = linf_cm,
         tl_linf_cm = tl_linf_cm,
         length_cm_max = length_cm_max,
         k = k,
         tmax_wild_yr = tmax_wild_yr,
         tmax_yr = tmax_yr,
         tmat_yr = coalesce(tmat_yr.x, tmat_yr.y),
         tmat_yr_lo = tmat_yr_lo,
         tmat_yr_hi = tmat_yr_hi,
         m=m,
         fecundity_rel_avg = fecundity_rel_avg,
         fecundity_abs_min = fecundity_abs_min,
         fecundity_abs_max = fecundity_abs_max,
         fecundity_rel_min = fecundity_rel_min,
         fecundity_rel_max = fecundity_rel_max)

names = c("location.x","location.y","sex.x","sex.y", "tmat_yr.x","tmat_yr.y")
inv_df_red<-inv_df_red[,!names(inv_df_red)%in%names]

# #calculate averages 
inv_df_red<-inv_df_red%>%
  group_by(species,comm_name,location,sex)%>%
  summarise_all(mean,na.rm = T)

# #now, just group by species for the final DF
inv_df_final<-inv_df_red%>%
  group_by(species,comm_name)%>%
  summarise_all(mean,na.rm = T)

#remove location and sex
inv_df_final$location<- NULL
inv_df_final$sex<- NULL

#make NaN values NA
NaN_replace <- function(x) {
  if (is.numeric(x)) {
    x[is.nan(x)] <- NA
  }
  return(x)
}

# Apply the function to each column in the data frame
inv_df_final <- inv_df_final %>%
  mutate(across(everything(), NaN_replace))

#might not have recruitment. r is present on the website but not pulled. manually printed below
#table of r s ############################################################################################################
#Arctica islandica -- 0.47
#Chaceon quinquedens -- NA
#Chionoecetes bairdi -- NA
#Enteroctopus dofleini -- NA
#Farfantepenaeus aztecus --  0.52
#Farfantepenaeus duorarum -- 0.51
#Illex illecebrosus -- 0.50 -- NA, but 0.3 - 2.5 cited for K doubling time; using the midpoint, 1.4
#Lithodes aequispinus -- 6.93 -- NA, but 0.07-0.13 cited for K doubling time
#Litopenaeus setiferus -- 0.56
#Loligo pealeii -- 0.9 -- NA  but 0.59-0.95 cited for K doubling time; using midpoint 0.77
#Panulirus argus -- 0.55
#Paralithodes camtschaticus -- NA
#Paralithodes platypus -- NA
#Patinopecten caurinus -- 1.52 -- NA but 0.39-0.5 cited for K doubling time; using 0.455
#Placopecten magellanicus -- 0.56
#Pleoticus robustus -- 1.78 -- NA, but 0.39 cited for K doubling time
#Ranina ranina -- 0.93
#Sicyonia brevirostris -- 0.81      
#Spisula solidissima -- 0.47

################ not present
#Chionoecetes opilio -- 0.58 

#calculate r from K
calculate_growth_rate <- function(doubling_time) {
  growth_rate <- log(2) / doubling_time
  return(growth_rate)
}

calculate_growth_rate(1.4) #0.50
calculate_growth_rate(0.1) #6.93; this can't be right
calculate_growth_rate(0.77) #0.9
calculate_growth_rate(0.455) #1.52
calculate_growth_rate(0.39) #1.78

#assign values in df
inv_df_final$r<-NA

inv_df_final[1,18]<-0.47
inv_df_final[2,18]<-NA
inv_df_final[3,18]<-NA
inv_df_final[4,18]<-NA
inv_df_final[5,18]<-0.52
inv_df_final[6,18]<-0.51
inv_df_final[7,18]<-0.50
inv_df_final[8,18]<-6.93
inv_df_final[9,18]<-0.56
inv_df_final[10,18]<-0.9
inv_df_final[11,18]<-0.55
inv_df_final[12,18]<-NA
inv_df_final[13,18]<-NA
inv_df_final[14,18]<-1.52
inv_df_final[15,18]<-0.56
inv_df_final[16,18]<-1.78
inv_df_final[17,18]<-0.93
inv_df_final[18,18]<-0.81
inv_df_final[19,18]<-0.47

# #examine the NAs
inv_df_final[is.na(inv_df_final)]<-NA

# #remove all-NA columns
idf<- inv_df_final[, colSums(is.na(inv_df_final)) < nrow(inv_df_final), drop = FALSE]
idf<-idf[1:19,]

# #finalize the variables
#idf<- idf %>%
#  mutate(species = species,
#         comm_name = comm_name,
#         linf_cm = linf_cm,
#         k = k,
#         tmax = coalesce(tmax_yr, tmax_wild_yr),
#         tmat = coalesce(tmat_yr, tmat_yr_hi),
#         m=m,
#         fecundity_abs_max = fecundity_abs_max)

names2<-c("tl_linf_cm","tmax_yr", "tmax_wild_yr","tmat_yr", "tmat_yr_hi","fecundity_rel_min","fecundity_rel_max")

idf<-idf[!names(idf)%in%names2]

#### merge with fish data ###################################################################################################################################
#absolute fecundity is on a different scale than MASPS; eliminate for now
idf$fecundity_abs_max<-NULL

colnames(idf)<-c("scientific_name","common_name", "Loo", "K", "M", "r")

#merging by scientific name; remvoe common name column
idf$common_name<- NULL

#fix scientific name for merging
idf$scientific_name <- gsub(" ", " _ ", idf$scientific_name)

#merge to get rid of NAs in alldata for inverts
alldata <- alldata %>%
  full_join(idf, by = "scientific_name", suffix = c(".alldata", ".inv")) %>%
  mutate(
    Loo = coalesce(Loo.alldata, Loo.inv),
    K = coalesce(K.alldata, K.inv),
    M = coalesce(M.alldata, M.inv),
    r = coalesce(r.alldata, r.inv)
  ) %>%
  select(-Loo.alldata,-Loo.inv,-K.alldata,-K.inv,-M.alldata,-M.inv,-r.alldata,-r.inv)

#remove the complexes that weren't broken out by species by removing rows with all NA
columns<- colnames(alldata[7:45])

alldata <- alldata %>%
  filter(rowSums(is.na(select(., all_of(columns)))) != length(columns))

#write csv
write.csv(alldata,"raw_input_PSA_data_including_inverts.csv",row.names = F)
