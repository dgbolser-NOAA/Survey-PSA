#############################################################################################################
####### Plot everything 
#######
#############################################################################################################

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
library(scales)
library(viridis)

#define wds
####set wds
#wd <- getwd() 
wd<- "C:/Users/Derek.Bolser/Documents"
data <- file.path(wd, "Survey-PSA/Data")
results<- file.path(wd, "Survey-PSA/Results")
figures<- file.path(wd, "Survey-PSA/Figures")

#read in data
setwd(results)
all_ids<- read.csv("PSA_data_with_uuids_updated_HMS_salmon_excluded.csv")
ggs<- read.csv("genus_grouped_vulnerability_scores_updated_HMS_salmon_excluded.csv")
fgs<-read.csv("family_grouped_vulnerability_scores_updated_all_images_HMS_salmon_excluded.csv") #load in sheet with manually added images to make sure everything plots
survey_grouped_scores<- read.csv("survey_grouped_scores_with_type_and_region_updated_HMS_salmon_excluded_other_surveys_included.csv")
no_survey_grouped_scores<-read.csv("unsurveyed_by_region_summary_updated_HMS_salmon_excluded_other_surveys_included.csv")
region_grouped_scores<- read.csv("surveyed_stocks_regional_summary_updated_HMS_salmon_excluded_other_surveys_included.csv")
scores_survey_type_region<- read.csv("survey_type_and_region_summary_updated_HMS_salmon_excluded_other_surveys_included.csv")
scores_survey_type<- read.csv("survey_type_summary_updated_HMS_salmon_excluded_other_surveys_included.csv")

#families with SD error bars
ggplot(fgs) + 
  theme_classic() + 
  labs(x = "Productivity", y = "Susceptibility") +
  coord_cartesian(xlim = c(3, 1), ylim = c(1, 3)) +
  scale_x_continuous(breaks = seq(1.0, 3.0, by = 0.5),
                     labels = label_number(accuracy = 0.1),
                     trans = "reverse") + 
  geom_curve(aes(x = 3, y = 1.5, xend = 2.5, yend = 1), 
             color = "gray", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 2, xend = 2, yend = 1),
             color = "gray", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 2.5, xend = 1.5, yend = 1),
             color = "gray", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 3, xend = 1, yend = 1),
             color = "gray", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 3.5, xend = 0.5, yend = 1),
             color = "gray", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_errorbar(aes(x = avg_p_score_mean, ymin = avg_s_score_mean - avg_s_score_se, ymax = avg_s_score_mean + avg_s_score_se), 
                data = fgs, 
                width = 0.05, # Adjust width as needed
                color = "black") + 
  geom_errorbarh(aes(y = avg_s_score_mean, xmin = avg_p_score_mean - avg_p_score_se, xmax = avg_p_score_mean + avg_p_score_se), 
                 data = fgs, 
                 height = 0.05, # Adjust height as needed
                 color = "black") +
  add_phylopic(x = fgs$avg_p_score_mean, y = fgs$avg_s_score_mean, uuid = as.character(fgs$phylopic_uuid), alpha = 1, ysize = 0.05, #tried 0.07
               fill = "gray", color = "black") +
  # geom_point(aes(x = avg_p_score_mean, y = avg_s_score_mean, color = n_stocks), 
  #            data = fgs, 
  #            size = 3) + 
  geom_text_repel(aes(x = avg_p_score_mean, y = avg_s_score_mean, label = Family, color = n_stocks), 
                  data = fgs, 
                  vjust = 2, 
                  size = 4, 
                  box.padding = 0.25, 
                  point.padding = 0.25, 
                  max.overlaps = Inf,
                  force = T) +
  scale_color_gradientn(colors = c("darkblue","darkgreen","darkgreen", "gold","gold","orange","orange","red","red","red"), name = "n 
stocks") 

#ggsave(filename = 'PSA_NMFS_fish_images_by_family_small.png',plot = last_plot() , path = figures, width = 9.5, height = 6.5, device = 'png', dpi = 300)

#by type and region with abbreviated names 
#abbreviate names
scores_survey_type_region$type_and_region_abbrev<- scores_survey_type_region$type_and_region

scores_survey_type_region$type_and_region_abbrev<-gsub("Pacific Islands","PI",scores_survey_type_region$type_and_region_abbrev)
scores_survey_type_region$type_and_region_abbrev<-gsub("West Coast","WC",scores_survey_type_region$type_and_region_abbrev)
scores_survey_type_region$type_and_region_abbrev<-gsub("Alaska","AK",scores_survey_type_region$type_and_region_abbrev)
scores_survey_type_region$type_and_region_abbrev<-gsub("Gulf of Mexico","GA",scores_survey_type_region$type_and_region_abbrev)
scores_survey_type_region$type_and_region_abbrev<-gsub("Northeast Shelf","NE",scores_survey_type_region$type_and_region_abbrev)
scores_survey_type_region$type_and_region_abbrev<-gsub("Southeast Shelf","SE",scores_survey_type_region$type_and_region_abbrev)
scores_survey_type_region$type_and_region_abbrev<-gsub("Caribbean Sea","CS",scores_survey_type_region$type_and_region_abbrev)

ggplot(scores_survey_type_region, aes(x= avg_p_score_mean, y= avg_s_score_mean,label = type_and_region_abbrev)) +
  coord_cartesian(xlim = c(3, 1), ylim = c(1, 3)) +
  scale_x_continuous(breaks = seq(1.0, 3.0, by = 0.5),
                     labels = label_number(accuracy = 0.1),
                     trans = "reverse") + 
  geom_curve(aes(x = 3, y = 1.5, xend = 2.5, yend = 1), 
             color = "gray", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 2, xend = 2, yend = 1),
             color = "gray", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 2.5, xend = 1.5, yend = 1),
             color = "gray", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 3, xend = 1, yend = 1),
             color = "gray", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 3.5, xend = 0.5, yend = 1),
             color = "gray", linetype = "dashed", size = 1, curvature = -0.3) +
  theme_classic() + labs(x = "Productivity", y = "Susceptibility") +
  geom_errorbar(aes(x = avg_p_score_mean, ymin = avg_s_score_mean - avg_s_score_se, ymax = avg_s_score_mean + avg_s_score_se), 
                data = scores_survey_type_region, 
                width = 0.05, # Adjust width as needed
                color = "black") + 
  geom_errorbarh(aes(y = avg_s_score_mean, xmin = avg_p_score_mean - avg_p_score_se, xmax = avg_p_score_mean + avg_p_score_se), 
                 data = scores_survey_type_region, 
                 height = 0.05, # Adjust height as needed
                 color = "black") +
  geom_point(aes(x = avg_p_score_mean, y = avg_s_score_mean, color = n_stocks),
             data = scores_survey_type_region,
             size = 3) +
  geom_label_repel(aes(label = type_and_region_abbrev, color = n_stocks), 
                   box.padding = 0.5, #0.15
                   point.padding = 0, #0
                   max.overlaps = 200, vjust = 0.9, hjust = 0.9, size = 4) + #200, 0.9, 0.9, 5
  scale_color_gradientn(colors = c("darkblue","darkblue","darkgreen","darkgreen", "gold","gold","orange","orange",
                                   "red","red"),
                        name = "n 
stocks") 

#ggsave(filename = 'PSA_plot_survey_type_region_abbreviated.png',plot = last_plot() , path = figures, width = 9.5, height = 8.5, device = 'png', dpi = 300)

####unsurveyed regional summary
#change to gulf of america
no_survey_grouped_scores$Regional.Ecosystem[no_survey_grouped_scores$Regional.Ecosystem == "Gulf of Mexico"]<- "Gulf of America"

#by region
ggplot(no_survey_grouped_scores, aes(x= avg_p_score_mean, y= avg_s_score_mean,label = Regional.Ecosystem)) +
  coord_cartesian(xlim = c(3, 1), ylim = c(1, 3)) +
  scale_x_continuous(breaks = seq(1.0, 3.0, by = 0.5),
                     labels = label_number(accuracy = 0.1),
                     trans = "reverse") + 
  geom_curve(aes(x = 3, y = 1.5, xend = 2.5, yend = 1), 
             color = "gray", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 2, xend = 2, yend = 1),
             color = "gray", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 2.5, xend = 1.5, yend = 1),
             color = "gray", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 3, xend = 1, yend = 1),
             color = "gray", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 3.5, xend = 0.5, yend = 1),
             color = "gray", linetype = "dashed", size = 1, curvature = -0.3) +
  theme_classic() + labs(x = "Productivity", y = "Susceptibility") +
  geom_errorbar(aes(x = avg_p_score_mean, ymin = avg_s_score_mean - avg_s_score_se, ymax = avg_s_score_mean + avg_s_score_se), 
                data = no_survey_grouped_scores, 
                width = 0.05, # Adjust width as needed
                color = "black") + 
  geom_errorbarh(aes(y = avg_s_score_mean, xmin = avg_p_score_mean - avg_p_score_se, xmax = avg_p_score_mean + avg_p_score_se), 
                 data = no_survey_grouped_scores, 
                 height = 0.05, # Adjust height as needed
                 color = "black") +
  geom_point(aes(x = avg_p_score_mean, y = avg_s_score_mean, color = n_stocks),
             data = no_survey_grouped_scores,
             size = 3) +
  geom_label_repel(aes(label = Regional.Ecosystem, color = n_stocks), 
                   box.padding = 1.2, 
                   point.padding = 0, 
                   max.overlaps = 200, vjust = 0.5, hjust = 0.5, size = 5) +
  scale_color_gradientn(colors = c("darkblue","darkgreen","darkgreen", "gold","gold","orange","orange","red","red","red"), name = "n 
stocks") 

#ggsave(filename = 'PSA_plot_unsurveyed_stocks_jurisdiction.png',plot = last_plot() , path = figures, width = 9.5, height = 8.5, device = 'png', dpi = 300)

#### extra exploratory plots #################################################################################################################################################################
#for stocks
ggplot(all_ids) + 
  theme_minimal() + 
  labs(x = "Productivity", y = "Risk") +
  coord_cartesian(xlim = c(3, 1), ylim = c(1, 3)) + 
  scale_x_continuous(breaks = seq(1.0, 3.0, by = 0.5),
                     labels = label_number(accuracy = 0.1),
                     trans = "reverse") +  
  geom_curve(aes(x = 3, y = 1.5, xend = 2.5, yend = 1), 
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 2, xend = 2, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 2.5, xend = 1.5, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 3, xend = 1, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 3.5, xend = 0.5, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  add_phylopic(x = all_ids$avg_p_score, y = all_ids$avg_s_score, uuid = as.character(all_ids$phylopic_uuid), alpha = 1, ysize = 0.05, 
               fill = "gray", color = "black")

#ggsave(filename = 'PSA_NMFS_fish_images.png',plot = last_plot() , path = figures, width = 9.5, height = 8.5, device = 'png', dpi = 300)

#for genera; some NA uuids
ggplot(ggs) + 
  theme_minimal() + 
  labs(x = "Productivity", y = "Risk") +
  coord_cartesian(xlim = c(3, 1), ylim = c(1, 3)) +
  scale_x_continuous(breaks = seq(1.0, 3.0, by = 0.5),
                     labels = label_number(accuracy = 0.1),
                     trans = "reverse") +  
  geom_curve(aes(x = 3, y = 1.5, xend = 2.5, yend = 1), 
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 2, xend = 2, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 2.5, xend = 1.5, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 3, xend = 1, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 3.5, xend = 0.5, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_errorbar(aes(x = avg_p_score_mean, ymin = avg_s_score_mean - avg_s_score_se, ymax = avg_s_score_mean + avg_s_score_se), 
                data = ggs, 
                width = 0.05, # Adjust width as needed
                color = "black") + 
  geom_errorbarh(aes(y = avg_s_score_mean, xmin = avg_p_score_mean - avg_p_score_se, xmax = avg_p_score_mean + avg_p_score_se), 
                 data = ggs, 
                 height = 0.05, # Adjust height as needed
                 color = "black") +
  add_phylopic(x = ggs$avg_p_score_mean, y = ggs$avg_s_score_mean, uuid = as.character(ggs$phylopic_uuid), alpha = 1, ysize = 0.05, 
               fill = "gray", color = "black") +
  geom_text_repel(aes(x = avg_p_score_mean, y = avg_s_score_mean, label = Genus, color = n_stocks), 
                  data = ggs, 
                  size = 3,
                  box.padding = 0.25, 
                  point.padding = 0.25, 
                  vjust = 2,
                  max.overlaps = Inf,
                  force = T) +
  scale_color_gradientn(colors = c("darkblue","darkgreen","darkgreen", "gold","gold","orange","orange","red","red","red"), name = "n 
stocks") 

#ggsave(filename = 'PSA_NMFS_fish_images_by_genus.png',plot = last_plot() , path = figures, width = 9.5, height = 8.5, device = 'png', dpi = 300)


#for families no labels color points
#with error bars
ggplot(fgs) + 
  theme_minimal() + 
  labs(x = "Productivity", y = "Risk") +
  coord_cartesian(xlim = c(3, 1), ylim = c(1, 3)) +
  scale_x_continuous(breaks = seq(1.0, 3.0, by = 0.5),
                     labels = label_number(accuracy = 0.1),
                     trans = "reverse") + 
  geom_curve(aes(x = 3, y = 1.5, xend = 2.5, yend = 1), 
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 2, xend = 2, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 2.5, xend = 1.5, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 3, xend = 1, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 3.5, xend = 0.5, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_errorbar(aes(x = avg_p_score_mean, ymin = avg_s_score_mean - avg_s_score_se, ymax = avg_s_score_mean + avg_s_score_se), 
                data = fgs, 
                width = 0.05, # Adjust width as needed
                color = "black") + 
  geom_errorbarh(aes(y = avg_s_score_mean, xmin = avg_p_score_mean - avg_p_score_se, xmax = avg_p_score_mean + avg_p_score_se), 
                 data = fgs, 
                 height = 0.05, # Adjust height as needed
                 color = "black") +
  add_phylopic(x = fgs$avg_p_score_mean, y = fgs$avg_s_score_mean, uuid = as.character(fgs$phylopic_uuid), alpha = 1, ysize = 0.07, 
               fill = "gray", color = "black") +
  geom_point(aes(x = avg_p_score_mean, y = avg_s_score_mean, color = n_stocks),
             data = fgs,
             size = 3) +
  # geom_text_repel(aes(x = avg_p_score_mean, y = avg_s_score_mean, label = family, color = n_stocks), 
  #                 data = fgs, 
  #                 vjust = 2, 
  #                 size = 4, 
  #                 box.padding = 0.25, 
  #                 point.padding = 0.25, 
  #                 max.overlaps = Inf,
  #                 force = T) +
  scale_color_gradientn(colors = c("darkblue","darkgreen","darkgreen", "gold","gold","orange","orange","red","red","red"), name = "n
stocks")

#ggsave(filename = 'PSA_NMFS_fish_images_by_family_no_label_points.png',plot = last_plot() , path = figures, width = 9.5, height = 6.5, device = 'png', dpi = 300)

#for families no labels
#with error bars
ggplot(fgs) + 
  theme_minimal() + 
  labs(x = "Productivity", y = "Risk") +
  coord_cartesian(xlim = c(3, 1), ylim = c(1, 3)) +
  scale_x_continuous(breaks = seq(1.0, 3.0, by = 0.5),
                     labels = label_number(accuracy = 0.1),
                     trans = "reverse") + 
  geom_curve(aes(x = 3, y = 1.5, xend = 2.5, yend = 1), 
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 2, xend = 2, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 2.5, xend = 1.5, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 3, xend = 1, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 3.5, xend = 0.5, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_errorbar(aes(x = avg_p_score_mean, ymin = avg_s_score_mean - avg_s_score_se, ymax = avg_s_score_mean + avg_s_score_se), 
                data = fgs, 
                width = 0.05, # Adjust width as needed
                color = "black") + 
  geom_errorbarh(aes(y = avg_s_score_mean, xmin = avg_p_score_mean - avg_p_score_se, xmax = avg_p_score_mean + avg_p_score_se), 
                 data = fgs, 
                 height = 0.05, # Adjust height as needed
                 color = "black") +
  add_phylopic(x = fgs$avg_p_score_mean, y = fgs$avg_s_score_mean, uuid = as.character(fgs$phylopic_uuid), alpha = 1, ysize = 0.07, 
               fill = "gray", color = "black") #+
# geom_point(aes(x = avg_p_score_mean, y = avg_s_score_mean, color = n_stocks),
#            data = fgs,
#            size = 3) +
# geom_text_repel(aes(x = avg_p_score_mean, y = avg_s_score_mean, label = family, color = n_stocks), 
#                 data = fgs, 
#                 vjust = 2, 
#                 size = 4, 
#                 box.padding = 0.25, 
#                 point.padding = 0.25, 
#                 max.overlaps = Inf,
#                 force = T) +
#   scale_color_gradientn(colors = c("darkblue","darkgreen","darkgreen", "gold","gold","orange","orange","red","red","red"), name = "n
# stocks")

#ggsave(filename = 'PSA_NMFS_fish_images_by_family_no_label.png',plot = last_plot() , path = figures, width = 9.5, height = 6.5, device = 'png', dpi = 300)


#for families images
#with error bars
ggplot(fgs) + 
  theme_minimal() + 
  labs(x = "Productivity", y = "Risk") +
  coord_cartesian(xlim = c(3, 1), ylim = c(1, 3)) +
  scale_x_continuous(breaks = seq(1.0, 3.0, by = 0.5),
                     labels = label_number(accuracy = 0.1),
                     trans = "reverse") + 
  geom_curve(aes(x = 3, y = 1.5, xend = 2.5, yend = 1), 
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 2, xend = 2, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 2.5, xend = 1.5, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 3, xend = 1, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 3.5, xend = 0.5, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_errorbar(aes(x = avg_p_score_mean, ymin = avg_s_score_mean - avg_s_score_se, ymax = avg_s_score_mean + avg_s_score_se), 
                data = fgs, 
                width = 0.05,
                color = "black") + 
  geom_errorbarh(aes(y = avg_s_score_mean, xmin = avg_p_score_mean - avg_p_score_se, xmax = avg_p_score_mean + avg_p_score_se), 
                 data = fgs, 
                 height = 0.05,
                 color = "black") +
  geom_point(aes(x = avg_p_score_mean, y = avg_s_score_mean, color = n_stocks), 
             data = fgs, 
             size = 3) + 
  geom_text_repel(aes(x = avg_p_score_mean, y = avg_s_score_mean, label = Family, color = n_stocks),
                  data = fgs,
                  vjust = 2,
                  size = 4,
                  box.padding = 0.25,
                  point.padding = 0.25,
                  max.overlaps = Inf,
                  force = T) +
  scale_color_gradientn(colors = c("darkblue","darkgreen","darkgreen", "gold","gold","orange","orange","red","red","red"), name = "n stocks")

#ggsave(filename = 'PSA_NMFS_fish_images_by_family_no_images.png',plot = last_plot() , path = figures, width = 9.5, height = 6.5, device = 'png', dpi = 300)

#survey plots ######################################################################################################
#all surveys
#color by n in the future
ggplot(survey_grouped_scores, aes(x= p_score, y= s_score,label = survey)) + 
  coord_cartesian(xlim = c(3, 1), ylim = c(1, 3)) +
  scale_x_continuous(breaks = seq(1.0, 3.0, by = 0.5),
                     labels = label_number(accuracy = 0.1),
                     trans = "reverse") + 
  geom_curve(aes(x = 3, y = 1.5, xend = 2.5, yend = 1), 
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 2, xend = 2, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 2.5, xend = 1.5, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 3, xend = 1, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 3.5, xend = 0.5, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_point(stat = "identity") + 
  theme_minimal() + labs(x = "Productivity", y = "Risk") +
  geom_label_repel(aes(x = p_score, 
                       y = s_score, 
                       label = survey), 
                   box.padding = 1.5, 
                   point.padding = 0, 
                   max.overlaps = 200, vjust = 0.5, hjust = 0.5, size = 1.5)

#ggsave(filename = 'PSA_surveys_FishLife_extended_variables.png',plot = last_plot() , path = figures, width = 9.5, height = 6.5, device = 'tiff', dpi = 300)

#by type
ggplot(scores_survey_type, aes(x= avg_p_score_mean, y= avg_s_score_mean,label = type)) +
  coord_cartesian(xlim = c(3, 1), ylim = c(1, 3)) +
  scale_x_continuous(breaks = seq(1.0, 3.0, by = 0.5),
                     labels = label_number(accuracy = 0.1),
                     trans = "reverse") + 
  geom_curve(aes(x = 3, y = 1.5, xend = 2.5, yend = 1), 
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 2, xend = 2, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 2.5, xend = 1.5, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 3, xend = 1, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 3.5, xend = 0.5, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_point(stat = "identity") + 
  theme_minimal() + labs(x = "Productivity", y = "Risk") +
  geom_errorbar(aes(x = avg_p_score_mean, ymin = avg_s_score_mean - avg_s_score_se, ymax = avg_s_score_mean + avg_s_score_se), 
                data = scores_survey_type, 
                width = 0.05, # Adjust width as needed
                color = "black") + 
  geom_errorbarh(aes(y = avg_s_score_mean, xmin = avg_p_score_mean - avg_p_score_se, xmax = avg_p_score_mean + avg_p_score_se), 
                 data = scores_survey_type, 
                 height = 0.05, # Adjust height as needed
                 color = "black") +
  geom_label_repel(aes(label = type, color = n_stocks), 
                   box.padding = 1.5, 
                   point.padding = 0, 
                   max.overlaps = 200, vjust = 0.5, hjust = 0.5, size = 5) +
  scale_color_gradientn(colors = c("darkblue","darkgreen","darkgreen", "gold","gold","orange","orange","red","red","red"), name = "n 
stocks") 

#ggsave(filename = 'PSA_plot_survey_type.png',plot = last_plot() , path = figures, width = 9.5, height = 8.5, device = 'png', dpi = 300)


#by region
#change to gulf of america
region_grouped_scores$Survey.Ecosystem[region_grouped_scores$Survey.Ecosystem == "Gulf of Mexico"]<- "Gulf of America"

#plot
ggplot(region_grouped_scores, aes(x= avg_p_score_mean, y= avg_s_score_mean,label = Survey.Ecosystem)) +
  coord_cartesian(xlim = c(3, 1), ylim = c(1, 3)) +
  scale_x_continuous(breaks = seq(1.0, 3.0, by = 0.5),
                     labels = label_number(accuracy = 0.1),
                     trans = "reverse") + 
  geom_curve(aes(x = 3, y = 1.5, xend = 2.5, yend = 1), 
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 2, xend = 2, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 2.5, xend = 1.5, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 3, xend = 1, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 3.5, xend = 0.5, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_point(stat = "identity") + 
  theme_minimal() + labs(x = "Productivity", y = "Risk") +
  geom_errorbar(aes(x = avg_p_score_mean, ymin = avg_s_score_mean - avg_s_score_se, ymax = avg_s_score_mean + avg_s_score_se), 
                data = region_grouped_scores, 
                width = 0.05, # Adjust width as needed
                color = "black") + 
  geom_errorbarh(aes(y = avg_s_score_mean, xmin = avg_p_score_mean - avg_p_score_se, xmax = avg_p_score_mean + avg_p_score_se), 
                 data = region_grouped_scores, 
                 height = 0.05, # Adjust height as needed
                 color = "black") +
  geom_label_repel(aes(label = Survey.Ecosystem, color = n_stocks), 
                   box.padding = 1.5, 
                   point.padding = 0, 
                   max.overlaps = 200, vjust = 0.5, hjust = 0.5, size = 5) +
  scale_color_gradientn(colors = c("darkblue","darkgreen","darkgreen", "gold","gold","orange","orange","red","red","red"), name = "n 
stocks") 

#ggsave(filename = 'PSA_plot_survey_region.png',plot = last_plot() , path = figures, width = 9.5, height = 8.5, device = 'png', dpi = 300)

#by type and region
ggplot(scores_survey_type_region, aes(x= avg_p_score_mean, y= avg_s_score_mean,label = type_and_region)) +
  coord_cartesian(xlim = c(3, 1), ylim = c(1, 3)) +
  scale_x_continuous(breaks = seq(1.0, 3.0, by = 0.5),
                     labels = label_number(accuracy = 0.1),
                     trans = "reverse") + 
  geom_curve(aes(x = 3, y = 1.5, xend = 2.5, yend = 1), 
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 2, xend = 2, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 2.5, xend = 1.5, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 3, xend = 1, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  geom_curve(aes(x = 3, y = 3.5, xend = 0.5, yend = 1),
             color = "red", linetype = "dashed", size = 1, curvature = -0.3) +
  theme_minimal() + labs(x = "Productivity", y = "Risk") +
  geom_errorbar(aes(x = avg_p_score_mean, ymin = avg_s_score_mean - avg_s_score_se, ymax = avg_s_score_mean + avg_s_score_se), 
                data = scores_survey_type_region, 
                width = 0.05, # Adjust width as needed
                color = "black") + 
  geom_errorbarh(aes(y = avg_s_score_mean, xmin = avg_p_score_mean - avg_p_score_se, xmax = avg_p_score_mean + avg_p_score_se), 
                 data = scores_survey_type_region, 
                 height = 0.05, # Adjust height as needed
                 color = "black") +
  geom_point(aes(x = avg_p_score_mean, y = avg_s_score_mean, color = n_stocks),
             data = scores_survey_type_region,
             size = 1.5) +
  geom_label_repel(aes(label = type_and_region, color = n_stocks), 
                   box.padding = 0.15,
                   point.padding = 0, 
                   max.overlaps = 200, vjust = 0.9, hjust = 0.9, size = 5) +
  scale_color_gradientn(colors = c("darkblue","darkgreen","darkgreen", "gold","gold","orange","orange",
                                   "red","red","red","red","red","red","red","red","red","red","red"),
                        name = "n 
stocks") 

#ggsave(filename = 'PSA_plot_survey_type_region.png',plot = last_plot() , path = figures, width = 9.5, height = 8.5, device = 'png', dpi = 300)

