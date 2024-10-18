#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# KBay Wrack Analyses                                                            ##
# Data are current as of 2024-10-17                                              ##
# Data source: Ross Whippo/NOAA Edwin Viramontes SBB                             ##
# R code prepared by Ross Whippo                                                 ##  
# Last updated 2024-10-17                                                        ##
#                                                                                ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:


# Required Files (check that script is loading latest version):
# wrackData.csv

# Associated Scripts:
# FILE.R

# TO DO 

# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# TABLE OF CONTENTS                                                            ####
#                                                                                 +
# RECENT CHANGES TO SCRIPT                                                        +
# LOAD PACKAGES                                                                   +
# READ IN AND PREPARE DATA                                                        +
# MANIPULATE DATA                                                                 +
#                                                                                 +
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# RECENT CHANGES TO SCRIPT                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# LOAD PACKAGES                                                                ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)
library(viridis)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

wrackData <- read_csv("data/wrackData.csv") # current

old_data <- read_csv("data/2021_otg_wrack_sa_NORTH.csv") 

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                              ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

updated_old <- old_data %>%
  select(`SAMPLE MONTH`, SITE, `WRACK AREA (m^2)`) %>%
  mutate(SITE = case_when(SITE == "BB" ~ "Bishops",
                          SITE == "BP" ~ "diamondCreek")) %>%
  filter(SITE %in% c("Bishops", "diamondCreek"))

updated_new <- wrackData %>%
  group_by(Site) %>%
  summarise(mean_width = mean(wrackWidth)) %>%
  mutate(`WRACK AREA (m^2)` = (mean_width * 0.01) * 50) %>%
  mutate(`SAMPLE MONTH` = "Oct") %>%
  mutate(SITE = Site) %>%
  select(`SAMPLE MONTH`, `SITE`, `WRACK AREA (m^2)`)

all_update <- updated_old %>%
  bind_rows(updated_new) %>%
  mutate(`SAMPLE MONTH` = factor(`SAMPLE MONTH`, 
                                 levels = c("Mar",
                                            "Apr",
                                            "May",
                                            "Jun",
                                            "Jul",
                                            "Aug",
                                            "Sep",
                                            "Oct")))

ggplot(all_update) +
  geom_point(aes(x = `SAMPLE MONTH`, y = `WRACK AREA (m^2)`,
             color = SITE))

# calculate mean wrack surface area
wrackData %>%
  group_by(Site, Transect) %>%
  summarise(mean_width = mean(wrackWidth)) %>%
  mutate(area = 20 * ((mean_width * 0.01) * 50)) %>%
  ggplot() +
  geom_boxplot(aes(x = Site, y = area)) 

############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####