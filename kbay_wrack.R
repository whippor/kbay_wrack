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
all_update %>%
  mutate(MO = case_when(`SAMPLE MONTH` == 'Mar' ~ 3,
                        `SAMPLE MONTH` == 'Apr' ~ 4,
                        `SAMPLE MONTH` == 'May' ~ 5,
                        `SAMPLE MONTH` == 'Jun' ~ 6,
                        `SAMPLE MONTH` == 'Jul' ~ 7,
                        `SAMPLE MONTH` == 'Aug' ~ 8,
                        `SAMPLE MONTH` == 'Sep' ~ 9,
                        `SAMPLE MONTH` == 'Oct' ~ 10)) %>%
ggplot(aes(x = MO, y = `WRACK AREA (m^2)`,
                       color = SITE)) +
  geom_point() +
  geom_smooth(method = "lm")

# calculate mean wrack surface area
wrackData %>%
  group_by(Site, Transect) %>%
  summarise(mean_width = mean(wrackWidth)) %>%
  mutate(area = 20 * ((mean_width * 0.01) * 50)) %>%
  ggplot() +
  geom_boxplot(aes(x = Site, y = area)) 


# Bishops only mean wrack area - old data

old_data %>%
  filter(SITE == "BP") %>%
  mutate(MO = case_when(`SAMPLE MONTH` == 'Mar' ~ 3,
                        `SAMPLE MONTH` == 'Apr' ~ 4,
                        `SAMPLE MONTH` == 'May' ~ 5,
                        `SAMPLE MONTH` == 'Jun' ~ 6,
                        `SAMPLE MONTH` == 'Jul' ~ 7,
                        `SAMPLE MONTH` == 'Aug' ~ 8,
                        `SAMPLE MONTH` == 'Sep' ~ 9,
                        `SAMPLE MONTH` == 'Oct' ~ 10)) %>%
  pivot_longer(`WRACK WIDTH (m) #1`:`WRACK WIDTH (m) #10`,
               names_to = "Quadrat",
               values_to = "Area") %>%
  select(MO, SITE, Quadrat, Area) %>%
  ggplot(aes(x = MO, y = Area)) +
  geom_point() +
  geom_smooth(method = "lm")

############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####