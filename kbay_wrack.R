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
  mutate(areaKM = `WRACK AREA (m^2)`*20)
  filter(SITE %in% c("Bishops", "diamondCreek"))

updated_new <- wrackData %>%
  mutate(Mon = month(ymd(Date), 
                     label = TRUE,
                     abbr = TRUE)) %>%
  group_by(Site, Mon) %>%
  summarise(mean_width = mean(wrackWidth)) %>%
  mutate(`WRACK AREA (m^2)` = (mean_width * 0.01) * 50) %>%
  mutate(areaKM = `WRACK AREA (m^2)`*20) %>%
  mutate(SITE = Site) %>%
  ungroup() %>%
  select(-Site)

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
                                            "Oct",
                                            "Nov")))
all_update %>%
  mutate(MO = case_when(`SAMPLE MONTH` == 'Mar' ~ 3,
                        `SAMPLE MONTH` == 'Apr' ~ 4,
                        `SAMPLE MONTH` == 'May' ~ 5,
                        `SAMPLE MONTH` == 'Jun' ~ 6,
                        `SAMPLE MONTH` == 'Jul' ~ 7,
                        `SAMPLE MONTH` == 'Aug' ~ 8,
                        `SAMPLE MONTH` == 'Sep' ~ 9,
                        `SAMPLE MONTH` == 'Oct' ~ 10,
                        `SAMPLE MONTH` == 'Nov' ~ 11)) %>%
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

# Combine old and new mean and variation in single figure
datedOld <- old_data %>% 
  filter(SITE %in% c("BB", "BP")) %>%
  mutate(Date = ymd(rep(c("2024-03-15",
                          "2024-04-15",
                          "2024-05-15",
                          "2024-06-15",
                          "2024-07-15",
                          "2024-08-15",
                          "2024-09-15"), 2))) %>%
  pivot_longer(`WRACK WIDTH (m) #1`:`WRACK WIDTH (m) #10`,
               names_to = "meterPoint",
               values_to = "wrackWidth") %>%
  mutate(meterPoint = rep(c(5,10,15,20,25,30,35,40,45,50), 14)) %>%
  select(Date, SITE, meterPoint, wrackWidth) %>%
  mutate(SITE = case_when(SITE == "BB" ~ "Bishops",
                          SITE == "BP" ~ "diamondCreek")) %>%
  mutate(areaKM = wrackWidth*50*20) %>%
  mutate(Source = "Ulaski 2023")

datedNew <- wrackData %>%
  filter(Site %in% c("Bishops", "diamondCreek")) %>%
  mutate(Date = ymd(Date)) %>%
  select(Date, Site, meterPoint, wrackWidth) %>%
  mutate(wrackWidth = wrackWidth*0.01) %>%
  mutate(areaKM = wrackWidth*50*20) %>%
  mutate(SITE = Site) %>%
  ungroup() %>%
  select(-Site) %>%
  mutate(Source = "Viramontes 2024")

datedAll <- datedNew %>%
  bind_rows(datedOld) %>%
  mutate(Month = month(ymd(Date), label = TRUE, abbr = TRUE))

rect_df <- data.frame(start = seq(as.Date("2024-03-01"), 
                                  length.out = 10, by = "month"),
                      end = seq(as.Date("2024-04-01"), 
                                length.out = 10, by = "month"),
                      fill = c("grey80", "white"))

site_labels <- c('Bishops' =  "Bishop's Beach", 
                    'diamondCreek' = "Bluff Point")
datedAll %>%
  ggplot(aes(x = Date, y = areaKM, color = Source, group = Source)) +
  geom_rect(data = rect_df,
            aes(xmin = start, xmax = end, fill = fill, ymin = -Inf, ymax = Inf),
            alpha = 0.1, color = NA, inherit.aes = FALSE) +
  stat_summary(fun.data = "mean_se") +
  stat_summary(fun.y=mean, geom="line", aes(group = Source)) +
  scale_color_viridis(discrete = TRUE,
                      begin = 0.3,
                      end = 0.7) +
  scale_fill_grey() +
  theme_bw() +
  facet_wrap(.~SITE, ncol = 1, labeller = as_labeller(site_labels)) +
  guides(fill = FALSE) +
  labs(x = "Month", y = m^2~km^-1)


# Compare monthly variation in datasets
datedAll %>%
  filter(SITE == "Bishops") %>%
  group_by(Source, Month) %>%
  count(Source)
n_labels <- data.frame(sample = c("n = 50", "n = 10", "n = 60",
                         "n = 10", "n = 10", "n = 10",
                         "n = 20", "n = 10", "n = 10",
                         "n = 10"),
                       Source = c(seq(from = 0.5, to = 9.5, by = 1)))

datedAll %>%
  filter(SITE == "Bishops") %>%
  group_by(Month, Source) %>%
  summarise(Variance = sd(wrackWidth)) %>%
  unite(sourceMonth, c("Source", "Month"), sep = " - ", remove = FALSE) %>%
  arrange(desc(Variance)) %>%
  ungroup() %>%
  mutate(Sample = c("n = 50", "n = 10", "n = 60",
                    "n = 10", "n = 10", "n = 10",
                    "n = 20", "n = 10", "n = 10",
                    "n = 10")) %>%
  ggplot(aes(y = Variance, x = fct_rev(fct_reorder(sourceMonth, Variance)), fill = Source)) +
  geom_col() + 
  scale_fill_viridis(discrete = TRUE,
                     begin = 0.3,
                     end = 0.7) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(x = "Source - Month", y = Standard~Deviation~(m^2~m^-1)) + 
  geom_text(aes(label = Sample), nudge_y = 0.05)

# Temporal change in wrack composition at Bishops

bishops_comp <- wrackData %>%
  select(Site, Date, Transect, meterPoint, totalCover, algalComp) %>%
  filter(Site == "Bishops") %>%
  separate_wider_delim(algalComp, delim = "|", names = paste0("col", c(1:9)),
                       too_few = "align_start") %>%
  mutate(meterPoint = as.character(meterPoint)) %>%
  unite(Quadrat, Transect, meterPoint, sep = "_", remove = FALSE)

codes_only <- bishops_comp[,7:15]
result <- unlist(codes_only)
unique(result)
algae <- result %>%
  unique() 

extra_cols <- data.frame(matrix(nrow = nrow(bishops_comp),
                                ncol = length(algae)))
colnames(extra_cols) <- algae

bishops_comp_extra <- bishops_comp %>%
  bind_cols(extra_cols)

 
  
  library(dplyr) 
  match_columns <- function(data, value_cols, match_cols) { 
    # Convert to a tibble for easier column selection 
    data <- as_tibble(data) 
    # Select the value_cols and match_cols using tidyselect 
    value_cols <- select(data, {{ value_cols }}) %>% 
      colnames() 
    match_cols <- select(data, {{ match_cols }}) %>% 
      colnames() 
    # Iterate over each row 
    for (i in 1:nrow(data)) { 
      # Get the values from the specified columns for the current row 
      row_values <- as.character(unlist(data[i, value_cols]))
      # Filter row values to include only those that match valid match_cols 
      valid_matches <- row_values[row_values %in% match_cols] 
      # Update the match_cols with 1s and 0s 
      data[i, match_cols] <- as.list(as.numeric(match_cols %in% valid_matches)) 
    } 
    return(as.data.frame(data)) 
    # Return a dataframe 
    }



 new_bishops_comp <- match_columns(bishops_comp_extra, col1:col9, SAGR:PLGA)

    ############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####
df <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("name", "age", "gender")
colnames(df) <- x

