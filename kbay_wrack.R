#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                                                                                ##
# KBay Wrack Analyses                                                            ##
# Data are current as of 2024-12-10                                              ##
# Data source: Ross Whippo/NOAA Edwin Viramontes SBB                             ##
# R code prepared by Ross Whippo                                                 ##  
# Last updated 2024-12-10                                                        ##
#                                                                                ##
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# SUMMARY:


# Required Files (check that script is loading latest version):
# wrackData.csv
# 2021_otg_wrack_sa_NORTH.csv

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
library(vegan)
library(ggpubr)
library(psych)
library(corrr)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# READ IN AND PREPARE DATA                                                     ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

wrackData <- read_csv("data/wrackData.csv", 
                      col_types = cols(Date = col_date(format = "%m/%d/%Y"))) # current

oldData <- read_csv("data/2021_otg_wrack_sa_NORTH.csv") 

tides <- read_csv("data/CO-OPS_9455500_met.csv", 
                                        col_types = cols(Date = col_date(format = "%Y/%m/%d"), 
                                                         `Verified (m)` = col_double()))

tidesMinMax <- tides %>%
  na.exclude()

wind <- read_table("data/HomerWind.csv", 
                      col_types = cols(yr = col_character(), 
                                       mo = col_character(), dy = col_character(), 
                                       `m/s_1` = col_double()))
wind <- wind %>%
  mutate(Date = make_date(yr, mo, dy))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# MANIPULATE DATA                                                              ####
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

updated_old <- oldData %>%
  select(`SAMPLE MONTH`, SITE, `WRACK AREA (m^2)`) %>%
  mutate(SITE = case_when(SITE == "BB" ~ "Bishops",
                          SITE == "BP" ~ "diamondCreek")) %>%
  mutate(areaKM = `WRACK AREA (m^2)`*20) %>%
  filter(SITE %in% c("Bishops", "diamondCreek"))

updated_new <- wrackData %>%
  mutate(Mon = month(ymd(Date), 
                     label = TRUE,
                     abbr = TRUE)) %>%
  mutate(SITE = Site) %>%
  group_by(SITE, Mon) %>%
  summarise(mean_width = mean(wrackWidth)) %>%
  mutate(`WRACK AREA (m^2)` = (mean_width * 0.01) * 50) %>%
  mutate(areaKM = `WRACK AREA (m^2)`*20) %>%
  ungroup()

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

#wrackData %>%
#  ggplot() +
#  geom_boxplot(aes(x = Site, y = wrackWidth))

# Bishops only mean wrack area - old data

oldData %>%
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
datedOld <- oldData %>% 
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

#Bishops Only
monthlyFIG <- datedAll %>%
  filter(SITE == "Bishops") %>%
  ggplot(aes(x = Date, y = areaKM, color = Source, group = Source)) +
  geom_rect(data = rect_df,
            aes(xmin = start, xmax = end, fill = fill, ymin = -Inf, ymax = Inf),
            alpha = 0.1, color = NA, inherit.aes = FALSE) +
  #stat_summary(fun.data = "mean_se", size = 1) +
  stat_summary(fun.x = "mean", size = 1, color = "black", geom = "errorbar", width = 0) +
  stat_summary(geom = "point", size = 5) +
  stat_summary(fun.y=mean, geom="line", aes(group = Source), size = 1) +
  scale_color_viridis(discrete = TRUE,
                      begin = 0.3,
                      end = 0.7) +
  scale_fill_grey() +
  theme_bw() +
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

varianceFIG1 <- datedAll %>%
  filter(SITE == "Bishops") %>%
  group_by(Month, Source) %>%
  summarise(Variance = sd(wrackWidth)) %>%
  #reframe(Range = range(wrackWidth)) %>%
  group_by(Month, Source) %>%
  #reframe(Variance = max(Range) - min(Range)) %>%
  unite(sourceMonth, c("Source", "Month"), sep = " - ", remove = FALSE) %>%
  arrange(desc(Variance)) %>%
  ungroup() %>%
  mutate(Sample = c("n = 10", "n = 90", "n = 60",
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
  #labs(x = "Source - Month", y = Absolute~Range~(m^width)) + 
  labs(x = "Source - Month", y = Standard~Deviation~(wrack~width~m)) +
  geom_text(aes(label = Sample), nudge_y = 0.05)

# Overall variance between datasets
varianceFIG2 <- datedAll %>%
  filter(SITE == "Bishops") %>%
  group_by(Month, Source) %>%
  summarise(Variance = sd(wrackWidth)) %>%
  arrange(desc(Variance)) %>%
  ungroup() %>%
  ggplot(aes(y = Variance, x = Source, fill = Source)) +
  geom_boxplot() + 
  scale_fill_viridis(discrete = TRUE,
                     begin = 0.3,
                     end = 0.7) +
  theme_bw() +
  stat_summary(fun.y = "mean", group = "Source", geom="line", show.legend = FALSE) + 
  stat_summary(geom = "point", pch = 19, size = 5, show.legend = FALSE) +
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  #labs(x = "Source - Month", y = Absolute~Range~(m^width)) + 
  labs(x = NULL, y = Monthly~Variance~(SD~of~wrack~width~m))


ggarrange(varianceFIG2, monthlyFIG, widths = c(1,3), common.legend = TRUE)



# TIDES and Wind FIGURE

# extract mean gust
meanGust <- wind %>%
  group_by(Date) %>%
  summarise(meanGust = mean(`m/s_1`)) %>%
  na.exclude()
  
# extract max gust
maxGust <- wind %>%
  group_by(Date) %>%
  summarise(maxGust = max(`m/s_1`)) %>%
  na.exclude()

# extract mean wind
meanWind <- wind %>%
  group_by(Date) %>%
  summarise(`Mean Wind (m/s)` = mean(`m/s`)) %>%
  na.exclude()

# extract max wind
maxWind <- wind %>%
  group_by(Date) %>%
  summarise(maxWind = max(`m/s`)) %>%
  na.exclude()


tidesMinMax %>%
  ggplot() +
  geom_rect(data = meanWind, aes(xmin = Date,
                                xmax = Date + 1, 
                                ymin = -2, ymax = 10, 
                                fill = `Mean Wind (m/s)`),
           alpha = 0.25) +
  scale_fill_viridis(option = "D") +
  geom_line(aes(x = Date, y = `Verified (m)`), alpha = 0.6, linewidth = 3,
            color = "white") +
  theme_bw() +
  labs(x = "Month", y = Tidal~Height) +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Wrack Width (m)")) +
  geom_jitter(data = wrackData, aes(x = Date, y = wrackWidth/100)) +
  geom_smooth(data = wrackData, aes(x = Date, y = wrackWidth/100), 
              se = FALSE, color = "darkred") +
  coord_cartesian(ylim = c(-2, 7.5)) +
  scale_x_date(limits = as.Date(c('2024-09-18','2024-11-30'))) +
  ylab("Tidal Height (m)")

# Compare mean wind versus wrack width
WrackWindnull <- wrackData %>%
  select(Date, wrackWidth) %>%
  full_join(meanWind) %>%
  group_by(wrackWidth) %>%
  filter(is.na(wrackWidth)) %>%
  slice(rep(1:n(), each = 10))
WrackWindsample <- wrackData %>%
  select(Site, Date, wrackWidth) %>%
  full_join(meanWind)
 WrackWind <- WrackWindsample %>%
   full_join(WrackWindnull) %>%
   arrange(Date)

# same day
WrackWind %>%
  na.exclude() %>%
  ggplot() +
  geom_point(aes(x = `Mean Wind (m/s)`, y = wrackWidth, color = Site)) +
  scale_color_viridis(discrete = TRUE, option = "H",
                      begin = 0.2, end = 0.8) +
  geom_smooth(aes(x = `Mean Wind (m/s)`, y = wrackWidth),
              method = "lm") + 
  theme_bw()

# -1 day
WrackWind %>%
  mutate(minus1 = lag(`Mean Wind (m/s)`, 10)) %>%
  na.exclude() %>%
  ggplot() +
  geom_point(aes(x = minus1, y = wrackWidth, color = Site)) +
  scale_color_viridis(discrete = TRUE, option = "H",
                      begin = 0.2, end = 0.8) +
  geom_smooth(aes(x = minus1, y = wrackWidth),
              method = "lm") + 
  theme_bw()

# -2 day
WrackWind %>%
  mutate(minus2 = lag(`Mean Wind (m/s)`, 20)) %>%
  na.exclude() %>%
  filter(Site == "Bishops") %>%
  ggplot() +
  geom_point(aes(x = minus2, y = wrackWidth)) +
  scale_color_viridis(discrete = TRUE, option = "H",
                      begin = 0.2, end = 0.8) +
  geom_smooth(aes(x = minus2, y = wrackWidth),
              method = "lm") + 
  theme_bw() +
  xlab("Mean Wind Speed (m/s) - Two Day Offset") +
  ylab("Wrack Width (cm)") +
  annotate("text", label = "Pearson's Correlation = 0.433",
           x = 9, y = 450)

# -3 day
WrackWind %>%
  mutate(minus3 = lag(`Mean Wind (m/s)`, 30)) %>%
  na.exclude() %>%
  ggplot() +
  geom_point(aes(x = minus3, y = wrackWidth, color = Site)) +
  scale_color_viridis(discrete = TRUE, option = "H",
                      begin = 0.2, end = 0.8) +
  geom_smooth(aes(x = minus3, y = wrackWidth),
              method = "lm") + 
  theme_bw()

# -4 day
WrackWind %>%
  mutate(minus4 = lag(`Mean Wind (m/s)`, 40)) %>%
  na.exclude() %>%
  ggplot() +
  geom_point(aes(x = minus4, y = wrackWidth, color = Site)) +
  scale_color_viridis(discrete = TRUE, option = "H",
                      begin = 0.2, end = 0.8) +
  geom_smooth(aes(x = minus4, y = wrackWidth),
              method = "lm") + 
  theme_bw()

# -5 day
WrackWind %>%
  mutate(minus5 = lag(`Mean Wind (m/s)`, 50)) %>%
  na.exclude() %>%
  ggplot() +
  geom_point(aes(x = minus5, y = wrackWidth, color = Site)) +
  scale_color_viridis(discrete = TRUE, option = "H",
                      begin = 0.2, end = 0.8) +
  geom_smooth(aes(x = minus5, y = wrackWidth),
              method = "lm") + 
  theme_bw()

# -6 day
WrackWind %>%
  mutate(minus6 = lag(`Mean Wind (m/s)`, 60)) %>%
  na.exclude() %>%
  ggplot() +
  geom_point(aes(x = minus6, y = wrackWidth, color = Site)) +
  scale_color_viridis(discrete = TRUE, option = "H",
                      begin = 0.2, end = 0.8) +
  geom_smooth(aes(x = minus6, y = wrackWidth),
              method = "lm") + 
  theme_bw()

# -7 day
WrackWind %>%
  mutate(minus7 = lag(`Mean Wind (m/s)`, 70)) %>%
  na.exclude() %>%
  ggplot() +
  geom_point(aes(x = minus7, y = wrackWidth, color = Site)) +
  scale_color_viridis(discrete = TRUE, option = "H",
                      begin = 0.2, end = 0.8) +
  geom_smooth(aes(x = minus7, y = wrackWidth),
              method = "lm") + 
  theme_bw()

# -8 day
WrackWind %>%
  mutate(minus8 = lag(`Mean Wind (m/s)`, 80)) %>%
  na.exclude() %>%
  ggplot() +
  geom_point(aes(x = minus8, y = wrackWidth, color = Site)) +
  scale_color_viridis(discrete = TRUE, option = "H",
                      begin = 0.2, end = 0.8) +
  geom_smooth(aes(x = minus8, y = wrackWidth),
              method = "lm") + 
  theme_bw()

# -9 day
WrackWind %>%
  mutate(minus9 = lag(`Mean Wind (m/s)`, 90)) %>%
  na.exclude() %>%
  ggplot() +
  geom_point(aes(x = minus9, y = wrackWidth, color = Site)) +
  scale_color_viridis(discrete = TRUE, option = "H",
                      begin = 0.2, end = 0.8) +
  geom_smooth(aes(x = minus9, y = wrackWidth),
              method = "lm") + 
  theme_bw()

# -10 day
WrackWind %>%
  mutate(minus10 = lag(`Mean Wind (m/s)`, 100)) %>%
  na.exclude() %>%
  ggplot() +
  geom_point(aes(x = minus10, y = wrackWidth, color = Site)) +
  scale_color_viridis(discrete = TRUE, option = "H",
                      begin = 0.2, end = 0.8) +
  geom_smooth(aes(x = minus10, y = wrackWidth),
              method = "lm") + 
  theme_bw()

# correlation between lags and tides?
tideRange <- tidesMinMax %>%
  group_by(Date) %>%
  mutate(range = max(`Verified (m)`) - min(`Verified (m)`)) %>%
  select(Date, range) %>%
  unique()

WindTideWrack <- WrackWind %>%
  left_join(tideRange) 
  
params <- seq(10, 90, by = 10)

#set the column names, add leading zeroes based om max(params)
run_names <- paste0("minus", formatC(params, width = nchar(max(params)), flag = "0"))

#what functions to perform
lag_functions <- setNames(paste("dplyr::lag( ., ", params, ")"), run_names)
#perform functions 
OffsetAll <- WindTideWrack %>% 
  mutate_at(vars(`Mean Wind (m/s)`, range), funs_(lag_functions ))

OffsetAll %>%
  select(`Mean Wind (m/s)`:range_minus90) %>%
pairs.panels(stars = TRUE)

allCor <- OffsetAll %>%
  na.exclude() %>%
  select(`Mean Wind (m/s)`:range_minus90) %>%
  cor() %>%
  data.frame()

longCor <- allCor %>%
  rownames_to_column() %>%
  pivot_longer(`Mean.Wind..m.s.`:`range_minus90`) %>%
  filter(value != 1) %>%
  group_by(rowname, name)

# Wrack wide correlations
OffsetAll %>% 
  select(wrackWidth, `Mean Wind (m/s)`:range_minus90) %>%
  correlate() %>% 
  focus(wrackWidth) 



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

 # values only for PERMANOVA and nMDS
presabs_only <- new_bishops_comp %>%
   select(totalCover, SAGR:PLGA) 
tot <- rowSums(presabs_only)
presabs_only <- presabs_only[tot > 0, ]
 
presabs_mds <- metaMDS(presabs_only, distance = "jaccard")



# extract points from the nMDS for plotting
presabs_mds_points <- presabs_mds$points

# make plot points into a dataframe that ggplot2 can read
presabs_mds_points <- data.frame(presabs_mds_points)

# join plot points with taxonomy
plot_data_presabs <- data.frame(presabs_mds_points, new_bishops_comp[tot > 0, c(1,3,4)])

library(plyr)
chulls_tax <- ddply(plot_data_presabs, .(Transect), function(df) df[chull(df$MDS1, df$MDS2), ])
detach(package:plyr)

# all quads plot
plot_data_presabs %>%
  mutate(Transect = factor(Transect, levels = c("BB1",
                                                "BB2",
                                                "BB3",
                                                "BB4",
                                                "BB5",
                                                "BB6",
                                                "BB7",
                                                "BB8",
                                                "BB9",
                                                "BB10",
                                                "BB11",
                                                "BB12",
                                                "BB13"))) %>%
ggplot(aes(x=MDS1, y=MDS2, 
                          color = Transect)) +  
  labs(x = "nMDS1", y = "nMDS2") +
  theme_classic() + 
  geom_point(pch = 19, size = 4) + 
  scale_color_viridis(discrete = TRUE, option = "C", name = "transect") +
  geom_polygon(data=chulls_tax, aes(x=MDS1, y=MDS2, group=Transect), fill=NA)




# transect only analysis and plot
transect_only <- data.frame(presabs_only, new_bishops_comp[tot > 0, c(1,3,4)])
cover_means <- transect_only %>%
  group_by(Transect) %>%
  summarise(meanCover = mean(totalCover))
presabs_means <- transect_only %>%
  select(SAGR:Transect) %>%
  group_by(Transect) %>%
  summarise(across(SAGR:PLGA, ~ sum(.x)))
  
transect_only_means <- data.frame(presabs_means, cover_means) %>%
  select(-Transect.1)

tomatrix <- transect_only_means[c(1:3, 5:12),2:43]
transect_mds <- metaMDS(tomatrix, distance = "altGower")

# extract points from the nMDS for plotting
transect_mds_points <- transect_mds$points

# make plot points into a dataframe that ggplot2 can read
transect_mds_points <- data.frame(transect_mds_points)

Transect <- transect_only_means$Transect
Transect2 <- Transect[c(1:3, 5:12)]
# join plot points with taxonomy
plot_data_transect <- data.frame(transect_mds_points, Transect2)


# all quads plot
plot_data_transect %>%
  mutate(Transect = factor(Transect2, levels = c("BB1",
                                                "BB2",
                                                "BB3",
                                                "BB4",
                                                "BB5",
                                                "BB6",
                                                "BB7",
                                                "BB8",
                                                "BB9",
                                                "BB10",
                                                "BB11",
                                                "BB13"))) %>%
  ggplot(aes(x=MDS1, y=MDS2, 
             color = Transect)) +  
  labs(x = "nMDS1", y = "nMDS2") +
  theme_classic() + 
  geom_point(pch = 19, size = 4) + 
  scale_color_viridis(discrete = TRUE, option = "C", name = "transect")

    ############### SUBSECTION HERE

####
#<<<<<<<<<<<<<<<<<<<<<<<<<<END OF SCRIPT>>>>>>>>>>>>>>>>>>>>>>>>#

# SCRATCH PAD ####
df <- data.frame(matrix(ncol = 3, nrow = 0))
x <- c("name", "age", "gender")
colnames(df) <- x





# Sample data

data <- data.frame(
  
  x = 1:10,
  
  primary_y = runif(10, 0, 10),
  
  secondary_y = runif(10, 0, 50)
  
)



# Create the plot with a secondary axis

ggplot(data, aes(x = x, y = primary_y)) +
  
  geom_line() +
  
  scale_y_continuous(sec.axis = sec_axis(~ . * 5, name = "Secondary Y"))
