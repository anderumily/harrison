# -------------------------------------------------------------------------
# Date created:      2021-11-23
# Author:            E. Anderson
#'Description:       
# -------------------------------------------------------------------------


# set up ------------------------------------------------------------------

setwd("C:/Users/andersone/Documents/Projects Local/Stage Fall Local/harrison")

# load libraries
library(tidyhydat)
library(fasstr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

# figure width and height
w <- 8
h <- 6

# load lake level data ----------------------------------------------------

lake <- read.csv("data/Stage.Working@08MG012.EntireRecord.csv",
                 stringsAsFactors = FALSE, skip = 14) %>% #1362629 obs
    # rename local time
    rename(raw_datetime = Timestamp..UTC.08.00.) %>%
    # select variables
    select(raw_datetime, Value) %>% 
    # create datetime and actual stage based on CGVD conversion
    mutate(datetime = ymd_hms(raw_datetime),
           actual_stage = Value + 0.148,
           datetime_rounded = round_date(ymd_hms(raw_datetime), "15 minutes")) %>% 
    # group by 15 minute data
    group_by(datetime_rounded) %>% 
    # get 15 minute data
    summarise(stage_15 = mean(actual_stage)) %>% 
    ungroup()

# rename
names(lake) <- c("datetime", "stage")

# summary
summary(lake)

# plot
ggplot(lake, aes(x = datetime, y = stage)) + 
    geom_line() + 
    theme_bw() + 
    labs(x = "Year", y = "Actual Stage (m)", title = "08MG012 Harrison Lake near Harrison Hot Springs")
ggsave("output/levels analysis/08MG012 15 min level.png", width = w, height = h)

# load harrison at morris level data --------------------------------------

harrison_morris <- read.csv("data/Stage.Working@08MG022.EntireRecord.csv",
                         stringsAsFactors = FALSE, skip = 14) %>% 
    # rename local time
    rename(raw_datetime = Timestamp..UTC.08.00.) %>%
    # select variables
    select(raw_datetime, Value) %>% 
    # create datetime and actual stage based on CGVD conversion
    mutate(datetime = ymd_hms(raw_datetime),
           actual_stage = Value + 0.161,
           datetime_rounded = round_date(ymd_hms(raw_datetime), "15 minutes")) %>% 
    # group by 15 minute data
    group_by(datetime_rounded) %>% 
    # get 15 minute data
    summarise(stage_15 = mean(actual_stage)) %>% 
    # ungroup
    ungroup()

# rename
names(harrison_morris) <- c("datetime", "stage")

# summary
summary(harrison_morris)

# plot
ggplot(harrison_morris, aes(x = datetime, y = stage)) + 
    geom_line() + 
    theme_bw() + 
    labs(x = "Year", y = "Actual Stage (m)", title = "08MG022 Harrison River below Morris Creek")
ggsave("output/levels analysis/08MG022 15 min level.png", width = w, height = h)


# load harrison at harrison mills level data ------------------------------

harrison_mills <- read.csv("data/Stage.Working@08MG014.EntireRecord.csv",
                        stringsAsFactors = FALSE, skip = 14) %>% 
    # rename local time
    rename(raw_datetime = Timestamp..UTC.08.00.) %>%
    # select variables
    select(raw_datetime, Value) %>% 
    # create datetime and actual stage based on CGVD conversion
    mutate(datetime = ymd_hms(raw_datetime),
           actual_stage = Value - 0.144,
           datetime_rounded = round_date(ymd_hms(raw_datetime), "15 minutes")) %>% 
    # group by 15 minute data
    group_by(datetime_rounded) %>% 
    # get 15 minute data
    summarise(stage_15 = mean(actual_stage)) %>% 
    # ungroup
    ungroup()

# rename
names(harrison_mills) <- c("datetime", "stage")

# summary
summary(harrison_mills)

# plot
ggplot(harrison_mills, aes(x = datetime, y = stage)) + 
    geom_line() + 
    theme_bw() + 
    labs(x = "Year", y = "Actual Stage (m)", title = "08MG014 Harrison River at Harrison Mills")
ggsave("output/levels analysis/08MG014 15 minute level.png", width = w, height = h)

# load fraser river at harrison mills level data --------------------------

fraser_at_harrison <- read.csv("data/Stage.Working@08MF073.EntireRecord.csv",
                        stringsAsFactors = FALSE, skip = 14) %>% 
    # rename local time
    rename(raw_datetime = Timestamp..UTC.08.00.) %>%
    # select variables
    select(raw_datetime, Value) %>% 
    # create datetime and actual stage based on CGVD conversion
    mutate(datetime = ymd_hms(raw_datetime),
           actual_stage = Value - 0.136,
           datetime_rounded = round_date(ymd_hms(raw_datetime), "15 minutes")) %>% 
    # group by 15 minute data
    group_by(datetime_rounded) %>% 
    # get 15 minute data
    summarise(stage_15 = mean(actual_stage)) %>% 
    # ungroup
    ungroup()

# rename
names(fraser_at_harrison) <- c("datetime", "stage")

# summary
summary(fraser_at_harrison)

# plot
ggplot(fraser_at_harrison, aes(x = datetime, y = stage)) + 
    geom_line() + 
    theme_bw() + 
    labs(x = "Year", y = "Actual Stage (m)", title = "08MF073 Fraser River at Harrison Mills")
ggsave("output/levels analysis/08MF073 15 minute level.png", width = w, height = h)


# load field visit table --------------------------------------------------

# get mmnts from FieldVisitExport.csv - retreived from Regan's python export tool
aquarius_mmnts <- read.csv("data/FieldVisitExport.csv",
                           stringsAsFactors = FALSE) %>%
    # format datetime
    mutate(datetime = as.POSIXct(Date)) %>%
    # round datetime
    mutate(datetime_rounded = round_date(ymd_hms(datetime), "15 minutes")) %>% 
    # rename variables
    rename(q = Discharge) %>% 
    #select variables of interest
    select(datetime_rounded, q)

names(aquarius_mmnts) <- c("datetime", "q")

# fraser vs harrison at harrison mills ------------------------------------

# what is the difference between fraser at harrison and harrison at harrison mills?

confluence_comparison <- merge(fraser_at_harrison, harrison_mills, by = "datetime") %>% 
    rename(fraser_stage = stage.x,
           harrison_mills_stage = stage.y) %>% 
    mutate(difference = harrison_mills_stage - fraser_stage)

summary(confluence_comparison)

ggplot(confluence_comparison, aes(x = datetime, y = difference)) + 
    geom_point() + 
    theme_bw() + 
    labs(x = "Year", y = "Difference in Stage (m)", title = "Difference between Harrison River at Harrison Mills and Fraser River at Harrison Mills")
ggsave("output/levels analysis/Difference between Harrison and Fraser at confluence.png", width = w, height = h)

# lake to harrison mills differential -------------------------------------

lake_to_mills_comparison <- merge(lake, harrison_mills, by = "datetime") %>% 
    rename(lake_stage = stage.x,
            harrison_mills_stage = stage.y) %>% 
    mutate(difference = lake_stage - harrison_mills_stage)

summary(lake_to_mills_comparison)

ggplot(lake_to_mills_comparison, aes(x = datetime, y = difference)) + 
    geom_line() + 
    theme_bw() + 
    labs(x = "Year", y = "Difference in Stage (m)", title = "Difference between Harrison Lake and Harrison River at Harrison Mills")
ggsave("output/levels analysis/Difference between Harrison Lake and Harrison River at Harrison Mills.png", width = w, height = h)

# overlay measurements
aquarius_mmnts <- aquarius_mmnts %>% 
    mutate(q_scaled = q/200)

lake_to_mills_comparison_with_mmnts <- 
    lake_to_mills_comparison %>% 
    select(datetime, difference) %>% 
    full_join(aquarius_mmnts) %>% 
    select(datetime, difference, q_scaled) %>% 
    pivot_longer(cols = c("difference","q_scaled"),
                 names_to = "variable",
                 values_to = "value") %>% 
    filter(datetime >= as.POSIXct("2013-03-18 00:00")) %>% 
    drop_na()

ggplot(lake_to_mills_comparison_with_mmnts, aes(x = datetime, y = value)) + 
    geom_point(aes(colour = variable)) + 
    scale_colour_manual(values = c("grey", "black")) +
    theme_bw() + 
    labs(x = "Year", y = "Difference in Stage (m) and Q/200 (cms/200)", 
         title = "Difference between Harrison Lake and Harrison River at Harrison Mills plus Discrete Measurements Scaled")

# difference vs measurements ----------------------------------------------

# START HERE change this analysis to Fall vs Measured Discharge for more points 
# (Harrison at Mills only starts in Aq in 2013)

differential_vs_mmnts <- full_join(lake_to_mills_comparison, aquarius_mmnts) %>% 
    drop_na()

# basic
ggplot(differential_vs_mmnts, aes(x = q, y = difference)) +
    geom_point() + 
    labs(y = "Difference in stage between Harrison Lake \nand Harrison River at Harrison Mills (m)",
         x = "Measured Discharge Harrison River near Harrison Hot Springs (cms)\n YEAR TO YEAR") + 
    theme_bw()

# according to lake stage
ggplot(differential_vs_mmnts, aes(x = q, y = difference, size = lake_stage, colour = lake_stage)) +
    geom_point() + 
    labs(y = "Difference in stage between Harrison Lake \nand Harrison River at Harrison Mills (m)",
         x = "Measured Discharge Harrison River near Harrison Hot Springs (cms)\n YEAR TO YEAR") + 
    theme_bw()

# according to date of measurement
ggplot(differential_vs_mmnts, aes(x = q, y = difference, size = datetime)) +
    geom_point() + 
    labs(y = "Difference in stage between Harrison Lake \nand Harrison River at Harrison Mills (m)",
         x = "Measured Discharge Harrison River near Harrison Hot Springs (cms)\n YEAR TO YEAR") + 
    theme_bw()

# according to which type of measurement

# two slope sections ------------------------------------------------------

all_levels <- full_join(lake, harrison_morris, by = "datetime") %>% 
    full_join(harrison_mills, by = "datetime") %>% 
    full_join(fraser_at_harrison, by = "datetime") %>% 
    # filter for common timespan
    filter(datetime >= as.POSIXct("2013-03-18")) %>% 
    # only keep records where we have all 3 stations
    drop_na()

# rename
names(all_levels) <- c("datetime", "lake", "harrison_morris", "harrison_mills", "fraser_at_harrison")

# pivot longer
all_levels <- all_levels %>% 
    pivot_longer(cols = c("lake", "harrison_morris", "harrison_mills", "fraser_at_harrison"), 
                 names_to = "station", 
                 values_to = "stage")

# add km markers for each station
all_levels$distance <- -999
all_levels$distance <- ifelse(all_levels$station == "lake", 0, all_levels$distance)
all_levels$distance <- ifelse(all_levels$station == "harrison_morris", 8.4, all_levels$distance)
all_levels$distance <- ifelse(all_levels$station == "harrison_mills", 16.4, all_levels$distance)
all_levels$distance <- ifelse(all_levels$station == "fraser_at_harrison", 19, all_levels$distance)

all_levels_doy <- all_levels %>% 
    mutate(doy = yday(datetime)) %>% 
    group_by(doy, distance) %>% 
    summarise(stage_daily = mean(stage)) %>% 
    drop_na()

ggplot(all_levels_doy, aes(x = distance, y = stage_daily, colour = factor(doy))) + 
    geom_point() + 
    geom_line() + 
    theme_bw() + 
    labs(x = "Distance Downstream from Harrison Lake (km)", y = "Daily Stage (m)")

all_levels_week <- all_levels %>% 
    mutate(week = week(datetime)) %>% 
    group_by(week, distance) %>% 
    summarise(stage_weekly = mean(stage)) %>% 
    drop_na()

ggplot(all_levels_week, aes(x = distance, y = stage_weekly, colour = factor(week))) + 
    geom_point() + 
    geom_line() + 
    theme_bw() + 
    labs(x = "Distance Downstream from Harrison Lake (km)", y = "Weekly Stage (m)")


# harrison lake and morris only
lake_to_morris <- full_join(lake, harrison_morris, by = "datetime")

names(lake_to_morris) <- c("datetime", "lake", "harrison_morris")

# pivot longer
lake_to_morris <- lake_to_morris %>% 
    pivot_longer(cols = c("lake", "harrison_morris"), 
                 names_to = "station", 
                 values_to = "stage")

# add km markers for each station
lake_to_morris$distance <- -999
lake_to_morris$distance <- ifelse(lake_to_morris$station == "lake", 0, lake_to_morris$distance)
lake_to_morris$distance <- ifelse(lake_to_morris$station == "harrison_morris", 8.4, lake_to_morris$distance)

lake_to_morris <- lake_to_morris %>% 
    mutate(doy = yday(datetime)) %>% 
    group_by(doy, distance) %>% 
    summarise(stage_daily = mean(stage))
    
# START HERE
ggplot(lake_to_morris, aes(x = distance, y = stage_daily, colour = factor(doy))) + 
    geom_point() + 
    geom_line() + 
    theme_bw() + 
    labs(x = "Distance Downstream from Harrison Lake (km)", y = "Daily Stage (m)")    


# rating curve ------------------------------------------------------------

# take differential_vs_mmnts

hq <- differential_vs_mmnts %>% 
    select(datetime, lake_stage, harrison_mills_stage, difference, q)


ggplot(hq, aes(x = q, y = lake_stage)) + 
    geom_point(aes(colour = difference)) + 
    theme_bw() + 
    labs(x = "Discharge (cms)",
         y = "Harrison Lake Stage (m)")
