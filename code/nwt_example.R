# -------------------------------------------------------------------------
# Date created:      2020-04-30
# Author:            E. Anderson
# Description:       Let's look at some NWT stations.
#   
# Required input(s): N/A
# Optional input(s): N/A
# Outputs:           N/A
# -------------------------------------------------------------------------


# set up ------------------------------------------------------------------

setwd("C:/Users/andersone/Documents/R/R_Resources")

# load libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyhydat)

# uncomment to download database
# download_hydat()

# keyboard shortcuts ------------------------------------------------------

# add a section: CTRL SHIFT R
# assign <- ALT DASH
# pipe CTRL SHIFT M %>% 
# run a line CTRL ENTER


# NWT data ----------------------------------------------------------------

search_stn_name("hay river near hay river")
search_stn_name("fort simpson")
search_stn_name("fort mcpherson")

nwt_nums <- c("07OB001", "10GC001", "10MC002")

nwt_names <- hy_stations(station_number = nwt_nums) %>% 
    pull(STATION_NAME)
    
# get historical data
nwt_hist <- hy_daily_flows(nwt_nums) %>% 
    select(STATION_NUMBER, Date, Value)

# add day of year
nwt_hist <- nwt_hist %>% 
    mutate(doy = yday(Date))

# calculate the median flow by doy
nwt_hist_median <- nwt_hist %>% 
    group_by(doy, STATION_NUMBER) %>% 
    summarise(median = median(Value, na.rm = TRUE))

# plot
ggplot(nwt_hist_median, aes(x = doy, y = median, colour = STATION_NUMBER)) + 
    geom_line() + 
    theme_bw() +
    facet_grid(rows = vars(STATION_NUMBER), scales = "free") +
    labs(
        title = "NWT Stations",
        x = "Day of Year",
        y = "Median discharge (cms)"
    )


# fort simpson real time --------------------------------------------------

search_stn_name("fort simpson")

# assign a station number
fs_num <- "10GC001"

fs_name <- hy_stations(fs_num) %>% 
    pull(STATION_NAME)

# get realtime data

fs_realtime <- realtime_dd(fs_num) %>% 
    realtime_add_local_datetime()
    
# plot it
ggplot(fs_realtime, aes(x = Date, y = Value)) + 
    geom_line()

# select flow parameter
fs_realtime_flow <- fs_realtime %>% 
    filter(Parameter == "Flow") %>% 
    select(local_datetime, Value)

# plot realtime
ggplot(fs_realtime_flow, aes(x = local_datetime, y = Value)) + 
    geom_line()

fs_realtime_flow_daily <- fs_realtime_flow %>% 
    mutate(doy = yday(local_datetime)) %>% 
    group_by(doy) %>% 
    summarise(Value = mean(Value))
    

# combine realtime and historical flows -----------------------------------

# filter nwt_hist_median for fort simpson
fs_median <- nwt_hist_median %>% 
    filter(STATION_NUMBER == fs_num)

# select columns
fs_median <- fs_median %>% 
    select(doy, median) %>% 
    rename(Value = median)

fs_realtime_flow_daily$type <- "Real time (2020)"
fs_median$type <- "Historical median"

fs_realtime_flow_daily
fs_median

# combine the dataframes
fs <- bind_rows(fs_realtime_flow_daily, fs_median)
fs

# plot
ggplot(fs, aes(doy, Value, colour = type, size = type)) +
    geom_line() +
    scale_color_manual(values = c("black", "red")) +
    scale_size_manual(values = c(1, 2)) +
    labs(
        title = fs_name,
        x = "Day of Year",
        y = "Discharge (cms)"
    ) + theme_bw()

ggsave("output/mac_river.png")
