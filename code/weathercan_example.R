# -------------------------------------------------------------------------
# Date created:      2020-04-22
# Author:            Emily Anderson
# Description:       Example using weathercan.
#
# Required input(s): N/A
# Optional input(s): N/A
# Outputs:           plot to output folder
# -------------------------------------------------------------------------


# set up ------------------------------------------------------------------

# set working directory to the R_Resources folder
setwd("C:/Users/andersone/Documents/R/R_Resources")

# uncomment if you need to install packages
# install.packages("weathercan")
# install.packages("ggplot2")
# install.packages("dplyr")

# load libraries
library(ggplot2)
library(dplyr)
library(weathercan)


# explore -----------------------------------------------------------------

help(package = "weathercan")

# look at some functions
?stations
?stations_dl
?stations_search

# all the stations
stations # dataframe 14 variables of 26,289 rows

# filter for sea level stations
stations %>%
  filter(elev <= 1)

# filter for current stations
stations %>%
  filter(elev <= 1) %>%
  filter(end == 2020)

# with daily data
stations %>%
  filter(elev <= 1) %>%
  filter(end == 2020) %>%
  filter(interval == "day")

# get station IDs
stations %>%
  filter(elev <= 1) %>%
  filter(end == 2020) %>%
  filter(interval == "day") %>%
  pull(station_id)

# assign these to an object
sea_level_stations <- stations %>%
  filter(elev <= 1) %>%
  filter(end == 2020) %>%
  filter(interval == "day") %>%
  pull(station_id)

# download data for 2 weeks
sea_level_data <- weather_dl(station_ids = sea_level_stations, interval = "day", start = "2020-01-01", end = "2020-01-15")

# open dataframe
View(sea_level_data)

# are these all sea level stations?
sea_level_data %>%
  group_by(prov) %>%
  summarise()

# plot
p <- ggplot(sea_level_data, aes(x = date, y = mean_temp, colour = station_name)) +
  geom_point() +
  geom_line() +
  labs(
    title = "Sea Level Temps for One Week",
    x = "Date",
    y = "Average Daily Temperature in Degrees C"
  ) +
  theme_bw()

# more accurate title?
new_title <- "Temps for stations with elevation < 1 m according to MSC"

p <- p + labs(title = new_title,
              x = "Date",
              y = "Average Daily Temperature in Degrees C")
p

# save to output folder
ggsave("output/temperature_plot.png")
