
#------------------------------------------------------------------------------
# annualhydrographs.r
#
# General purpose script which plots the historical flow distribution and 
# overlays a few random hydrographs in ggplot2. Shows the use of the 
# geom_ribbon function to shade portions of a graph.
# 
# challenge: modify the script to read in the annual maximum daily series
# and plot the hydrographs with the top 3 annual maximum daily flows (or minimum)
# (hint: use hy_annual_stats() to pull the annual maximum series!)
#
# D.Hutchinson 2020-04-28
#-------------------------------------------------------------------------------

# required libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyhydat)

# script parameters 
wsc_id <- "08MF005"

# pull the station metadata
stn_info <- hy_stations(station_number = wsc_id)

# get historical daily flow series
dly_flow <- hy_daily_flows(station_number = wsc_id)

# add a columns for Year, day of year (DOY)
dly_flow <- dly_flow %>%
            mutate(YEAR = year(Date), DOY = yday(Date))

# now compute historical quantiles (interdecile, interquartile, median)
pctiles <- c(0.10, 0.25,0.50,0.75,0.90)
dly_stats <- dly_flow %>% 
             select(STATION_NUMBER, DOY, Value) %>%
             group_by(STATION_NUMBER, DOY) %>%
             do(
               data.frame(
                 t(quantile(.$Value, probs = pctiles, na.rm = TRUE))
             ))

# filter the last 5 years of hydrographs 
max_year <- max(dly_flow$YEAR)
select_years <- dly_flow %>% filter(YEAR >= max_year - 5) %>%
                mutate(YEAR = as.factor(YEAR))

# now plot the results
ggplot(dly_stats) + 
  geom_ribbon(aes(x = DOY, ymin = X10., ymax = X90.), fill = "light grey", alpha = 0.2) +
  geom_ribbon(aes(x = DOY, ymin = X25., ymax = X75.), fill = "dark grey", alpha = 0.2) +
  geom_line(aes(x = DOY, y = X50.), lty = 2, colour = "blue") +
  geom_line(data = select_years, aes(x = DOY, y = Value, colour = YEAR)) +
  labs(
    title = sprintf("%s [%s]", stn_info$STATION_NAME[1], wsc_id),
    x = "Day of year",
    y = "Discharge"
  ) +
  theme_bw()

# End script                