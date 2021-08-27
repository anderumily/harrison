# -------------------------------------------------------------------------
# Date created:      2020-04-22
# Author:            Emily Anderson
# Description:       Example using tidyhydat.
#
# Required input(s): N/A
# Optional input(s): N/A
# Outputs:           plots to output folder
# -------------------------------------------------------------------------

# set up ------------------------------------------------------------------

# set working directory to the R_Resources folder
setwd("C:/Users/andersone/Documents/R/R_Resources")

# uncomment if you need to install packages
# install.packages("tidyhydat")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("lubridate")

# load libraries
library(ggplot2)
library(dplyr)
library(tidyhydat)
library(lubridate)


# download hydat database -------------------------------------------------

# uncomment to download hydat sqlite
# download_hydat()

# where is it stored?
hy_dir()


# browse tidyhydat --------------------------------------------------------

help(package = "tidyhydat")

# click on "User guides, package vignettes, and other documentation" (second bullet point).
#    A "vinette" is a type of long form documentation.
#    See the vinettes available:
vignette(package = "tidyhydat")

# call them:
vignette("tidyhydat_an_introduction", package = "tidyhydat")
vignette("tidyhydat_example_analysis", package = "tidyhydat")
vignette("tidyhydat_hydat_db", package = "tidyhydat")

# look at some funcions
?allstations
?hy_daily_flows
?realtime_dd
?search_stn_number

# yukon stations ----------------------------------------------------------

# get list of active yukon stations
yukon <- hy_stations() %>%
  filter(PROV_TERR_STATE_LOC == "YT") %>% # filter yukon stations
  filter(HYD_STATUS == "ACTIVE") %>% # filter for active stations
  select(STATION_NUMBER, STATION_NAME, DRAINAGE_AREA_GROSS) # select columns

# all active stations in Yukon: 72
yukon

# arrange by gross drainage area
arrange(yukon, desc(DRAINAGE_AREA_GROSS))

# get yukon river at whitehorse flows
whitehorse_flows <- hy_daily_flows("09AB001")

# look at summary
summary(whitehorse_flows)

# plot --------------------------------------------------------------------

# plot all
p <- ggplot(whitehorse_flows, aes(x = Date, y = Value)) + 
  geom_line(colour = "dark blue") + # add geometry
  labs(
    title = "Yukon River at Whitehorse",
    x = "Year",
    y = "Flow (cms)"
  ) + # add labels
  theme_bw() # add a theme

# show the plot
p

# plot flows since 2010
p <- p +
  scale_x_date(limits = c(as.Date("2010-01-01"), as.Date("2017-12-31")))

# show the plot
p

# save the last plot
ggsave("output/whitehorse_flows.png", width = 6, height = 4)


# annual peaks ------------------------------------------------------------

# get the annual peaks for yukon at whitehorse
whitehorse_annual_peak <- whitehorse_flows %>%
  mutate(year = year(Date)) %>% # add a column with the year (uses package lubridate)
  group_by(year) %>% # group the dataframe by year column
  summarise(max(Value)) %>% # get the max value for each year
  rename(peak_flow = `max(Value)`) # rename the max value column

# look at dataframe
whitehorse_annual_peak

# look at summary
summary(whitehorse_annual_peak)

# plot
ggplot(whitehorse_annual_peak, aes(x = year, y = peak_flow)) +
  geom_point(colour = "dark red", shape = 15) +
  labs(
    title = "Whitehorse Annual Peak Flow",
    x = "Year",
    y = "Annual Peak Flow (cms)"
  ) +
  theme_bw()

# save
ggsave("output/whitehorse_annual_peaks.png", width = 6, height = 4)

# plot the distribution of peak flows
ggplot(whitehorse_annual_peak, aes(peak_flow)) +
  geom_density() +
  labs(
    title = "Whitehorse Annual Peak Flow Distribution",
    x = "Annual Peak Flow (cms)",
    y = "Density"
  )

# save
ggsave("output/whitehorse_peaks_dist.png", width = 6, height = 4)
