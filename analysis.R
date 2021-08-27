# -------------------------------------------------------------------------
# Date created:      2021-07-14
# Author:            E. Anderson
# Description:       Write a description here.
#   
# Required input(s): N/A
# Optional input(s): N/A
# Outputs:           N/A
# -------------------------------------------------------------------------


# set up ------------------------------------------------------------------

setwd("W:/Stage Fall/R/")

# load necessary packages
# source("functions/packages.R")       

# load up our functions into memory
# source("functions/summarise_data.R") 

# load libraries
library(tidyhydat)
library(fasstr)
library(ggplot2)
library(dplyr)


search_stn_name("harrison lake") # Harrison Lake near Harrison Hot Springs 08MG012
search_stn_name("harrison river") # Harrison River below Morris Creek 08MG022
search_stn_name("fraser river at harrison") # Fraser River at Harrison Mills BC 08MF073

#' Key stations
#' 08MG012 Harrison Lake near Harrison Hot Springs
#' 08MG013 Harrison River near Harrison Hot Springs
#' 08MG022 Harrison River below Morris Creek
#' 


daily <- hy_daily(station_number = c("08MG012", "08MG013", "08MG022"))
summary(daily)

daily %>% group_by(Parameter) %>% 
    summarise()

# keep only levels
daily_levels <- daily %>% 
    filter(Parameter == "Level")

datestart <- as.Date("2018-01-01")
dateend <- as.Date("2019-01-01")

ggplot(daily_levels, aes(x = Date, y = Value)) + 
    geom_line(aes(colour = STATION_NUMBER), na.rm = TRUE) + 
    theme_bw() + 
    scale_x_date(limits = c(datestart, dateend)) +
    scale_colour_discrete(name = "", 
                            breaks = c("08MG012","08MG022"),
                            labels = c("Harrison Lake near Harrison Hot Springs",
                                       "Harrison River below Morris Creek"))
