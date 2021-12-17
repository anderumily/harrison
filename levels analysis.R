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

# load level data ---------------------------------------------------------

# load lake level data ----------------------------------------------------

lake <- read.csv("data/Stage.Working@08MG012.EntireRecord.csv",
                 stringsAsFactors = FALSE, skip = 14) %>% 
    # rename local time
    rename(raw_datetime = Timestamp..UTC.08.00.) %>%
    # select variables
    select(raw_datetime, Value) %>% 
    # create datetime and actual stage based on CGVD conversion
    mutate(datetime = ymd_hms(raw_datetime),
           actual_stage = Value + 0.148) %>% 
    # group by date and hour
    group_by(date(datetime), hour(datetime)) %>% 
    # find hourly
    summarise(hourly_stage = mean(actual_stage)) %>% 
    # ungroup
    ungroup()

lake$datetime <- as.POSIXct(paste(lake$`date(datetime)`, lake$`hour(datetime)`), format = "%Y-%m-%d %H")

# select datetime and hourly_stage
lake <- lake %>% select(datetime, hourly_stage)

# summary
summary(lake)

# plot
ggplot(lake, aes(x = datetime, y = hourly_stage)) + 
    geom_line() + 
    theme_bw() + 
    labs(x = "Year", y = "Actual Stage (m)", title = "08MG012 Harrison Lake near Harrison Hot Springs")
ggsave("output/levels analysis/08MG012 hourly level.png", width = w, height = h)

# load harrison at morris level data --------------------------------------

harrison_morris <- read.csv("data/Stage.Working@08MG022.EntireRecord.csv",
                         stringsAsFactors = FALSE, skip = 14) %>% 
    # rename local time
    rename(raw_datetime = Timestamp..UTC.08.00.) %>%
    # select variables
    select(raw_datetime, Value) %>% 
    # create datetime and actual stage based on CGVD conversion
    mutate(datetime = ymd_hms(raw_datetime),
           actual_stage = Value + 0.161) %>% 
    # group by date and hour
    group_by(date(datetime), hour(datetime)) %>% 
    # find hourly
    summarise(hourly_stage = mean(actual_stage)) %>% 
    # ungroup
    ungroup()

harrison_morris$datetime <- as.POSIXct(paste(harrison_morris$`date(datetime)`, harrison_morris$`hour(datetime)`), format = "%Y-%m-%d %H")

# select datetime and hourly_stage
harrison_morris <- harrison_morris %>% select(datetime, hourly_stage)

# summary
summary(harrison_morris)

# plot
ggplot(harrison_morris, aes(x = datetime, y = hourly_stage)) + 
    geom_line() + 
    theme_bw() + 
    labs(x = "Year", y = "Actual Stage (m)", title = "08MG022 Harrison River below Morris Creek")
ggsave("output/levels analysis/08MG022 hourly level.png", width = w, height = h)


# load harrison at harrison mills level data ------------------------------

harrison_mills <- read.csv("data/Stage.Working@08MG014.EntireRecord.csv",
                        stringsAsFactors = FALSE, skip = 14) %>% 
    # rename local time
    rename(raw_datetime = Timestamp..UTC.08.00.) %>%
    # select variables
    select(raw_datetime, Value) %>% 
    # create datetime and actual stage based on CGVD conversion
    mutate(datetime = ymd_hms(raw_datetime),
           actual_stage = Value - 0.144) %>% 
    # group by date and hour
    group_by(date(datetime), hour(datetime)) %>% 
    # find hourly
    summarise(hourly_stage = mean(actual_stage)) %>% 
    # ungroup
    ungroup()

harrison_mills$datetime <- as.POSIXct(paste(harrison_mills$`date(datetime)`, harrison_mills$`hour(datetime)`), format = "%Y-%m-%d %H")

# select datetime and hourly_stage
harrison_mills <- harrison_mills %>% select(datetime, hourly_stage)

# summary
summary(harrison_mills)

# plot
ggplot(harrison_mills, aes(x = datetime, y = hourly_stage)) + 
    geom_line() + 
    theme_bw() + 
    labs(x = "Year", y = "Actual Stage (m)", title = "08MG014 Harrison River at Harrison Mills")
ggsave("output/levels analysis/08MG014 hourly level.png", width = w, height = h)

# load fraser river at harrison mills level data --------------------------

fraser <- read.csv("data/Stage.Working@08MF073.EntireRecord.csv",
                        stringsAsFactors = FALSE, skip = 14) %>% 
    # rename local time
    rename(raw_datetime = Timestamp..UTC.08.00.) %>%
    # select variables
    select(raw_datetime, Value) %>% 
    # create datetime and actual stage based on CGVD conversion
    mutate(datetime = ymd_hms(raw_datetime),
           actual_stage = Value - 0.136) %>% 
    # group by date and hour
    group_by(date(datetime), hour(datetime)) %>% 
    # find hourly
    summarise(hourly_stage = mean(actual_stage)) %>% 
    # ungroup
    ungroup()

fraser$datetime <- as.POSIXct(paste(fraser$`date(datetime)`, fraser$`hour(datetime)`), format = "%Y-%m-%d %H")

# select datetime and hourly_stage
fraser <- fraser %>% select(datetime, hourly_stage)

# summary
summary(fraser)

# plot
ggplot(fraser, aes(x = datetime, y = hourly_stage)) + 
    geom_line() + 
    theme_bw() + 
    labs(x = "Year", y = "Actual Stage (m)", title = "08MF073 Fraser River at Harrison Mills")
ggsave("output/levels analysis/08MF073 hourly level.png", width = w, height = h)


# load field visit table --------------------------------------------------

# get mmnts from FieldVisitExport.csv
aquarius_mmnts <- read.csv("data/FieldVisitExport.csv",
                           stringsAsFactors = FALSE) %>%
    mutate(datetime = as.POSIXct(Date)) %>%
    rename(q = Discharge) %>% 
    select(datetime, q)
    # select(datetime, Station, MGH, Discharge, DischargeMethod, DeploymentMethod, ControlCondition)

# fraser vs harrison at harrison mills ------------------------------------

# what is the difference between fraser at harrison and harrison at harrison mills?

confluence_comparison <- merge(fraser, harrison_mills, by = "datetime") %>% 
    rename(fraser_stage = hourly_stage.x,
           harrison_mills_stage = hourly_stage.y) %>% 
    mutate(difference = harrison_mills_stage - fraser_stage)

summary(confluence_comparison)

ggplot(confluence_comparison, aes(x = datetime, y = difference)) + 
    geom_point() + 
    theme_bw() + 
    labs(x = "Year", y = "Difference in Stage (m)", title = "Difference between Harrison River at Harrison Mills and Fraser River at Harrison Mills")
ggsave("output/levels analysis/Difference between Harrison and Fraser at confluence.png", width = w, height = h)



# lake to harrison mills differential -------------------------------------

lake_to_mills_comparison <- merge(lake, harrison_mills, by = "datetime") %>% 
    rename(lake_stage = hourly_stage.x,
            harrison_mills_stage = hourly_stage.y) %>% 
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
    # scale_colour_manual(colours = c("red", "black")) +
    theme_bw() + 
    labs(x = "Year", y = "Difference in Stage (m)", title = "Difference between Harrison Lake and Harrison River at Harrison Mills")


# difference vs measurements ----------------------------------------------

differential_vs_mmnts <- full_join(lake_to_mills_comparison, aquarius_mmnts) %>% 
    select(datetime, difference, q)

ggplot(differential_vs_mmnts, aes(x = q, y = difference)) +
    geom_point() + 
    labs(y = "Difference in stage between Harrison Lake and Harrison River at Harrison Mills (m)",
         x = "Discharge (cms)")

# plus colour code points according to lake level?

# two slope sections ------------------------------------------------------


