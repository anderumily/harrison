# -------------------------------------------------------------------------
# Date created:      2021-07-14
# Author:            E. Anderson
# Description:       This script calculates discharge based on the J Curve method using water levels from the lake 
#                    and river stations in HYDAT. 
# Dependencies:      Run jcurve.R script first.
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

search_stn_name("harrison lake") # Harrison Lake near Harrison Hot Springs 08MG012
search_stn_name("harrison river") # Harrison River below Morris Creek 08MG022
search_stn_name("fraser river at harrison") # Fraser River at Harrison Mills BC 08MF073

#' Key stations                                                             Flow or Level  Datum
#' 08MG012 Harrison Lake near Harrison Hot Springs (used to calculate fall) LEVEL          GEODETIC SURVEY OF CANADA
#' 08MG013 Harrison River near Harrison Hot Springs (Q mmnt location)       FLOW           ASSUMED DATUM
#' 08MG022 Harrison River below Morris Creek (used to calculate fall)       LEVEL          ASSUMED DATUM
#' 08MG014 Harrison River at Harrison Mills                                 LEVEL          GEODETIC SURVEY OF CANADA DATUM, CONVERSION FACTOR 4.923 m
#' 08MF073 Fraser River at Harrison Mills                                   LEVEL          GEODETIC SURVEY OF CANADA
#' 08MH024 Fraser River at Mission                                          FLOW AND LEVEL GEODETIC SURVEY OF CANADA DATUM, CONVERSION FACTOR 0.043 m

daily <- hy_daily(station_number = c("08MG012", "08MG013", "08MG022", "08MG014", "08MF073", "08MH024"))
summary(daily)

daily %>% group_by(Parameter) %>% 
    summarise()

# keep only levels at Harrison Lake and Harrison River
daily_levels <- daily %>% 
    filter(Parameter == "Level") %>% 
    filter(STATION_NUMBER == "08MG012" | STATION_NUMBER == "08MG022")


# When is J Curve applied? ------------------------------------------------

which_model <- daily_levels %>% 
    select(STATION_NUMBER, Date, Value) %>% 
    pivot_wider(names_from = STATION_NUMBER, values_from = Value) %>% 
    rename(date = Date,
           lake = `08MG012`,
           river = `08MG022`) %>% 
    drop_na()

# when does data exist for both stations?
ggplot(daily_levels, aes(x = Date, y = Value, colour = STATION_NUMBER)) + 
    geom_line() + 
    scale_colour_discrete(name = "", 
                          breaks = c("08MG012","08MG022"),
                          labels = c("Harrison Lake near \nHarrison Hot Springs",
                                     "Harrison River below \nMorris Creek")) +
    theme_bw() +
    labs(x = "Year",
         y = "Stage (m)",
         title = "Full Record Harrison Lake and River")
ggsave("output/Harrison Lake and River full record.png")

# calculate fall
which_model <- which_model %>% 
    mutate(fall = lake - river)

# calculate free fall in ft
for(i in 1:nrow(which_model)){
    if (which_model$lake[i] <= 29 * 0.3048) {
        which_model$freefall[i] <- curveA_slope * which_model$lake[i] + curveA_intercept
    } else if (which_model$lake[i] > 29 * 0.3048 && which_model$lake[i] <= 34 * 0.3048) {
        which_model$freefall[i] <- curveB_slope * which_model$lake[i] + curveB_intercept
    } else {
        which_model$freefall[i] <- curveC_slope * which_model$lake[i] + curveC_intercept
    }
}

# plot fall vs freefall
ggplot(which_model, aes(x = fall, y = freefall)) + 
    geom_point() +
    xlim(-0.2,1.2) +
    ylim(-0.2,1.2) +
    geom_abline(intercept = 0, slope = 1)

# calculate free fall ratio
which_model$freefall_ratio <- which_model$fall / which_model$freefall

# plot freefall ratio
ggplot(which_model, aes(x = date, y = freefall_ratio)) + 
    geom_point()

# calculate free fall discharge
which_model$freefall_discharge <- A * (which_model$lake - B)^n

# free fall Q ratio
which_model$freefall_q_ratio <- curveJ_slope * which_model$freefall_ratio + curveJ_intercept

# J-Curve rated discharge
which_model$jcurve_discharge <- which_model$freefall_discharge * which_model$freefall_q_ratio

# if freefall_ratio >= 1, then use freefall dicharge ELSE if freefall_ration < 1 then use j curve discharge

for(i in 1:nrow(which_model)){
    if (which_model$freefall_ratio[i] < 1) {
        which_model$discharge <- which_model$jcurve_discharge
    } else {
        which_model$discharge <- which_model$freefall_discharge
    }
}

# now add actual discharge

harrison_q_hydat <- daily %>% 
    filter(STATION_NUMBER == "08MG013",
           Parameter == "Flow") %>% 
    select(Date, Value) %>% 
    rename(date = Date,
           harrison_q_hydat = Value)

harrison_q_hydat

which_model <- merge(which_model, harrison_q_hydat) %>% 
    drop_na()

# check if calculated q matches hydat q
ggplot(which_model, aes(x = harrison_q_hydat, y = discharge)) + 
    geom_point() + 
    geom_smooth(method = 'lm', formula = y~x) + 
    xlim(0,2000) + 
    ylim(0,2000) + 
    geom_abline(slope = 1, intercept = 0) + 
    labs(title = "08MG013 HARRISON RIVER NEAR HARRISON HOT SPRINGS",
           x = "HYDAT Discharge (cms)",
           y = "Calculated Discharge (cms)") + 
    theme_bw() + 
    annotate("text", label = "R2 = 0.95", x = 1500, y = 800)
ggsave("output/Calculated Q vs HYDAT Q.png", width = 6, height = 6)

lm <- lm(discharge ~ harrison_q_hydat, which_model)
summary(lm)
summary(which_model)

# when is j curve applied?

# if freefall_ratio >= 1, then use freefall dicharge ELSE if freefall_ration < 1 then use j curve discharge
which_model$indicator_text <- ifelse(which_model$freefall_q_ratio < 1, "J Curve Q", "Freefall Q")
which_model$indicator_num <- ifelse(which_model$freefall_q_ratio < 1, 1, 0)

# which years have most backwater ie most uses of the J curve?
    # consider the water year
    # Thus, the year ending September 30, 1999 is called the "1999" water year. https://water.usgs.gov/nwc/explain_data.html

backwater_years <- which_model %>% 
    select(date, indicator_text, indicator_num) %>% 
    mutate(year = year(date)) %>% 
    mutate(month = month(date)) %>% 
    mutate(water_year = year)
    
    backwater_years$water_year <- ifelse(backwater_years$month == 10, backwater_years$water_year + 1, backwater_years$water_year)

backwater_years <- backwater_years %>% 
    group_by(water_year) %>% 
    summarize(sum(indicator_num)) %>% 
    ungroup() %>% 
    arrange(desc(`sum(indicator_num)`))

# highest backwater years
head(backwater_years)

# lowest backwater years
tail(backwater_years)
    
ggplot(which_model, aes(x = date, y = indicator_num)) + 
    geom_line() +
    labs(title = "Which Model?",
         x = "Year",
         y = "0 = Freefall Q     1 = J Curve Q") + 
    scale_y_continuous(breaks = c(0,1))
ggsave("output/which model.png")
    

# plot --------------------------------------------------------------------

plot_year <- function(datestart, dateend, title = ""){
    ggplot(daily_levels, aes(x = Date, y = Value)) + 
        geom_line(aes(colour = STATION_NUMBER), na.rm = TRUE) + 
        theme_bw() + 
        scale_x_date(limits = c(datestart, dateend)) +
        scale_colour_discrete(name = "", 
                              breaks = c("08MG012","08MG022"),
                              labels = c("Harrison Lake near \nHarrison Hot Springs",
                                         "Harrison River below \nMorris Creek")) + 
        labs(x = "Date",
             y = "Discharge (cms)",
             title = title)
    
    ggsave(paste0("output/", title, ".png"))
}

# high backwater
plot_year(as.Date("2004-10-01"), as.Date("2005-09-30"), "2005")
plot_year(as.Date("2016-10-01"), as.Date("2017-09-30"), "2017")
plot_year(as.Date("2005-10-01"), as.Date("2006-09-30"), "2006")
plot_year(as.Date("1993-10-01"), as.Date("1994-09-30"), "1994")
plot_year(as.Date("2010-10-01"), as.Date("2011-09-30"), "2011")

# low backwater
plot_year(as.Date("2018-10-01"), as.Date("2019-09-30"), "2019")
plot_year(as.Date("1999-10-01"), as.Date("2000-09-30"), "2000") # lots of data missing
plot_year(as.Date("1986-10-01"), as.Date("1987-09-30"), "1987")
plot_year(as.Date("2017-10-01"), as.Date("2018-09-30"), "2018")



