# -------------------------------------------------------------------------
# Date created:      2021-06-23
# Author:            E. Anderson
# Description:       Write a description here.
# -------------------------------------------------------------------------


# set up ------------------------------------------------------------------

setwd("C:/Users/andersone/Documents/Projects Local/Stage Fall Local/harrison")

# load libraries
library(tidyr)
library(segmented)
library(dplyr)
library(ggplot2)

# load tables -------------------------------------------------------------

# get column names
data_table_headers <- names(read.csv("DataTable.csv", skip = 3, nrows = 1, header = TRUE))

# read in data
data_table_raw <- read.csv("DataTable.csv", skip = 5, header = FALSE, stringsAsFactors = FALSE)

# assign names
names(data_table_raw) <- data_table_headers

# remove headers
rm(data_table_headers)

# select columns and rename
data_table <- data_table_raw %>%
  select(Date, MGH.Harrison.Lake, MGH.Harrison.River.Below.Morris, Measured.Discharge, J.Curve.Model.Discharge) %>%
  rename(
    date = Date,
    harrison_lake_elevation = MGH.Harrison.Lake,
    harrison_river_morris_elevation = MGH.Harrison.River.Below.Morris,
    measured_discharge = Measured.Discharge,
    jcurve_discharge_spreadsheet = J.Curve.Model.Discharge
  ) %>%
  mutate(date = as.Date(date)) %>%
  mutate(measured_discharge = as.numeric(measured_discharge)) %>%
  mutate(jcurve_discharge_spreadsheet = as.numeric(jcurve_discharge_spreadsheet)) %>%
  mutate(jcurve_discharge_calculated = -9999)

freefall_table <- read.csv("FreeFallTable.csv", skip = 1, header = TRUE, col.names = c("gauge_height", "freefall", "difference"))
hq <- read.csv("StageDischargeTable.csv", skip = 2, header = TRUE, col.names = c("gauge_height", "freefall_q", "difference"))
jcurve <- read.csv("JTable.csv", skip = 1, header = TRUE, col.names = c("freefall_ratio", "freefall_q_ratio", "difference"))

# free fall ---------------------------------------------------------------

ggplot(freefall_table, aes(x = gauge_height, y = freefall)) +
  geom_point() +
  theme_bw() +
  labs(
    x = "Harrison Lake Gauge Height (ft)",
    y = "Free Fall (ft)",
    title = "Free Fall Plot"
  ) +
  scale_x_continuous(limits = c(26, 44), breaks = seq(from = 26, to = 44, by = 1)) +
  # find breakpoints
  geom_vline(xintercept = 29) +
  geom_vline(xintercept = 34)

# add definition for each part of the curve
# for gauge_height <= 29, Curve A
# for gauge_height > 29 and <= 34, Curve B
# for gauge_height > 34, Curve C

freefall_table$curve <- ifelse(freefall_table$gauge_height <= 29, "Curve A", "Curve B")
freefall_table$curve <- ifelse(freefall_table$gauge_height > 34, "Curve C", freefall_table$curve)

# linear models
freefall_A <- freefall_table %>%
  filter(curve == "Curve A")

curveA <- lm(freefall ~ gauge_height, data = freefall_A)
curveA_slope <- as.numeric(curveA$coefficients[2])
curveA_intercept <- as.numeric(curveA$coefficients[1])
eqA <- paste("y = ", signif(curveA_slope, 4), "x + ", signif(curveA_intercept, 4), sep = "")

freefall_B <- freefall_table %>%
  filter(curve == "Curve B")

curveB <- lm(freefall ~ gauge_height, data = freefall_B)
curveB_slope <- as.numeric(curveB$coefficients[2])
curveB_intercept <- as.numeric(curveB$coefficients[1])
eqB <- paste("y = ", signif(curveB_slope, 4), "x + ", signif(curveB_intercept, 4), sep = "")

freefall_C <- freefall_table %>%
  filter(curve == "Curve C")

curveC <- lm(freefall ~ gauge_height, data = freefall_C)
curveC_slope <- as.numeric(curveC$coefficients[2])
curveC_intercept <- as.numeric(curveC$coefficients[1])
eqC <- paste("y = ", signif(curveC_slope, 4), "x + ", signif(curveC_intercept, 4), sep = "")

# plot again
ggplot(freefall_table, aes(x = gauge_height, y = freefall, colour = curve)) +
  geom_point() +
  scale_colour_hue(l = 50) +
  theme_bw() +
  labs(
    x = "Harrison Lake Gauge Height (ft)",
    y = "Free Fall (ft)",
    title = "Free Fall Plot"
  ) +
  # eliminate title
  theme(legend.title = element_blank())+
  scale_x_continuous(limits = c(26, 44), breaks = seq(from = 26, to = 44, by = 1)) +
  # find breakpoints
  geom_vline(xintercept = 29) +
  geom_vline(xintercept = 34) +
  # add regression
  geom_smooth(method = lm, formula = y ~ x, se = FALSE) +
  # add equations
  annotate("text", label = eqA, x = 27, y = 2.7) +
  annotate("text", label = eqB, x = 31.5, y = 2.7) +
  annotate("text", label = eqC, x = 36, y = 2.7) # +
# check equations
# geom_abline(intercept = -2.57, slope = 0.1) +
# geom_abline(intercept = -6.92, slope = 0.25) +
# geom_abline(intercept = -4.54, slope = 0.18)
ggsave("output/Free Fall Plot.png")

# stage-discharge ---------------------------------------------------------

A <- 845.463615649259
B <- 25.5136132413396
n <- 1.58997291940515

hq$model_freefall_q <- A*(hq$gauge_height-B)^n

hq_plotting <- hq %>% 
  select(gauge_height, freefall_q, model_freefall_q) %>% 
  pivot_longer(!gauge_height, names_to = "source", values_to = "q")

ggplot(hq_plotting, aes(x = q, y = gauge_height, colour = source)) +
  geom_point() +
  theme_bw() +
  labs(
    x = "Free Fall Discharge (cfs)",
    y = "Harrison Lake Gauge Height (ft)",
    title = "Stage Discharge Curve"
  ) +
  # scale_x_continuous(breaks = seq(from = 26, to = 44, by = 1)) +
  theme(legend.title = element_blank()) # +
  # add equations
  # annotate("text", label = eqD, x = 28.5, y = 60000) +
  # annotate("text", label = eqE, x = 36, y = 60000)
ggsave("output/Free Fall Stage Discharge Curve.png")


# j curve -----------------------------------------------------------------

curveJ <- lm(freefall_q_ratio ~ freefall_ratio, data = jcurve)
curveJ_slope <- as.numeric(curveJ$coefficients[2])
curveJ_intercept <- as.numeric(curveJ$coefficients[1])
eqJ <- paste("y = ", signif(curveJ_slope, 4), "x + ", signif(curveJ_intercept, 4), sep = "")


ggplot(jcurve, aes(x = freefall_ratio, y = freefall_q_ratio)) +
  geom_point() +
  theme_bw() +
  labs(
    x = "Free Fall Ratio (unitless)",
    y = "Free Fall Discharge Ratio (unitless)",
    title = "J Curve"
  ) +
  xlim(0, 1) +
  ylim(0, 1) +
  geom_smooth(method = "lm", formula = y ~ x) +
  annotate("text", label = eqJ, x = 0.75, y = 0.6)
ggsave("output/J Curve.png", width = 5, height = 5)


# calculate discharge -----------------------------------------------------

calculate_discharge <- function(harrison_lake_elevation, harrison_river_morris_elevation) {
  harrison_lake_elevation_ft <- harrison_lake_elevation * 3.28084

  # calculate fall in ft
  fall_ft <- (harrison_lake_elevation - harrison_river_morris_elevation) * 3.28084

  # calculate free fall in ft
  if (harrison_lake_elevation_ft <= 29) {
    freefall_ft <- curveA_slope * harrison_lake_elevation_ft + curveA_intercept
  } else if (harrison_lake_elevation_ft > 29 && harrison_lake_elevation_ft <= 34) {
    freefall_ft <- curveB_slope * harrison_lake_elevation_ft + curveB_intercept
  } else {
    freefall_ft <- curveC_slope * harrison_lake_elevation_ft + curveC_intercept
  }

  # calculate free fall ratio
  freefall_ratio <- fall_ft / freefall_ft

  # calculate free fall discharge
  freefall_discharge <- A*(harrison_lake_elevation_ft-B)^n
  
  # free fall Q ratio
  freefall_q_ratio <- curveJ_slope * freefall_ratio + curveJ_intercept

  # J-Curve rated discharge
  jcurve_discharge <- freefall_discharge * freefall_q_ratio

  # if freefall_ratio < 1, then use freefall dicharge ELSE if freefall_ration >= 1 then use j curve discharge

  if (freefall_ratio < 1) {
    discharge_cfs <- jcurve_discharge
  } else {
    discharge_cfs <- freefall_discharge 
  }

  # get discharge in cms
  discharge_cms <- discharge_cfs / 35.314666212661

  return(round(discharge_cms))
}

# enter variables ---------------------------------------------------------

# test case: 2013-01-09

harrison_lake_elevation <- 8.887 # in meters
harrison_river_morris_elevation <- 8.737 # in meters

for (i in 1:nrow(data_table)) {
  data_table$jcurve_discharge_calculated[i] <- calculate_discharge(data_table$harrison_lake_elevation[i], data_table$harrison_river_morris_elevation[i])
}

# # backwater or no?
# data_table$backwater <- ifelse()


# script performance ------------------------------------------------------

# calculate r2
model <- lm(jcurve_discharge_calculated ~ jcurve_discharge_spreadsheet, data = data_table)
summary <- summary(model)
r2 <- round(as.numeric(summary[8]), 4)

ggplot(data_table, aes(x = jcurve_discharge_spreadsheet, y = jcurve_discharge_calculated)) + 
    geom_point(colour = "darkblue") + 
    geom_abline(slope = 1, intercept = 0) + 
    # geom_smooth(method = "lm", 
    #             se = FALSE, 
    #             formula = y ~ x) +
    theme_bw() + 
    labs(x = "Modelled Discharge from Spreadsheet (cms)",
         y = "Modelled Discharge Calculated (cms)", 
         title = "Script Performance") + 
    annotate("text", x = 1000, y = 950, label = "1:1 line", angle = 45) + 
    annotate("text", x = 400, y = 800, label = paste0("R² = ", r2))
ggsave("output/Script Performance.png", width = 5, height = 5)



# j curve model performance -----------------------------------------------

# calculate r2
model <- lm(jcurve_discharge_calculated ~ measured_discharge, data = data_table)
summary <- summary(model)
r2 <- round(as.numeric(summary[8]), 4)

ggplot(data_table, aes(x = measured_discharge, y = jcurve_discharge_calculated)) + 
    geom_point(colour = "darkblue") + 
    geom_abline(slope = 1, intercept = 0) + 
    geom_smooth(method = "lm",
                se = FALSE,
                formula = y ~ x) +
    theme_bw() + 
    labs(x = "Measured Discharge (cms)",
         y = "Modelled Discharge Calculated (cms)", 
         title = "J Curve Model Performance") + 
    annotate("text", x = 1000, y = 950, label = "1:1 line", angle = 45) + 
    annotate("text", x = 400, y = 800, label = paste0("R² = ", r2))
ggsave("output/J Curve Model Performance.png", width = 5, height = 5)

