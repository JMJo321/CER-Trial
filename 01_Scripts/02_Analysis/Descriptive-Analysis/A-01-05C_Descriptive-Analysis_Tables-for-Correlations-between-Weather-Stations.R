# < Description > *
# > Script Group Indicator Number and Name
# # : A-01, Descriptive Analysis
# #
# > Script Number(s)
# # : A-01-05C
# #
# > Purpose of the script(s)
# # : To make a table that shows correlation coefficients in average daily
# #   temperatures among weather stations.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(arrow)
library(stringr)
library(data.table)


# ------------------------------------------------------------------------------
# Set working directory, and run header script
# ------------------------------------------------------------------------------
# ------- Set project name -------
PROJ.NAME <- "CER-Trial"


# ------- Set working directory -------
PATH_PROJ <-
  paste("/Users/jmjo/Dropbox/00_JMJo/Projects", PROJ.NAME, sep = "/")
setwd(PATH_PROJ)


# ------- Run the header script -------
PATH_HEADER <- paste0("01_Scripts/H-", PROJ.NAME, ".R")
source(PATH_HEADER)


# ------------------------------------------------------------------------------
# Define path(s), parameter(s) and function(s)
# ------------------------------------------------------------------------------
# ------- Define path(s) -------
# # 1. Path(s) from which dataset(s) and script(s) are loaded
# # 1.1. For the DT including hourly weather data
FILE_TO.LOAD_WEATHER_HOURLY <-
  "Met-Eireann_Weather-Data_Hourly.parquet"
PATH_TO.LOAD_WEATHER_HOURLY <-
  paste(PATH_DATA_INTERMEDIATE_WEATHER, FILE_TO.LOAD_WEATHER_HOURLY, sep = "/")


# ------- Define parameter(s) -------
# # 1. Set a list of weather stations that will be excluded from the table.
STATIONS_TO.EXCLUDE <- c(
  "Casement Aerodrome", "Claremorris", "Dunsany", "Roches Point SWS",
  "Phoenix Park", # Close to other stations
  "Athenry", "Finner" # Less observations
)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Load dataset(s) and/or script(s) required to run regression(s)
# ------------------------------------------------------------------------------
# ------- Load necessary dataset(s) and/or script(s) -------
# # 1. Load required DT(s) for regression analysis
# # 1.1. Load the DT for Met Eireann's daily weather data
dt_weather_hourly <-
  read_parquet(PATH_TO.LOAD_WEATHER_HOURLY) %>%
    setDT(.)


# ------------------------------------------------------------------------------
# Compute correlation coefficients
# ------------------------------------------------------------------------------
# ------- Compute correlation coefficients -------
# # 1. Create DT(s) that will be used to compute correlation coefficients
# # 1.1. For the sample period
subdt_weather_sample.period <- dt_weather_hourly[
  (!station %in% STATIONS_TO.EXCLUDE) &
    ((year == 2009 & month %in% 7:12) | (year == 2010 & month %in% 7:12)),
][
  ,
  lapply(.SD, mean, na.rm = TRUE), .SDcols = "temp_f",
  by = .(station, year, month, day)
]

# # 1.2. For the experiment period
subdt_weather_exp.period <- dt_weather_hourly[
  (!station %in% STATIONS_TO.EXCLUDE) &
    ((year == 2009 & month %in% 7:12) | (year == 2010 & month %in% 1:12)),
][
  ,
  lapply(.SD, mean, na.rm = TRUE), .SDcols = "temp_f",
  by = .(station, year, month, day)
]


# # 2. Create DT(s) by computing correlation coefficients
# # 2.1. For the sample period
dt_cor_sample.period <- setDT(NULL)
stations <-
  subdt_weather_sample.period[
    station != "Dublin Airport", .N, by = .(station)
  ] %>%
    .$station
for (tmp_station in stations) {
  tmp_test.results <- cor.test(
    subdt_weather_sample.period[station == "Dublin Airport"]$temp_f,
    subdt_weather_sample.period[station == tmp_station]$temp_f,
    alternative = "two.sided", method = "pearson", exact = TRUE,
      conf.level = 0.95
  )
  tmp_dt <- data.table(
    station = tmp_station,
    cor.coef_sample.period = round(tmp_test.results$estimate, digits = 5),
    p.value_sample.period = round(tmp_test.results$p.value, digits = 5)
  )
  dt_cor_sample.period <- rbind(dt_cor_sample.period, tmp_dt)
}

# # 2.2. For the experiment period
dt_cor_exp.period <- setDT(NULL)
for (tmp_station in stations) {
  tmp_test.results <- cor.test(
    subdt_weather_exp.period[station == "Dublin Airport"]$temp_f,
    subdt_weather_exp.period[station == tmp_station]$temp_f,
    alternative = "two.sided", method = "pearson", exact = TRUE,
      conf.level = 0.95
  )
  tmp_dt <- data.table(
    station = tmp_station,
    cor.coef_exp.period = round(tmp_test.results$estimate, digits = 5),
    p.value_exp.period = round(tmp_test.results$p.value, digits = 5)
  )
  dt_cor_exp.period <- rbind(dt_cor_exp.period, tmp_dt)
}

# # 2.3. Make a DT by merging the two DTs created above
dt_cor <- merge(
  x = dt_cor_sample.period,
  y = dt_cor_exp.period,
  by = "station"
)
dt_cor
