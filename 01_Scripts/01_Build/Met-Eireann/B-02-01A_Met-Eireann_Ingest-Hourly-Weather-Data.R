# < Description > *
# > Script Group Indicator Number and Name
# # : B-02, Met-Eireann
# #
# > Script Number(s)
# # : B-02-01A
# #
# > Purpose of the script(s)
# # : Ingest Met Eireann's Hourly-Level Weather Data

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
# # 1. Path(s) from which Hourly Weather Data files will be read
DIR_TO.LOAD_WEATHER <-
  paste(PATH_DATA_RAW_USE, "Met-Eireann/Hourly-Data", sep = "/")

# # 2. Path(s) to which Output will be saved
FILE_TO.SAVE_WEATHER <- "Met-Eireann_Weather-Data_Hourly.parquet"
PATH_TO.SAVE_WEATHER <-
  paste(PATH_DATA_INTERMEDIATE_WEATHER, FILE_TO.SAVE_WEATHER, sep = "/")


# ------- Define parameter(s) -------
# # 1. Range of Years to subset Weather Data
RANGE_YEARS <- 2000:2020


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Create a DT from Met Eireann's Hourly Weather Data
# ------------------------------------------------------------------------------
# ------- Create a DT by importing CSV files -------
# # 1. Make a list of CSV files
files <- list.files(DIR_TO.LOAD_WEATHER)
csv.files <- files[str_detect(files, ".csv$")]

# # 2. Create DTs by importing CSV files
for (file in csv.files) {
  tmp_name <- paste0(
    "dt_", (str_replace(file, ".csv", "") %>% str_replace(., "hly", ""))
  )
  tmp_path <- paste(DIR_TO.LOAD_WEATHER, file, sep = "/")
  tmp_code <- str_extract(file, "[0-9]+") %>% as.integer(.)

  assign(tmp_name, fread(file = tmp_path))
  get(tmp_name)[, station_code := tmp_code]
}

# # 3. Create a DT by combining DTs made above
dts <- ls()[str_detect(ls(), "^dt_")]
dt_weather_hourly <-
  rbindlist(mget(dts) %>% as.list(.), use.names = TRUE, fill = TRUE)


# ------- Modify the DT created -------
# # 1. Drop unnecessary columns
names_old <- c(
  "date", "ind", "rain", "ind", "temp", "ind", "wetb", "dewpt", "vappr",
  "rhum", "msl", "ind", "wdsp", "ind", "wddir", "station_code"
)
names_new <- c(
  "date", "irain", "rain", "itemp", "temp_c", "iwb", "wetb", "dewpt", "vappr",
  "rhum", "msl", "iwdsp", "wdsp", "iwddir", "wddir", "station_code"
)
# ## Note:
# ## There are columns that have the same name. They can be distinguished
# ## based on Met Eireann's data dictionary.
names_keep <- names(dt_weather_hourly)[names(dt_weather_hourly) %in% names_old]
dt_weather_hourly <- dt_weather_hourly[, .SD, .SDcols = names_keep]


# # 2. Rename columns
names(dt_weather_hourly) <- names_new


# # 3. Add columns
# # 3.1. Add columns that are related to date and time
# # 3.1.1. Add a column showing datetime
dt_weather_hourly[
  ,
  hour := str_extract(date, " [0-9]+") %>%
    str_trim(., side = "left") %>%
    as.integer(.)
]
dt_weather_hourly[
  ,
  tmp_datetime := as.Date(date, format = "%d-%b-%Y")
]
dt_weather_hourly[
  ,
  datetime := lubridate::ymd_h(paste(tmp_datetime, hour, sep = " "))
]
# ## Note: Direct conversion does not work.

# # 3.1.2. Add columns showing year, month and day
dt_weather_hourly[
  ,
  `:=` (
    year = year(datetime),
    month = month(datetime),
    day = lubridate::mday(datetime)
  )
]
# # 3.1.3. Drop unnecessary and temporary columns
dt_weather_hourly[, `:=` (date = NULL, tmp_datetime = NULL)]

# # 3.2. Add a column showing station names
for (code in names(list_stations)) {
  dt_weather_hourly[
    station_code == as.integer(code),
    station := list_stations[[code]]
  ]
}

# # 3.3. Add a column that includes temperature in Fahrenheit
dt_weather_hourly[, temp_f := temp_c * (9 / 5) + 32]


# # 4. Drop unnecessary observations
dt_weather_hourly <- dt_weather_hourly[year %in% RANGE_YEARS]


# # 5. Reorder columns
cols_reorder <- c(
  "station", "station_code", "datetime", "year", "month", "day", "hour",
  "irain", "rain", "itemp", "temp_c", "temp_f"
)
setcolorder(dt_weather_hourly, cols_reorder)


# ------- Save the DT in Parquet Format -------
write_parquet(
  dt_weather_hourly,
  sink = PATH_TO.SAVE_WEATHER,
  compression = "snappy",
  use_dictionary = TRUE
)
