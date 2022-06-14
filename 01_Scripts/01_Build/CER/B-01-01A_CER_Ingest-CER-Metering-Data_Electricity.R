# < Description > *
# > Script Group Indicator Number and Name
# # : B-01, CER
# #
# > Script Number(s)
# # : B-01-01A
# #
# > Purpose of the script(s)
# # : Ingest CER Metering Data for Electricity.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(arrow)
library(stringr)
library(lubridate)
library(data.table)


# ------------------------------------------------------------------------------
# Set working directory, and run header script
# ------------------------------------------------------------------------------
# ------- Set project name -------
PROJ.NAME <- "CER-Trial"


# ------- Set working directory -------
PATH_PROJ <- paste("/Users/jmjo/Dropbox/00_JMJo/Projects", PROJ.NAME, sep = "/")
setwd(PATH_PROJ)


# ------- Run the header script -------
PATH_HEADER <- paste0("01_Scripts/H-", PROJ.NAME, ".R")
source(PATH_HEADER)


# ------------------------------------------------------------------------------
# Define path(s), parameter(s) and function(s)
# ------------------------------------------------------------------------------
# ------- Define path(s) -------
# # 1. Path(s) from which Dataset(s) is(are) loaded
# # 1.1. For Electricity Metering Data
DIR_TO.LOAD_CER_METERING <-
  "CER/38_CER Electricity_Gas/CER Electricity Revised March 2012"
PATH_TO.LOAD_CER_METERING <-
  paste(PATH_DATA_RAW_USE, DIR_TO.LOAD_CER_METERING, sep = "/")

# # 2. Path(s) to which output will be saved
FILE_TO.SAVE_CER_METERING <- "CER_Metering_Electricity.parquet"
PATH_TO.SAVE_CER_METERING <- paste(
  PATH_DATA_INTERMEDIATE_CER_METERING, FILE_TO.SAVE_CER_METERING, sep = "/"
)


# ------- Define parameter(s) -------
# (Refer to the R header script)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Ingest CER Metering Data for Electricity
# ------------------------------------------------------------------------------
# ------- Import Raw Data Files -------
# # 1. Get TXT files by extracting ZIP files
# # 1.1. Make a list of filenames of ZIP files
list_zip.files <-
  list.files(path = PATH_TO.LOAD_CER_METERING) %>%
    .[str_detect(., "txt.zip$")] %>%
    paste(PATH_TO.LOAD_CER_METERING, ., sep = "/") %>%
    as.list(.)

# # 1.2. Extract ZIP files to a temporary folder
dir_extract.to <- "TMP"
lapply(list_zip.files, unzip, exdir = dir_extract.to)


# # 2. Create a DT by importing TXT files
# # 2.1. Make a list of filenames of TXT files
list_files_metering_e <-
  list.files(path = dir_extract.to) %>%
    paste(dir_extract.to, ., sep = "/") %>%
    as.list(.)

# # 2.2. Create a DT by importing TXT files
dt_raw.data <- lapply(
  list_files_metering_e,
  fread,
  col.names = c("id", "daytime", "kwh")
) %>%
  rbindlist(.)


# # 3. Delete the temporary folder that contains TXT files
unlink(dir_extract.to, recursive = TRUE)


# ------- Modify the DT created above -------
# # 1. Add columns by splitting a column
dt_raw.data[
  ,
  `:=` (
    day = str_sub(daytime, start = 1L, end = 3L) %>% as.numeric(.),
    interval_30min = str_sub(daytime, start = 4L, end = 5L) %>% as.numeric(.)
  )
]

# # 2. Modifications to take account of Daylight Saving Time
# ## Refer to this website:
# ## https://www.ucd.ie/issda/data/commissionforenergyregulationcer/frequentlyaskedquestions/

# # 2.1. For Data for days at which daylight daving time ended
dt_raw.data[
  day %in% DAYLIGHT.SAVING.TIME_END & interval_30min %in% c(5, 6),
  tmp_mark := TRUE
]
dt_raw.data[
  day %in% DAYLIGHT.SAVING.TIME_END & interval_30min > 4,
  interval_30min := interval_30min - 2
]
dt_raw.data <- dt_raw.data[is.na(tmp_mark)][, tmp_mark := NULL]
# ## Note:
# ## There are 50 observations for days on which daylight saving time starts.
# ## To be specific, when local standard time was about to reach
# ## Sunday, March 29, 2009, 1:00:00 am clocks were turned forward 1 hour to
# ## Sunday, March 29, 2009, 2:00:00 am local daylight time instead. And
# ## when local standard time was about to reach Sunday, March 28, 2010,
# ## 1:00:00 am clocks were turned forward 1 hour to Sunday, March 28, 2010,
# ## 2:00:00 am local daylight time instead.
# ## For simplification, I drop two observations from each start date.

# # 2.2. For Data for days at which daylight daving time started
dt_raw.data[
  day %in% DAYLIGHT.SAVING.TIME_BEGIN & interval_30min == 4,
  interval_30min := 2
]
# ## Note:
# ## There is no observation for `interval_30min == 2` and
# ## `interval_30min == 3` due to Daylifht Saving Time. To be specific,
# ## when local standard time was about to reach Sunday, March 28, 2010,
# ## 1:00:00 am clocks were turned forward 1 hour to Sunday, March 28, 2010,
# ## 2:00:00 am local daylight time instead.
# ## It seems that `interval_30min == 4` should be `interval_30min == 2` based
# ## on the fact given above.


# # 3. Add columns that show hour of day and date, respectively
# # 3.1. Make DTs that will be used to add columns
# # 3.1.1. Make a DT that contains hour of day corresponding to
# #        30-minute-interval.
interval_30min <- dt_raw.data[, .N, keyby = .(interval_30min)]$interval_30min
dt_intervals <- data.table(
  interval_30min = interval_30min,
  interval_hour = (
    rep(seq(0, length(interval_30min) / 2, by = 1), each = 2) %>%
      .[-length(.)]
  )
)
# # 3.1.2. Make a DT that contains day-date pairs
days <- dt_raw.data[, .N, keyby = .(day)]$day
dt_days <- data.table(
  day = seq(1, max(days), by = 1)
)
dt_days[, ref.date := as.Date("2009-01-01") - 1]
dt_days[, date := ref.date + day]
dt_days[, ref.date := NULL]

# # 3.2. Create a DT, which contain 30-minute-level consumption data, by
# #      merging DTs created above.
tmp_dt_merge_1 <- merge(
  x = dt_raw.data, y = dt_intervals, by = "interval_30min", all.x = TRUE
)
dt_metering_30min <- merge(
  x = tmp_dt_merge_1[, -c("daytime")], y = dt_days, by = "day", all.x = TRUE
)


# # 4. Modify the DT created above
# # 4.1. Add columns
# # 4.1.1. Add a column that includes datetimes
# # 4.1.1.1. Add a temporary column that shows minute of time
dt_metering_30min[interval_30min %% 2 == 1, tmp_minute := "00"]
dt_metering_30min[interval_30min %% 2 == 0, tmp_minute := "30"]
# # 4.1.1.2. Create a temporary DT that includes unique observation at datetime
# #          level
tmp_dt_datetime <- dt_metering_30min[
  ,
  .N, by = .(date, interval_hour, tmp_minute)
][
  ,
  N := NULL
]
tmp_dt_datetime[
  ,
  tmp_datetime := paste0(date, " ", interval_hour, ":", tmp_minute)
]
# # 4.1.1.3. Create a DT by computing datetime from the temporary DT created
# #          above
tmp_dt_datetime_converted <- tmp_dt_datetime[
  ,
  lapply(.SD, ymd_hm, tz = "Europe/Dublin"), .SDcols = "tmp_datetime"
]
setnames(tmp_dt_datetime_converted, old = "tmp_datetime", new = "datetime")
# # 4.1.1.4. Create a DT by binding the DTs created above
dt_datetime <- cbind(
  tmp_dt_datetime[, -c("tmp_datetime")], tmp_dt_datetime_converted
)
# # 4.1.1.5. Merge the DT created above to `dt_metering_30min`
dt_metering_30min <- merge(
  x = dt_metering_30min,
  y = dt_datetime,
  by = c("date", "interval_hour", "tmp_minute"),
  all.x = TRUE
)
# # 4.1.2. Add a column that shows day of week
# # 4.1.2.1. Create a temporary DT that include unique observations at date
# #          level
tmp_dt_date <- dt_metering_30min[, .N, by = .(date)][, N := NULL]
tmp_dt_date[, day.of.week := lubridate::wday(date, label = TRUE, abbr = FALSE)]
# # 4.1.2.2. Merge the DT created above to `dt_metering_30min`
dt_metering_30min <- merge(
  x = dt_metering_30min[, -c("tmp_minute")],
  y = tmp_dt_date,
  by = c("date"),
  all.x = TRUE
)
# # 4.1.3. Add an indicator variable that shows whether an observation is for
# #        weekends or not
dt_metering_30min[, is_weekend := day.of.week %in% c("Saturday", "Sunday")]
# # 4.1.4. Add an indicator variable that shows whether an observation is for
# #        holidays or not
dt_metering_30min[, is_holiday := date %in% HOLIDAYS]

# # 4.2. Reorder columns
cols_reorder <- c(
  "id", "datetime", "date", "day", "interval_hour", "interval_30min",
  "day.of.week", "is_weekend", "is_holiday", "kwh"
)
setcolorder(dt_metering_30min, cols_reorder)

# # 4.3. Drop observations that are unnecessary
# # 4.3.1. With respect to `datetime`
dt_metering_30min[is.na(datetime), .N, by = .(date, day)]
dt_metering_30min[is.na(datetime), .N, by = .(interval_hour)]
dt_metering_30min <- dt_metering_30min[!is.na(datetime)]
# ## Note:
# ## Weird values for `interval_hour` are not converted correctly. These
# ## observations are dropped from my sample.
# # 4.3.2. With respect to duplicated observations
dt_metering_30min[interval_hour > 23, .N, by = .(id)]
ids_drop <- dt_metering_30min[interval_hour > 23, .N, by = .(id)]$id
dt_metering_30min <- dt_metering_30min[!(id %in% ids_drop)]
# ## Note:
# ## There are observations with `interval_hour == 24` even after taking account
# ## of daylight saving time. Those observations are dropped from my sample.

# 4.4. Sort observations by setting keys
keys <- c("id", "datetime")
stopifnot(dt_metering_30min[, .N, by = keys][N > 1, .N] == 0)
setkeyv(dt_metering_30min, keys)


# ------------------------------------------------------------------------------
# Save the DT created above in Parquet Format
# ------------------------------------------------------------------------------
# ------- Save the DT -------
write_parquet(
  dt_metering_30min,
  sink = PATH_TO.SAVE_CER_METERING,
  compression = "snappy",
  use_dictionary = TRUE
)
