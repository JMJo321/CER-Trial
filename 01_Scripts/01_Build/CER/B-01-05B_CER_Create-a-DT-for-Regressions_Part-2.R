# < Description > *
# > Script Group Indicator Number and Name
# # : B-01, CER
# #
# > Script Number(s)
# # : B-01-05B
# #
# > Purpose of the script(s)
# # : Drop data fields that are unnecessary for regression analysis.

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
# # 1. Path(s) from which Dataset(s) and Script(s) is(are) loaded
# # 1.1. For the DT for regression analysis
FILE_TO.LOAD_CER_FOR.REG <-
  "CER_DT-for-Regressions-with-Survey-Data_Electricity.parquet"
PATH_TO.LOAD_CER_FOR.REG <-
  paste(PATH_DATA_INTERMEDIATE_CER, FILE_TO.LOAD_CER_FOR.REG, sep = "/")


# # 2. Path(s) to which Ouputs will be stored
# # 2.1. For the DT created
FILE_TO.SAVE_CER_FOR.REG <-
  "CER_SubDT-for-Regressions-with-Survey-Data_Electricity.parquet"
PATH_TO.SAVE_CER_FOR.REG <-
  paste(PATH_DATA_INTERMEDIATE_CER, FILE_TO.SAVE_CER_FOR.REG, sep = "/")


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Drop columns that are unnecessary for regression analysis
# ------------------------------------------------------------------------------
# ------- Load the DT for regression analysis -------
# # 1. Load the DT for regression analysis
dt_for.reg <-
  read_parquet(PATH_TO.LOAD_CER_FOR.REG) %>%
    setDT(.)
gc(reset = TRUE)


# ------- Drop unnecessary columns, and then save the DT -------
cols_extract <- c(
  "id", "alloc_group", "alloc_group_desc",
  "alloc_r_tariff", "alloc_r_tariff_desc", "rate_cents.per.kwh",
  "alloc_r_stimulus", "alloc_r_stimulus_desc",
  "rate.change",
  "is_treated_r", "group", "is_treatment.period", "period",
  "is_treatment.and.post",
  "treatment_times_hdd", "post_times_hdd", "treatment.and.post_times_hdd",
  "treatment_times_rate.change",
  "treatment_times_hdd_times_rate.change",
  "treatment.and.post_times_rate.change",
  "treatment.and.post_times_hdd_times_rate.change",
  # "day",
  "date",
  # "datetime",
  "interval_hour", "interval_30min",
  "rate.period", "length_rate.period",
  # "rate.period_detail_level1", "length_rate.period_detail_level1",
  # "rate.period_detail_level2", "length_rate.period_detail_level2",
  "day.of.week",
  # "season",
  "heating.type_space_desc", "heating.type_water_desc",
  "is_weekend", "is_holiday",
  "is_having.zero.consumption.day", "is_missing.date",
  "is_after.ending.daylight.saving.time.in.oct", "is_last.five.days.of.year",
  "is_within.temperature.range",
  "is_date.with.harsh.temperature.only.in.treatment.period",
  "is_elec.heating_space_pre", "is_elec.heating_space_post",
  "is_elec.heating_water_pre", "is_elec.heating_water_post",
  "is_in.sample_incl.control_base",
  "is_in.sample_incl.control_base.only.second.half",
  "is_in.sample_incl.control_case1",
  "is_in.sample_incl.control_case1.only.second.half",
  "is_in.sample_excl.control_base",
  "kwh", "kwh_per.hour",
  "temp_f",
  # "soil_f", "range_temp_f", "range_temp_f_selected",
  # "mean.temp_extremes_f",
  "mean.temp_all_f",
  # "hdd_extremes_65f", "hdd_all_65f", "hdd_soil_65f",
  # "hdd_extremes_60f",
  "hdd_all_60f",
  # "hdd_soil_60f",
  # "diff.in.temp_f_65f", "diff.in.temp_soil_f_65f",
  # "ref.temp_by.season.and.rate.period_f", "hd_by.season.and.rate.period",
  "id_in.factor", "interval_hour_in.factor", "interval_30min_in.factor",
  "day_in.factor", "day.of.week_in.factor", "id.and.hour.interval_in.factor",
  "id.and.30min.interval_in.factor", "id.and.day.of.week_in.factor",
  # "id.and.rate.period.level1_in.factor",
  # "id.and.day.of.week.and.rate.period.level1_in.factor",
  "id.and.day_in.factor",
  "day.of.week.and.hour.interval_in.factor",
  "day.of.week.and.30min.interval_in.factor",
  # "day.of.week.and.rate.period.level1_in.factor",
  "month_in.factor",
  # "month.and.rate.period.level1_in.factor",
  "month.and.30min.interval_in.factor",
  # "month.and.rate.period.level1.and.30min.interval_in.factor",
  "year.and.month_in.factor"
)
# ## Note:
# ## Re-use the array for re-ordering columns, which is defined in in B-01-05A.
sdt_for.reg <- dt_for.reg[, .SD, .SDcols = cols_extract]
write_parquet(
  sdt_for.reg,
  sink = PATH_TO.SAVE_CER_FOR.REG,
  compression = "snappy",
  use_dictionary = TRUE
)
