# < Description > *
# > Script Group Indicator Number and Name
# # : D, Data Dictionary
# #
# > Script Number(s)
# # : D-CER-Trial
# #
# > Purpose of the script(s)
# # : Make a list including labels for data fields in CER datasets.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Set working directory, and run header script
# ------------------------------------------------------------------------------
# ------- Set project name -------
# (Not Applicable)


# ------- Set working directory -------
# (Not Applicable)


# ------- Run the header script -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Define path(s), parameter(s) and function(s)
# ------------------------------------------------------------------------------
# ------- Define path(s) -------
# (Not Applicable)


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Create a List containing Labels for Data Fields
# ------------------------------------------------------------------------------
# ------- Create a List containing Labels for Data Fields -------
# # Note:
# # The list created below includes data fields in the data.table "dt_for.reg".
labels_cer <- list(
  alloc_group = "Allocation Group",
  alloc_group_desc = "Description of Allocation Group",
  alloc_r_stimulus = "Stimulus Group for Residential Customers",
  alloc_r_stimulus_desc = "Description of Stimulus Group for Residential Customers",
  alloc_r_tariff = "Tariff Group for Residential Customers",
  alloc_r_tariff_desc = "Description of Tariff Group for Residential Customers",
  # date = "Date",
  # datetime = "Datetime",
  day = "Day (Day 1 means January 1, 2009)",
  day_in.factor = "FEs: Day (in Factor Data Type)",
  day.of.week = "Day of Week",
  day.of.week_in.factor = "FEs: Day of Week (in Factor Data Type)",
  day.of.week.and.30min.interval_in.factor = "FEs: Day of Week by 30-Minute Interval (in Factor Data Type)",
  day.of.week.and.hour.interval_in.factor = "FEs: Day of Week by 1-Hour Interval (in Factor Data Type)",
  day.of.week.and.rate.period.level1_in.factor = "FEs: Day of Week by Level-1 Rate Period (in Factor Data Type)",
  diff.in.temp_f_65f = "Temperature Difference from 65 Degrees Fahrenheit, Based on Hourly Air Temperatures",
  diff.in.temp_soil_f_65f = "Temperature Difference from 65 Degrees Fahrenheit, Based on Daily Mean 10cm Soil Temperature",
  gmin_c = "Daily Grass Minimum Temperature in Degrees Celsius",
  gmin_f = "Daily Grass Minimum Temperature in Degrees Fahrenheit",
  group = "Either Treatment or Control Group",
  hd_by.season.and.rate.period = "(Hourly) Heating Degrees by Season and Rate Period",
  hdd_all_65f = "Heating Degree Days that are computed by using the Evenly Weighted Mean Temperature, Reference Temperature is 65 Degrees Fahrenheit",
  hdd_all_60f = "Heating Degree Days that are computed by using the Evenly Weighted Mean Temperature, Reference Temperature is 60 Degrees Fahrenheit",
  hdd_extremes_65f = "Heating Degree Days that are computed by using the Minimum- and Maximum-Temperature-based Mean Temperature, Reference Temperature is 65 Degrees Fahrenheit",
  hdd_extremes_60f = "Heating Degree Days that are computed by using the Minimum- and Maximum-Temperature-based Mean Temperature, Reference Temperature is 60 Degrees Fahrenheit",
  hdd_soil_65f = "Heating Degree Days that are computed by using the Daily Mean 10cm Soil Temperature, Reference Temperature is 65 Degrees Fahrenheit",
  hdd_soil_60f = "Heating Degree Days that are computed by using the Daily Mean 10cm Soil Temperature, Reference Temperature is 60 Degrees Fahrenheit",
  id = "Meter ID",
  id_in.factor = "FEs: Meter ID (in Factor Data Type)",
  id.and.30min.interval_in.factor = "FEs: Meter ID by 30-Minute Interval (in Factor Data Type)",
  id.and.day.of.week_in.factor = "FEs: Meter ID by Day of Week (in Factor Data Type)",
  id.and.day.of.week.and.rate.period.level1_in.factor = "FEs: Meter ID by Day of Week by Level-1 Rate Period (in Factor Data Type)",
  id.and.hour.interval_in.factor = "FEs: Meter ID by 1-Hour Interval (in Factor Data Type)",
  id.and.rate.period.level1_in.factor = "FEs: Meter ID by Level-1 Rate Period (in Factor Data Type)",
  id.and.day_infactor = "FEs: Meter ID by Day (in Factor Data Type)",
  interval_30min = "30-Minute Interval",
  interval_30min_in.factor = "FEs: 30-Minute Interval (in Factor Data Type)",
  interval_hour = "1-Hour Interval",
  interval_hour_in.factor = "FEs: 1-Hour Interval (in Factor Data Type)",
  is_after.ending.daylight.saving.time.in.oct = "Indicator Variable: Whether an Observation is about the Period after ending Daylight Saving Time in October",
  is_date.with.harsh.temperature.only.in.treatment.period = "Indicator Variable: Whether the Temperature of a Given Date is out of the Normal Temperature Range, Only for the Treatment Period",
  is_having.zero.consumption.day = "Indicator Variable: Whether a Meter ID indicates 0 kWh during a Day",
  is_holiday = "Indicator Variable: Whether a Given Day is a Holiday",
  is_in.sample_excl.control_base = "Indicator Variable: Whether an Observation is included in the Sample that excludes the Control Group",
  is_in.sample_incl.control_base = "Indicator Variable: Whether an Observation is included in the Sample that includes the Control Group, Base Case",
  is_in.sample_incl.control_base.only.second.half = "Indicator Variable: Whether an Observation is included in the Sample that includes the Control Group, Base Case but only including Observations for the Second Half",
  is_in.sample_incl.control_case1 = "Indicator Variable: Whether an Observation is included in the Sample that includes the Control Group, Case 1",
  is_in.sample_incl.control_case1.only.second.half = "Indicator Variable: Whether an Observation is included in the Sample that includes the Control Group, Case 1 but only including Observations for the Second Half",
  is_last.five.days.of.year = "Indicator Variable: Whether an Observation is for the Last Five Days in Each Year",
  is_missing.date = "Indicator Variable: Whether a Meter ID lacks Observations for Some Dates",
  is_treated_r = "Indicator Variable: Whether a Residential Meter ID is included in the Treatment Group",
  is_treatment.and.post = "Indicator Variable: Whether an Observation is included in both the Treatment Group and the Treatment Period",
  is_treatment.period = "Indicator Variable: Whether a Given Date is included in the Treatment Period",
  is_weekend = "Indicator Variable: Whether an Observation is for Weekend",
  is_within.temperature.range = "Indicator Variable: Whether an Observation is included in Normal Temperature Ranges",
  kwh = "Electricity consumed during 30-Minute Interval in kWh",
  length_rate.period = "Length of Rate Period",
  length_rate.period_detail_level1 = "Length of Rate Period, At the Detailed Level 1",
  length_rate.period_detail_level2 = "Length of Rate Period, At the Detailed Level 2",
  maxtp_c = "Daily Maximum Air Temperature in Degrees Celsius",
  maxtp_f = "Daily Maximum Air Temperature in Degrees Fahrenheit",
  mean.temp_all_f = "Daily Mean Temperature that is computed by evenly weighting Hourly Temperature, In Degrees Fahrenheit",
  mean.temp_extremes_f = "Daily Mean Temperature that is computed only by using the Minimum and Maximum Temperatures in a Given Date, In Degrees Fahrenheit",
  mintp_c = "Daily Minimum Air Temperature in Degrees Celsius",
  mintp_f = "Daily Minimum Air Temperature in Degrees Fahrenheit",
  month_in.factor = "Month of Year (in Factor Data Type)",
  month.and.30min.interval_in.factor = "FEs: Month by 30-Minute Interval (in Factor Data Type)",
  month.and.rate.period.level1_in.factor = "FEs: Month by Level-1 Rate Period (in Factor Data Type)",
  month.and.rate.period.level1.and.30min.interval_in.factor = "FEs: Month by Level-1 Rate Period by 30-Minute Interval (in Factor Data Type)",
  period = "Either Baseline or Treatment Period",
  range_temp_f = "2-Degrees-Fahrenheit-Interval Range of Temperature",
  range_temp_f_selected = "2-Degrees-Fahrenheit-Interval Range of Temperature, Only for Normal Ranges",
  rate_cents.per.kwh = "Cents per kWh",
  rate.period = "Rate Period, One among Night, Day, Peak",
  rate.period_detail_level1 = "Detailed Rate Period, One among Night (23-7), Day: Pre-Peak (8-16), Peak (17-18), Day: Post-Peak (19-22)",
  rate.period_detail_level2 = "Detailed Rate Period, One among Night: Post-Day Transition (23-2), Night: Steady (3-4), Night: Pre-Day Transition (5-7), Day: Pre-Peak Steady (8-14), Day: Pre-Peak Transition (15-16), Peak (17-18), Day: Post-Peak Steady (19-21), Day: Post-Peak Transition (22)",
  ref.temp_by.season.and.rate.period_f = "Maximum Temperature by Season and Rate Period (Either Peak-Warm or Others) for Computing (Hourly) Heating Degrees",
  season = "Either Warm or Cold Season, Only for the Second Half of a Year",
  soil_c = "Daily Mean 10cm Soil Temperature in Degrees Celsius",
  soil_f = "Daily Mean 10cm Soil Temperature in Degrees Fahrenheit",
  temp_c = "Hourly Air Temperature in Degrees Celsius",
  temp_f = "Hourly Air Temperature in Degrees Fahrenheit",
  year.and.month_in.factor = "FEs: Year by Month (in Factor Data Type)"
)
