# < Description > *
# > Script Group Indicator Number and Name
# # : H, (Not Applicable)
# #
# > Script Number(s)
# # : H
# #
# > Purpose of the script(s)
# # : R header script for CER Trial Project.

# ------------------------------------------------------------------------------
# Prepare for empirical analysis
# ------------------------------------------------------------------------------
# ------- Load required libraries -------
# (Not Applicable)


# ------- Clear the workspace -------
rm(list = setdiff(ls(), c("PATH_PROJ")))


# ------- Set working directory -------
# (In each R script)


# ------------------------------------------------------------------------------
# Define parameter(s)
# ------------------------------------------------------------------------------
# ------- Define path(s) -------
# # 1. Set folder name(s)
# # 1.1. For scripts
PATH_SCRIPT <- "01_Scripts"
PATH_SCRIPT_BUILD <- paste(PATH_SCRIPT, "01_Build", sep= "/")
PATH_SCRIPT_ANALYSIS <- paste(PATH_SCRIPT, "02_Analysis", sep= "/")

# # 1.2. For data
# # 1.2.1. For folders commonly used
PATH_DATA <- "02_Data"
PATH_DATA_RAW <- paste(PATH_DATA, "01_Raw-Data", sep = "/")
PATH_DATA_RAW_ORIGINAL <- paste(PATH_DATA_RAW, "01_Original", sep = "/")
PATH_DATA_RAW_USE <- paste(PATH_DATA_RAW, "02_Use", sep = "/")
PATH_DATA_INTERMEDIATE <- paste(PATH_DATA, "02_Intermediate-Data", sep = "/")
PATH_DATA_ANALYSIS <- paste(PATH_DATA, "03_For-Analysis", sep = "/")
PATH_DATA_ANALYSIS_REG.RESULTS <-
  paste(PATH_DATA_ANALYSIS, "Regression-Results", sep = "/")
# # 1.2.2. For specific datasets
# # 1.2.2.1. For CER data
PATH_DATA_INTERMEDIATE_CER <- paste(PATH_DATA_INTERMEDIATE, "CER", sep = "/")
PATH_DATA_INTERMEDIATE_CER_METERING <-
  paste(PATH_DATA_INTERMEDIATE_CER, "Metering", sep = "/")
PATH_DATA_INTERMEDIATE_CER_ALLOCATION <-
  paste(PATH_DATA_INTERMEDIATE_CER, "Allocation", sep = "/")
# # 1.2.2.2. For Met Eireann data
PATH_DATA_INTERMEDIATE_WEATHER <-
  paste(PATH_DATA_INTERMEDIATE, "Met-Eireann", sep = "/")

# # 1.3. For output
PATH_OUTPUT <- "03_Output"
PATH_OUTPUT_FIGURE <- paste(PATH_OUTPUT, "01_Figures", sep = "/")
PATH_OUTPUT_TABLE <- paste(PATH_OUTPUT, "02_Tables", sep = "/")


# # 2. Generate the folders defined above
paths <- c(
  PATH_SCRIPT,
  PATH_SCRIPT_BUILD,
  PATH_SCRIPT_ANALYSIS,
  PATH_DATA,
  PATH_DATA_RAW,
  PATH_DATA_RAW_ORIGINAL,
  PATH_DATA_RAW_USE,
  PATH_DATA_INTERMEDIATE,
  PATH_DATA_INTERMEDIATE_CER,
  PATH_DATA_INTERMEDIATE_CER_METERING,
  PATH_DATA_INTERMEDIATE_CER_ALLOCATION,
  PATH_DATA_INTERMEDIATE_WEATHER,
  PATH_DATA_ANALYSIS,
  PATH_OUTPUT,
  PATH_OUTPUT_FIGURE,
  PATH_OUTPUT_TABLE
)
for (path in paths) {
  if (!dir.exists(path)) {
    dir.create(path)
  }
}


# ------- Define parameters commonly used -------
# # 1. For CER data
# # 1.1. About date/datetime
# # 1.1.1. About Trial
DATE_TRIAL_BEGIN <- as.Date("2009-07-01")
DATE_TRIAL_END <- as.Date("2010-12-31")
DATE_BEGIN.OF.TREATMENT <- as.Date("2010-01-01")
# # 1.1.2. Dates about daylight saving time
DAYLIGHT.SAVING.TIME_BEGIN <- 452L
DAYLIGHT.SAVING.TIME_END <- c(298L, 669L)
# ## Note:
# ## Day 298, 452, and 669 correspond to Oct 25, 2009, Mar 28, 2010, and
# ## Oct 31, 2010.
DATE_AFTER.ENDING.DAYLIGHT.SAVING.TIME <- c(
  seq.Date(
    from = as.Date("2009-10-25"), to = as.Date("2009-10-31"), by = "day"
  ),
  as.Date("2010-10-31")
)
# ## Note:
# ## Dates, only in October, after ending daylight saving time.

# # 1.1.3. Public holidays in Ireland
holidays <- c(
  "Jan 1, 2009",   # New Year's Day
  "Mar 17, 2009",   # Saint Patrick's Day
  "Apr 13, 2009",   # Easter Monday
  "May 4, 2009",   # Early May Bank Holiday
  "Jun 1, 2009",   # June Bank Holiday
  "Aug 3, 2009",   # August Bank Holiday
  "Oct 26, 2009",   # October Bank Holiday
  "Dec 25, 2009",   # Christmas Day
  "Dec 26, 2009",   # Saint Stephen's Day
  "Jan 1, 2010",   # New Year's Day
  "Mar 17, 2010",   # Saint Patrick's Day
  "Apr 5, 2010",   # Easter Monday
  "May 3, 2010",   # Early May Bank Holiday
  "Jun 7, 2010",   # June Bank Holiday
  "Aug 2, 2010",   # August Bank Holiday
  "Oct 25, 2010",   # October Bank Holiday
  "Dec 25, 2010",   # Christmas Day
  "Dec 26, 2010"   # Saint Stephen's Day
) %>%
  as.Date(., format = "%b %d, %Y")

# # 1.2. About allocation
# # 1.2.1. Descriptions for allocation groups
list_allocation_groups <- list(
  `1` = list(
    simple = "Residential",
    detail = "Residential"
  ),
  `2` = list(
    simple = "SME",
    detail = "Small-to-Medium Enterprises"
  ),
  `3` = list(
    simple = "Other",
    detail = "Other"
  )
)
# # 1.2.2. Descriptions for information stimuli
# # 1.2.2.1. For residential customers
list_allocation_stimuli_residential <- list(
  `E` = list(
    simple = "Control",
    detail = "Control"
  ),
  `1` = list(
    simple = "Bi-Monthly Bill",
    detail = "Bi-Monthly Detailed Bill"
  ),
  `2` = list(
    simple = "Monthly Bill",
    detail = "Monthly Detailed Bill"
  ),
  `3` = list(
    simple = "Bi-Monthly Bill + IHD",
    detail = "Bi-Monthly Detailed Bill and In-Home Display (IHD)"
  ),
  `4` = list(
    simple = "Bi-Monthly Bill + OLR",
    detail =
      "Bi-Monthly Detailed Bill and Overall Load Reduction (OLR) Incentive"
  )
)
# # 1.2.2.2. For enterprises
list_allocation_stimuli_enterprises <- list(
  `1` = list(
    simple = "Monthly Bill",
    detail = "Monthly Detailed Bill"
  ),
  `2` = list(
    simple = "Bi-Monthly Bill + IOD",
    detail = "Bi-Monthly Detailed Bill and In-Office Display (IOD)"
  ),
  `3` = list(
    simple = "Bi-Monthly Bill + WA",
    detail = "Bi-Monthly Detailed Bill and Web Access to Energy Usage Info."
  ),
  `4` = list(
    simple = "Bi-Monthly Bill",
    detail = "Bi-Monthly Detailed Bill"
  ),
  `C` = list(
    simple = "Control",
    detail = "Control"
  )
)
# # 1.2.3. Descriptions for TOU tariffs
list_allocation_tariffs <- list(
  `E` = list(
    simple = "Control",
    detail = "Control"
  ),
  `A` = list(
    simple = "Tariff A",
    detail = "Tariff A"
  ),
  `B` = list(
    simple = "Tariff B",
    detail = "Tariff B"
  ),
  `C` = list(
    simple = "Tariff C",
    detail = "Tariff C"
  ),
  `D` = list(
    simple = "Tariff D",
    detail = "Tariff D"
  ),
  `W` = list(
    simple = "Weekend Tariff",
    detail = "Weekend Tariff"
  )
)


# # 2. For Met Eireann data
# # 2.1. List of weather stations
list_stations <- list(
  `1875` = "Athenry",
  `675` = "Ballyhaise",
  `2375` = "Belmullet",
  `3723` = "Casement Aerodrome",
  `2175` = "Claremorris",
  `2437` = "Clones",
  `3904` = "Cork Airport",
  `532` = "Dublin Airport",
  `1375` = "Dunsany",
  `2075` = "Finner",
  `1475` = "Gurteen",
  `1775` = "Johnstown",
  `275` = "Mace",
  `1575` = "Malin",
  `1275` = "Markree Castle",
  `575` = "Moore Park",
  `1975` = "Mount Dillon",
  `875` = "Mullingar",
  `1175` = "Newport Furnace",
  `375` = "Oak Park",
  `175` = "Phoenix Park",
  `1075` = "Roches Point SWS",
  `518` = "Shannon Airport",
  `775` = "Sherkin Island"
)


# ------------------------------------------------------------------------------
# Define function(s)
# ------------------------------------------------------------------------------
# ------- Function(s) related to regression analysis -------
# # 1. Function(s) for generating formula(s)
# # 1.1. For `felm` function
get_formula_felm <- function (
  dep.var,
  indep.vars_covariates,
    indep.vars_fes,
    indep.vars_ivs,
    indep.vars_clustered.ses
) {
  indep.vars <- paste(
    indep.vars_covariates,
    indep.vars_fes,
    indep.vars_ivs,
    indep.vars_clustered.ses,
    sep = " | "
  )
  formula_in.str <- paste(dep.var, indep.vars, sep = " ~ ")
  return (formula(formula_in.str))
}


# # 2. Function(s) for creating subsetting condition(s) in string
# # 2.1. For half-hourly ATEs
get_subsetting.condition.in.str_ate_half.hourly <- function (case_in.vector) {
  # ## Note:
  # ## The list must be in the form of
  # ## (
  # ##    `sample` = "Base" or "Case1",
  # ##    `range` = "Both Halves" or "Only Second Half"
  # ## )
  indicator.var.name <- paste0(
    "is_in.sample_incl.control_",
    tolower(case_in.vector[1]),
    if (case_in.vector[2] == "Both Halves") {
      ""
    } else {
      tolower(case_in.vector[2]) %>%
        str_replace_all(., " ", ".") %>%
        paste0(".", .)
    }
  )
  condition_in.str <- paste(
    paste0(indicator.var.name, " == TRUE"),
    sep = " & "
  )
  return (condition_in.str)
}

# # 2.2.  For hourly ATEs
get_subsetting.condition.in.str_ate_hourly.in.peak.by.rate.period <- function (
  case_in.vector
) {
  # ## Note:
  # ## The list must be in the form of
  # ## (
  # ##    `sample` = "Base" or "Case1",
  # ##    `range` = "Both Halves" or "Only Second Half",
  # ##    `rate.period` = "Night", "Day", or "Peak",
  # ##    `tariff` = "A", "B", "C", or "D"
  # ## )
  indicator.var.name <- paste0(
    "is_in.sample_incl.control_",
    tolower(case_in.vector[1]),
    if (case_in.vector[2] == "Both Halves") {
      ""
    } else {
      tolower(case_in.vector[2]) %>%
        str_replace_all(., " ", ".") %>%
        paste0(".", .)
    }
  )
  condition_in.str <- paste(
    paste0(indicator.var.name, " == TRUE"),
    paste0("as.character(rate.period) == '", case_in.vector[3], "'"),
    paste0("alloc_r_tariff %in% c('", case_in.vector[4], "', 'E')"),
    sep = " & "
  )
  return (condition_in.str)
}

# # 2.3. For breaking down hourly ATEs in the peak rate period
get_subsetting.condition.in.str_breakdown.of.ate_hourly.in.peak <-
  function (case_in.vector) {
  # ## Note:
  # ## The list must be in the form of
  # ## (
  # ##    `sample` = "Base" or "Case1",
  # ##    `range` = "Both Halves" or "Only Second Half",
  # ##    `rate.period` = "Night", "Day", or "Peak"
  # ## )
  indicator.var.name <- paste0(
    "is_in.sample_incl.control_",
    tolower(case_in.vector[1]),
    if (case_in.vector[2] == "Both Halves") {
      ""
    } else {
      tolower(case_in.vector[2]) %>%
        str_replace_all(., " ", ".") %>%
        paste0(".", .)
    }
  )
  condition_in.str <- paste(
    paste0(indicator.var.name, " == TRUE"),
    paste0("as.character(rate.period) == '", case_in.vector[3], "'"),
    sep = " & "
  )
  return (condition_in.str)
}

# # 2.4. For breaking down hourly ATEs in the peak rate period, by tariff
get_subsetting.condition.in.str_breakdown.of.ate_hourly.in.peak_by.tariff <-
  function (case_in.vector) {
  # ## Note:
  # ## The list must be in the form of
  # ## (
  # ##    `sample` = "Base" or "Case1",
  # ##    `range` = "Both Halves" or "Only Second Half",
  # ##    `rate.period` = "Night", "Day", or "Peak",
  # ##    `tariff` = "A", "B", "C", or "D"
  # ## )
  indicator.var.name <- paste0(
    "is_in.sample_incl.control_",
    tolower(case_in.vector[1]),
    if (case_in.vector[2] == "Both Halves") {
      ""
    } else {
      tolower(case_in.vector[2]) %>%
        str_replace_all(., " ", ".") %>%
        paste0(".", .)
    }
  )
  condition_in.str <- paste(
    paste0(indicator.var.name, " == TRUE"),
    paste0("as.character(rate.period) == '", case_in.vector[3], "'"),
    paste0("alloc_r_tariff %in% c('", case_in.vector[4], "', 'E')"),
    sep = " & "
  )
  return (condition_in.str)
}

# # 3. Extract estimates
# # 3.1. From a `felm` object
get_estimates_from.felm <- function (
  felm.object,
  level = 0.95, # The confidence level to use for the confidence interval
  fe = FALSE, # Logical indicating whether or not to include estimates of FEs
  se.type = "robust" # One of "default", "iid", "robust", or "cluster"
) {
  dt_estimates <-
    broom::tidy( # Refer to the document for broom::tidy.felm
      felm.object,
      conf.int = TRUE, conf.level = level,
      fe = fe,
      se.type = se.type
    ) %>%
      data.table::setDT(.)
  return (dt_estimates)
}


# ------- Functions for miscellaneous tasks -------
# # 1. To export a ggplot object in PNG format
export_figure.in.png <- function (
  ggplot.obj, filename_str,
  width_numeric = 20, height_numeric = 10, units_str = "cm", dpi_int = 320
) {
  ggplot2::ggsave(
    plot = ggplot.obj,
    filename = filename_str,
    dpi = dpi_int,
    width = width_numeric,
    height = height_numeric,
    units = units_str,
    device = "png",
    limitsize = FALSE
  )
}


# # 2. To label a DT's columns
label_data.fields <- function(
  dt_in.str, data.field_to.label_in.str, list_labels
) {
  tmp_obj.name <- paste(dt_in.str, data.field_to.label_in.str, sep = "$")
  data.table::setattr(
    eval(parse(text = tmp_obj.name)),
    name = "label",
    value = list_labels[[data.field_to.label_in.str]]
  )
}
