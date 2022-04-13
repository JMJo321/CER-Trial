# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Regression-Analysis
# #
# > Script Number(s)
# # : A-02-03A-1
# #
# > Purpose of the script(s)
# # : Run regressions with various models to breakdown hourly ATEs.
# #   Models utilized in this script will be exploited in A-02-03A.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(arrow)
library(lfe)
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
# # 1.1. For the DT for regression analysis
FILE_TO.LOAD_CER_FOR.REG <- "CER_DT-for-Regressions_Electricity.parquet"
PATH_TO.LOAD_CER_FOR.REG <-
  paste(PATH_DATA_INTERMEDIATE_CER, FILE_TO.LOAD_CER_FOR.REG, sep = "/")

# # 1.2. For the R script including regression model(s)
FILE_TO.LOAD_CER_MODELS <- "M-CER-Trial_Regression-Specifications.R"
PATH_TO.LOAD_CER_MODELS <- paste(
  PATH_SCRIPT, FILE_TO.LOAD_CER_MODELS, sep = "/"
)


# # 2. Path(s) to which regression results will be stored
DIR_TO.SAVE_REG.RESULTS <-
  paste(
    PATH_DATA_ANALYSIS_REG.RESULTS,
    "Breakdown-of-Hourly-ATEs", "Model-wtih-Two-Indicator-Variables",
    sep = "/"
  )


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# # 1. Function(s) for running regression(s)
get_reg.results <- function (data_in.DT, formula, case_in.vector) {
  subsetting.condition <-
    get_subsetting.condition.in.str_breakdown.of.ate_hourly.in.peak(
      case_in.vector
    )
  reg.results <- felm(
    data = data_in.DT[eval(parse(text = subsetting.condition))],
    formula = formula
  )
  return (reg.results)
}


# ------------------------------------------------------------------------------
# Run regression(s) to estimate hourly ATEs in the peak rate period
# ------------------------------------------------------------------------------
# ------- Load the DT for regression analysis -------
# # 1. Load required DT(s) and/or script(s) for regression analysis
# # 1.1. Load the DT for regression analysis
dt_for.reg <-
  read_parquet(PATH_TO.LOAD_CER_FOR.REG) %>%
    setDT(.)

# # 1.2. Load the R script including model specification(s)
source(PATH_TO.LOAD_CER_MODELS)


# # 2. Modify the DT loaded
# # 2.1. Add column(s)
# # 2.1.1. A column contenting hourly electricity consumption
dt_for.reg[
  ,
  kwh_per.hour := lapply(.SD, sum, na.rm = TRUE), .SDcols = "kwh",
  by = .(id, date, interval_hour)
]
# # 2.1.2. Columns that will be used when running regressions.
dt_for.reg[
  ,
  `:=` (
    treatment_by_hdd = as.numeric(is_treated_r) * hdd_all_60f,
    treatment.period_by_hdd = as.numeric(is_treatment.period) * hdd_all_60f,
    treatment.and.post_by_hdd = as.numeric(is_treatment.and.post) * hdd_all_60f
  )
]
# # 2.1.3. Columns utilized as FEs
dt_for.reg[, week.of.year := lubridate::week(date)]
dt_for.reg[
  ,
  week.of.sample_in.factor :=
    paste(year(date), week.of.year, sep = "-") %>%
      as.factor(.)
]
dt_for.reg[
  ,
  week.of.sample.and.30min.interval_in.factor :=
    as.character(week.of.sample_in.factor) %>%
      paste(., interval_30min, sep = "-") %>%
      as.factor(.)
]


# ------- Create object(s) required to run regression(s) -------
# # 1. Create a list including values for subsetting the DT
# # 1.1. Create a DT including values for subsetting the DT
dt_cases <-
  expand.grid(
    # sample = c("Base", "Case1"),
    # range = c("Both Halves", "Only Second Half"),
    sample = "Base",
    range = "Only Second Half",
    rate.period = "Peak",
    stringsAsFactors = FALSE
  ) %>%
    setDT(.)

# # 1.2. Convert the DT into a list
list_cases <- list()
for (row in 1:dt_cases[, .N]) {
  sample_ <- dt_cases[row]$sample
  range <- dt_cases[row]$range
  rate.period <- dt_cases[row]$rate.period

  names <- paste(sample_, range, rate.period, sep = "_")
  list_ <- list(c(sample_, range, rate.period))
  names(list_) <- names
  list_cases <- c(list_cases, list_)
}


# # 2. Create a list including regression model(s)
list_models <- list(
  model_breakdown.of.ate_hourly.in.peak_dw.mw =
    model_breakdown.of.ate_hourly.in.peak_dw.mw,
  model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version1 =
    model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version1,
  model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version4 =
    model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version4,
  model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version5 =
    model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version5,
  model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version2 =
    model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version2,
  model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version6 =
    model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version6,
  model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version7 =
    model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version7,
  model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version3 =
    model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version3,
  model_breakdown.of.ate_hourly.in.peak_iw.dw.mw =
    model_breakdown.of.ate_hourly.in.peak_iw.dw.mw,
  model_breakdown.of.ate_hourly.in.peak_iw.dw.mw.s =
    model_breakdown.of.ate_hourly.in.peak_iw.dw.mw.s,
  model_breakdown.of.ate_hourly.in.peak_iw.dw.mw.sw =
    model_breakdown.of.ate_hourly.in.peak_iw.dw.mw.sw
)


# ------- Run regression(s) -------
n_cases <- length(list_cases)
for (idx in 1:n_cases) {
  # ## Create temporary objects that will be used later
  obj.name_results <-
    names(list_cases)[idx] %>%
      tolower(.) %>%
      str_replace_all(., " ", ".") %>%
      paste0("reg.results_", .)
  obj.name_estimates <-
    names(list_cases)[idx] %>%
      tolower(.) %>%
      str_replace_all(., " ", ".") %>%
      paste0("estimates_", .)
  case.name <-
    names(list_cases)[idx] %>%
      str_replace_all(., " ", "-")

  # ## Run regression(s)
  print("Be Running Regressions ...")
  assign(
    obj.name_results,
    lapply(
      list_models,
      FUN = get_reg.results,
      data_in.DT = dt_for.reg,
      case_in.vector = list_cases[[idx]]
    )
  )

  # ## Show the current work progress
  print(
    paste0("Estimation is completed : Model ", idx, " out of ", n_cases)
  )
}


# ------- Make a regression table -------
title_ <- "Breakdown of Average Treatment Effects in the Peak Rate Period"
covariate.labels_text <- c(
  "HDDs",
  "1[Treatment]",
  "1[Treatment] x HDDs",
  "1[Post]",
  "1[Post] x HDDs",
  "1[Treatment and Post]",
  "1[Treatment and Post] x HDDs"
)
dep.var.caption <- "Dependent Variable"
dep.var.labels <- "Hourly Electricity Consumption  (kWh/Hour)"
add.lines <- list(
  c("FEs: ID by Half-Hourly Time Window", "No", rep("Yes", times = 10)),
  c("FEs: Day of Week by Half-Hourly Time Window", rep("Yes", times = 11)),
  c("FEs: Month of Year by Half-Hourly Time Window", rep("Yes", times = 11)),
  c("FEs: Week of Sample", rep("No", times = 9), "Yes", "No"),
  c(
    "FEs: Week of Sample by Half-Hourly Time Window",
    rep("No", times = 10), "Yes"
  )
)
model.names <- FALSE
omit.stat <- c("ser", "rsq")
omit.table.layout <- "n"
order <- c(
  "hdd_all_60f",
  "is_treated_r",
  "treatment_by_hdd",
  "is_treatment.period",
  "treatment.period_by_hdd",
  "is_treatment.and.post",
  "treatment.and.post_by_hdd"
)

stargazer::stargazer(
  reg.results_base_only.second.half_peak,
  type = "text",
  title = title_,
  covariate.labels = covariate.labels_text,
  dep.var.caption = dep.var.caption,
  dep.var.labels = dep.var.labels,
  add.lines = add.lines,
  model.names = model.names,
  omit.stat = omit.stat,
  omit.table.layout = omit.table.layout,
  order = order
)
