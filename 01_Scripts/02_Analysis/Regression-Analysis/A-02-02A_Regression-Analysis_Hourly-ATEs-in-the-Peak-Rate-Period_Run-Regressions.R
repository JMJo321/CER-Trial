# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Regression-Analysis
# #
# > Script Number(s)
# # : A-02-02A
# #
# > Purpose of the script(s)
# # : Run regressions to estimate the ATEs in the peak rate period. And then
# #   save the regression results in .RData format.

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
  paste(PATH_DATA_ANALYSIS_REG.RESULTS, "Hourly-ATEs", sep = "/")


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# # 1. Function(s) for running regression(s)
get_reg.results <- function (data_in.DT, formula, case_in.vector) {
  subsetting.condition <-
    get_subsetting.condition.in.str_ate_hourly.in.peak(case_in.vector)
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
dt_for.reg <- read_parquet(PATH_TO.LOAD_CER_FOR.REG)

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


# ------- Create object(s) required to run regression(s) -------
# # 1. Create a list including values for subsetting the DT
# # 1.1. Create a DT including values for subsetting the DT
dt_cases <-
  expand.grid(
    sample = c("Base", "Case1"),
    range = c("Both Halves", "Only Second Half"),
    rate.period = "Peak",
    tariff = c("A", "B", "C", "D"),
    stringsAsFactors = FALSE
  ) %>%
    setDT(.)

# # 1.2. Convert the DT into a list
list_cases <- list()
for (row in 1:dt_cases[, .N]) {
  sample_ <- dt_cases[row]$sample
  range <- dt_cases[row]$range
  rate.period <- dt_cases[row]$rate.period
  tariff <- dt_cases[row]$tariff

  names <- paste(sample_, range, rate.period, tariff, sep = "_")
  list_ <- list(c(sample_, range, rate.period, tariff))
  names(list_) <- names
  list_cases <- c(list_cases, list_)
}


# # 2. Create a list including regression model(s)
list_models <- list(
  model_ate_hourly.in.peak_iw.dw.m = model_ate_hourly.in.peak_iw.dw.m
)


# ------- Run regression(s), and then save results -------
utils::timestamp()
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

  # ## Save regression result(s)
  print("Be Saving Regression Results ...")
  save(
    list = obj.name_results,
    file = paste(
      DIR_TO.SAVE_REG.RESULTS,
      paste0(
        "CER_Regression-Results_ATEs_Hourly-in-the-Peak-Rate-Period_",
        paste0(case.name, ".RData")
      ),
      sep = "/"
    )
  )

  # ## Extract estimates
  assign(
    obj.name_estimates,
    lapply(
      get(obj.name_results),
      get_estimates_from.felm,
      level = 0.95,
      fe = FALSE,
      se.type = "cluster"
    )
  )

  # ## Remove regression result(s)
  rm(list= obj.name_results)
  gc(reset = TRUE, full = TRUE)

  # ## Show the current work progress
  print(
    paste0("Estimation is completed : Model ", idx, " out of ", n_cases)
  )
}
utils::timestamp()
# ## Save estimates in a separate .RData file
obj.to.save_estimates <- ls()[str_detect(ls(), "^estimates_")]
save(
  list = obj.to.save_estimates,
  file = paste(
    DIR_TO.SAVE_REG.RESULTS,
    "CER_Estimates_ATEs_Hourly-in-the-Peak-Rate-Period.RData",
    sep = "/"
  )
)
