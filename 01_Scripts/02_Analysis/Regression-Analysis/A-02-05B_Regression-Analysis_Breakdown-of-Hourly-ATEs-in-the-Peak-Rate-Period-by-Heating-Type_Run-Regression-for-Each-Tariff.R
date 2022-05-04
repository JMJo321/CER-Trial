# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Regression-Analysis
# #
# > Script Number(s)
# # : A-02-05B
# #
# > Purpose of the script(s)
# # : Run regressions, by heating type
# #   (three heating types, `Space`, `Water`, and `Both`) and tariff,
# #   to breakdown hourly ATEs.
# #   And then save the regression table(s) in .tex format.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(arrow)
library(stargazer)
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
FILE_TO.LOAD_CER_FOR.REG <-
  "CER_DT-for-Regressions-with-Survey-Data_Electricity.parquet"
PATH_TO.LOAD_CER_FOR.REG <-
  paste(PATH_DATA_INTERMEDIATE_CER, FILE_TO.LOAD_CER_FOR.REG, sep = "/")

# # 1.2. For the R script including regression model(s)
FILE_TO.LOAD_CER_MODELS <- "M-CER-Trial_Regression-Specifications.R"
PATH_TO.LOAD_CER_MODELS <- paste(
  PATH_SCRIPT, FILE_TO.LOAD_CER_MODELS, sep = "/"
)


# # 2. Path(s) to which regression results will be stored
# # 2.1. For regression table(s)
DIR_TO.SAVE_LATEX <- paste(
  PATH_OUTPUT_TABLE,
  "From-Stargazer/Breakdown-of-Hourly-ATEs/DID-Model-with-ID-FEs",
  sep = "/"
)

# # 2.2. For regression result(s)
DIR_TO.SAVE_REG.RESULTS <- paste(
  PATH_DATA_ANALYSIS_REG.RESULTS,
  "Breakdown-of-Hourly-ATEs/DID-Model-with-ID-FEs",
  sep = "/"
)


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# # 1. Function(s) for running regression(s)
get_reg.results <- function (data_in.DT, formula, case_in.vector) {
  func.name <- paste0(
    "get_subsetting.condition.in.str_breakdown.of.ate_",
    "hourly.in.peak_by.heating.type.and.tariff"
  )
  subsetting.condition <-
    get(func.name)(
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
dt_for.reg <- read_parquet(PATH_TO.LOAD_CER_FOR.REG)
setDT(dt_for.reg)

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

dt_for.reg[
  ,
  `:=` (
    treatment_by_hdd = as.numeric(is_treated_r) * hdd_all_60f,
    treatment.period_by_hdd = as.numeric(is_treatment.period) * hdd_all_60f,
    treatment.and.post_by_hdd = as.numeric(is_treatment.and.post) * hdd_all_60f
  )
]


# ------- Create object(s) required to run regression(s) -------
# # 1. Create a list including values for subsetting the DT
# # 1.1. Create a DT including values for subsetting the DT
dt_cases <-
  expand.grid(
    sample = "Base",
    range = "Only Second Half",
    rate.period = "Peak",
    tariff = LETTERS[1:4],
    heating.type = c("Space", "Water", "Both"),
    is_electric.heating = c(TRUE, FALSE),
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
  heating.type <- dt_cases[row]$heating.type
  is_electric.heating <- dt_cases[row]$is_electric.heating

  names <- paste(
    sample_, range, rate.period, tariff, heating.type, is_electric.heating,
    sep = "_"
  )
  list_ <- list(
    c(sample_, range, rate.period, tariff, heating.type, is_electric.heating)
  )
  names(list_) <- names
  list_cases <- c(list_cases, list_)
}


# # 2. Create a list including regression model(s)
list_models <- list(
  model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version1 =
    model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version1
)


# ------- Create object(s) required to make regression table(s) -------
# # 1. Create objects that will be utilized to generate stargazer object(s)
title_ <- paste0(
  "Breakdown of Average Treatment Effects in the Peak Rate Period, ",
  "By Heating Type and Tariff"
)
out.header <- FALSE
column.labels <- c("Electric Heating", "Non-Electric Heating")
column.separate <- c(12, 12)
covariate.labels_text <- c(
  "HDDs",
  "1[Treatment] x HDDs",
  "1[Post]",
  "1[Post] x HDDs",
  "1[Treatment and Post]",
  "1[Treatment and Post] x HDDs"
)
covariate.labels_latex <- c(
  "HDDs",
  "$\\mathbb{1}$[Treatment] $\\times$ HDDs",
  "$\\mathbb{1}$[Post]",
  "$\\mathbb{1}$[Post] $\\times$ HDDs",
  "$\\mathbb{1}$[Treatment \\& Post]",
  "$\\mathbb{1}$[Treatment \\& Post] $\\times$ HDDs"
)
dep.var.caption <- "Dependent Variable"
dep.var.labels <- "Hourly Electricity Consumption  (kWh/Hour)"
add.lines <- list(
  c("Tariff", rep(LETTERS[1:4], times = 6)),
  c(
    "Heating Type",
    rep(rep(c("Space only", "Water only", "Both"), each = 4), times = 2)
  ),
  c("FEs: ID by Half-Hourly Time Window", rep("Yes", times = 24)),
  c("FEs: Day of Week by Half-Hourly Time Window", rep("Yes", times = 24)),
  c("FEs: Month of Year by Half-Hourly Time Window", rep("Yes", times = 24))
)
column.sep.width <- "20pt"
font.size <- "small"
header <- FALSE
label <- paste0(
  "Table:Breakdown-of-Average-Treatment-Effects-in-the-Peak-Rate-Period_",
  "By-Heating-Type-and-Tariff"
)
model.names <- FALSE
omit.stat <- c("ser", "rsq")
omit.table.layout <- "n"
order <- c(
  "hdd_all_60f",
  "treatment_by_hdd",
  "is_treatment.period",
  "treatment.period_by_hdd",
  "is_treatment.and.post",
  "treatment.and.post_by_hdd"
)


# ------- Run regression(s), and then save results -------
n_models <- length(list_models)
for (idx in 1:n_models) {
  # ## Create temporary objects that will be used later
  model.name <-
    names(list_models)[idx] %>%
      str_extract(., "(?<=peak_).+(?=_version1)")
  obj.name_results <- paste0("reg.results_", model.name)
  obj.name_estimates <- paste0("estimates_", model.name)

  # ## Run regression(s)
  print("Be Running Regressions ...")
  assign(
    obj.name_results,
    lapply(
      list_cases,
      FUN = get_reg.results,
      data_in.DT = dt_for.reg,
      formula = list_models[[idx]]
    )
  )

  # ## Create stargazer object(s)
  # ### For a screenshot
  stargazer(
    get(obj.name_results),
    type = "text",
    title = title_,
    out.header = out.header,
    column.labels = column.labels,
    column.separate = column.separate,
    covariate.labels = covariate.labels_text,
    dep.var.caption = dep.var.caption,
    dep.var.labels = dep.var.labels,
    add.lines = add.lines,
    column.sep.width = column.sep.width,
    font.size = font.size,
    header = header,
    label = label,
    model.names = model.names,
    order = order
  )
  # ### For exporting in .tex format
  stargazer(
    get(obj.name_results),
    type = "latex",
    title = title_,
    out = paste(
      DIR_TO.SAVE_LATEX,
      paste0(
        "Breakdown-of-ATEs_Hourly-in-the-Peak-Rate-Period_",
        "By-Heating-Type-and-Tariff.tex"
      ),
      sep = "/"
    ),
    out.header = out.header,
    column.labels = column.labels,
    covariate.labels = covariate.labels_latex,
    dep.var.caption = dep.var.caption,
    dep.var.labels = dep.var.labels,
    add.lines = add.lines,
    column.sep.width = column.sep.width,
    font.size = font.size,
    header = header,
    label = label,
    model.names = model.names,
    omit.stat = omit.stat,
    omit.table.layout = omit.table.layout,
    order = order
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

  # ## Show the current work progress
  print(
    paste0("Estimation is completed : Model ", idx, " out of ", n_models)
  )
}
# ## Save estimates in a separate .RData file
obj.to.save_estimates <- ls()[str_detect(ls(), "^estimates_")]
save(
  list = obj.to.save_estimates,
  file = paste(
    DIR_TO.SAVE_REG.RESULTS,
    paste0(
      "CER_Estimates_Breakdown-of-ATEs_",
      "Hourly-in-the-Peak-Rate-Period_By-Heating-Type-and-Tariff.RData"
    ),
    sep = "/"
  )
)
