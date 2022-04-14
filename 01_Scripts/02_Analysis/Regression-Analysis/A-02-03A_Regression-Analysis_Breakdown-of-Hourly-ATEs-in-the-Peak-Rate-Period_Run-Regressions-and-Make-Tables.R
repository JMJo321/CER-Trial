# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Regression-Analysis
# #
# > Script Number(s)
# # : A-02-03A
# #
# > Purpose of the script(s)
# # : Run regressions to breakdown hourly ATEs and make regression tables.
# #   And then save the regression results in .RData format.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(arrow)
library(lfe)
library(stargazer)
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


# # 2. Path(s) to which outpus will be stored
# # 2.1. For regression table(s)
DIR_TO.SAVE_LATEX <- paste(
  PATH_OUTPUT_TABLE,
  "From-Stargazer/Breakdown-of-Hourly-ATEs/DID-Model-with-ID-FEs",
  sep = "/"
)

# # 2.2. For regression result(s)
DIR_TO.SAVE_REG.RESULTS <-
  paste(
    PATH_DATA_ANALYSIS_REG.RESULTS,
    "Breakdown-of-Hourly-ATEs/DID-Model-with-ID-FEs",
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
# # 2.1.2. Add columns that will directly utilized when running regressions
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
  model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version6 =
    model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version6
)


# ------- Create object(s) required to make regression table(s) -------
# # 1. Create objects that will be utilized to generate stargazer object(s)
title_ <- "Breakdown of Average Treatment Effects in the Peak Rate Period"
out.header <- FALSE
covariate.labels_text <- c(
  "HDDs",
  "1[Treatment]",
  "1[Treatment] x HDDs",
  "1[Post]",
  "1[Post] x HDDs",
  "1[Treatment and Post]",
  "1[Treatment and Post] x HDDs"
)
covariate.labels_latex <- c(
  "HDDs",
  "$\\mathbb{1}$[Treatment]",
  "$\\mathbb{1}$[Treatment] $\\times$ HDDs",
  "$\\mathbb{1}$[Post]",
  "$\\mathbb{1}$[Post] $\\times$ HDDs",
  "$\\mathbb{1}$[Treatment \\& Post]",
  "$\\mathbb{1}$[Treatment \\& Post] $\\times$ HDDs"
)
dep.var.caption <- "Dependent Variable"
dep.var.labels <- "Hourly Electricity Consumption  (kWh/Hour)"
add.lines <- list(
  c("FEs: ID by Half-Hourly Time Window", "No", rep("Yes", times = 2)),
  c("FEs: Day of Week by Half-Hourly Time Window", rep("Yes", times = 3)),
  c("FEs: Month of Year by Half-Hourly Time Window", rep("Yes", times = 3))
)
column.sep.width <- "20pt"
font.size <- "small"
header <- FALSE
label <- "Table:Breakdown-of-Average-Treatment-Effects-in-the-Peak-Rate-Period"
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


# ------- Run regression(s), and then save results -------
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

  # # 1.2. Create stargazer object(s)
  # # 1.2.1. For a screenshot
  stargazer(
    get(obj.name_results),
    type = "text",
    title = title_,
    out.header = out.header,
    # column.labels = "All",
    covariate.labels = covariate.labels_text,
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
  # # 1.2.2. For exporting in .tex format
  stargazer(
    get(obj.name_results),
    type = "latex",
    title = title_,
    out = paste(
      DIR_TO.SAVE_LATEX,
      "Breakdown-of-ATEs_Hourly-in-the-Peak-Rate-Period.tex",
      sep = "/"
    ),
    out.header = out.header,
    # column.labels = "All",
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

  # ## Save regression result(s)
  print("Be Saving Regression Results ...")
  assign(
    obj.name_results,
    get(obj.name_results)[2]
  )
  save(
    list = obj.name_results,
    # ## Note:
    # ## Save the regression result from the second model only.
    file = paste(
      DIR_TO.SAVE_REG.RESULTS,
      paste0(
        "CER_Regression-Results_Breakdown-of-ATEs_",
        "Hourly-in-the-Peak-Rate-Period_",
        paste0(case.name, ".RData")
      ),
      sep = "/"
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
# ## Note:
# ## This work takes about 1 hour.
# ## Save estimates in a separate .RData file
obj.to.save_estimates <- ls()[str_detect(ls(), "^estimates_")]
save(
  list = obj.to.save_estimates,
  file = paste(
    DIR_TO.SAVE_REG.RESULTS,
    "CER_Estimates_Breakdown-of-ATEs_Hourly-in-the-Peak-Rate-Period.RData",
    sep = "/"
  )
)
