# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Regression Analysis
# #
# > Script Number(s)
# # : A-02-06B
# #
# > Purpose of the script(s)
# # : Run regression(s), and then export the result(s) in TEX format.
# #   1) Breakdown of average treatment effects in and near the peak rate
# #      period.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(arrow)
library(stringr)
library(data.table)
library(lfe)
library(stargazer)


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
  "CER_SubDT-for-Regressions-with-Survey-Data_Electricity.parquet"
PATH_TO.LOAD_CER_FOR.REG <-
  paste(PATH_DATA_INTERMEDIATE_CER, FILE_TO.LOAD_CER_FOR.REG, sep = "/")

# # 1.2. For the R script including regression model(s)
FILE_TO.LOAD_CER_MODELS <- "M-CER-Trial_Regression-Specifications.R"
PATH_TO.LOAD_CER_MODELS <- paste(
  PATH_SCRIPT, FILE_TO.LOAD_CER_MODELS, sep = "/"
)

# # 2. Path(s) to which regression results will be stored
DIR_TO.SAVE_LATEX <-
  paste(PATH_OUTPUT_TABLE, "For-Dissertation_Chapter-2", sep = "/")


# ------- Define parameter(s) -------
# # 1. Parameters for making figure(s)
# # 1.1. Value of knot for spline regression(s)
KNOT <- 15


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Load dataset(s) and/or script(s) required to run regression(s)
# ------------------------------------------------------------------------------
# ------- Load necessary dataset(s) and/or script(s) -------
# # 1. Load required DT(s) for regression analysis
# # 1.1. Load the DT for regression analysis
dt_for.reg <-
  read_parquet(PATH_TO.LOAD_CER_FOR.REG) %>%
    setDT(.)
gc(reset = TRUE)


# # 2. Load required script(s) for regression analysis
# # 2.1. Load the R script including model specification(s)
source(PATH_TO.LOAD_CER_MODELS)


# ------------------------------------------------------------------------------
# Run regression(s), and then export the result(s)
# ------------------------------------------------------------------------------
# ------- Modify the DT for regression analysis -------
# # 1. Add columns
# # 1.1. Add columns related to knot(s)
dt.oper_add.col_knot(dt_for.reg, KNOT)


# ------- Run regression(s) -------
list_reg.results <- lapply(
  LIST_INTERVALS[1:3],
  get_felm.obj_interval.hour,
  dt = dt_for.reg,
  formula = get_change.terms.in.formula(
    model_breakdown.of.ate_hourly.in.peak_spline_dw,
    term_old_in.str = "hdd.knot", term_new_in.str = paste0("hdd.knot", KNOT)
  )
)
stargazer(list_reg.results, type = "text")


# ------- Create stargazer object(s) -------
# # 1. Create objects that will be utilized to generate stargazer object(s)
title_ <- "Breakdown of Hourly Average Treatment Effects"
out <- paste(
  DIR_TO.SAVE_LATEX,
  paste0(
    "Stargazer-Table_Breakdown-of-Hourly-ATEs",
    ".tex"
  ),
  sep = "/"
)
out.header <- FALSE
column.labels <- NULL
covariate.labels <- c(
  "HDDs",
  "HDDs$^{*}$",
  "$\\mathbb{1}$[Treatment]",
  "$\\mathbb{1}$[Treatment] $\\times$ HDDs",
  "$\\mathbb{1}$[Treatment] $\\times$ HDDs$^{*}$",
  "$\\mathbb{1}$[Post]",
  "$\\mathbb{1}$[Post] $\\times$ HDDs",
  "$\\mathbb{1}$[Post] $\\times$ HDDs$^{*}$",
  "$\\mathbb{1}$[Treatment \\& Post]",
  "$\\mathbb{1}$[Treatment \\& Post] $\\times$ HDDs",
  "$\\mathbb{1}$[Treatment \\& Post] $\\times$ HDDs$^{*}$"
)
dep.var.caption <- "Dependent Variable"
dep.var.labels <- "Hourly Electricity Consumption  (kWh/Hour)"
add.lines <- list(
  c("Description of Interval", c("Pre-Peak", "Peak", "Post-Peak")),
  c("Interval of Hours", names(LIST_INTERVALS[1:3])),
  c("Knot", rep(KNOT, times = 3)),
  c("FEs: Day of Week by Half-Hourly Time Window", rep("Yes", times = 3))
)
column.sep.width <- "95pt"
font.size <- "small"
header <- FALSE
label <- "Table:Breakdown-of-Hourly-ATEs"
model.names <- FALSE
omit.stat <- c("ser", "rsq")
omit.table.layout <- "n"


# # 2. Export the stargazer object(s)
# # 2.1. Just print the stargazer object(s)
stargazer(
  list_reg.results,
  type = "text",
  title = title_,
  out.header = out.header,
  column.labels = column.labels,
  covariate.labels = covariate.labels,
  dep.var.caption = dep.var.caption,
  dep.var.labels = dep.var.labels,
  add.lines = add.lines,
  column.sep.width = column.sep.width,
  font.size = font.size,
  header = header,
  label = label,
  model.names = model.names,
  omit.stat = omit.stat
)

# # 2.2. Export the stargazer object(s) in TEX format
stargazer(
  list_reg.results,
  type = "text",
  title = title_,
  out = out,
  out.header = out.header,
  column.labels = column.labels,
  covariate.labels = covariate.labels,
  dep.var.caption = dep.var.caption,
  dep.var.labels = dep.var.labels,
  add.lines = add.lines,
  column.sep.width = column.sep.width,
  font.size = font.size,
  header = header,
  label = label,
  model.names = model.names,
  omit.stat = omit.stat,
  omit.table.layout = omit.table.layout
)
