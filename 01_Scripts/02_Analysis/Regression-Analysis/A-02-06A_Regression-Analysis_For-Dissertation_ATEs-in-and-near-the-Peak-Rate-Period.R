# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Regression Analysis
# #
# > Script Number(s)
# # : A-02-06A
# #
# > Purpose of the script(s)
# # : Run regression(s), and then export the result(s) in TEX format.
# #   1) Average treatment effects in and near the peak rate period by rate
# #      period and tariff group.

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
# (Not Applicable)


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
# ------- Run regression(s) -------
# # 1. Create object(s) that will be used to run regression(s)
list_tariff.group <-
  as.list(LETTERS[1:4]) %>%
    c(., list(LETTERS[1:4]))
names(list_tariff.group) <- c(LETTERS[1:4], "All")


# # 2. Run regression(s)
# # 2.1. For pre-peak hours
list_reg.results_pre <- lapply(
  list_tariff.group,
  get_felm.obj_interval.hour_by.tariff,
  dt = dt_for.reg,
  formula = model_ate_hourly.in.peak_iw.dw.m,
  interval.hours_in.array = LIST_INTERVALS[[1]]
)

# # 2.2. For peak hours
list_reg.results_peak <- lapply(
  list_tariff.group,
  get_felm.obj_interval.hour_by.tariff,
  dt = dt_for.reg,
  formula = model_ate_hourly.in.peak_iw.dw.m,
  interval.hours_in.array = LIST_INTERVALS[[2]]
)

# # 2.3. For post-peak hours
list_reg.results_post <- lapply(
  list_tariff.group,
  get_felm.obj_interval.hour_by.tariff,
  dt = dt_for.reg,
  formula = model_ate_hourly.in.peak_iw.dw.m,
  interval.hours_in.array = LIST_INTERVALS[[3]]
)


# ------- Create stargazer object(s) -------
# # 1. Create objects that will be utilized to generate stargazer object(s)
list_reg.results_for.body <- c(
  list_reg.results_peak[1:4],
  list_reg.results_pre[5], list_reg.results_peak[5], list_reg.results_post[5]
)
list_reg.results_for.appendix <- c(
  list_reg.results_pre[1:4],
  list_reg.results_peak[1:4],
  list_reg.results_post[1:4]
)

title_ <- "Average Treatment Effects in and near the Peak Rate Period"
out_body <- paste(
  DIR_TO.SAVE_LATEX,
  paste0(
    "Stargazer-Table_ATEs_Hourly-in-and-near-the-Peak-Rate-Period",
    ".tex"
  ),
  sep = "/"
)
out_body_se <- paste(
  DIR_TO.SAVE_LATEX,
  paste0(
    "Stargazer-Table_ATEs_Hourly-in-and-near-the-Peak-Rate-Period_With-SE",
    ".tex"
  ),
  sep = "/"
)
out_appendix <- paste(
  DIR_TO.SAVE_LATEX,
  paste0(
    "Stargazer-Table_ATEs_Hourly-in-and-near-the-Peak-Rate-Period_For-Appendix",
    ".tex"
  ),
  sep = "/"
)
out_appendix_se <- paste(
  DIR_TO.SAVE_LATEX,
  paste0(
    "Stargazer-Table_ATEs_Hourly-in-and-near-the-Peak-Rate-Period_With-SE_For-Appendix",
    ".tex"
  ),
  sep = "/"
)
out.header <- FALSE
column.labels <- NULL
covariate.labels <- "$\\mathbb{1}$[Treatment \\& Post]"
dep.var.caption <- "Dependent Variable"
dep.var.labels <- "Hourly Electricity Consumption  (kWh/Hour)"
add.lines_for.body <- list(
  c(
    "Description of Interval",
    c(rep("Peak", each = 4), "Pre-Peak", "Peak", "Post-Peak")
  ),
  c(
    "Interval of Hours",
    c(rep(names(LIST_INTERVALS[2]), each = 4), names(LIST_INTERVALS[1:3]))
  ),
  c("Tariff Group", c(LETTERS[1:4], rep("All", times = 3))),
  c(
    "Price Change in the Peak Rate Period",
    c(RATE.CHANGES, rep("[-]", times = 3))
  ),
  c("FEs: Household by Half-Hourly Time Window", rep("Yes", times = 7)),
  c("FEs: Day of Week by Half-Hourly Time Window", rep("Yes", times = 7)),
  c("FEs: Month of Year", rep("Yes", times = 7))
)
add.lines_for.appendix <- list(
  c(
    "Description of Interval",
    rep(c("Pre-Peak", "Peak", "Post-Peak"), each = 4)
  ),
  c(
    "Interval of Hours",
    rep(names(LIST_INTERVALS[1:3]), each = 4)
  ),
  c("Tariff Group", rep(LETTERS[1:4], times = 3)),
  c(
    "Price Change in the Peak Rate Period",
    rep(RATE.CHANGES, times = 3)
  ),
  c("FEs: Household by Half-Hourly Time Window", rep("Yes", times = 12)),
  c("FEs: Day of Week by Half-Hourly Time Window", rep("Yes", times = 12)),
  c("FEs: Month of Year", rep("Yes", times = 12))
)
column.sep.width <- "1pt"
font.size <- "small"
header <- FALSE
label <- "Table:Average-Treatment-Effects-in-and-near-the-Peak-Rate-Period"
model.names <- FALSE
omit.stat <- c("ser", "rsq")
omit.table.layout <- "n"


# # 2. Export the stargazer object(s)
# # 2.1. Just print the stargazer object(s)
# # 2.1.1. Table(s) for body
stargazer(
  list_reg.results_for.body,
  type = "text",
  title = title_,
  out.header = out.header,
  column.labels = column.labels,
  covariate.labels = covariate.labels,
  dep.var.caption = dep.var.caption,
  dep.var.labels = dep.var.labels,
  add.lines = add.lines_for.body,
  column.sep.width = column.sep.width,
  font.size = font.size,
  header = header,
  label = label,
  model.names = model.names,
  omit.stat = omit.stat
)
stargazer(
  list_reg.results_for.body,
  type = "text",
  title = title_,
  out.header = out.header,
  column.labels = column.labels,
  covariate.labels = covariate.labels,
  dep.var.caption = dep.var.caption,
  dep.var.labels = dep.var.labels,
  ci = TRUE, ci.level = 0.95,
  add.lines = add.lines_for.body,
  column.sep.width = column.sep.width,
  font.size = font.size,
  header = header,
  label = label,
  model.names = model.names,
  omit.stat = omit.stat
)
# # 2.1.2. Table(s) for appendix
stargazer(
  list_reg.results_for.appendix,
  type = "text",
  title = title_,
  out.header = out.header,
  column.labels = column.labels,
  covariate.labels = covariate.labels,
  dep.var.caption = dep.var.caption,
  dep.var.labels = dep.var.labels,
  add.lines = add.lines_for.appendix,
  column.sep.width = column.sep.width,
  font.size = font.size,
  header = header,
  label = label,
  model.names = model.names,
  omit.stat = omit.stat
)
stargazer(
  list_reg.results_for.appendix,
  type = "text",
  title = title_,
  out.header = out.header,
  column.labels = column.labels,
  covariate.labels = covariate.labels,
  dep.var.caption = dep.var.caption,
  dep.var.labels = dep.var.labels,
  ci = TRUE, ci.level = 0.95,
  add.lines = add.lines_for.appendix,
  column.sep.width = column.sep.width,
  font.size = font.size,
  header = header,
  label = label,
  model.names = model.names,
  omit.stat = omit.stat
)

# # 2.2. Export the stargazer object(s) in TEX format
# # 2.2.1. Table(s) for body
stargazer(
  list_reg.results_for.body,
  type = "text",
  title = title_,
  out = out_body,
  out.header = out.header,
  column.labels = column.labels,
  covariate.labels = covariate.labels,
  dep.var.caption = dep.var.caption,
  dep.var.labels = dep.var.labels,
  add.lines = add.lines_for.body,
  column.sep.width = column.sep.width,
  font.size = font.size,
  header = header,
  label = label,
  model.names = model.names,
  omit.stat = omit.stat,
  omit.table.layout = omit.table.layout
)
stargazer(
  list_reg.results_for.body,
  type = "text",
  title = title_,
  out = out_body_se,
  out.header = out.header,
  column.labels = column.labels,
  covariate.labels = covariate.labels,
  dep.var.caption = dep.var.caption,
  dep.var.labels = dep.var.labels,
  ci = TRUE, ci.level = 0.95,
  add.lines = add.lines_for.body,
  column.sep.width = column.sep.width,
  font.size = font.size,
  header = header,
  label = label,
  model.names = model.names,
  omit.stat = omit.stat,
  omit.table.layout = omit.table.layout
)
# # 2.2.2. Table(s) for appendix
stargazer(
  list_reg.results_for.appendix,
  type = "text",
  title = title_,
  out = out_appendix,
  out.header = out.header,
  column.labels = column.labels,
  covariate.labels = covariate.labels,
  dep.var.caption = dep.var.caption,
  dep.var.labels = dep.var.labels,
  add.lines = add.lines_for.appendix,
  column.sep.width = column.sep.width,
  font.size = font.size,
  header = header,
  label = label,
  model.names = model.names,
  omit.stat = omit.stat,
  omit.table.layout = omit.table.layout
)
stargazer(
  list_reg.results_for.appendix,
  type = "text",
  title = title_,
  out = out_appendix_se,
  out.header = out.header,
  column.labels = column.labels,
  covariate.labels = covariate.labels,
  dep.var.caption = dep.var.caption,
  dep.var.labels = dep.var.labels,
  ci = TRUE, ci.level = 0.95,
  add.lines = add.lines_for.appendix,
  column.sep.width = column.sep.width,
  font.size = font.size,
  header = header,
  label = label,
  model.names = model.names,
  omit.stat = omit.stat,
  omit.table.layout = omit.table.layout
)
