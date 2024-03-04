# < Description > *
# > Script Group Indicator Number and Name
# # : A-03, Additional Analysis
# #
# > Script Number(s)
# # : A-03-01A
# #
# > Purpose of the script(s)
# # : Run regression(s) to measure the relationship between household
# #   electricity consumption and daily HDDs.

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


# ------------------------------------------------------------------------------
# Run regression(s)
# ------------------------------------------------------------------------------
# ------- Run regression(s) -------
# # 1. Run regression(s)
reg.result_1 <- felm(
  formula =
    kwh_per.hour ~
      hdd_all_60f |
      0 |
      0 |
      id_in.factor + day_in.factor,
  data = dt_for.reg[is_in.sample_incl.control_base.only.second.half == TRUE]
)
reg.result_2 <- felm(
  formula =
    kwh_per.hour ~
      hdd_all_60f |
      id_in.factor |
      0 |
      id_in.factor + day_in.factor,
  data = dt_for.reg[is_in.sample_incl.control_base.only.second.half == TRUE]
)
reg.result_3 <- felm(
  formula =
    kwh_per.hour ~
      hdd_all_60f |
      id_in.factor + month_in.factor |
      0 |
      id_in.factor + day_in.factor,
  data = dt_for.reg[is_in.sample_incl.control_base.only.second.half == TRUE]
)
reg.result_4 <- felm(
  formula =
    kwh_per.hour ~
      hdd_all_60f |
      id.and.30min.interval_in.factor |
      0 |
      id_in.factor + day_in.factor,
  data = dt_for.reg[is_in.sample_incl.control_base.only.second.half == TRUE]
)
reg.result_5 <- felm(
  formula =
    kwh_per.hour ~
      hdd_all_60f |
      id.and.30min.interval_in.factor +
        day.of.week.and.30min.interval_in.factor |
      0 |
      id_in.factor + day_in.factor,
  data = dt_for.reg[is_in.sample_incl.control_base.only.second.half == TRUE]
)
reg.result_6 <- felm(
  formula =
    kwh_per.hour ~
      hdd_all_60f |
      id.and.30min.interval_in.factor +
        day.of.week.and.30min.interval_in.factor +
        month_in.factor |
      0 |
      id_in.factor + day_in.factor,
  data = dt_for.reg[is_in.sample_incl.control_base.only.second.half == TRUE]
)

# # 2. Combine regression results in a list
list_reg.results <- list(
  reg.result_1, reg.result_2, reg.result_3,
  reg.result_4, reg.result_5, reg.result_6
)


# ------- Create stargazer object(s) -------
# # 1. Create objects that will be utilized to generate stargazer object(s)
title_ <- "The Relationship between Household Electricity Consumption and Daily Heating Degree Days"
out <- paste(
  DIR_TO.SAVE_LATEX,
  paste0(
    "Stargazer-Table_Relationship-between-Household-Electricity-Consumption-and-Daily-HDDs",
    ".tex"
  ),
  sep = "/"
)
out.header <- FALSE
column.labels <- NULL
covariate.labels_text <- c(
  "HDDs",
  "(Constant)"
)
covariate.labels_latex <- c(
  "HDDs",
  "(Constant)"
)
dep.var.caption <- "Dependent Variable"
dep.var.labels <- "Hourly Electricity Consumption  (kWh/Hour)"
add.lines <- list(
  c("FEs: Household", c("No", rep("Yes", times = 2), rep("No", times = 3))),
  c("FEs: Month of Year", rep(c("No", "No", "Yes"), times = 2)),
  c("FEs: Household by Half-Hourly Time Time Window",
    c(rep("No", times = 3), rep("Yes", times = 3))
  ),
  c(
    "FEs: Day of Week by Half-Hourly Time Window",
    c(rep("No", times = 4), rep("Yes", times = 2))
  )
)
column.sep.width <- "95pt"
font.size <- "small"
header <- FALSE
label <- "Table:Relationship-between-Household-Electricity-Consumption-and-Daily-HDDs"
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
  covariate.labels = covariate.labels_text,
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
  omit.table.layout = omit.table.layout
)
