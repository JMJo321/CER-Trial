# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Regression Analysis
# #
# > Script Number(s)
# # : A-02-02C
# #
# > Purpose of the script(s)
# # : Create a regression table in .tex format.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
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
# # 1.1. For DTs including felm object(s)
DIR_TO.LOAD_CER_REG.RESULTS <-
  paste(PATH_DATA_ANALYSIS_REG.RESULTS, "Hourly-ATEs", sep = "/")


# # 2. Path(s) to which regression results will be stored
DIR_TO.SAVE_LATEX <-
  paste(PATH_OUTPUT_TABLE, "From-Stargazer", sep = "/")


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Create a regression table
# ------------------------------------------------------------------------------
# ------- Load .RData file(s) including regression results -------
sample_ <- "base"
range <- "only.second.half"
filename_base <- paste(
  "CER_Regression-Results_ATEs_Hourly-in-the-Peak-Rate-Period",
  str_to_title(sample_),
  str_replace_all(range, "\\.", " ") %>%
    str_to_title(.) %>%
    str_replace_all(., " ", "-"),
  "Peak",
  sep = "_"
)
for (tariff in LETTERS[1:4]) {
  filename <- paste0(paste(filename_base, tariff, sep = "_"), ".RData")
  PATH_TO.LOAD_CER_REG.RESULTS <-
    paste(DIR_TO.LOAD_CER_REG.RESULTS, filename, sep = "/")
  load(PATH_TO.LOAD_CER_REG.RESULTS)
}


# ------- Create stargazer object(s) -------
# # 1. Create objects that will be utilized to generate stargazer object(s)
title_ <- "Average Treatment Effects in the Peak Rate Period"
out <- paste(
  DIR_TO.SAVE_LATEX,
  paste0("ATEs_Hourly-in-the-Peak-Rate-Period", ".tex"),
  sep = "/"
)
out.header <- FALSE
column.labels <- c(paste0("Tariff ", LETTERS[1:4]))
covariate.labels <- "$\\mathbb{1}$[Treatment \\& Post]"
dep.var.caption <- "Dependent Variable"
dep.var.labels <- "Hourly Electricity Consumption  (kWh/Hour)"
add.lines <- list(
  c("FEs: Household by Half-Hourly Time Window", rep("Yes", times = 4)),
  c("FEs: Day of Week by Half-Hourly Time Window", rep("Yes", times = 4)),
  c("FEs: Month of Year", rep("Yes", times = 4))
)
column.sep.width <- "20pt"
font.size <- "small"
header <- FALSE
label <- "Table:Average-Treatment-Effects-in-the-Peak-Rate-Period"
model.names <- FALSE
omit.stat <- c("ser", "rsq")
omit.table.layout <- "n"


# # 2. Create stargazer object(s)
# # 2.1. For a screenshot
stargazer(
  list(
    mget(ls()[str_detect(ls(), "^reg.results")])
  ),
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

# # 2.2. For exporting in .tex format
stargazer(
  list(
    mget(ls()[str_detect(ls(), "^reg.results")])
  ),
  type = "latex",
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
