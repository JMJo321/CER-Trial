# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Regression Analysis
# #
# > Script Number(s)
# # : A-02-04C
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
  paste(PATH_DATA_ANALYSIS_REG.RESULTS, "Breakdown-of-Hourly-ATEs", sep = "/")


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
# ------- Create object(s) that will be used to make regression table(s) -------
# # 1. Object(s) for loading .RData file(s) incl. regression result(s)
sample_ <- "base"
range <- "only.second.half"
filename_base <- paste(
  paste0(
    "CER_Regression-Results_Breakdown-of-ATEs_",
    "Hourly-in-the-Peak-Rate-Period_For-Each-Tariff"
  ),
  str_to_title(sample_),
  str_replace_all(range, "\\.", " ") %>%
    str_to_title(.) %>%
    str_replace_all(., " ", "-"),
  "Peak",
  sep = "_"
)


# # 2. Create objects that will be utilized to generate stargazer object(s)
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
  c("FEs: Day of Week by Half-Hourly Time Window", rep("Yes", times = 4)),
  c("FEs: Month of Year by Half-Hourly Time Window", rep("Yes", times = 4))
)
column.sep.width <- "20pt"
font.size <- "small"
header <- FALSE
label <- paste0(
  "Table:Breakdown-of-Average-Treatment-Effects-in-the-Peak-Rate-Period_",
  "For-Each-Tariff"
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


# ------- Create stargazer object(s) -------
# # 1. For Tariff A and B
# # 1.1. Load .RData file(s)
filename <- paste0(
  "CER_Regression-Results_Breakdown-of-ATEs_Hourly-in-the-Peak-Rate-Period_",
  "Base_Only-Second-Half_Peak.RData"
)
PATH_TO.LOAD_CER_REG.RESULTS <-
  paste(DIR_TO.LOAD_CER_REG.RESULTS, filename, sep = "/")
load(PATH_TO.LOAD_CER_REG.RESULTS)

# # 1.2. Create stargazer object(s)
# # 1.2.1. For a screenshot
stargazer(
  list(
    mget(ls()[str_detect(ls(), "^reg.results")])
  ),
  type = "text",
  title = title_,
  out.header = out.header,
  column.labels = "All",
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
  list(
    mget(ls()[str_detect(ls(), "^reg.results")])
  ),
  type = "latex",
  title = title_,
  out = paste(
    DIR_TO.SAVE_LATEX,
    "Breakdown-of-ATEs_Hourly-in-the-Peak-Rate-Period_All.tex",
    sep = "/"
  ),
  out.header = out.header,
  column.labels = "All",
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


# # 2. For Tariff A and B
# # 2.1. Load .RData file(s)
# # 2.1.1. Remove the .RData file(s) already loaded from the workspace
rm(list = ls()[str_detect(ls(), "^reg.results_")])
gc(reset = TRUE, full = TRUE)
# # 2.1.2. Load .RData file(s)
for (tariff in LETTERS[1:2]) {
  filename <- paste0(paste(filename_base, tariff, sep = "_"), ".RData")
  PATH_TO.LOAD_CER_REG.RESULTS <-
    paste(DIR_TO.LOAD_CER_REG.RESULTS, filename, sep = "/")
  load(PATH_TO.LOAD_CER_REG.RESULTS)
}

# # 2.2. Create stargazer object(s)
# # 2.2.1. For a screenshot
stargazer(
  list(
    mget(ls()[str_detect(ls(), "^reg.results")])
  ),
  type = "text",
  title = title_,
  out.header = out.header,
  column.labels = c(paste0("Tariff ", LETTERS[1:2])),
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
# # 2.2.2. For exporting in .tex format
stargazer(
  list(
    mget(ls()[str_detect(ls(), "^reg.results")])
  ),
  type = "latex",
  title = title_,
  out = paste(
    DIR_TO.SAVE_LATEX,
    paste0(
      "Breakdown-of-ATEs_Hourly-in-the-Peak-Rate-Period_",
      "For-Each-Tariff_A-and-B.tex"
    ),
    sep = "/"
  ),
  out.header = out.header,
  column.labels = c(paste0("Tariff ", LETTERS[1:2])),
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


# # 3. For Tariff C and D
# # 3.1. Load .RData file(s)
# # 3.1.1. Remove the .RData file(s) already loaded from the workspace
rm(list = ls()[str_detect(ls(), "^reg.results_")])
gc(reset = TRUE, full = TRUE)
# # 3.1.2. Load .RData file(s)
for (tariff in LETTERS[3:4]) {
  filename <- paste0(paste(filename_base, tariff, sep = "_"), ".RData")
  PATH_TO.LOAD_CER_REG.RESULTS <-
    paste(DIR_TO.LOAD_CER_REG.RESULTS, filename, sep = "/")
  load(PATH_TO.LOAD_CER_REG.RESULTS)
}

# # 3.2. Create stargazer object(s)
# # 3.2.1. For a screenshot
stargazer(
  list(
    mget(ls()[str_detect(ls(), "^reg.results")])
  ),
  type = "text",
  title = title_,
  out.header = out.header,
  column.labels = c(paste0("Tariff ", LETTERS[3:4])),
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
# # 3.2.2. For exporting in .tex format
stargazer(
  list(
    mget(ls()[str_detect(ls(), "^reg.results")])
  ),
  type = "latex",
  title = title_,
  out = paste(
    DIR_TO.SAVE_LATEX,
    paste0(
      "Breakdown-of-ATEs_Hourly-in-the-Peak-Rate-Period_",
      "For-Each-Tariff_C-and-D.tex"
    ),
    sep = "/"
  ),
  out.header = out.header,
  column.labels = c(paste0("Tariff ", LETTERS[3:4])),
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
