# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Regression Analysis
# #
# > Script Number(s)
# # : A-02-04B
# #
# > Purpose of the script(s)
# # : Create a DT including estimates.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
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
FILE_TO.LOAD_CER_ESTIMATES <- paste0(
  "CER_Estimates_Breakdown-of-ATEs_",
  "Hourly-in-the-Peak-Rate-Period_For-Each-Tariff.RData"
)
DIR_TO.LOAD_CER_ESTIMATES <- "Breakdown-of-Hourly-ATEs"
PATH_TO.LOAD_CER_ESTIMATES <- paste(
  PATH_DATA_ANALYSIS_REG.RESULTS,
  DIR_TO.LOAD_CER_ESTIMATES,
  FILE_TO.LOAD_CER_ESTIMATES,
  sep = "/"
)


# # 2. Path(s) to which the DT created will be stored
FILE_TO.SAVE_CER_ESTIMATES <-
  "CER_Breakdown-of-ATEs_Hourly-in-the-Peak-Rate-Period_For-Each-Tariff.RData"
DIR_TO.SAVE_CER_ESTIMATES <-
  paste(PATH_DATA_ANALYSIS_REG.RESULTS, "Breakdown-of-Hourly-ATEs", sep = "/")
PATH_TO.SAVE_CER_ESTIMATES <-
  paste(DIR_TO.SAVE_CER_ESTIMATES, FILE_TO.SAVE_CER_ESTIMATES, sep = "/")


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# # 1. Convert a list including estimates in DT format into a DT
get_estimates.in.dt <- function (obj.name_in.str) {
  # ## Create objects that will be used later
  model <-
    get(obj.name_in.str) %>%
      names(.) %>%
      str_extract(., "(?<=peak_).+$") %>%
      str_replace_all(., "\\.", "-")
  sample_ <-
    str_split(obj.name_in.str, "_")[[1]][2] %>%
      str_to_title(.)
  range <-
    str_split(obj.name_in.str, "_")[[1]][3] %>%
      str_replace_all(., "\\.", " ") %>%
      str_to_title(.)
  rate.period <-
    str_split(obj.name_in.str, "_")[[1]][4] %>%
      str_to_title(.)
  tariff <-
    str_split(obj.name_in.str, "_")[[1]][5] %>%
      str_to_title(.)
  # ## Extract DT(s)
  dt <- get(obj.name_in.str)[[1]]
  # ## Add columns
  dt[
    ,
    `:=` (
      model = model,
      sample = sample_,
      range = range,
      rate.period = rate.period,
      tariff = tariff
    )
  ]
  # ## Return the DT created
  return (dt)
}


# ------------------------------------------------------------------------------
# Create a DT from lists incl. estimates
# ------------------------------------------------------------------------------
# ------- Load lists including regression results -------
load(PATH_TO.LOAD_CER_ESTIMATES)


# ------- Convert lists into a DT -------
obj.names_estimates <- ls()[str_detect(ls(), "^estimates_")]
dt_estimates <-
  lapply(obj.names_estimates, get_estimates.in.dt) %>%
    rbindlist(.)


# ------- Save the DT created -------
save(dt_estimates, file = PATH_TO.SAVE_CER_ESTIMATES)
