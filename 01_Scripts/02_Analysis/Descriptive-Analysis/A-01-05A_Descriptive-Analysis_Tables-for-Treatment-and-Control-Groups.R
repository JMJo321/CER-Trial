# < Description > *
# > Script Group Indicator Number and Name
# # : A-01, Descriptive Analysis
# #
# > Script Number(s)
# # : A-01-05A
# #
# > Purpose of the script(s)
# # : To generate a table showing treatment and control group assignments.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(arrow)
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
  "CER_SubDT-for-Regressions-with-Survey-Data_Electricity.parquet"
PATH_TO.LOAD_CER_FOR.REG <-
  paste(PATH_DATA_INTERMEDIATE_CER, FILE_TO.LOAD_CER_FOR.REG, sep = "/")


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
# Create a table showing treatment and control group assignments
# ------------------------------------------------------------------------------
# ------- Create a table -------
# # 1. Create a temporary DT by subsetting the DT for regression analysis
# # 1.1. Create a temporary DT
tmp_dt_assign <- dt_for.reg[
  is_in.sample_incl.control_base.only.second.half == TRUE,
  .N,
  by = .(alloc_r_tariff_desc, alloc_r_stimulus_desc, id)
][
  ,
  .N,
  by = .(alloc_r_tariff_desc, alloc_r_stimulus_desc)
]

# # 1.2. Modify the temporary DT
# # 1.2.1. Convert columns' data type
tmp_dt_assign[
  ,
  alloc_r_stimulus_desc := factor(
    alloc_r_stimulus_desc,
    levels = c(
      "Monthly Bill", "Bi-Monthly Bill",
      "Bi-Monthly Bill + IHD", "Bi-Monthly Bill + OLR", "Control"
    )
  )
]


# # 2. Crate a table by dcasting the temporary DT
# # 2.1. Create a table
dt_assign <- dcast(
  tmp_dt_assign, alloc_r_stimulus_desc ~ alloc_r_tariff_desc,
  fill = 0,
  value.var = "N"
)

# # 2.2. Modify the table
names(dt_assign) <- c("Stimulus", "Control", LETTERS[1:4])
setcolorder(dt_assign, c(1, 3:6))
dt_assign
