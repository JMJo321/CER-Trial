# < Description > *
# > Script Group Indicator Number and Name
# # : B-01, CER
# #
# > Script Number(s)
# # : B-01-02A
# #
# > Purpose of the script(s)
# # : To ingest allocation data.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(arrow)
library(openxlsx)
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
# # 1. Path(s) for loading dataset(s)
# # 1.1. For CER allocation data
DIR_TO.LOAD_CER_ALLOCATION <- paste(
  "CER/38_CER Electricity_Gas/CER Electricity Revised March 2012",
  "CER_Electricity_Documentation",
  sep = "/"
)
FILE_TO.LOAD_CER_ALLOCATION <- "SME and Residential allocations.xlsx"
PATH_TO.LOAD_CER_ALLOCATION <- paste(
  PATH_DATA_RAW_USE,
  DIR_TO.LOAD_CER_ALLOCATION,
  FILE_TO.LOAD_CER_ALLOCATION,
  sep = "/"
)


# # 2. Path(s) for saving output
# # 2.1. For CER allocation data
FILE_TO.SAVE_CER_ALLOCATION <- "CER_Allocation_Electricity.parquet"
PATH_TO.SAVE_CER_ALLOCATION <- paste(
  PATH_DATA_INTERMEDIATE_CER_ALLOCATION, FILE_TO.SAVE_CER_ALLOCATION, sep = "/"
)


# ------- Define parameter(s) -------
# (Refer to the R header script)



# ------- Define function(s) -------
# # 1. To extact a value from a list
get_description <- function(colname, list) {
  return(list[[colname]][["simple"]])
}


# ------------------------------------------------------------------------------
# Ingest Allocation Data for Electricity
# ------------------------------------------------------------------------------
# ------- Create a DT by importing an Excel file -------
# # 1. Import an Excel file
# # 1.1. Create a DT
dt_allocation_elec <-
  readWorkbook(PATH_TO.LOAD_CER_ALLOCATION, sheet = "Sheet1", cols = 1:5) %>%
    setDT(.)
colnames <-
  c("id", "alloc_group", "alloc_r_tariff", "alloc_r_stimulus", "alloc_sme")
names(dt_allocation_elec) <- colnames

# # 1.2. Check the primary key(s), and then set keys
stopifnot(dt_allocation_elec[, .N, by = .(id)][N > 1, .N] == 0)
setkey(dt_allocation_elec, "id")


# ------- Modify the DT created above -------
# # 1. Add column(s)
# # 1.1. Add columns that include descriptions for codes
dt_allocation_elec[
  ,
  `:=` (
    alloc_group_desc =
      lapply(
        alloc_group, get_description,
        list = LIST_ALLOCATION_GROUPS
      ) %>%
        as.vector(., mode = "character"),
    alloc_r_tariff_desc =
      lapply(
        alloc_r_tariff, get_description,
        list = LIST_ALLOCATION_TARIFFS
      ) %>%
        as.vector(., mode = "character"),
    alloc_r_stimulus_desc =
      lapply(
        alloc_r_stimulus, get_description,
        list = LIST_ALLOCATION_STIMULI_RESIDENTIAL
      ) %>%
        as.vector(., mode = "character"),
    alloc_sme_desc =
      lapply(
        alloc_sme, get_description,
        list = LIST_ALLOCATION_STIMULI_ENTERPRISES
      ) %>%
        as.vector(., mode = "character")
  )
]


# # 2. Change values
# # 2.1. From NULL to NA
for (col in names(dt_allocation_elec)) {
  dt_allocation_elec[is.null(get(col)), eval(col) := NA]
  dt_allocation_elec[get(col) == "NULL", eval(col) := NA]
}


# # 3. Convert data types
# # 3.1. From numeric to integer
dt_allocation_elec[
  ,
  `:=` (
    id = as.integer(id),
    alloc_group = as.integer(alloc_group)
  )
]


# # 4. Re-order columns
cols_reorder <- c(
  "id",
  "alloc_group", "alloc_group_desc",
  "alloc_r_tariff", "alloc_r_tariff_desc",
  "alloc_r_stimulus", "alloc_r_stimulus_desc",
  "alloc_sme", "alloc_sme_desc"
)
setcolorder(dt_allocation_elec, cols_reorder)


# ------------------------------------------------------------------------------
# Save the DT in Parquet format
# ------------------------------------------------------------------------------
# ------- Save the DT in Parquet format -------
write_parquet(
  dt_allocation_elec,
  sink = PATH_TO.SAVE_CER_ALLOCATION,
  compression = "snappy",
  use_dictionary = TRUE
)
