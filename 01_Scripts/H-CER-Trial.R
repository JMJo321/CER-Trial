# < Description > *
# > Script Group Indicator Number and Name
# # : H, (Not Applicable)
# #
# > Script Number(s)
# # : H
# #
# > Purpose of the script(s)
# # : R header script for CER Trial Project.

# ------------------------------------------------------------------------------
# Prepare for empirical analysis
# ------------------------------------------------------------------------------
# ------- Load required libraries -------
# (Not Applicable)


# ------- Clear the workspace -------
rm(list = setdiff(ls(), c("PATH_PROJ")))


# ------- Set working directory -------
# (In each R script)


# ------------------------------------------------------------------------------
# Define parameter(s)
# ------------------------------------------------------------------------------
# ------- Define path(s) -------
# # 1. Set folder name(s)
# # 1.1. For scripts
PATH_SCRIPT <- "01_Scripts"
PATH_SCRIPT_BUILD <- paste(PATH_SCRIPT, "01_Build", sep= "/")
PATH_SCRIPT_ANALYSIS <- paste(PATH_SCRIPT, "02_Analysis", sep= "/")

# # 1.2. For data
PATH_DATA <- "02_Data"
PATH_DATA_RAW <- paste(PATH_DATA, "01_Raw-Data", sep = "/")
PATH_DATA_RAW_ORIGINAL <- paste(PATH_DATA_RAW, "01_Original", sep = "/")
PATH_DATA_RAW_USE <- paste(PATH_DATA_RAW, "02_Use", sep = "/")
PATH_DATA_INTERMEDIATE <- paste(PATH_DATA, "02_Intermediate-Data", sep = "/")
PATH_DATA_ANALYSIS <- paste(PATH_DATA, "03_For-Analysis", sep = "/")

# # 1.3. For output
PATH_OUTPUT <- "03_Output"


# # 2. Generate the folders defined above
paths <- c(
  PATH_SCRIPT,
  PATH_SCRIPT_BUILD,
  PATH_SCRIPT_ANALYSIS,
  PATH_DATA,
  PATH_DATA_RAW,
  PATH_DATA_RAW_ORIGINAL,
  PATH_DATA_RAW_USE,
  PATH_DATA_INTERMEDIATE,
  PATH_DATA_ANALYSIS,
  PATH_OUTPUT
)
for (path in paths) {
  if (!dir.exists(path)) {
    dir.create(path)
  }
}


# ------------------------------------------------------------------------------
# Define function(s)
# ------------------------------------------------------------------------------
# ------- Functions for miscellaneous tasks -------
# # 1. To export a ggplot object in PNG format
export_figure.in.png <- function (
  ggplot.obj, filename_str,
  width_numeric = 20, height_numeric = 10, units_str = "cm", dpi_int = 320
) {
  ggplot2::ggsave(
    plot = ggplot.obj,
    filename = filename_str,
    dpi = dpi_int,
    width = width_numeric,
    height = height_numeric,
    units = units_str,
    device = "png",
    limitsize = FALSE
  )
}
