# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Descriptive Analysis
# #
# > Script Number(s)
# # : A-02-06A
# #
# > Purpose of the script(s)
# # : Make a histogram showing the distribution of HDDs.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(arrow)
library(stringr)
library(data.table)
library(ggplot2)
library(latex2exp)
library(unikn)


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
# # 2.1. For figure(s)
DIR_TO.SAVE_FIGURE_FOR.DISSERTATION <- "For-Dissertation_Chapter-2"
PATH_TO.SAVE_FIGURE_FOR.DISSERTATION <-
  paste(PATH_OUTPUT_FIGURE, DIR_TO.SAVE_FIGURE_FOR.DISSERTATION, sep = "/")


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
# Create figure(s)
# ------------------------------------------------------------------------------
# ------- Generate DT(s) that will be used to create figure(s) -------
# # 1. Generate DT(s)
# # Note:
# # Only use the second halves of 2009 and 2010.

# # 1.1. Create a DT including eacy day's HDDs
dt_for.plot_hdds <- dt_for.reg[
  month(date) >= 7, .N, by = .(date, period, hdd_all_60f)
][
  , `:=` (N = NULL, date = NULL)
]

# # 1.2. Create a DT containing the mean and median values
tmp_dt_mean <- dt_for.plot_hdds[
  ,
  lapply(.SD, mean, na.rm = TRUE), .SDcols = "hdd_all_60f",
  by = .(period)
][
  ,
  category := "Mean"
]
tmp_dt_median <- dt_for.plot_hdds[
  ,
  lapply(.SD, median, na.rm = TRUE), .SDcols = "hdd_all_60f",
  by = .(period)
][
  ,
  category := "Median"
]
dt_for.plot_mean.and.med <- rbind(tmp_dt_mean, tmp_dt_median)


# ------- Create objects for setting plot options -------
# # 1. Define common plot options
# # 1.1. Create a list including common options
plot.options <- list(
  theme_linedraw(),
  theme(
    strip.text = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 12)),
    legend.text = element_text(margin = margin(r = 10, unit = "pt")),
    legend.position = "bottom",
    legend.margin = margin(-0.2, 0, 0, 0, unit = "cm"),
    legend.box.just = "left",
    legend.box = "vertical",
    legend.box.margin = margin(0.5, 0, 0.1, 0, unit = "cm")
  )
)

# # 1.2. Create color palette(s)
col.pal_custom <- usecol(c("firebrick", "gold", "forestgreen", "steelblue"))


# ------- Create ggplot object(s) -------
plot_dist.of.hdds <-
  ggplot() +
    geom_vline(
      data = dt_for.plot_mean.and.med,
      aes(xintercept = hdd_all_60f, group = period, linetype = category),
      color = "black", alpha = 0.3, lwd = 1
    ) +
    geom_vline(
      data = dt_for.plot_mean.and.med,
      aes(xintercept = hdd_all_60f, color = period, linetype = category)
    ) +
    geom_histogram(
      data = dt_for.plot_hdds,
      aes(
        x = hdd_all_60f, y = ..density..,
        fill = period
      ),
      binwidth = 2, alpha = 0.5, position = "dodge"
    ) +
    scale_color_manual(values = col.pal_custom) +
    scale_fill_manual(values = col.pal_custom) +
    labs(
      x = "Heating Degree Days",
      y = "Frequency",
      color = "Periods", fill = "Periods",
      linetype = "Values"
    ) +
    plot.options


# ------- Export the ggplot object(s) created above -------
export_figure.in.png(
  plot_dist.of.hdds,
  filename_str = paste(
    PATH_TO.SAVE_FIGURE_FOR.DISSERTATION,
    "Figure_Distribution-of-HDDs.png",
    sep = "/"
  ),
  width_numeric = 20, height_numeric = 14
)
