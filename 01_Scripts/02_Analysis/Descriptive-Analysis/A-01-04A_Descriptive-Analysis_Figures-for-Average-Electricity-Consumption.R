# < Description > *
# > Script Group Indicator Number and Name
# # : A-01, Descriptive Analysis
# #
# > Script Number(s)
# # : A-01-04A
# #
# > Purpose of the script(s)
# # : To make figure(s) illustrating average electricity consumption.
# #   1) Average hourly electricity consumption by time of day
# #   2) Average daily electricity consumption with average daily temperatures.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(zoo)
library(ggplot2)
library(unikn)
library(latex2exp)
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


# # 2. Path(s) to which regression results will be stored
# # 2.1. For figure(s)
DIR_TO.SAVE_FIGURE <- "Average-kWh"
PATH_TO.SAVE_FIGURE <- paste(PATH_OUTPUT_FIGURE, DIR_TO.SAVE_FIGURE, sep = "/")
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
# Make DT(s) that will be utilized to create ggplot object(s)
# ------------------------------------------------------------------------------
# ------- Make a DT for average hourly consumption by time of day -------
# # 1. Make a DT
dt_avg.kwh_within.day <- dt_for.reg[
  is_in.sample_incl.control_base.only.second.half == TRUE,
  .N,
  by = .(interval_hour, group, period, kwh_per.hour, id)
][
  ,
  lapply(.SD, mean, na.rm = TRUE), .SDcols = "kwh_per.hour",
  by = .(interval_hour, group, period)
]


# # 2. Modify the DT
# # 2.1. Add data field(s)
dt_avg.kwh_within.day[, hour.for.plot := interval_hour + 0.5]

# # 2.2. Replace value(s)
dt_avg.kwh_within.day[, period.for.plot := paste(period, "Period")]


# ------- Make a DT for average daily consumption -------
# # 1. Make a DT
# # 1.1. Make temporary DTs
tmp_dt_kwh <- dt_for.reg[
  is_in.sample_incl.control_base == TRUE,
  .N,
  by = .(date, group, id, kwh, interval_30min)
][
  ,
  lapply(.SD, sum, na.rm = TRUE), .SDcols = "kwh",
  by = .(date, group, id)
]

tmp_dt_avg.kwh <- tmp_dt_kwh[
  ,
  lapply(.SD, mean, na.rm = TRUE), .SDcols = "kwh",
  by = .(date, group)
]
tmp_dt_sd <- tmp_dt_kwh[
  ,
  lapply(.SD, sd, na.rm = TRUE), .SDcols = "kwh",
  by = .(date, group)
]
setnames(tmp_dt_sd, "kwh", "kwh_sd")
tmp_dt_n <- tmp_dt_kwh[
  ,
  .N,
  by = .(date, group)
]

# # 1.2. Create a DT by merging temporary DTs
dt_avg.kwh_by.date <-
  merge(x = tmp_dt_avg.kwh, y = tmp_dt_sd, by = c("date", "group"))
dt_avg.kwh_by.date <-
  merge(x = dt_avg.kwh_by.date, y = tmp_dt_n, by = c("date", "group"))


# # 2. Modify the DT
# # 2.1. Add data field(s)
dt_avg.kwh_by.date[, kwh_se := kwh_sd / sqrt(N)]
dt_avg.kwh_by.date[, kwh_upper := kwh + kwh_se]
dt_avg.kwh_by.date[, kwh_lower := kwh - kwh_se]


# ------------------------------------------------------------------------------
# Create figure(s)
# ------------------------------------------------------------------------------
# ------- Create objects for setting plot options -------
# # 1. Define common plot options
# # 1.1. Create a list including common options
plot.options <- list(
  theme_linedraw(),
  theme(
    strip.text = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 12)),
    axis.title.y.right = element_text(margin = margin(l = 12)),
    legend.text = element_text(margin = margin(r = 10, unit = "pt")),
    legend.position = "bottom"
  )
)

# # 1.2. Create color palette(s)
col.pal_custom <- usecol(c("firebrick", "gold", "forestgreen", "steelblue"))

# # 1.2. Set the scale factor for the second y axis
sec.axis_scale.factor <- 2


# ------- Create ggplot object(s) -------
# # 1. Figure for average hourly electricity consumption by time of day
plot_avg.kwh_within.day <-
  ggplot() +
    geom_line(
      data = dt_avg.kwh_within.day,
      aes(x = hour.for.plot, y = kwh_per.hour, group = group),
      color = "black", alpha = 0.3, lwd = 1
    ) +
    geom_line(
      data = dt_avg.kwh_within.day,
      aes(x = hour.for.plot, y = kwh_per.hour, color = group)
    ) +
    geom_point(
      data = dt_avg.kwh_within.day,
      aes(x = hour.for.plot, y = kwh_per.hour, shape = group),
      color = "black", alpha = 0.3, size = 2.5
    ) +
    geom_point(
      data = dt_avg.kwh_within.day,
      aes(x = hour.for.plot, y = kwh_per.hour, shape = group, color = group),
      size = 2.0
    ) +
    facet_grid(. ~ period.for.plot) +
    scale_x_continuous(breaks = seq(0, 24, by = 2)) +
    scale_y_continuous(breaks = seq(0.0, 2.0, by = 0.2)) +
    scale_color_manual(values = col.pal_custom[c(1, 2)]) +
    scale_shape_manual(values = 15:16) +
    labs(
      x = "Hour of Day",
      y = "Average Hourly Consumption  (kWh/Hour)",
      color = "Groups ",
      shape = "Groups "
    ) +
    plot.options


# # 2. Figure for average daily electricity consumption with average daily
# #    temperatures
plot_avg.kwh_by.date <-
  ggplot() +
    geom_vline(
      xintercept = as.Date("2010-01-01"),
      linetype = "dotdash", alpha = 0.3
    ) +
    geom_line(
      data = dt_for.reg[, .N, by = .(date, mean.temp_all_f)],
      aes(x = date, y = mean.temp_all_f / sec.axis_scale.factor),
      color = "black", alpha = 0.3, lwd = 1
    ) +
    geom_line(
      data = dt_for.reg[, .N, by = .(date, mean.temp_all_f)],
      aes(x = date, y = mean.temp_all_f / sec.axis_scale.factor),
      color = col.pal_custom[4]
    ) +
    geom_ribbon(
      data = dt_avg.kwh_by.date,
      aes(x = date, ymin = kwh_lower, ymax = kwh_upper, fill = group),
      alpha = 0.5
    ) +
    geom_line(
      data = dt_avg.kwh_by.date,
      aes(x = date, y = kwh, group = group),
      color = "black", alpha = 0.3, lwd = 1
    ) +
    geom_line(
      data = dt_avg.kwh_by.date,
      aes(x = date, y = kwh, color = group)
    ) +
    scale_x_date(
      date_labels = "%b., %Y",
      date_breaks = "2 months"
    ) +
    scale_y_continuous(
      sec.axis = sec_axis(
        trans = ~ . * sec.axis_scale.factor,
        name = TeX(r'(Average Daily Temperature $ (\degree F)$)')
      )
    ) +
    scale_color_manual(values = col.pal_custom[c(1, 2)]) +
    scale_fill_manual(values = col.pal_custom[c(1, 2)]) +
    labs(
      x = "Date",
      y = "Average Daily Consumption  (kWh/Day)",
      color = "Groups ",
      fill = "Groups "
    ) +
    plot.options


# ------- Export the ggplot object(s) in PNG format -------
# # 1. Figure for average hourly electricity consumption by time of day
export_figure.in.png(
  plot_avg.kwh_within.day,
  filename_str = paste(
    PATH_TO.SAVE_FIGURE,
    paste0(
      "Figure_Average-Electricity-Consumption_",
      "Within-Day-Average-Hourly-Consumption",
      ".png"
    ),
    sep = "/"
  ),
  width_numeric = 30, height_numeric = 17
)
export_figure.in.png(
  plot_avg.kwh_within.day,
  filename_str = paste(
    PATH_TO.SAVE_FIGURE_FOR.DISSERTATION,
    paste0(
      "Figure_Average-Electricity-Consumption_",
      "Within-Day-Average-Hourly-Consumption",
      ".png"
    ),
    sep = "/"
  ),
  width_numeric = 30, height_numeric = 17
)


# # 2. Figure for average daily electricity consumption
export_figure.in.png(
  plot_avg.kwh_by.date,
  filename_str = paste(
    PATH_TO.SAVE_FIGURE,
    paste0(
      "Figure_Average-Electricity-Consumption_",
      "Average-Daily-Consumption-by-Date",
      ".png"
    ),
    sep = "/"
  ),
  width_numeric = 30, height_numeric = 15
)
export_figure.in.png(
  plot_avg.kwh_by.date,
  filename_str = paste(
    PATH_TO.SAVE_FIGURE_FOR.DISSERTATION,
    paste0(
      "Figure_Average-Electricity-Consumption_",
      "Average-Daily-Consumption-by-Date",
      ".png"
    ),
    sep = "/"
  ),
  width_numeric = 30, height_numeric = 15
)
