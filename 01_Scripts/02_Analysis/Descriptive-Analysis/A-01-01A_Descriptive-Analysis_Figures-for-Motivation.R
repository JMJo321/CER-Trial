# < Description > *
# > Script Group Indicator Number and Name
# # : A-01, Descriptive Analysis
# #
# > Script Number(s)
# # : A-01-01A
# #
# > Purpose of the script(s)
# # : To make figures for motivation

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(arrow)
library(ggplot2)
library(latex2exp)
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
FILE_TO.LOAD_CER_FOR.REG <- "CER_DT-for-Regressions_Electricity.parquet"
PATH_TO.LOAD_CER_FOR.REG <-
  paste(PATH_DATA_INTERMEDIATE_CER, FILE_TO.LOAD_CER_FOR.REG, sep = "/")


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Create DT(s) that will be used to create ggplot object(s)
# ------------------------------------------------------------------------------
# ------- Load the DT for regression analysis -------
dt_for.reg <- read_parquet(PATH_TO.LOAD_CER_FOR.REG)


# ------- Create Object(s) -------
# # 1. Create a DT that includes rate-period-level consumption
# # 1.1. Make objects that will be use to aggregate data
cols_by_sum <- c(
  "id", "group",
  "period", "month_in.factor", "date", "rate.period",
  "is_after.ending.daylight.saving.time.in.oct", "is_last.five.days.of.year",
  "length_rate.period",
  # The three variables just above are necessary to compute per-hour
  #  consumption.
  "mean.temp_all_f"
  # The variable just above is for making figures. This variable has a unique
  # value within a day.
)
cols_by_mean <- cols_by_sum[-1]

# # 1.2. Create a DT by aggregating the DT loaded above
dt_avg.kwh_by.rate.period <-
  dt_for.reg[ # Aggregate data
    is_in.sample_incl.control_case1 == TRUE,  # TODO: Redo with aother samples
    lapply(.SD, sum, na.rm = TRUE), .SDcols = "kwh",
    by = cols_by_sum
  ][ # Compute average househole electricity consumption
    ,
    lapply(.SD, mean, na.rm = TRUE), .SDcols = "kwh",
    by = cols_by_mean
  ]

# # 1.3. Modify the DT created above
# # 1.3.1. Add column(s)
# # 1.3.1.1. Add a column that shows per-hour consumption
dt_avg.kwh_by.rate.period[, kwh_per.hour := kwh / length_rate.period]
# ## Note:
# ## Each rate period has different lengths of time.


# ------------------------------------------------------------------------------
# Create ggplot object(s)
# ------------------------------------------------------------------------------
# ------- Set Common Plot Options -------
# # 1. Set Common Plot Options
plot.options <- list(
  theme_linedraw(),
  theme(strip.text = element_text(face = "bold"))
)


# # 2. Create a Color Palette
color.palette_signal <- unikn::usecol(pal = pal_signal, n = 3)


# ------- Create ggplot object(s) -------
# # 1. Average hourly household electricity consumption across temperatures,
# #    facetting based on `period` and `group`
plot_hourly.consumption_across.temperature <-
  ggplot() +
    geom_vline(
      xintercept = 60, color = "black", alpha = 0.2, linetype = "dashed"
    ) +
    geom_smooth(
      data = dt_avg.kwh_by.rate.period[
        month_in.factor %in% 7:12 &
          is_after.ending.daylight.saving.time.in.oct == FALSE &
          is_last.five.days.of.year == FALSE &
          rate.period %in% c("Night", "Day")
      ],
      aes(x = mean.temp_all_f, y = kwh_per.hour, linetype = group),
      method = "lm", formula = y ~ splines::bs(x, degree = 2),
      color = "black", lwd = 0.6, alpha = 0
    ) +
    geom_smooth(
      data = dt_avg.kwh_by.rate.period[
        month_in.factor %in% 7:10 &
          is_after.ending.daylight.saving.time.in.oct == FALSE &
          is_last.five.days.of.year == FALSE &
          rate.period == "Peak"
      ],
      aes(x = mean.temp_all_f, y = kwh_per.hour, linetype = group),
      method = "lm", formula = y ~ splines::bs(x, degree = 2),
      color = "black", lwd = 0.6, alpha = 0
    ) +
    geom_smooth(
      data = dt_avg.kwh_by.rate.period[
        month_in.factor %in% 11:12 &
          is_after.ending.daylight.saving.time.in.oct == FALSE &
          is_last.five.days.of.year == FALSE &
          rate.period == "Peak"
      ],
      aes(x = mean.temp_all_f, y = kwh_per.hour, linetype = group),
      method = "lm", formula = y ~ splines::bs(x, degree = 2),
      color = "black", lwd = 0.6, alpha = 0
    ) +
    geom_point(
      data = dt_avg.kwh_by.rate.period[
        month_in.factor %in% 7:12 &
          is_after.ending.daylight.saving.time.in.oct == FALSE &
          is_last.five.days.of.year == FALSE
      ]
      ,
      aes(
        x = mean.temp_all_f, y = kwh_per.hour,
        color = month_in.factor, shape = group
      ),
      color = "black", size = 1.7, alpha = 0.5
    ) +
    geom_point(
      data = dt_avg.kwh_by.rate.period[
        month_in.factor %in% 7:12 &
          is_after.ending.daylight.saving.time.in.oct == FALSE &
          is_last.five.days.of.year == FALSE
      ],
      aes(
        x = mean.temp_all_f, y = kwh_per.hour,
        color = month_in.factor, shape = group
      ),
      size = 1.3, alpha = 0.8
    ) +
    facet_grid(rate.period ~ period) +
    scale_y_continuous(labels = scales::comma) +
    scale_color_brewer(palette = "Spectral", direction = -1) +
    scale_linetype_manual(values = c("dotdash", "solid")) +
    labs(
      x = TeX(r'(Temperature $ (\degree F)$)'),
      y = "Average Hourly Consumption  (kWh per Hour)\n",
      color = "Month of Year",
      shape = "Groups",
      linetype = "Groups"
    ) +
    plot.options +
    theme(legend.key.size = unit(0.8, "cm"))


# ------------------------------------------------------------------------------
# Export output
# ------------------------------------------------------------------------------
# ------- Export ggplot object(s) in PNG format -------
export_figure.in.png(
  plot_hourly.consumption_across.temperature,
  paste(PATH_OUTPUT_FIGURE, "Figure_For-Motivation.png", sep = "/"),
  width_numeric = 40, height_numeric = 22
)