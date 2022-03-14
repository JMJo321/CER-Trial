# < Description > *
# > Script Group Indicator Number and Name
# # : A-01, Descriptive Analysis
# #
# > Script Number(s)
# # : A-01-01A
# #
# > Purpose of the script(s)
# # : To make scatter figure(s) for motivation

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
cols_by_sum_by.rate.period <- c(
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
cols_by_mean_by.rate.period <- cols_by_sum_by.rate.period[-1]
cols_by_sum_daily <-
  cols_by_sum_by.rate.period[
    cols_by_sum_by.rate.period %in% c(
      "id", "group", "period", "month_in.factor", "date",
      "is_after.ending.daylight.saving.time.in.oct",
      "is_last.five.days.of.year", "mean.temp_all_f"
    )
  ]
cols_by_mean_daily <-
  cols_by_sum_by.rate.period[
    cols_by_sum_by.rate.period %in% c(
      "group", "period", "month_in.factor", "date", "mean.temp_all_f"
    )
  ]

# # 1.2. Create a DT by aggregating the DT loaded above
# # 1.2.1. DT incl. average daily consumption
dt_avg.kwh_daily <- dt_for.reg[ # Aggregate data
  # is_in.sample_incl.control_base.only.second.half == TRUE,
  is_in.sample_incl.control_case1.only.second.half == TRUE,
  lapply(.SD, sum, na.rm = TRUE), .SDcols = "kwh",
  by = cols_by_sum_daily
][ # Compute average househole electricity consumption
  ,
  lapply(.SD, mean, na.rm = TRUE), .SDcols = "kwh",
  by = cols_by_mean_daily
]
# ## Note:
# ## Using `is_in.sample_incl.control_case1.only.second.half` is better for
# ## story telling.
# # 1.2.2. DT incl. average hourly consumption by rate period
dt_avg.kwh_by.rate.period <- dt_for.reg[ # Aggregate data
  is_in.sample_incl.control_case1.only.second.half == TRUE,
  lapply(.SD, sum, na.rm = TRUE), .SDcols = "kwh",
  by = cols_by_sum_by.rate.period
][ # Compute average househole electricity consumption
  ,
  lapply(.SD, mean, na.rm = TRUE), .SDcols = "kwh",
  by = cols_by_mean_by.rate.period
]

# # 1.3. Modify the DT created above
# # 1.3.1. Add column(s)
# # 1.3.1.1. Add a column that shows per-hour consumption
dt_avg.kwh_daily[, kwh_per.hour := kwh / 24]
dt_avg.kwh_by.rate.period[, kwh_per.hour := kwh / length_rate.period]
# ## Note:
# ## Each rate period has different lengths of time.


# ------------------------------------------------------------------------------
# Create ggplot object(s)
# ------------------------------------------------------------------------------
# ------- Create object(s) for plot options -------
# # 1. Set common plot options
plot.options <- list(
  scale_y_continuous(labels = scales::comma),
  theme_linedraw(),
  theme(
    strip.text = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 12)),
    legend.position = "bottom"
  )
)

# # 2. Create color palette(s)
col.pal_signal <- unikn::usecol(pal = pal_signal, n = 3)
col.pal_custom <- unikn::usecol(c("firebrick", "gold", "steelblue"))
col.pal_light <- unikn::usecol(pal = pal_unikn_light)

# # 3. Create object for label(s)
label_group <- c("Control Group", "Treatment Group")
names(label_group) <- str_replace(label_group, " Group", "")
label_period <- c("Baseline Period", "Treatment Period")
names(label_period) <- str_replace(label_period, " Period", "")
label_rate.period <-
  c("Rate Period: Night", "Rate Period: Day", "Rate Period: Peak")
names(label_rate.period) <- str_replace(label_rate.period, "Rate Period: ", "")


# ------- Create ggplot object(s) -------
# # 1. Average daily household electricity consumption across temperatures
# # 1.1. A plot facetted by `group`
plot_daily.consumption_facetted.by.group <-
  ggplot(data = dt_avg.kwh_daily) +
    geom_vline(
      xintercept = 60, color = "black", alpha = 0.2, linetype = "dashed"
    ) +
    geom_smooth(
      data = dt_avg.kwh_daily[
        period == "Treatment"
      ],
      aes(
        x = mean.temp_all_f, y = kwh,
        color = period, linetype = period
      ),
      method = "lm", formula = y ~ splines::bs(x, degree = 2),
      lwd = 1.0, color = "black", alpha = 0
    ) +
    geom_smooth(
      aes(
        x = mean.temp_all_f, y = kwh,
        color = period, linetype = period
      ),
      method = "lm", formula = y ~ splines::bs(x, degree = 2),
      lwd = 0.6, alpha = 0.2
    ) +
    geom_point(
      aes(
        x = mean.temp_all_f, y = kwh,
        color = period, shape = period
      ),
      color = "black", size = 2.0, alpha = 0.4
    ) +
    geom_point(
      aes(
        x = mean.temp_all_f, y = kwh,
        color = period, shape = period
      ),
      size = 1.5
    ) +
    facet_grid(. ~ group, labeller = labeller(group = label_group)) +
    scale_color_manual(
      values = col.pal_custom[c(1, 2)],
      labels = c("Baseline Period", "Treatment Period")
    ) +
    scale_linetype_manual(
      values = c("dotted", "solid"),
      labels = c("Baseline Period", "Treatment Period")
    ) +
    scale_shape_manual(
      values = c(16, 18),
      labels = c("Baseline Period", "Treatment Period")
    ) +
    labs(
      x = TeX(r'(Temperature $ (\degree F)$)'),
      y = "Average Daily Consumption  (kWh/Day)",
      color = "",
      shape = "",
      linetype = ""
    ) +
    plot.options

# # 1.2. A plot facetted by `period`
plot_daily.consumption_facetted.by.period <-
  ggplot(data = dt_avg.kwh_daily) +
    geom_vline(
      xintercept = 60, color = "black", alpha = 0.2, linetype = "dashed"
    ) +
    geom_smooth(
      data = dt_avg.kwh_daily[group == "Treatment"],
      aes(
        x = mean.temp_all_f, y = kwh,
        color = group, linetype = group
      ),
      method = "lm", formula = y ~ splines::bs(x, degree = 2),
      lwd = 1.0, color = "black", alpha = 0
    ) +
    geom_smooth(
      aes(
        x = mean.temp_all_f, y = kwh,
        color = group, linetype = group
      ),
      method = "lm", formula = y ~ splines::bs(x, degree = 2),
      lwd = 0.6, alpha = 0.2
    ) +
    geom_point(
      aes(
        x = mean.temp_all_f, y = kwh,
        color = group, shape = group
      ),
      color = "black", size = 2.0, alpha = 0.4
    ) +
    geom_point(
      aes(
        x = mean.temp_all_f, y = kwh,
        color = group, shape = group
      ),
      size = 1.5
    ) +
    facet_grid(. ~ period, labeller = labeller(period = label_period)) +
    scale_color_manual(
      values = col.pal_custom[c(1, 2)],
      labels = c("Control Group", "Treatment Group")
    ) +
    scale_linetype_manual(
      values = c("dotdash", "solid"),
      labels = c("Control Group", "Treatment Group")
    ) +
    scale_shape_manual(
      values = c(16, 18),
      labels = c("Control Group", "Treatment Group")
    ) +
    labs(
      x = TeX(r'(Temperature $ (\degree F)$)'),
      y = "Average Daily Consumption  (kWh/Day)",
      color = "",
      shape = "",
      linetype = ""
    ) +
    plot.options


# # 2. Average hourly household electricity consumption across temperatures,
# #    facetting based on `period`/`group` and `rate.period`
# # 2.1. A plot facetted by `group` and `rate.period`
plot_hourly.consumption_facetted.by.group.and.rate.period <-
  ggplot() +
    geom_vline(
      xintercept = 60, color = "black", alpha = 0.2, linetype = "dashed"
    ) +
    geom_smooth(
      data = dt_avg.kwh_by.rate.period[rate.period %in% c("Night", "Day")],
      aes(x = mean.temp_all_f, y = kwh_per.hour, linetype = period),
      method = "lm", formula = y ~ splines::bs(x, degree = 2),
      color = "black", lwd = 0.6, alpha = 0.15
    ) +
    geom_smooth(
      data = dt_avg.kwh_by.rate.period[rate.period == "Peak"],
      aes(x = mean.temp_all_f, y = kwh_per.hour, linetype = period),
      method = "lm", formula = y ~ splines::bs(x, degree = 2),
      color = "black", lwd = 0.6, alpha = 0.15
    ) +
    geom_smooth(
      data = dt_avg.kwh_by.rate.period[rate.period == "Peak"],
      aes(x = mean.temp_all_f, y = kwh_per.hour, linetype = period),
      method = "lm", formula = y ~ splines::bs(x, degree = 2),
      color = "black", lwd = 0.6, alpha = 0.15
    ) +
    geom_point(
      data = dt_avg.kwh_by.rate.period,
      aes(
        x = mean.temp_all_f, y = kwh_per.hour,
        color = month_in.factor, shape = period
      ),
      color = "black", size = 1.7, alpha = 0.5
    ) +
    geom_point(
      data = dt_avg.kwh_by.rate.period,
      aes(
        x = mean.temp_all_f, y = kwh_per.hour,
        color = month_in.factor, shape = period
      ),
      size = 1.3, alpha = 0.8
    ) +
    facet_grid(
      rate.period ~ group,
      labeller = labeller(rate.period = label_rate.period, group = label_group)
    ) +
    scale_color_brewer(palette = "Spectral", direction = -1) +
    scale_linetype_manual(values = c("dotdash", "solid")) +
    labs(
      x = TeX(r'(Temperature $ (\degree F)$)'),
      y = "Average Hourly Consumption  (kWh per Hour)",
      color = "Month of Year",
      shape = "Periods",
      linetype = "Periods"
    ) +
    plot.options +
    theme(legend.key.size = unit(0.8, "cm")) +
    guides(color = guide_legend(nrow = 1))

# # 2.2. A plot facetted by `period` and `rate.period`
plot_hourly.consumption_facetted.by.period.and.rate.period <-
  ggplot() +
    geom_vline(
      xintercept = 60, color = "black", alpha = 0.15, linetype = "dashed"
    ) +
    geom_smooth(
      data = dt_avg.kwh_by.rate.period[rate.period %in% c("Night", "Day")],
      aes(x = mean.temp_all_f, y = kwh_per.hour, linetype = group),
      method = "lm", formula = y ~ splines::bs(x, degree = 2),
      color = "black", lwd = 0.6, alpha = 0.15
    ) +
    geom_smooth(
      data = dt_avg.kwh_by.rate.period[rate.period == "Peak"],
      aes(x = mean.temp_all_f, y = kwh_per.hour, linetype = group),
      method = "lm", formula = y ~ splines::bs(x, degree = 2),
      color = "black", lwd = 0.6, alpha = 0.15
    ) +
    geom_smooth(
      data = dt_avg.kwh_by.rate.period[rate.period == "Peak"],
      aes(x = mean.temp_all_f, y = kwh_per.hour, linetype = group),
      method = "lm", formula = y ~ splines::bs(x, degree = 2),
      color = "black", lwd = 0.6, alpha = 0.15
    ) +
    geom_point(
      data = dt_avg.kwh_by.rate.period,
      aes(
        x = mean.temp_all_f, y = kwh_per.hour,
        color = month_in.factor, shape = group
      ),
      color = "black", size = 1.7, alpha = 0.5
    ) +
    geom_point(
      data = dt_avg.kwh_by.rate.period,
      aes(
        x = mean.temp_all_f, y = kwh_per.hour,
        color = month_in.factor, shape = group
      ),
      size = 1.3, alpha = 0.8
    ) +
    facet_grid(
      rate.period ~ period,
      labeller =
        labeller(rate.period = label_rate.period, period = label_period)
    ) +
    scale_color_brewer(palette = "Spectral", direction = -1) +
    scale_linetype_manual(values = c("dotdash", "solid")) +
    labs(
      x = TeX(r'(Temperature $ (\degree F)$)'),
      y = "Average Hourly Consumption  (kWh per Hour)",
      color = "Month of Year",
      shape = "Groups",
      linetype = "Groups"
    ) +
    plot.options +
    theme(legend.key.size = unit(0.8, "cm")) +
    guides(color = guide_legend(nrow = 1))


# ------------------------------------------------------------------------------
# Export output
# ------------------------------------------------------------------------------
# ------- Export ggplot object(s) in PNG format -------
# # 1. Figure(s) for daily consumption
export_figure.in.png(
  plot_daily.consumption_facetted.by.group,
  paste(
    PATH_OUTPUT_FIGURE,
    "Figure_For-Motivation_Daily-Consumption-Facetted-by-Group.png",
    sep = "/"
  ),
  width_numeric = 50, height_numeric = 20, dpi_int = 700
)
export_figure.in.png(
  plot_daily.consumption_facetted.by.period,
  paste(
    PATH_OUTPUT_FIGURE,
    "Figure_For-Motivation_Daily-Consumption-Facetted-by-Period.png",
    sep = "/"
  ),
  width_numeric = 50, height_numeric = 20, dpi_int = 700
)
# # 2. Figure(s) for hourly consumption
export_figure.in.png(
  plot_hourly.consumption_facetted.by.group.and.rate.period,
  paste(
    PATH_OUTPUT_FIGURE,
    paste0(
      "Figure_For-Motivation_",
      "Hourly-Consumption-Facetted-by-Group-and-Rate-Period.png"
    ),
    sep = "/"
  ),
  width_numeric = 50, height_numeric = 25, dpi_int = 700
)
export_figure.in.png(
  plot_hourly.consumption_facetted.by.period.and.rate.period,
  paste(
    PATH_OUTPUT_FIGURE,
    paste0(
      "Figure_For-Motivation_",
      "Hourly-Consumption-Facetted-by-Period-and-Rate-Period.png"
    ),
    sep = "/"
  ),
  width_numeric = 50, height_numeric = 25, dpi_int = 700
)
