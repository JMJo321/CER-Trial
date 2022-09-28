# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Regression Analysis
# #
# > Script Number(s)
# # : A-02-05H-2
# #
# > Purpose of the script(s)
# # : Create figure(s) showing treatment effects over HDDs
# #   1) Treatment effects as a linear function of price changes in the peak
# #      rate period.
# #   2) Treatment effect on electricity consumption for non-temperature-control
# #      uses and that for temperature-control uses, respectively.
# #   3) For pre-peak, peak, and post-peak intervals.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(arrow)
library(stringr)
library(data.table)
library(lfe)
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

# # 1.2. For the R script including regression model(s)
FILE_TO.LOAD_CER_MODELS <- "M-CER-Trial_Regression-Specifications.R"
PATH_TO.LOAD_CER_MODELS <- paste(
  PATH_SCRIPT, FILE_TO.LOAD_CER_MODELS, sep = "/"
)


# # 2. Path(s) to which regression results will be stored
# # 2.1. For figure(s)
DIR_TO.SAVE_FIGURE <- "Breakdown-of-Hourly-ATEs"
PATH_TO.SAVE_FIGURE <- paste(PATH_OUTPUT_FIGURE, DIR_TO.SAVE_FIGURE, sep = "/")
DIR_TO.SAVE_FIGURE_FOR.DISSERTATION <- "For-Dissertation_Chapter-2"
PATH_TO.SAVE_FIGURE_FOR.DISSERTATION <-
  paste(PATH_OUTPUT_FIGURE, DIR_TO.SAVE_FIGURE_FOR.DISSERTATION, sep = "/")


# ------- Define parameter(s) -------
# # 1. Parameters for making figure(s)
# # 1.1. Value of knot for spline regression(s)
KNOT <- 15

# # 1.2. Heating degree days
HDDS <- seq(0, 30, by = 2)
names(HDDS) <- HDDS


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


# # 2. Load required script(s) for regression analysis
# # 2.1. Load the R script including model specification(s)
source(PATH_TO.LOAD_CER_MODELS)


# ------------------------------------------------------------------------------
# Run regression(s)
# ------------------------------------------------------------------------------
# ------- Modify the DT for regression analysis -------
# # 1. Add columns
# # 1.1. Add columns related to knot(s)
dt.oper_add.col_knot(dt_for.reg, KNOT)


# ------- Run spline regression(s) -------
list_felm.objects <- lapply(
  LIST_INTERVALS[1:3],
  get_felm.obj_interval.hour,
  dt = dt_for.reg,
  formula = get_change.terms.in.formula(
    model_breakdown.of.ate_hourly.in.peak_rate.change.in.spline_dw,
    term_old_in.str = "hdd.knot", term_new_in.str = paste0("hdd.knot", KNOT)
  )
)


# ------------------------------------------------------------------------------
# Generate DT(s) from regression result(s) to create figure(s)
# ------------------------------------------------------------------------------
# ------- Create a DT by extracting estimates from the felm objects -------
# # 1. Create a DT that includes estimates
# # 1.1. Extract estimates from the felm objects above
dt_estimates <- lapply(
  list_felm.objects,
  get_estimates_from.felm,
  level = 0.95, fe = FALSE, se.type = "cluster"
) %>%
  rbindlist(., idcol = "interval")


# # 1.2. Create a DT that includes estimates
# # 1.2.1. Create an empty DT
dt_for.plot <- expand.grid(
  interval = names(LIST_INTERVALS[1:3]),
  hdd = HDDS,
  rate.change = RATE.CHANGES,
  stringsAsFactors = FALSE
) %>%
  setDT(.)
dt_for.plot[, hdd.knot := (hdd - KNOT) * as.numeric(hdd > KNOT)]
# # 1.2.2. Append necessary estimates
# # 1.2.2.1. Non-temperature-control-related estimates
dt_for.plot <- merge(
  x = dt_for.plot,
  y = dt_estimates[
    term == "is_treatment.and.postTRUE",
    .(interval, estimate)
  ] %>%
    setnames(., old = "estimate", "ate_non.temp_base"),
  by = "interval",
  all.x = TRUE
)
dt_for.plot <- merge(
  x = dt_for.plot,
  y = dt_estimates[
    term == "treatment.and.post_times_rate.change",
    .(interval, estimate)
  ] %>%
    setnames(., old = "estimate", "ate_non.temp_rate.change"),
  by = "interval",
  all.x = TRUE
)
# # 1.2.2.2. Temperature-control-related estimates
dt_for.plot <- merge(
  x = dt_for.plot,
  y = dt_estimates[
    term == "treatment.and.post_times_hdd",
    .(interval, estimate)
  ] %>%
    setnames(., old = "estimate", "ate_temp_hdd"),
  by = "interval",
  all.x = TRUE
)
dt_for.plot <- merge(
  x = dt_for.plot,
  y = dt_estimates[
    term == paste0("treatment.and.post_times_hdd.knot", KNOT),
    .(interval, estimate)
  ] %>%
    setnames(., old = "estimate", "ate_temp_hdd.knot"),
  by = "interval",
  all.x = TRUE
)
dt_for.plot <- merge(
  x = dt_for.plot,
  y = dt_estimates[
    term == "treatment.and.post_times_hdd_times_rate.change",
    .(interval, estimate)
  ] %>%
    setnames(., old = "estimate", "ate_temp_hdd.times.rate.change"),
  by = "interval",
  all.x = TRUE
)
dt_for.plot <- merge(
  x = dt_for.plot,
  y = dt_estimates[
    term == paste0(
      "treatment.and.post_times_hdd.knot", KNOT, "_times_rate.change"
    ),
    .(interval, estimate)
  ] %>%
    setnames(., old = "estimate", "ate_temp_hdd.knot.times.rate.change"),
  by = "interval",
  all.x = TRUE
)
# # 1.2.3. Add data field(s)
# # 1.2.3.1. Columns showing the predicted electricity savings
dt_for.plot[
  ,
  `:=` (
    predicted.kwh_non.temp = (
      ate_non.temp_base + ate_non.temp_rate.change * rate.change
    ),
    predicted.kwh_temp = (
      ate_temp_hdd * hdd +
        ate_temp_hdd.knot * hdd.knot +
        ate_temp_hdd.times.rate.change * hdd * rate.change +
        ate_temp_hdd.knot.times.rate.change * hdd.knot * rate.change
    )
  )
]
dt_for.plot[
  ,
  predicted.kwh_total := predicted.kwh_non.temp + predicted.kwh_temp
]

# # 1.3. Create DT(s) by melting the DT(s) created above
# # 1.3.1. Create DT(s)
dt_for.plot_melted <- melt(
  dt_for.plot,
  id.vars = names(dt_for.plot)[
    str_detect(names(dt_for.plot), "^predicted", negate = TRUE)
  ],
  measure.vars = names(dt_for.plot)[
    str_detect(names(dt_for.plot), "^predicted", negate = FALSE)
  ],
  variable.name = "category",
  value.name = "predicted.kwh"
)
dt_for.plot_melted[
  ,
  `:=` (
    # hdd = factor(hdd, levels = HDDS),
    rate.change = factor(rate.change, levels = RATE.CHANGES)
  )
]
# # 1.3.2. Modify the DT(s)
# # 1.3.2.1. Replace value(s)
dt_for.plot_melted[
  category == "predicted.kwh_non.temp",
  category := "Non-Temperature-Control"
]
dt_for.plot_melted[
  category == "predicted.kwh_temp",
  category := "Temperature-Control"
]
dt_for.plot_melted[
  category == "predicted.kwh_total",
  category := "Total"
]
# # 1.3.2.2. Add data field(s)
dt_for.plot_melted[
  category %in% c("Non-Temperature-Control", "Temperature-Control"),
  group := "Treatment Effects"
]
dt_for.plot_melted[
  is.na(group),
  group := "Predicted Electricity Savings"
]
dt_for.plot_melted[
  ,
  group := factor(
    group, levels = c("Treatment Effects", "Predicted Electricity Savings")
  )
]


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


# # 2. Create an object that includes the labels for faceting variable(s)
labels_interval <- c("Pre-Peak Hours", "Peak Hours", "Post-Peak Hours")
names(labels_interval) <- names(LIST_INTERVALS[1:3])


# ------- Create ggplot object(s) -------
# # 1. By interval
# # 1.1. For pre-peak interval
plot_predicted.kwh_15to16 <-
  ggplot() +
    geom_hline(
      yintercept = 0,
      linetype = "dashed", alpha = 0.3
    ) +
    geom_vline(
      xintercept = KNOT,
      linetype = "dotdash", alpha = 0.3
    ) +
    geom_line(
      data = dt_for.plot_melted[
        interval == "15 to 16" & category == "Non-Temperature-Control"
      ],
      aes(x = hdd, y = predicted.kwh, group = rate.change),
      color = "black", alpha = 0.3, lwd = 1
    ) +
    geom_line(
      data = dt_for.plot_melted[
        interval == "15 to 16" & category == "Temperature-Control"
      ],
      aes(x = hdd, y = predicted.kwh, group = rate.change),
      color = "black", alpha = 0.3, lwd = 1
    ) +
    geom_line(
      data = dt_for.plot_melted[
        interval == "15 to 16" & category == "Total"
      ],
      aes(x = hdd, y = predicted.kwh, group = rate.change),
      color = "black", alpha = 0.3, lwd = 1
    ) +
    geom_line(
      data = dt_for.plot_melted[
        interval == "15 to 16" & category == "Non-Temperature-Control"
      ],
      aes(x = hdd, y = predicted.kwh, group = rate.change, color = rate.change)
    ) +
    geom_line(
      data = dt_for.plot_melted[
        interval == "15 to 16" & category == "Temperature-Control"
      ],
      aes(x = hdd, y = predicted.kwh, group = rate.change, color = rate.change)
    ) +
    geom_line(
      data = dt_for.plot_melted[
        interval == "15 to 16" & category == "Total"
      ],
      aes(x = hdd, y = predicted.kwh, group = rate.change, color = rate.change)
    ) +
    geom_point(
      data = dt_for.plot_melted[
        interval == "15 to 16" & category == "Non-Temperature-Control"
      ],
      aes(x = hdd, y = predicted.kwh, shape = category),
      color = "black", alpha = 0.3, size = 2.5
    ) +
    geom_point(
      data = dt_for.plot_melted[
        interval == "15 to 16" & category == "Temperature-Control"
      ],
      aes(x = hdd, y = predicted.kwh, shape = category),
      color = "black", alpha = 0.3, size = 2.5
    ) +
    geom_point(
      data = dt_for.plot_melted[
        interval == "15 to 16" & category == "Total"
      ],
      aes(x = hdd, y = predicted.kwh, shape = category),
      color = "black", alpha = 0.3, size = 2.5
    ) +
    geom_point(
      data = dt_for.plot_melted[
        interval == "15 to 16" & category == "Non-Temperature-Control"
      ],
      aes(x = hdd, y = predicted.kwh, color = rate.change, shape = category),
      size = 2.0
    ) +
    geom_point(
      data = dt_for.plot_melted[
        interval == "15 to 16" & category == "Temperature-Control"
      ],
      aes(x = hdd, y = predicted.kwh, color = rate.change, shape = category),
      size = 2.0
    ) +
    geom_point(
      data = dt_for.plot_melted[
        interval == "15 to 16" & category == "Total"
      ],
      aes(x = hdd, y = predicted.kwh, color = rate.change, shape = category),
      size = 2.0
    ) +
    facet_grid(. ~ group) +
    scale_x_continuous(breaks = seq(0, 30, by = 5)) +
    scale_y_continuous(breaks = seq(-0.1, 0, by = 0.02)) +
    scale_color_viridis_d() +
    scale_shape_manual(values = 15:17) +
    labs(
      x = "Heating Degree Days",
      y = TeX(r'(Treatment Effects ($\Delta$ kWh per Hour))'),
      color = TeX(r'(Rate Changes in the Peak Rate Period  ($\Delta$ Cents per kWh))'),
      shape = "Treatment Effects"
    ) +
    plot.options

# # 1.2. For peak interval
plot_predicted.kwh_17to18 <-
  ggplot() +
    geom_hline(
      yintercept = 0,
      linetype = "dashed", alpha = 0.3
    ) +
    geom_vline(
      xintercept = KNOT,
      linetype = "dotdash", alpha = 0.3
    ) +
    geom_line(
      data = dt_for.plot_melted[
        interval == "17 to 18" & category == "Non-Temperature-Control"
      ],
      aes(x = hdd, y = predicted.kwh, group = rate.change),
      color = "black", alpha = 0.3, lwd = 1
    ) +
    geom_line(
      data = dt_for.plot_melted[
        interval == "17 to 18" & category == "Temperature-Control"
      ],
      aes(x = hdd, y = predicted.kwh, group = rate.change),
      color = "black", alpha = 0.3, lwd = 1
    ) +
    geom_line(
      data = dt_for.plot_melted[
        interval == "17 to 18" & category == "Total"
      ],
      aes(x = hdd, y = predicted.kwh, group = rate.change),
      color = "black", alpha = 0.3, lwd = 1
    ) +
    geom_line(
      data = dt_for.plot_melted[
        interval == "17 to 18" & category == "Non-Temperature-Control"
      ],
      aes(x = hdd, y = predicted.kwh, group = rate.change, color = rate.change)
    ) +
    geom_line(
      data = dt_for.plot_melted[
        interval == "17 to 18" & category == "Temperature-Control"
      ],
      aes(x = hdd, y = predicted.kwh, group = rate.change, color = rate.change)
    ) +
    geom_line(
      data = dt_for.plot_melted[
        interval == "17 to 18" & category == "Total"
      ],
      aes(x = hdd, y = predicted.kwh, group = rate.change, color = rate.change)
    ) +
    geom_point(
      data = dt_for.plot_melted[
        interval == "17 to 18" & category == "Non-Temperature-Control"
      ],
      aes(x = hdd, y = predicted.kwh, shape = category),
      color = "black", alpha = 0.3, size = 2.5
    ) +
    geom_point(
      data = dt_for.plot_melted[
        interval == "17 to 18" & category == "Temperature-Control"
      ],
      aes(x = hdd, y = predicted.kwh, shape = category),
      color = "black", alpha = 0.3, size = 2.5
    ) +
    geom_point(
      data = dt_for.plot_melted[
        interval == "17 to 18" & category == "Total"
      ],
      aes(x = hdd, y = predicted.kwh, shape = category),
      color = "black", alpha = 0.3, size = 2.5
    ) +
    geom_point(
      data = dt_for.plot_melted[
        interval == "17 to 18" & category == "Non-Temperature-Control"
      ],
      aes(x = hdd, y = predicted.kwh, color = rate.change, shape = category),
      size = 2.0
    ) +
    geom_point(
      data = dt_for.plot_melted[
        interval == "17 to 18" & category == "Temperature-Control"
      ],
      aes(x = hdd, y = predicted.kwh, color = rate.change, shape = category),
      size = 2.0
    ) +
    geom_point(
      data = dt_for.plot_melted[
        interval == "17 to 18" & category == "Total"
      ],
      aes(x = hdd, y = predicted.kwh, color = rate.change, shape = category),
      size = 2.0
    ) +
    facet_grid(. ~ group) +
    scale_x_continuous(breaks = seq(0, 30, by = 5)) +
    scale_y_continuous(breaks = seq(-0.18, 0.02, by = 0.02)) +
    scale_color_viridis_d() +
    scale_shape_manual(values = 15:17) +
    labs(
      x = "Heating Degree Days",
      y = TeX(r'(Treatment Effects ($\Delta$ kWh per Hour))'),
      color = TeX(r'(Rate Changes in the Peak Rate Period  ($\Delta$ Cents per kWh))'),
      shape = "Treatment Effects"
    ) +
    plot.options

# # 1.3. For post-peak interval
plot_predicted.kwh_19to20 <-
  ggplot() +
    geom_hline(
      yintercept = 0,
      linetype = "dashed", alpha = 0.3
    ) +
    geom_vline(
      xintercept = KNOT,
      linetype = "dotdash", alpha = 0.3
    ) +
    geom_line(
      data = dt_for.plot_melted[
        interval == "19 to 20" & category == "Non-Temperature-Control"
      ],
      aes(x = hdd, y = predicted.kwh, group = rate.change),
      color = "black", alpha = 0.3, lwd = 1
    ) +
    geom_line(
      data = dt_for.plot_melted[
        interval == "19 to 20" & category == "Temperature-Control"
      ],
      aes(x = hdd, y = predicted.kwh, group = rate.change),
      color = "black", alpha = 0.3, lwd = 1
    ) +
    geom_line(
      data = dt_for.plot_melted[
        interval == "19 to 20" & category == "Total"
      ],
      aes(x = hdd, y = predicted.kwh, group = rate.change),
      color = "black", alpha = 0.3, lwd = 1
    ) +
    geom_line(
      data = dt_for.plot_melted[
        interval == "19 to 20" & category == "Non-Temperature-Control"
      ],
      aes(x = hdd, y = predicted.kwh, group = rate.change, color = rate.change)
    ) +
    geom_line(
      data = dt_for.plot_melted[
        interval == "19 to 20" & category == "Temperature-Control"
      ],
      aes(x = hdd, y = predicted.kwh, group = rate.change, color = rate.change)
    ) +
    geom_line(
      data = dt_for.plot_melted[
        interval == "19 to 20" & category == "Total"
      ],
      aes(x = hdd, y = predicted.kwh, group = rate.change, color = rate.change)
    ) +
    geom_point(
      data = dt_for.plot_melted[
        interval == "19 to 20" & category == "Non-Temperature-Control"
      ],
      aes(x = hdd, y = predicted.kwh, shape = category),
      color = "black", alpha = 0.3, size = 2.5
    ) +
    geom_point(
      data = dt_for.plot_melted[
        interval == "19 to 20" & category == "Temperature-Control"
      ],
      aes(x = hdd, y = predicted.kwh, shape = category),
      color = "black", alpha = 0.3, size = 2.5
    ) +
    geom_point(
      data = dt_for.plot_melted[
        interval == "19 to 20" & category == "Total"
      ],
      aes(x = hdd, y = predicted.kwh, shape = category),
      color = "black", alpha = 0.3, size = 2.5
    ) +
    geom_point(
      data = dt_for.plot_melted[
        interval == "19 to 20" & category == "Non-Temperature-Control"
      ],
      aes(x = hdd, y = predicted.kwh, color = rate.change, shape = category),
      size = 2.0
    ) +
    geom_point(
      data = dt_for.plot_melted[
        interval == "19 to 20" & category == "Temperature-Control"
      ],
      aes(x = hdd, y = predicted.kwh, color = rate.change, shape = category),
      size = 2.0
    ) +
    geom_point(
      data = dt_for.plot_melted[
        interval == "19 to 20" & category == "Total"
      ],
      aes(x = hdd, y = predicted.kwh, color = rate.change, shape = category),
      size = 2.0
    ) +
    facet_grid(. ~ group) +
    scale_x_continuous(breaks = seq(0, 30, by = 5)) +
    scale_y_continuous(breaks = seq(-0.06, 0.1, by = 0.02)) +
    scale_color_viridis_d() +
    scale_shape_manual(values = 15:17) +
    labs(
      x = "Heating Degree Days",
      y = TeX(r'(Treatment Effects ($\Delta$ kWh per Hour))'),
      color = TeX(r'(Rate Changes in the Peak Rate Period  ($\Delta$ Cents per kWh))'),
      shape = "Treatment Effects"
    ) +
    plot.options


# # 2. All of the three intervals
plot_predicted.kwh <-
  ggplot() +
    geom_hline(
      yintercept = 0,
      linetype = "dashed", alpha = 0.3
    ) +
    geom_vline(
      xintercept = KNOT,
      linetype = "dotdash", alpha = 0.3
    ) +
    geom_line(
      data = dt_for.plot_melted[
        category == "Non-Temperature-Control" & group == "Treatment Effects"
      ],
      aes(x = hdd, y = predicted.kwh, group = rate.change),
      color = "black", alpha = 0.3, lwd = 1
    ) +
    geom_line(
      data = dt_for.plot_melted[
        category == "Temperature-Control" & group == "Treatment Effects"
      ],
      aes(x = hdd, y = predicted.kwh, group = rate.change),
      color = "black", alpha = 0.3, lwd = 1
    ) +
    geom_line(
      data = dt_for.plot_melted[
        category == "Total" & group == "Treatment Effects"
      ],
      aes(x = hdd, y = predicted.kwh, group = rate.change),
      color = "black", alpha = 0.3, lwd = 1
    ) +
    geom_line(
      data = dt_for.plot_melted[
        category == "Non-Temperature-Control" & group == "Treatment Effects"
      ],
      aes(x = hdd, y = predicted.kwh, group = rate.change, color = rate.change)
    ) +
    geom_line(
      data = dt_for.plot_melted[
        category == "Temperature-Control" & group == "Treatment Effects"
      ],
      aes(x = hdd, y = predicted.kwh, group = rate.change, color = rate.change)
    ) +
    geom_line(
      data = dt_for.plot_melted[
        category == "Total" & group == "Treatment Effects"
      ],
      aes(x = hdd, y = predicted.kwh, group = rate.change, color = rate.change)
    ) +
    geom_point(
      data = dt_for.plot_melted[
        category == "Non-Temperature-Control" & group == "Treatment Effects"
      ],
      aes(x = hdd, y = predicted.kwh, shape = category),
      color = "black", alpha = 0.3, size = 2.5
    ) +
    geom_point(
      data = dt_for.plot_melted[
        category == "Temperature-Control" & group == "Treatment Effects"
      ],
      aes(x = hdd, y = predicted.kwh, shape = category),
      color = "black", alpha = 0.3, size = 2.5
    ) +
    geom_point(
      data = dt_for.plot_melted[
        category == "Total" & group == "Treatment Effects"
      ],
      aes(x = hdd, y = predicted.kwh, shape = category),
      color = "black", alpha = 0.3, size = 2.5
    ) +
    geom_point(
      data = dt_for.plot_melted[
        category == "Non-Temperature-Control" & group == "Treatment Effects"
      ],
      aes(x = hdd, y = predicted.kwh, color = rate.change, shape = category),
      size = 2.0
    ) +
    geom_point(
      data = dt_for.plot_melted[
        category == "Temperature-Control" & group == "Treatment Effects"
      ],
      aes(x = hdd, y = predicted.kwh, color = rate.change, shape = category),
      size = 2.0
    ) +
    geom_point(
      data = dt_for.plot_melted[
        category == "Total" & group == "Treatment Effects"
      ],
      aes(x = hdd, y = predicted.kwh, color = rate.change, shape = category),
      size = 2.0
    ) +
    facet_grid(. ~ interval, labeller = labeller(interval = labels_interval)) +
    scale_x_continuous(breaks = seq(0, 30, by = 5)) +
    scale_y_continuous(breaks = seq(-0.16, 0.1, by = 0.02)) +
    scale_color_viridis_d() +
    scale_shape_manual(values = 15:17) +
    labs(
      x = "Heating Degree Days",
      y = TeX(r'(Treatment Effects ($\Delta$ kWh per Hour))'),
      color = TeX(r'(Rate Changes in the Peak Rate Period  ($\Delta$ Cents per kWh))'),
      shape = "Treatment Effects"
    ) +
    plot.options


# ------- Export ggplot object(s) in PNG format -------
export_figure.in.png(
  plot_predicted.kwh_15to16,
  filename_str = paste(
    PATH_TO.SAVE_FIGURE,
    paste0(
      "Figure_Breakdown-of-Hourly-ATEs_",
      "For-Different-Intervals_By-Tariff-Group_Pre-Peak-Hours_Knot-",
      KNOT, ".png"
    ),
    sep = "/"
  ),
  width_numeric = 30, height_numeric = 15
)
export_figure.in.png(
  plot_predicted.kwh_17to18,
  filename_str = paste(
    PATH_TO.SAVE_FIGURE,
    paste0(
      "Figure_Breakdown-of-Hourly-ATEs_",
      "For-Different-Intervals_By-Tariff-Group_Peak-Hours_Knot-",
      KNOT, ".png"
    ),
    sep = "/"
  ),
  width_numeric = 30, height_numeric = 15
)
export_figure.in.png(
  plot_predicted.kwh_19to20,
  filename_str = paste(
    PATH_TO.SAVE_FIGURE,
    paste0(
      "Figure_Breakdown-of-Hourly-ATEs_",
      "For-Different-Intervals_By-Tariff-Group_Post-Peak-Hours_Knot-",
      KNOT, ".png"
    ),
    sep = "/"
  ),
  width_numeric = 30, height_numeric = 15
)
export_figure.in.png(
  plot_predicted.kwh,
  filename_str = paste(
    PATH_TO.SAVE_FIGURE,
    paste0(
      "Figure_Breakdown-of-Hourly-ATEs_",
      "For-Different-Intervals_By-Tariff-Group_All_Knot-",
      KNOT, ".png"
    ),
    sep = "/"
  ),
  width_numeric = 33, height_numeric = 17
)
export_figure.in.png(
  plot_predicted.kwh,
  filename_str = paste(
    PATH_TO.SAVE_FIGURE_FOR.DISSERTATION,
    paste0(
      "Figure_Breakdown-of-Hourly-ATEs_",
      "For-Different-Intervals_By-Tariff-Group_All_Knot-",
      KNOT, ".png"
    ),
    sep = "/"
  ),
  width_numeric = 33, height_numeric = 17
)
