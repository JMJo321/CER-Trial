# < Description > *
# > Script Group Indicator Number and Name
# # : A-03, Regression Analysis
# #
# > Script Number(s)
# # : A-03-05H
# #
# > Purpose of the script(s)
# # : Create a figure that shows
# #   1) Treatment Effects
# #      (Non-Temperature- and Temperature-Control, separately)
# #   2) Predicted Electricity Savings

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

# # 1.2. For the R script including regression model(s)
FILE_TO.LOAD_CER_MODELS <- "M-CER-Trial_Regression-Specifications.R"
PATH_TO.LOAD_CER_MODELS <- paste(
  PATH_SCRIPT, FILE_TO.LOAD_CER_MODELS, sep = "/"
)


# # 2. Path(s) to which regression results will be stored
# # 2.1. For figure(s)
DIR_TO.SAVE_FIGURE <- "Dynamic-Behavior"
PATH_TO.SAVE_FIGURE <- paste(PATH_OUTPUT_FIGURE, DIR_TO.SAVE_FIGURE, sep = "/")


# ------- Define parameter(s) -------
# # 1. Parameters for making figure(s)
# # 1.1. Value of knot for spline regression(s)
KNOT <- 15

# # 1.2. Heating degree days
HDDS <- seq(0, 30, by = 5)
names(HDDS) <- HDDS


# ------- Define function(s) -------
# (Refer to the R header script.)


# ------------------------------------------------------------------------------
# Load necessary dataset(s) and/or script(s) for regression analysis
# ------------------------------------------------------------------------------
# ------- Load necessary dataset(s) and/or script(s) -------
# # 1. Load required DT(s) for regression analysis
# # 1.1. Load the DT for regression analysis
dt_for.reg <-
  read_parquet(PATH_TO.LOAD_CER_FOR.REG) %>%
    setDT(.)
gc(reset = TRUE, full = TRUE)


# # 2. Load required script(s) for regression analysis
# # 2.1. Load the R script including model specification(s)
source(PATH_TO.LOAD_CER_MODELS)


# ------------------------------------------------------------------------------
# Create a DT that will be used to create figure(s) by running spline regression
# ------------------------------------------------------------------------------
# ------- Modify the DT for regression analysis -------
# # 1. Add columns
# # 1.1. Add columns related to knot(s)
dt.oper_add.col_knot(dt_for.reg, KNOT)


# ------- Run spline regressions -------
list_felm.objs <- lapply(
  LIST_INTERVALS[1:3],
  get_felm.obj_interval.hour,
  dt = dt_for.reg,
  formula = get_change.terms.in.formula(
    model_breakdown.of.ate_hourly.in.peak_rate.change.in.spline_dw,
    term_old_in.str = "hdd.knot", term_new_in.str = paste0("hdd.knot", KNOT)
  )
)


# ------- Create a DT by extracting estimates from the felm objects -------
# # 1. Create a DT that includes estimates
# # 1.1. Extract estimates from the felm objects above
dt_estimates <- lapply(
  list_felm.objs,
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


# # 2. Modify the DT created above
# # 2.1. Add column(s)
# # 2.1.1. Columns showing the predicted electricity savings
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
# # 2.1.2. Melting the DT
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
    hdd = factor(hdd, levels = HDDS),
    rate.change = factor(rate.change, levels = RATE.CHANGES)
  )
]


# ------------------------------------------------------------------------------
# Create figure(s)
# ------------------------------------------------------------------------------
# ------- Create objects for setting plot options -------
# # 1. Define common plot options
plot.options <- list(
  theme_linedraw(),
  theme(
    strip.text = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 12)),
    legend.position = "bottom"
  )
)

# # 2. Create an object that includes the labels for faceting variable(s)
labels_category <- c(
  "Treatment Effect: Non-Temperature-Control",
  "Treatment Effect: Temperature-Control",
  "Predicted Savings"
)
names(labels_category) <-
  dt_for.plot_melted[, .N, keyby = .(category)]$category


# ------- Create ggplot object(s) -------
plot_predicted.kwh <-
  ggplot() +
    geom_hline(
      yintercept = 0,
      linetype = "dashed", alpha = 0.3
    ) +
    geom_vline(
      xintercept = factor(KNOT, levels = HDDS),
      linetype = "dotdash", alpha = 0.3
    ) +
    geom_line(
      data = dt_for.plot_melted,
      aes(x = hdd, y = predicted.kwh, group = rate.change),
      color = "black", alpha = 0.3, lwd = 1
    ) +
    geom_line(
      data = dt_for.plot_melted,
      aes(x = hdd, y = predicted.kwh, color = rate.change, group = rate.change)
    ) +
    geom_point(
      data = dt_for.plot_melted,
      aes(x = hdd, y = predicted.kwh, color = rate.change)
    ) +
    facet_grid(
      category ~ interval,
      labeller = labeller(category = labels_category)
    ) +
    scale_y_continuous(
      limits = c(-0.2, 0.1),
      breaks = seq(-0.2, 0.1, by = 0.05)
    ) +
    scale_color_viridis_d() +
    labs(
      x = "Heating Degree Days",
      y = TeX(r'(Treatment Effects $(\Delta kWh per Hour)$  and  Predicted Savings (kWh per Hour))'),
      color = "Size of Rate Changes"
    ) +
    plot.options


# ------- Save the figure(s) created above -------
export_figure.in.png(
  plot_predicted.kwh,
  filename_str = paste(
    PATH_TO.SAVE_FIGURE,
    paste0(
      "Predicted-Electricity-Savings_By-Intervals-and-HDDs_Knot-",
      KNOT, ".png"
    ),
    sep = "/"
  ),
  width_numeric = 35, height_numeric = 25
)
