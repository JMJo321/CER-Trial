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
# #   3) Additional Savings obtained by introducing an even more dynamic
# #      TOU pricing.

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
DIR_TO.SAVE_FIGURE_FOR.DISSERTATION <- "For-Dissertation_Chapter-2"
PATH_TO.SAVE_FIGURE_FOR.DISSERTATION <-
  paste(PATH_OUTPUT_FIGURE, DIR_TO.SAVE_FIGURE_FOR.DISSERTATION, sep = "/")


# ------- Define parameter(s) -------
# # 1. Parameters for making figure(s)
# # 1.1. Value of knot for spline regression(s)
KNOT <- 10

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
  LIST_INTERVALS[1:4],
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
  interval = names(LIST_INTERVALS[1:4]),
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
tmp_dt_for.plot_melted <- melt(
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
tmp_dt_for.plot_melted[
  ,
  `:=` (
    hdd = factor(hdd, levels = HDDS),
    rate.change = factor(
      paste0("+", rate.change), levels = paste0("+", RATE.CHANGES)
    )
  )
]


tmp_dt_for.plot_all.intervals <- tmp_dt_for.plot_melted[
  interval != "21 to 22",
  lapply(.SD, sum, na.rm = TRUE), .SDcols = "predicted.kwh",
  by = .(hdd, category, rate.change)
][
  ,
  interval := "Pre-Peak + Peak + Post-Peak"
]

dt_for.plot_melted <- rbind(
  tmp_dt_for.plot_melted[
    interval != "21 to 22",
    .(hdd, interval, category, rate.change, predicted.kwh)
  ],
  tmp_dt_for.plot_all.intervals
)

# ------- Create a DT to show additional savings -------
# # 1. Create a DT to show additiona savings under adoption of an alternative
# #    pricing.
tmp_dt_for.polygon <- dt_for.plot_melted[
  interval == "17 to 18" & category == "predicted.kwh_total"
]
dt_coord_1st <- data.table(
  id = rep("+6 to +12", times = 4),
  coord_x = c(3, 4, 4, 3),
  coord_y = c(
    tmp_dt_for.polygon[hdd == 10 & rate.change == "+6"]$predicted.kwh,
    tmp_dt_for.polygon[hdd == 15 & rate.change == "+6"]$predicted.kwh,
    tmp_dt_for.polygon[hdd == 15 & rate.change == "+12"]$predicted.kwh,
    tmp_dt_for.polygon[hdd == 10 & rate.change == "+12"]$predicted.kwh
  )
)
dt_coord_2nd <- data.table(
  id = rep("+6 to +18", times = 4),
  coord_x = c(4, 5, 5, 4),
  coord_y = c(
    tmp_dt_for.polygon[hdd == 15 & rate.change == "+6"]$predicted.kwh,
    tmp_dt_for.polygon[hdd == 20 & rate.change == "+6"]$predicted.kwh,
    tmp_dt_for.polygon[hdd == 20 & rate.change == "+18"]$predicted.kwh,
    tmp_dt_for.polygon[hdd == 15 & rate.change == "+18"]$predicted.kwh
  )
)
dt_coord_3rd <- data.table(
  id = rep("+6 to +24", times = 4),
  coord_x = c(5, 7, 7, 5),
  coord_y = c(
    tmp_dt_for.polygon[hdd == 20 & rate.change == "+6"]$predicted.kwh,
    tmp_dt_for.polygon[hdd == 30 & rate.change == "+6"]$predicted.kwh,
    tmp_dt_for.polygon[hdd == 30 & rate.change == "+24"]$predicted.kwh,
    tmp_dt_for.polygon[hdd == 20 & rate.change == "+24"]$predicted.kwh
  )
)
dt_coord <- rbind(dt_coord_1st, dt_coord_2nd, dt_coord_3rd)


# ------- Create a DT to show an alternative price scheme -------
tmp_dt_price_old <-
  data.table(hdd = seq(1, 7, by = 1)) %>%
    .[, `:=` (coord_y = 6, category = "Current")]
tmp_dt_price_new <- rbind(
  data.table(hdd = seq(1, 3, by = 1)) %>% .[, coord_y := 6],
  data.table(hdd = seq(3, 4, by = 1)) %>% .[, coord_y := 12],
  data.table(hdd = seq(4, 5, by = 1)) %>% .[, coord_y := 18],
  data.table(hdd = seq(5, 7, by = 1)) %>% .[, coord_y := 24]
) %>%
  .[, category := "Alternative"]
dt_price <- rbind(tmp_dt_price_old, tmp_dt_price_new)


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
# # 2.1. For ``category''
labels_category <- c(
  "Treatment Effect: Non-Temperature-Control",
  "Treatment Effect: Temperature-Control",
  "Aggregate Treatment Effect"
)
names(labels_category) <-
  dt_for.plot_melted[, .N, keyby = .(category)]$category

# # 2.2. For ``interval''
labels_interval <-
  c("Pre-Peak", "Peak", "Post-Peak", "Pre-Peak + Peak + Post-Peak")
names(labels_interval) <-
  dt_for.plot_melted[, .N, keyby = .(interval)]$interval


# ------- Create ggplot object(s) -------
# # 1. For Treatment Effects and Predicted Savings
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
      labeller = labeller(
        category = labels_category,
        interval = labels_interval
      )
    ) +
    scale_y_continuous(
      limits = c(-0.25, 0.1),
      breaks = seq(-0.25, 0.1, by = 0.05)
    ) +
    scale_color_viridis_d() +
    labs(
      x = "Heating Degree Days",
      y = TeX(r'(Treatment Effects ($\Delta$ kWh per Hour))'),
      color = "Price Change in the Peak Rate Period"
    ) +
    plot.options


# # 2. Additional Savings
plot_additional.savings <-
  ggplot() +
    geom_hline(
      yintercept = 0,
      linetype = "dashed", alpha = 0.3
    ) +
    geom_hline(
      yintercept = seq(6, 24, by = 6) / 200,
      linetype = "dashed", alpha = 0.3, lwd = 0.3
    ) +
    geom_vline(
      xintercept = factor(KNOT, levels = HDDS),
      linetype = "dotdash", alpha = 0.3
    ) +
    geom_line(
      data = dt_for.plot_melted[
        interval == "17 to 18" & category == "predicted.kwh_total"
      ],
      aes(x = hdd, y = predicted.kwh, group = rate.change),
      color = "black", alpha = 0.3, lwd = 1
    ) +
    geom_line(
      data = dt_for.plot_melted[
        interval == "17 to 18" & category == "predicted.kwh_total"
      ],
      aes(x = hdd, y = predicted.kwh, color = rate.change, group = rate.change)
    ) +
    geom_point(
      data = dt_for.plot_melted[
        interval == "17 to 18" & category == "predicted.kwh_total"
      ],
      aes(x = hdd, y = predicted.kwh, color = rate.change)
    ) +
    geom_polygon(
      data = dt_coord,
      aes(x = coord_x, y = coord_y, fill = id),
      alpha = 0.3
    ) +
    geom_step(
      data = dt_price,
      aes(x = hdd, y = coord_y / 200, linetype = category),
      color = col.pal_custom[1], lwd = 0.7
    ) +
    scale_y_continuous(
      limits = c(-0.2, 0.12),
      breaks = seq(-0.2, 0.1, by = 0.05),
      sec.axis = sec_axis(
        trans = ~ . * 200,
        breaks = c(seq(-40, 20, by = 10), seq(6, 24, by = 6)),
        name = "Rates (Cents per kWh)"
      )
    ) +
    scale_color_viridis_d() +
    scale_fill_manual(values = col.pal_custom[c(1:2, 4)]) +
    labs(
      x = "Heating Degree Days",
      y = TeX(r'(Treatment Effects ($\Delta$ kWh per Hour))'),
      color = "Price Change in the Peak Rate Period",
      fill = "Additional Changes in Electricity Consumption",
      linetype = "Price Scheme"
    ) +
    plot.options


# ------- Save the figure(s) created above -------
# # 1. For Treatment Effects and Predicted Savings
export_figure.in.png(
  plot_predicted.kwh,
  filename_str = paste(
    PATH_TO.SAVE_FIGURE,
    paste0(
      "Figure_Predicted-Electricity-Savings_By-Intervals-and-HDDs_Spline_Knot-",
      KNOT, ".png"
    ),
    sep = "/"
  ),
  width_numeric = 35, height_numeric = 26
)
export_figure.in.png(
  plot_predicted.kwh,
  filename_str = paste(
    PATH_TO.SAVE_FIGURE_FOR.DISSERTATION,
    paste0(
      "Figure_Predicted-Electricity-Savings_By-Intervals-and-HDDs_Spline_Knot-",
      KNOT, ".png"
    ),
    sep = "/"
  ),
  width_numeric = 35, height_numeric = 26
)


# # 2. For Additional Savings
export_figure.in.png(
  plot_additional.savings,
  filename_str = paste(
    PATH_TO.SAVE_FIGURE,
    paste0(
      "Figure_Additional-Electricity-Savings_Knot-",
      KNOT, ".png"
    ),
    sep = "/"
  ),
  width_numeric = 28, height_numeric = 23
)
export_figure.in.png(
  plot_additional.savings,
  filename_str = paste(
    PATH_TO.SAVE_FIGURE_FOR.DISSERTATION,
    paste0(
      "Figure_Additional-Electricity-Savings_Knot-",
      KNOT, ".png"
    ),
    sep = "/"
  ),
  width_numeric = 28, height_numeric = 23
)
