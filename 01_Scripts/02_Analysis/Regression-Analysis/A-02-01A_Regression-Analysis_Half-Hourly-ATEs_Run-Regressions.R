# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Regression-Analysis
# #
# > Script Number(s)
# # : A-02-01A
# #
# > Purpose of the script(s)
# # : Run a regression to estimate half-hourly ATEs. And then create a figure
# #   showing the estimated ATEs in a form of time profile.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(arrow)
library(lfe)
library(latex2exp)
library(ggplot2)
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
  "CER_DT-for-Regressions-with-Survey-Data_Electricity.parquet"
PATH_TO.LOAD_CER_FOR.REG <-
  paste(PATH_DATA_INTERMEDIATE_CER, FILE_TO.LOAD_CER_FOR.REG, sep = "/")

# # 1.2. For the R script including regression model(s)
FILE_TO.LOAD_CER_MODELS <- "M-CER-Trial_Regression-Specifications.R"
PATH_TO.LOAD_CER_MODELS <- paste(
  PATH_SCRIPT, FILE_TO.LOAD_CER_MODELS, sep = "/"
)


# # 2. Path(s) to which output will be saved
# # 2.1. For figure(s)
DIR_TO.SAVE_FIGURE <- "Half-Hourly-ATEs"
PATH_TO.SAVE_FIGURE <- paste(PATH_OUTPUT_FIGURE, DIR_TO.SAVE_FIGURE, sep = "/")


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# # 1. For obtaining half-hourly ATEs to generate ggplot object(s)
# # 1.1. To get felm object(s) by running regression(s)
get_felm.obj_half.hourly.ates_by.tariff <- function (tariff_in.str) {
  felm.obj <- felm(
    formula = model_ate_half.hourly_iw.dw.m,
    data = dt_for.reg[
      is_in.sample_incl.control_base.only.second.half == TRUE &
        alloc_r_tariff %in% c(tariff_in.str, "E")
    ]
  )
  return (felm.obj)
}

# # 1.2. To make DT(s) by extracting estimates from a felm object
get_dt.incl.estimates <- function (felm.obj) {
  dt_estimates <- get_estimates_from.felm(
      felm.obj, level = 0.95, fe = FALSE, se.type = "cluster"
    )
  dt_estimates[
    ,
    is_significant := !(conf.low <= 0 & 0 <= conf.high)
  ]
  dt_estimates[
    ,
    interval_30min := (
      str_extract(term, "[0-9]+?TRUE") %>%
        str_replace("TRUE", "") %>%
        as.numeric(.)
    ) - 0.5
  ]
  dt_estimates[, interval_hour := interval_30min / 2]
  return (dt_estimates)
}


# ------------------------------------------------------------------------------
# Run a regression to estimate half-hourly ATEs
# ------------------------------------------------------------------------------
# ------- Load the DT for regression analysis -------
# # 1. Load required DT(s) and/or script(s) for regression analysis
# # 1.1. Load the DT for regression analysis
dt_for.reg <- read_parquet(PATH_TO.LOAD_CER_FOR.REG)
setDT(dt_for.reg)
gc(reset = TRUE)

# # 1.2. Load the R script including model specification(s)
source(PATH_TO.LOAD_CER_MODELS)


# # 2. Add data fields indicating half-hourly intervals
for (interval in 1:48) {
  tmp_col.name <- paste0("is_treatment.and.post_30min_", interval)
  dt_for.reg[
    ,
    (tmp_col.name) := (
      is_treated_r == TRUE &
        is_treatment.period == TRUE &
        interval_30min == interval
    )
  ]
}


# ------- Run a regression to estimate half-hourly ATEs: For All Tariffs -------
# # 1. Create a felm object by running a regression
reg.result <- felm(
  formula = model_ate_half.hourly_iw.dw.m,
  data = dt_for.reg[is_in.sample_incl.control_base.only.second.half == TRUE]
)


# # 2. Create a DT including the estimated ATEs
dt_estimates <- get_dt.incl.estimates(reg.result)


# ------- Run a regression to estimate half-hourly ATEs: For Each Tariff -------
# # 1. Create a felm object by running a regression. And then create a DT
# #    including the estimated ATEs
# # 1.1. Obtain felm object(s) by running regressions
list_tariffs <- LETTERS[1:4]
names(list_tariffs) <- LETTERS[1:4]
list_reg.result_by.tariff <- lapply(
  list_tariffs,
  get_felm.obj_half.hourly.ates_by.tariff
)

# # 1.2. Create a DT incl. estimates
dt_estimates_by.tariff <- lapply(
  list_reg.result_by.tariff,
  get_dt.incl.estimates
) %>%
  rbindlist(., idcol = "alloc_r_tariff")


# ------------------------------------------------------------------------------
# Create a figure showing the estimated ATEs in a form of time profile
# ------------------------------------------------------------------------------
# ------- Set common plot option(s) -------
plot.options <- list(
  theme_linedraw(),
  theme(
    strip.text = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 12)),
    legend.text = element_text(margin = margin(r = 10, unit = "pt")),
    legend.position = "bottom"
  )
)


# ------- Create a ggplot object -------
# # 1. For all tariff groups
plot_time.profile <-
  ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_vline(
      xintercept = c(8, 17, 19, 23), linetype = "dotdash", alpha = 0.5
    ) +
    geom_errorbar(
      data = dt_estimates,
      aes(x = interval_hour, y = estimate, ymin = conf.low, ymax = conf.high),
      width = 0.2
    ) +
    geom_point(
      data = dt_estimates[is_significant == TRUE],
      aes(x = interval_hour, y = estimate),
      shape = 16, size = 1.8, color = unikn::usecol("firebrick")
    ) +
    geom_point(
      data = dt_estimates,
      aes(x = interval_hour, y = estimate),
      shape = 1, size = 1.8
    ) +
    scale_x_continuous(breaks = seq(0, 24, by = 1)) +
    labs(
      x = "Hour of Day",
      y = TeX(r'(Treatment Effects  ($\Delta$ kWh per Hour))')
    ) +
    plot.options


# # 2. For each tariff group
plot_time.profile_by.tariff <-
  ggplot() +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_vline(
      xintercept = c(8, 17, 19, 23), linetype = "dotdash", alpha = 0.5
    ) +
    geom_errorbar(
      data = dt_estimates_by.tariff,
      aes(x = interval_hour, y = estimate, ymin = conf.low, ymax = conf.high),
      width = 0.2
    ) +
    geom_point(
      data = dt_estimates_by.tariff[is_significant == TRUE],
      aes(x = interval_hour, y = estimate),
      shape = 16, size = 1.8, color = unikn::usecol("firebrick")
    ) +
    geom_point(
      data = dt_estimates_by.tariff,
      aes(x = interval_hour, y = estimate),
      shape = 1, size = 1.8
    ) +
    facet_grid(alloc_r_tariff ~ .) +
    scale_x_continuous(breaks = seq(0, 24, by = 1)) +
    labs(
      x = "Hour of Day",
      y = TeX(r'(Treatment Effects  ($\Delta$ kWh per Hour))')
    ) +
    plot.options


# ------- Export the ggplot object(s) in .PNG format -------
# # 1. For all tariff groups
export_figure.in.png(
  plot_time.profile,
  filename_str = paste(
    PATH_TO.SAVE_FIGURE,
    "Figure_Time-Profile-of-Half-Hourly-ATEs.png",
    sep = "/"
  ),
  width_numeric = 35, height_numeric = 17
)

# # 2. For each tariff group
export_figure.in.png(
  plot_time.profile_by.tariff,
  filename_str = paste(
    PATH_TO.SAVE_FIGURE,
    "Figure_Time-Profile-of-Half-Hourly-ATEs_By-Tariff.png",
    sep = "/"
  ),
  width_numeric = 35, height_numeric = 27
)
