# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Regression Analysis
# #
# > Script Number(s)
# # : A-02-05H-4
# #
# > Purpose of the script(s)
# # : To compare Tariff Group D relative to Tariff Group A.

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
DIR_TO.SAVE_FIGURE <- "Dynamic-Behavior"
PATH_TO.SAVE_FIGURE <- paste(PATH_OUTPUT_FIGURE, DIR_TO.SAVE_FIGURE, sep = "/")
DIR_TO.SAVE_FIGURE_FOR.DISSERTATION <- "For-Dissertation_Chapter-2"
PATH_TO.SAVE_FIGURE_FOR.DISSERTATION <-
  paste(PATH_OUTPUT_FIGURE, DIR_TO.SAVE_FIGURE_FOR.DISSERTATION, sep = "/")


# ------- Define parameter(s) -------
# # 1. Parameters for making figure(s)
# # 1.1. Value of knot for spline regression(s)
KNOT <- 10

# # 1.2. Hour of day
LIST_HOURS <- 0:23
names(LIST_HOURS) <- 0:23


# # 2. Control and treatment groups
CONTROL.GROUP <- "A"
TREATMENT.GROUP <- "D"
CODE.FOR.GROUPS <-
  paste0(tolower(CONTROL.GROUP), ".and.", tolower(TREATMENT.GROUP))


# ------- Define function(s) -------
# # 1. For add data fields required to run regressions
dt.oper_add.col_treatment.and.control.groups <- function (
  dt, knots_in.array,
  tariff.group.for.control_in.str, tariff.group.for.treatment_in.str
) {
  # ## Generate objects incl. temporary variable names
  tmp_var.name_groups <- paste0("_", CODE.FOR.GROUPS)
  tmp_var.name_treatment <-
    paste0("is_treatment", tmp_var.name_groups)
  tmp_var.name_treatment.and.post <-
    paste0("is_treatment.and.post", tmp_var.name_groups)
  tmp_var.name_treatment.times.hdd <-
    paste0("treatment_times_hdd", tmp_var.name_groups)
  tmp_var.name_post.times.hdd <-
    paste0("post_times_hdd", tmp_var.name_groups)
  tmp_var.name_treatment.and.post.times.hdd <-
    paste0("treatment.and.post_times_hdd", tmp_var.name_groups)

  # ## Add data fields
  dt[
    alloc_r_tariff == tariff.group.for.control_in.str,
    (tmp_var.name_treatment) := FALSE
  ]
  dt[
    alloc_r_tariff == tariff.group.for.treatment_in.str,
    (tmp_var.name_treatment) := TRUE
  ]
  dt[
    !is.na(get(tmp_var.name_treatment)),
    (tmp_var.name_treatment.and.post) :=
      is_treatment_a.and.d & is_treatment.period
  ]
  dt[
    ,
    (tmp_var.name_treatment.times.hdd) :=
        as.numeric(get(tmp_var.name_treatment)) * hdd_all_60f
  ]
  dt[
    ,
    (tmp_var.name_post.times.hdd) :=
        as.numeric(is_treatment.period) * hdd_all_60f
  ]
  dt[
    ,
    (tmp_var.name_treatment.and.post.times.hdd) :=
        as.numeric(get(tmp_var.name_treatment.and.post)) * hdd_all_60f
  ]
  for (KNOT in knots_in.array) {
    var.name_hdd.diff <- paste0("hdd.diff_", KNOT)
    var.name_indicator <- paste0("indicator_", KNOT)
    var.name_hdd.knot <- paste0("hdd.knot", KNOT)
    # # ## Add data fields
    dt_for.reg[, (var.name_hdd.diff) := hdd_all_60f - KNOT]
    dt_for.reg[, (var.name_indicator) := hdd_all_60f > KNOT]
    dt_for.reg[
      ,
      (var.name_hdd.knot) :=
        get(var.name_hdd.diff) * as.numeric(get(var.name_indicator))
    ]
    # # 1.2.3.2. Add knot-term-related interaction term(s)
    dt_for.reg[
      ,
      (paste0("treatment_times_hdd.knot", KNOT, tmp_var.name_groups)) :=
        as.numeric(is_treatment_a.and.d) * get(var.name_hdd.knot)
    ]
    dt_for.reg[
      ,
      (paste0("post_times_hdd.knot", KNOT, tmp_var.name_groups)) :=
        as.numeric(is_treatment.period) * get(var.name_hdd.knot)
    ]
    dt_for.reg[
      ,
      (paste0("treatment.and.post_times_hdd.knot", KNOT, tmp_var.name_groups)) :=
        as.numeric(is_treatment.and.post_a.and.d) * get(var.name_hdd.knot)
    ]
  }
}


# # 2. A helper function to run regressions
help_run.reg_using.treatment.and.control.groups <- function (
  formula, interval.hours_in.array,
  tariff.group.for.control_in.str, tariff.group.for.treatment_in.str
) {
  reg.result <- felm(
    formula = formula,
    data = dt_for.reg[
      is_in.sample_incl.control_base.only.second.half == TRUE &
        interval_hour %in% interval.hours_in.array &
        alloc_r_tariff %in%
          c(tariff.group.for.control_in.str, tariff.group.for.treatment_in.str)
    ]
  )
  return (reg.result)
}


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
# # 1.1. Add columns related
dt.oper_add.col_treatment.and.control.groups(
  dt_for.reg, KNOT,
  tariff.group.for.control_in.str = CONTROL.GROUP,
  tariff.group.for.treatment_in.str = TREATMENT.GROUP
)


# ------- Run spline regression(s) -------
# # 1. Define econometric model(s)
model_breakdown.of.ate_hourly.by.interval_spline <-
  get_formula_felm(
    dep.var =
      dep.var_breakdown.of.ate_hourly.in.peak,
    indep.vars_covariates = paste(
      "hdd_all_60f", "hdd.knot",
      paste0("is_treatment_", CODE.FOR.GROUPS),
      paste0("treatment_times_hdd_", CODE.FOR.GROUPS),
      paste0("treatment_times_hdd.knot_", CODE.FOR.GROUPS),
      "is_treatment.period",
      paste0("post_times_hdd_", CODE.FOR.GROUPS),
      paste0("post_times_hdd.knot_", CODE.FOR.GROUPS),
      paste0("is_treatment.and.post_", CODE.FOR.GROUPS),
      paste0("treatment.and.post_times_hdd_", CODE.FOR.GROUPS),
      paste0("treatment.and.post_times_hdd.knot_", CODE.FOR.GROUPS),
      sep = " + "
    ),
    indep.vars_fes = paste(
      "day.of.week.and.30min.interval_in.factor",
      sep = " + "
    ),
    indep.vars_ivs =
      indep.vars_ivs_breakdown.of.ate_hourly.in.peak,
    indep.vars_clustered.ses =
      indep.vars_clustered.ses_breakdown.of.ate_hourly.in.peak
  )


# # 2. Run regression(s)
list_felm.objects <- lapply(
  LIST_HOURS,
  help_run.reg_using.treatment.and.control.groups,
  # dt = dt_for.reg,
  formula = get_change.terms.in.formula(
    model_breakdown.of.ate_hourly.by.interval_spline,
    term_old_in.str = "hdd.knot", term_new_in.str = paste0("hdd.knot", KNOT)
  ),
  tariff.group.for.control_in.str = CONTROL.GROUP,
  tariff.group.for.treatment_in.str = TREATMENT.GROUP
)


# ------- Create a DT from regression result(s) -------
cols_to.extract <- c(
  paste0("is_treatment.and.post_", CODE.FOR.GROUPS, "TRUE"),
  paste0("treatment.and.post_times_hdd_", CODE.FOR.GROUPS),
  paste0("treatment.and.post_times_hdd.knot", KNOT, "_", CODE.FOR.GROUPS)
)
dt_estimates.for.plot <- lapply(
  list_felm.objects,
  get_estimates_from.felm,
  level = 0.95,
  fe = FALSE,
  se.type = "cluster"
) %>%
  rbindlist(., idcol = "interval_hour") %>%
    .[term %in% cols_to.extract] %>%
    .[, interval_hour := as.numeric(interval_hour)]
dt_estimates.for.plot[
  ,
  `:=` (
    is_significant = !(conf.low <= 0 & 0 <= conf.high),
    interval_hour = interval_hour + 0.5
  )
]


# ------------------------------------------------------------------------------
# Create figure(s)
# ------------------------------------------------------------------------------
# ------- Create DT(s) -------
# # 1. Create a DT indicating periods
dt_background <- data.table(
  x_min = c(15, 17, 19),
  x_max = c(17, 19, 21),
  y_min = rep(-Inf, times = 3),
  y_max = rep(Inf, times = 3),
  period = c("Pre-Peak", "Peak", "Post-Peak")
)
dt_background[
  ,
  period := factor(period, levels = c("Pre-Peak", "Peak", "Post-Peak"))
]


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


# # 2. Create object(s) that will be used later
coef.labels <- c(
  "1[Treatment & Post]",
  "1[Treatment & Post] x HDDs",
  "1[Treatment & Post] x HDDs*"
)
names(coef.labels) <- dt_estimates.for.plot[, .N, by = .(term)]$term


# ------- Create ggplot object(s) -------
plot_time.profiles <-
  ggplot() +
    geom_rect(
      data = dt_background,
      aes(
        xmin = x_min, xmax = x_max, ymin = y_min, ymax = y_max,
        fill = period
      ),
      alpha = 0.15
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
    geom_vline(
      xintercept = c(17, 19), linetype = "dotdash", alpha = 0.5
    ) +
    geom_errorbar(
      data = dt_estimates.for.plot[interval_hour %in% (15:20 + 0.5)],
      aes(x = interval_hour, y = estimate, ymin = conf.low, ymax = conf.high),
      width = 0.1
    ) +
    geom_point(
      data = dt_estimates.for.plot[
        interval_hour %in% (15:20 + 0.5) &
          is_significant == TRUE
      ],
      aes(x = interval_hour, y = estimate),
      shape = 16, size = 1.8, color = unikn::usecol("firebrick")
    ) +
    geom_point(
      data = dt_estimates.for.plot[interval_hour %in% (15:20 + 0.5)],
      aes(x = interval_hour, y = estimate),
      shape = 1, size = 1.8
    ) +
    facet_grid(
      term ~ ., scales = "free_y", labeller = labeller(term = coef.labels)
    ) +
    scale_x_continuous(
      breaks = seq(15, 21, by = 1), limits = c(15, 21)
    ) +
    scale_fill_manual(values = col.pal_custom[c(1:2, 4)]) +
    labs(
      x = "Hour of Day",
      y = TeX(r'(Treatment Effects  ($\Delta$ kWh per Hour))'),
      fill = "Period"
    ) +
    plot.options


# ------- Create ggplot object(s) -------
export_figure.in.png(
  plot_time.profiles,
  filename_str = paste(
    PATH_TO.SAVE_FIGURE_FOR.DISSERTATION,
    paste0(
      "Figure_Time-Profile-of-ATEs_Using-Tariff-Groups-A-and-D_Knot-",
      KNOT,
      ".png"
    ),
    sep = "/"
  ),
  width_numeric = 27, height_numeric = 20
)
