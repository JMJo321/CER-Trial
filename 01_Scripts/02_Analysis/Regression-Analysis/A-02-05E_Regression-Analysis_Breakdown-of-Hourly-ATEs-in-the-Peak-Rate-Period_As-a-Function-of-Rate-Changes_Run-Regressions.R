# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Regression-Analysis
# #
# > Script Number(s)
# # : A-02-05E
# #
# > Purpose of the script(s)
# #   The key purpose of this script is to break the ATEs down in two parts,
# #   temperature- and non-temperature-control, as a function of rate changes.
# #   1) Run regressions
# #      - Three econometic specifications
# #        : Model with no ID-related FEs,
# #          model with ID-related FEs, and
# #          model for spline regressions.
# #   2) Save the regression table(s) in .tex format.
# #   3) Make figures
# #      - Treatment effects
# #      - Predicted load profiles

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(arrow)
library(stringr)
library(data.table)
library(lfe)
library(ggplot2)
library(stargazer)
library(latex2exp)
library(gridExtra)
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
  "CER_DT-for-Regressions-with-Survey-Data_Electricity.parquet"
PATH_TO.LOAD_CER_FOR.REG <-
  paste(PATH_DATA_INTERMEDIATE_CER, FILE_TO.LOAD_CER_FOR.REG, sep = "/")

# # 1.2. For the R script including regression model(s)
FILE_TO.LOAD_CER_MODELS <- "M-CER-Trial_Regression-Specifications.R"
PATH_TO.LOAD_CER_MODELS <- paste(
  PATH_SCRIPT, FILE_TO.LOAD_CER_MODELS, sep = "/"
)


# # 2. Path(s) to which regression results will be stored
# # 2.1. For regression table(s)
DIR_TO.SAVE_LATEX_ATE <- paste(
  PATH_OUTPUT_TABLE,
  "From-Stargazer/ATEs",
  sep = "/"
)
DIR_TO.SAVE_LATEX_BREAKDOWN <- paste(
  PATH_OUTPUT_TABLE,
  "From-Stargazer/Breakdown-of-Hourly-ATEs/DID-Model-with-ID-FEs",
  sep = "/"
)

# # 2.2. For figure(s)
DIR_TO.SAVE_FIGURE <- "Breakdown-of-Hourly-ATEs"
PATH_TO.SAVE_FIGURE <- paste(PATH_OUTPUT_FIGURE, DIR_TO.SAVE_FIGURE, sep = "/")


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# # 1. Function(s) for ATEs
# # 1.1. Create a DT incl. the estimated ATEs
get_dt_estimated.ates <- function (
  felm.obj, knot_in.numeric, rate.change_in.numeric
) {
  # ## Extract the estimated coefficients
  coef_treatment.and.post <-
    felm.obj$coefficients["is_treatment.and.postTRUE",]
  coef_treatment.and.post_times_rate.change <-
    felm.obj$coefficients["treatment.and.post_times_rate.change",]
  coef_treatment.and.post_times_hdd <-
    felm.obj$coefficients["treatment.and.post_times_hdd",]
  coef_treatment.and.post_times_hdd.knot <-
    felm.obj$coefficients[
      paste0("treatment.and.post_times_hdd.knot", knot_in.numeric),
    ]
  coef_treatment.and.post_times_hdd_times_rate.change <-
    felm.obj$coefficients[
      "treatment.and.post_times_hdd_times_rate.change",
    ]
  coef_treatment.and.post_times_hdd.knot_times_rate.change <-
    felm.obj$coefficients[
      paste0(
        "treatment.and.post_times_hdd.knot",
        knot_in.numeric,
        "_times_rate.change"
      ),
    ]

  # ## Create a DT inlc. the estimated coefficients
  dt_for.plot <- data.table(hdd = seq(0, 30, by = 1))
  dt_for.plot[, rate.change := rate.change_in.numeric]
  dt_for.plot[
    ,
    treatment.effects_non.temp := (
      coef_treatment.and.post +
        coef_treatment.and.post_times_rate.change * rate.change
    )
  ]
  dt_for.plot[
    ,
    treatment.effects_temp := (
      (
        coef_treatment.and.post_times_hdd * hdd
      ) +
        (
          coef_treatment.and.post_times_hdd.knot *
            (hdd - knot_in.numeric) * (hdd > knot_in.numeric)
        ) +
        (
          coef_treatment.and.post_times_hdd_times_rate.change *
            hdd * rate.change
        ) +
        (
          coef_treatment.and.post_times_hdd.knot_times_rate.change *
            (hdd - knot_in.numeric) * (hdd > knot_in.numeric) * rate.change
        )
    )
  ]
  dt_for.plot[, rate.change := as.character(rate.change)]

  dt_for.plot_melted <- melt(
    dt_for.plot,
    id.vars = c("hdd", "rate.change"),
    measure.vars = c("treatment.effects_non.temp", "treatment.effects_temp"),
    variable.name = "category",
    value.name = "ates"
  )

  # ## Melt the DT created above
  dt_for.plot_melted[, category := str_extract(category, "(?<=_).+")]
  dt_for.plot_melted[
    ,
    category := lapply(
      category,
      function (x) if (str_detect(x, "^non")) {
        "Non-Temperature"
      } else {
        "Temperature"
      }) %>%
      as.vector(., mode = "character")
  ]

  return (dt_for.plot_melted)
}

# # 1.2. Function for making a ggplot object showing the estimated treatment
# #      effects
get_ggplot.obj_treatment.effects <- function (dt, knot_in.numeric) {
  ggplot.obj <-
    ggplot() +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.4) +
      geom_vline(
        xintercept = knot_in.numeric, linetype = "dotdash", alpha = 0.4
      ) +
      geom_line(
        data = dt[category == "Non-Temperature"],
        aes(x = hdd, y = ates, group = rate.change),
        color = "black", alpha = 0.25, lwd = 0.8
      ) +
      geom_line(
        data = dt[category == "Non-Temperature"],
        aes(x = hdd, y = ates, color = rate.change),
        lwd = 0.5, alpha = 0.8
      ) +
      geom_point(
        data = dt[category == "Non-Temperature"],
        aes(x = hdd, y = ates, shape = category),
        color = "black", alpha = 0.25, size = 2.5
      ) +
      geom_point(
        data = dt[category == "Non-Temperature"],
        aes(x = hdd, y = ates, shape = category, color = rate.change),
        size = 2.0, alpha = 0.8
      ) +
      geom_line(
        data = dt[category == "Temperature"],
        aes(x = hdd, y = ates, group = rate.change),
        color = "black", alpha = 0.25, lwd = 0.8
      ) +
      geom_line(
        data = dt[category == "Temperature"],
        aes(x = hdd, y = ates, color = rate.change),
        lwd = 0.5, alpha = 0.8
      ) +
      geom_point(
        data = dt[category == "Temperature"],
        aes(x = hdd, y = ates, shape = category),
        color = "black", alpha = 0.25, size = 2.5
      ) +
      geom_point(
        data = dt[category == "Temperature"],
        aes(x = hdd, y = ates, shape = category, color = rate.change),
        size = 2.0, alpha = 0.8
      ) +
      scale_x_continuous(breaks = seq(0, 30, by = 5)) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
      scale_color_viridis_d() +
      scale_shape_manual(values = c(16, 18)) +
      labs(
        x = "Heating Degree Days  (HDDs)",
        y = TeX(r'(Treatment Effects  ($\Delta$ kWh per Hour))'),
        color = TeX(r'(Rate Changes  ($\Delta$ Cents per kWh))'),
        shape = "Treatment Effects"
      ) +
      theme_linedraw() +
      theme(
        strip.text = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 12)),
        legend.position = "bottom",
        legend.margin = margin(-0.2, 0, 0, 0, unit = "cm"),
        legend.box.just = "left",
        legend.box = "vertical",
        legend.box.margin = margin(0.5, 0, 0.1, 0, unit = "cm")
      )

  return (ggplot.obj)
}


# # 2. Function(s) for load profile(s)
# # 2.1. Function for generate a DT incl. load profile(s) predicted by using
# #      the estimated ATEs
get_dt_load.profiles <- function (
  felm.obj, knot_in.numeric, rate.change_in.numeric
) {
  # ## Extract the estimated coefficients
  coef_hdd <-
    felm.obj$coefficients["hdd_all_60f",]
  coef_hdd.knot <-
    felm.obj$coefficients[paste0("hdd.knot", knot_in.numeric),]
  coef_treatment <-
    felm.obj$coefficients["is_treated_rTRUE",]
  coef_treatment_times_rate.change <-
    felm.obj$coefficients["treatment_times_rate.change",]
  coef_treatment_times_hdd <-
    felm.obj$coefficients["treatment_times_hdd",]
  coef_treatment_times_hdd.knot <-
    felm.obj$coefficients[paste0("treatment_times_hdd.knot", knot_in.numeric),]
  coef_treatment_times_hdd_times_rate.change <-
    felm.obj$coefficients["treatment_times_hdd_times_rate.change",]
  coef_treatment_times_hdd.knot_times_rate.change <-
    felm.obj$coefficients[
      paste0("treatment_times_hdd.knot", knot_in.numeric, "_times_rate.change"),
    ]
  coef_post <-
    felm.obj$coefficients["is_treatment.periodTRUE",]
  coef_post_times_hdd <-
    felm.obj$coefficients["post_times_hdd",]
  coef_post_times_hdd.knot <-
    felm.obj$coefficients[paste0("post_times_hdd.knot", knot_in.numeric),]
  coef_treatment.and.post <-
    felm.obj$coefficients["is_treatment.and.postTRUE",]
  coef_treatment.and.post_times_rate.change <-
    felm.obj$coefficients["treatment.and.post_times_rate.change",]
  coef_treatment.and.post_times_hdd <-
    felm.obj$coefficients["treatment.and.post_times_hdd",]
  coef_treatment.and.post_times_hdd.knot <- felm.obj$coefficients[
    paste0("treatment.and.post_times_hdd.knot", knot_in.numeric),
  ]
  coef_treatment.and.post_times_hdd_times_rate.change <-
    felm.obj$coefficients[
      "treatment.and.post_times_hdd_times_rate.change",
    ]
  coef_treatment.and.post_times_hdd.knot_times_rate.change <-
    felm.obj$coefficients[
      paste0(
        "treatment.and.post_times_hdd.knot",
        knot_in.numeric,
        "_times_rate.change"
      ),
    ]

  # ## Compute FEs
  dt_fes <- getfe(felm.obj) %>% setDT(.)
  # fes_id <- dt_fes[fe %like% "^id"]$effect %>% mean(.)
  fes_day <- dt_fes[fe %like% "^day"]$effect %>% mean(.)

  # ## Create a DT incl. predicted load profile(s)
  dt_plot <- data.table(hdd = seq(0, 30, by = 1))
  dt_plot[
    ,
    hdd.knot := (hdd - knot_in.numeric) * as.numeric(hdd > knot_in.numeric)
  ]
  dt_plot[
    ,
    hourly.kwh_initial := (
      (coef_hdd * hdd) +
        (coef_hdd.knot * hdd.knot) +
        (coef_treatment) +
        # (coef_treatment_times_rate.change * rate.change_in.numeric) +  #
        (coef_treatment_times_hdd * hdd) +
        (coef_treatment_times_hdd.knot * hdd.knot) +
        # (  #
        #   coef_treatment_times_hdd_times_rate.change * hdd *
        #     rate.change_in.numeric
        # ) +
        # (  #
        #   coef_treatment_times_hdd.knot_times_rate.change * hdd.knot *
        #     rate.change_in.numeric
        # ) +
        (coef_post) +
        (coef_post_times_hdd * hdd) +
        (coef_post_times_hdd.knot * hdd.knot) +
        # fes_id +
        fes_day
    )
  ]
  # ## Note:
  # ## To make the predicted load profile for the initial invariant across
  # #  rate changes, exclude `rate.change_in.numeric`-related terms.
  dt_plot[
    ,
    hourly.kwh_add.non.temp := hourly.kwh_initial + (
      (coef_treatment.and.post) +
        (coef_treatment.and.post_times_rate.change * rate.change_in.numeric)
    )
  ]
  dt_plot[
    ,
    hourly.kwh_final := hourly.kwh_add.non.temp + (
      (coef_treatment.and.post_times_hdd * hdd) +
        (coef_treatment.and.post_times_hdd.knot * hdd.knot) +
        (
          coef_treatment.and.post_times_hdd_times_rate.change * hdd *
            rate.change_in.numeric
        ) +
        (
          coef_treatment.and.post_times_hdd.knot_times_rate.change * hdd.knot *
            rate.change_in.numeric
        )
    )
  ]

  # ## Melt the DT generated above
  tmp_dt_1 <- dt_plot[, .(hdd, hourly.kwh_initial, hourly.kwh_add.non.temp)]
  tmp_dt_2 <- dt_plot[, .(hdd, hourly.kwh_add.non.temp, hourly.kwh_final)]
  names_new <- c("hdd", "hourly.kwh_min", "hourly.kwh_max")
  names(tmp_dt_1) <- names_new
  names(tmp_dt_2) <- names_new
  dt_plot_melted <- rbind(
    tmp_dt_1[, category := "Non-Temperature"],
    tmp_dt_2[, category := "Temperature"]
  )
  dt_plot_melted[, rate.change := as.character(rate.change_in.numeric)]
  return (dt_plot_melted)
}

# # 2.2. Function for creating a ggplot object showing predicted load profiles
get_ggplot.obj_load.profile <- function (
  dt.incl.load.profile_data.table, knot_in.numeric
) {
  col.pal_custom <- unikn::usecol(c("firebrick", "gold", "steelblue"))
  ggplot.obj <-
    ggplot() +
      geom_vline(
        xintercept = knot_in.numeric, alpha = 0.3, linetype = "dashed"
      ) +
      geom_ribbon(
        data = dt.incl.load.profile_data.table,
        aes(
          x = hdd,
          ymin = hourly.kwh_min,
          ymax = hourly.kwh_max,
          fill = category
        ),
        alpha = 0.3
      ) +
      geom_line(
        data = dt.incl.load.profile_data.table[category == "Non-Temperature"],
        aes(x = hdd, y = hourly.kwh_min),
        linetype = "solid", lwd = 0.5
      ) +
      geom_line(
        data = dt.incl.load.profile_data.table[category == "Non-Temperature"],
        aes(x = hdd, y = hourly.kwh_max),
        linetype = "dotdash", lwd = 0.5
      ) +
      geom_line(
        data = dt.incl.load.profile_data.table[category == "Temperature"],
        aes(x = hdd, y = hourly.kwh_max),
        color = col.pal_custom[3], lwd = 0.8
      ) +
      facet_wrap(. ~ rate.change) +
      scale_x_continuous(breaks = seq(0, 30, by = 5)) +
      scale_fill_manual(values = col.pal_custom[1:2]) +
      labs(
        x = "Heating Degree Days  (HDDs)",
        y = "Hourly Electricity Consumption  (kWh/Hour)",
        fill = "Treatment Effects"
      ) +
      theme_linedraw() +
      theme(
        strip.text = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 12)),
        legend.position = "bottom"
      )

  return (ggplot.obj)
}


# ------------------------------------------------------------------------------
# Load necessary dataset(s) and/or script(s) for regression analysis
# ------------------------------------------------------------------------------
# ------- Load necessary dataset(s) and/or script(s) -------
# # 1. Load required DT(s) for regression analysis
# # 1.1. Load the DT for regression analysis
dt_for.reg <- read_parquet(PATH_TO.LOAD_CER_FOR.REG)
setDT(dt_for.reg)


# # 2. Load required script(s) for regression analysis
# # 2.1. Load the R script including model specification(s)
source(PATH_TO.LOAD_CER_MODELS)


# ------- Modify the DT(s) loaded -------
# # 1. Add data field(s)
# # 1.1. Add a data field showing hourly electricity consumption
dt_for.reg[
  ,
  kwh_per.hour := lapply(.SD, sum, na.rm = TRUE), .SDcols = "kwh",
  by = .(id, date, interval_hour)
]

# # 1.2. Add data fields that will be used to run regression(s)
# # 1.2.1. Add HDD-related interaction term(s)
dt_for.reg[
  ,
  `:=` (
    treatment_times_hdd =
      as.numeric(is_treated_r) * hdd_all_60f,
    post_times_hdd =
      as.numeric(is_treatment.period) * hdd_all_60f,
    treatment.and.post_times_hdd =
      as.numeric(is_treatment.and.post) * hdd_all_60f
  )
]
# # 1.2.2. Add rate-change-related interaction term(s)
# # 1.2.2.1. Add a data field showing the size of rate changes
basis_rate <- dt_for.reg[
  group == "Control", .N, by = .(rate_cents.per.kwh)
]$rate_cents.per.kwh
dt_for.reg[
  ,
  dummy_rate.change := rate_cents.per.kwh - basis_rate
]
dt_rate.change <- dt_for.reg[
  period == "Treatment" & alloc_r_tariff %in% LETTERS[1:5],
  .N,
  by = .(alloc_r_tariff, rate.period, dummy_rate.change)
][
  , N := NULL
]
setnames(
  dt_rate.change,
  old = "dummy_rate.change",
  new = "rate.change"
)
dt_for.reg <- merge(
  x = dt_for.reg,
  y = dt_rate.change,
  by = c("alloc_r_tariff", "rate.period"),
  all.x = TRUE
)
# # 1.2.2.2. Add rate-change-related interaction term(s)
dt_for.reg[
  ,
  `:=` (
    treatment_times_rate.change =
      as.numeric(is_treated_r) * rate.change,
    treatment_times_hdd_times_rate.change =
      treatment_times_hdd * rate.change,
    treatment.and.post_times_rate.change =
      as.numeric(is_treatment.and.post) * rate.change,
    treatment.and.post_times_hdd_times_rate.change =
      treatment.and.post_times_hdd * rate.change
  )
]

# # 1.2.3. Add data fields for spline regression(s)
# # 1.2.3.1. Add data fields for spline regression(s)
# ## Set names of data fields
KNOTS <- seq(10, 14, by = 2)
for (KNOT in KNOTS) {
  var.name_hdd.diff <- paste0("hdd.diff_", KNOT)
  var.name_indicator <- paste0("indicator_", KNOT)
  var.name_hdd.knot <- paste0("hdd.knot", KNOT)
  # ## Add data fields
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
    (paste0("treatment_times_hdd.knot", KNOT)) :=
      as.numeric(is_treated_r) * get(var.name_hdd.knot)
  ]
  dt_for.reg[
    ,
    (paste0("treatment_times_hdd.knot", KNOT, "_times_rate.change")) :=
      as.numeric(is_treated_r) * get(var.name_hdd.knot) * rate.change
  ]
  dt_for.reg[
    ,
    (paste0("post_times_hdd.knot", KNOT)) :=
      as.numeric(is_treatment.period) * get(var.name_hdd.knot)
  ]
  dt_for.reg[
    ,
    (paste0("treatment.and.post_times_hdd.knot", KNOT)) :=
      as.numeric(is_treatment.and.post) * get(var.name_hdd.knot)
  ]
  dt_for.reg[
    ,
    (paste0("treatment.and.post_times_hdd.knot", KNOT, "_times_rate.change")) :=
      as.numeric(is_treatment.and.post) * get(var.name_hdd.knot) * rate.change
  ]
}


# ------------------------------------------------------------------------------
# Run regression(s), and then create stargazer object(s)
# ------------------------------------------------------------------------------
# ------- Run regression(s) -------
list_models_ols <- list(
  model_breakdown.of.ate_hourly.in.peak_rate.change_dw =
    model_breakdown.of.ate_hourly.in.peak_rate.change_dw,
  model_breakdown.of.ate_hourly.in.peak_rate.change_iw.dw =
    model_breakdown.of.ate_hourly.in.peak_rate.change_iw.dw
)
list_models_spline <- NULL
for (KNOT in KNOTS) {
  tmp_name_dw <- paste0(
    "model_breakdown.of.ate_hourly.in.peak_rate.change.in.spline_dw_knot",
    KNOT
  )
  tmp_name_iw.dw <- paste0(
    "model_breakdown.of.ate_hourly.in.peak_rate.change.in.spline_iw.dw_knot",
    KNOT
  )
  tmp_list <- list(
    get_change.terms.in.formula(
      model_breakdown.of.ate_hourly.in.peak_rate.change.in.spline_dw,
      term_old_in.str = "hdd.knot", term_new_in.str = paste0("hdd.knot", KNOT)
    ),
    get_change.terms.in.formula(
      model_breakdown.of.ate_hourly.in.peak_rate.change.in.spline_iw.dw,
      term_old_in.str = "hdd.knot", term_new_in.str = paste0("hdd.knot", KNOT)
    )
  )
  names(tmp_list) <- c(tmp_name_dw, tmp_name_iw.dw)
  list_models_spline <- c(list_models_spline, tmp_list)
}
list_models <- c(list_models_ols, list_models_spline)
reg.results <- lapply(
  list_models,
  felm,
  data = dt_for.reg[
    is_in.sample_incl.control_base.only.second.half == TRUE &
      rate.period == "Peak"
  ]
)


# ------- Create stargazer object(s) -------
# # 1. Create object(s) that will be used to make stargazer object(s)
title_ <- paste0(
  "Breakdonw of Average Treatment Effects in the Peak Rate Period: ",
  "As a Function of Rate Changes"
)
out.header <- FALSE
column.labels <- NULL
column.separate <- NULL
covariate.labels_text <- c(
  "HDDs",
  rep("(HDDs - Knot) x 1[HDDs > Knot]", times = length(list_models_spline) / 2),
  "1[Treatment]",
  "1[Treatment] x R.C.",
  "1[Treatment] x HDDs",
  rep("1[Treatment] x (HDDs - Knot) x 1[HDDs > Knot]", times = length(list_models_spline) / 2),
  "1[Treatment] x HDDs x R.C.",
  rep("1[Treatment] x (HDDs - Knot) x 1[HDDs > Knot] x R.C.", times = length(list_models_spline) / 2),
  "1[Post]",
  "1[Post] x HDDs",
  rep("1[Post] x (HDDs - Knot) x 1[HDDs > Knot]", times = length(list_models_spline) / 2),
  "1[Treatment and Post]",
  "1[Treatment and Post] x R.C.",
  "1[Treatment and Post] x HDDs",
  rep("1[Treatment and Post] x (HDDs - Knot) x 1[HDDs > Knot]", times = length(list_models_spline) / 2),
  "1[Treatment and Post] x HDDs x R.C.",
  rep("1[Treatment and Post] x (HDDs - Knot) x 1[HDDs > Knot] x R.C.", times = length(list_models_spline) / 2)
)
covariate.labels_latex <- c(
  "HDDs",
  rep("(HDDs - Knot) $\\times$ $\\mathbb{1}$[HDDs $>$ Knot]", times = length(list_models_spline) / 2),
  "$\\mathbb{1}$[Treatment]",
  "$\\mathbb{1}$[Treatment] $\\times$ $\\Delta$Price",
  "$\\mathbb{1}$[Treatment] $\\times$ HDDs",
  rep("$\\mathbb{1}$[Treatment] $\\times$ (HDDs - Knot) $\\times$ $\\mathbb{1}$[HDDs $>$ Knot]", times = length(list_models_spline) / 2),
  "$\\mathbb{1}$[Treatment] $\\times$ HDDs $\\times$ $\\Delta$Price",
  rep("$\\mathbb{1}$[Treatment] $\\times$ (HDDs - Knot) $\\times$ $\\mathbb{1}$[HDDs $>$ Knot] $\\times$ $\\Delta$Price", times = length(list_models_spline) / 2),
  "$\\mathbb{1}$[Post]",
  "$\\mathbb{1}$[Post] $\\times$ $\\Delta$Price",
  "$\\mathbb{1}$[Post] $\\times$ HDDs",
  rep("$\\mathbb{1}$[Post] $\\times$ (HDDs - Knot) $\\times$ $\\mathbb{1}$[HDDs $>$ Knot]", times = length(list_models_spline) / 2),
  "$\\mathbb{1}$[Treatment \\& Post]",
  "$\\mathbb{1}$[Treatment \\& Post] $\\times$ HDDs",
  rep("$\\mathbb{1}$[Treatment \\& Post] $\\times$ (HDDs - Knot) $\\times$ $\\mathbb{1}$[HDDs $>$ Knot]", times = length(list_models_spline) / 2),
  "$\\mathbb{1}$[Treatment \\& Post] $\\times$ HDDs $\\times$ $\\Delta$Price",
  rep("$\\mathbb{1}$[Treatment \\& Post] $\\times$ (HDDs - Knot) $\\times$ $\\mathbb{1}$[HDDs $>$ Knot] $\\times$ $\\Delta$Price", times = length(list_models_spline) / 2)
)
dep.var.caption <- "Dependent Variable"
dep.var.labels <- "Hourly Electricity Consumption  (kWh/Hour)"
add.lines <- list(
  c("Electrical Heating for Space", rep("False", times = length(list_models))),
  c("Electrical Heating for Water", rep("False", times = length(list_models))),
  c("Knot", c(rep("(N/A)", times = 2), rep(KNOTS, each = 2))),
  c(
    "FEs: Household by Half-Hourly Time Window",
    rep(c("No", "Yes"), times = length(list_models) / 2)
  ),
  c("FEs: Day of Week by Half-Hourly Time Window", rep("Yes", times = length(list_models)))
)
column.sep.width <- "0pt"
font.size <- "small"
header <- FALSE
label <- paste0(
  "Table:Breakdown-of-Average-Treatement-Effects-in-the-Peak-Rate-Period_",
  "As-a-Function-of-Rate-Changes"
)
model.names <- FALSE
omit.stat <- c("ser", "rsq")
omit.table.layout <- "n"
order <- NULL


# # 2. Create stargazer object(s)
# # 2.1. Print regression table(s)
stargazer(
  reg.results,
  type = "text",
  title = title_,
  out = NULL,
  out.header = out.header,
  column.labels = column.labels,
  column.separate = column.separate,
  covariate.labels = covariate.labels_text,
  dep.var.caption = dep.var.caption,
  dep.var.labels = dep.var.labels,
  add.lines = add.lines,
  column.sep.width = column.sep.width,
  font.size = font.size,
  header = header,
  label = label,
  model.names = model.names
)

# # 2.2. In .tex format
stargazer(
  reg.results,
  type = "latex",
  title = title_,
  out = paste(
    DIR_TO.SAVE_LATEX_BREAKDOWN,
    paste0(
      "Breakdown-of-ATEs_Hourly-in-the-Peak-Rate-Period_",
      "As-a-Function-of-Rate-Changes.tex"
    ),
    sep = "/"
  ),
  out.header = out.header,
  column.labels = column.labels,
  column.separate = column.separate,
  covariate.labels = covariate.labels_latex,
  dep.var.caption = dep.var.caption,
  dep.var.labels = dep.var.labels,
  add.lines = add.lines,
  column.sep.width = column.sep.width,
  font.size = font.size,
  header = header,
  label = label,
  model.names = model.names,
  omit.stat = omit.stat,
  omit.table.layout = omit.table.layout
)


# ------------------------------------------------------------------------------
# Make plots by using the regression result(s) above
# ------------------------------------------------------------------------------
# ------- Make plots by using the regression result(s) above -------
# # 0. Create object(s) that will be used later
INDICES <- c(3, 5, 7)
for (idx in INDICES) {
  FELM.OBJ <- reg.results[[idx]]
  KNOT <- KNOTS[which(INDICES == idx)]
  # ## Note:
  # ## THis value depends on the felm object exploited
  RATE.CHANGES <- seq(6, 24, by = 6)


  # # 1. Make plot(s): ATEs
  # # 1.1. Create a DT incl. the estimated ATEs
  dt_ates <- lapply(
    RATE.CHANGES,
    get_dt_estimated.ates,
    felm.obj = FELM.OBJ,
    knot_in.numeric = KNOT
  ) %>%
    rbindlist(.)
  dt_ates[, rate.change := factor(rate.change, levels = RATE.CHANGES)]

  # # 1.2. Create a ggplot object, and then export it in .PNG format
  plot_ates <- get_ggplot.obj_treatment.effects(dt_ates, KNOT)
  export_figure.in.png(
    plot_ates,
    filename_str = paste(
      PATH_TO.SAVE_FIGURE,
      paste0(
        "Figure_Breakdown-of-Hourly-ATEs-in-the-Peak-Rate-Period_",
        "As-a-Function-of-Rate-Changes_ATEs_Knot-", KNOT,".png"
      ),
      sep = "/"
    ),
    width_numeric = 30, height_numeric = 17
  )


  # # 2. Make plot(s): Predicted load profile(s)
  # # 2.1. Create a DT incl. the predicted load profile(s)
  obj.name <- paste0("dt_load.profiles_knot", KNOT)
  assign(
    obj.name,
    lapply(
      RATE.CHANGES,
      get_dt_load.profiles,
      felm.obj = FELM.OBJ,
      knot_in.numeric = KNOT
    ) %>%
      rbindlist(.)
  )
  get(obj.name)[
    ,
    rate.change := factor(rate.change, levels = RATE.CHANGES)
  ]

  # # 2.2. Create a ggplot object, and then export it in .PNG format
  plot_load.profiles <- get_ggplot.obj_load.profile(get(obj.name), KNOT)
  export_figure.in.png(
    plot_load.profiles,
    filename_str = paste(
      PATH_TO.SAVE_FIGURE,
      paste0(
        "Figure_Breakdown-of-Hourly-ATEs-in-the-Peak-Rate-Period_",
        "As-a-Function-of-Rate-Changes_Load-Profiles_Knot-", KNOT, ".png"
      ),
      sep = "/"
    ),
    width_numeric = 35, height_numeric = 20
  )
}
