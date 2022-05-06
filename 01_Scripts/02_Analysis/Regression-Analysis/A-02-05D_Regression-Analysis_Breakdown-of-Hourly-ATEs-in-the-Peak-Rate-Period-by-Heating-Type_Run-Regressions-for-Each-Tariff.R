# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Regression-Analysis
# #
# > Script Number(s)
# # : A-02-05D
# #
# > Purpose of the script(s)
# #   Basically the key purpose of this script is to examine the impact of
# #   excluding month-related FEs on the estimated treatment effects.
# #   1) Run regressions, by heating type and tariff, to breakdown hourly ATEs.
# #      - Three pairs of space-water heating type
# #        : True-True, False-True, and False-False
# #      - Tariffs
# #        : A, B, C, and D
# #      - Three econometic specifications
# #        : Model with no HDD-related interaction terms,
# #          model with HDD-related interaction terms, and
# #          model for spline regressions.
# #   2) Save the regression table(s) in .tex format.
# #   3) Make figures
# #      - Treatment effects
# #      - Predicted load profiles
# #   4) Get information for the transition matrix showing how Irish households
# #      changed their heating types.

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

# # 2.2. For regression result(s)
DIR_TO.SAVE_REG.RESULTS <- paste(
  PATH_DATA_ANALYSIS_REG.RESULTS,
  "Breakdown-of-Hourly-ATEs/DID-Model-with-ID-FEs",
  sep = "/"
)

# # 2.3. For figure(s)
DIR_TO.SAVE_FIGURE <- "Breakdown-of-Hourly-ATEs"
PATH_TO.SAVE_FIGURE <- paste(PATH_OUTPUT_FIGURE, DIR_TO.SAVE_FIGURE, sep = "/")


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# # 1. To create a subsetting condition
get_subsetting.condition.in.str <- function (case_in.vector) {
  # ## Note:
  # ## The list must be in the form of
  # ## (
  # ##    `heating.type` = "Space", "Water", or "Both",
  # ##    `is_elec.heating_space_pre/post` = TRUE, or FALSE,
  # ##    `is_elec.heating_water_pre/post` = TRUE, or FALSE
  # ## )
  condition_in.str <- paste(
    "is_in.sample_incl.control_base.only.second.half == TRUE",
    "as.character(rate.period) == 'Peak'",
    "alloc_r_tariff %in% LETTERS[1:5]",
    paste(
      c("is_elec.heating_space_pre", "is_elec.heating_space_post"),
      case_in.vector[1],
      sep = " == "
    ) %>%
      paste(., collapse = " & "),
    paste(
      c("is_elec.heating_water_pre", "is_elec.heating_water_post"),
      case_in.vector[2],
      sep = " == "
    ) %>%
      paste(., collapse = " & "),
    sep = " & "
  )
  return (condition_in.str)
}


# # 2. Functions for running regression(s)
# # 2.1. For heating-type-level sample(s)
# # 2.1.1. To get a felm object
get_felm.obj <- function (condition_in.str, formula) {
  felm.obj <- lfe::felm(
    data = dt_for.reg[eval(parse(text = condition_in.str))],
    formula = formula
  )
  return (felm.obj)
}
# # 2.1.2. To get a regression result for a case
get_reg.result <- function (case_in.vector, formula) {
  condition_in.str <-
    get_subsetting.condition.in.str(case_in.vector = case_in.vector)
  reg.result <-
    get_felm.obj(condition_in.str = condition_in.str, formula = formula)
  return (reg.result)
}
# # 2.1.3. To get the number of households
get_number.of.households <- function (case_in.vector) {
  condition_in.str <-
    get_subsetting.condition.in.str(case_in.vector = case_in.vector)
  n <- dt_for.reg[eval(parse(text = condition_in.str)), .N, by = .(id)][, .N]
  return (n)
}

# # 2.2. For tariff-level sample(s)
# # 2.2.1. To get a felm object
get_felm.obj_by.tariff <- function (condition_in.str, formula, tariff_in.str) {
  felm.obj <- lfe::felm(
    data = dt_for.reg[
      eval(parse(text = condition_in.str))
    ][
      alloc_r_tariff %in% c(tariff_in.str, "E")
    ],
    formula = formula
  )
  return (felm.obj)
}
# # 2.2.2. To get a regression result for a case
get_reg.result_by.tariff <- function (case_in.vector, formula, tariff_in.str) {
  condition_in.str <-
    get_subsetting.condition.in.str(case_in.vector = case_in.vector)
  reg.result <-
    get_felm.obj_by.tariff(
      condition_in.str = condition_in.str,
      formula = formula,
      tariff_in.str = tariff_in.str
    )
  return (reg.result)
}
# # 2.2.3. To get the number of households
get_number.of.households_by.tariff <- function (case_in.vector, tariff_in.str) {
  condition_in.str <-
    get_subsetting.condition.in.str(case_in.vector = case_in.vector)
  n <- dt_for.reg[
    eval(parse(text = condition_in.str))
  ][
    alloc_r_tariff %in% c(tariff_in.str, "E"), .N, by = .(id)
  ][
    , .N
  ]
  return (n)
}


# # 3. Functions for making figure(s)
# # 3.1. To generate a ggplot object for treatment effects
get_ggplot.obj_treatment.effects <- function (
  felm.obj, knot_in.numeric, case_in.vector
) {
  coef_treatment.and.post <-
    felm.obj$coefficients["is_treatment.and.postTRUE",]
  coef_treatment.and.post.by.hdd <-
    felm.obj$coefficients["treatment.and.post_by_hdd",]
  coef_treatment.and.post.by.hdd.knot <-
    felm.obj$coefficients[
      paste0("treatment.and.post_by_hdd.knot", knot_in.numeric),
    ]

  dt_for.plot <- data.table(hdd = seq(0, 30, by = 1))
  dt_for.plot[
    ,
    `:=` (
      treatment.effects_non.temp = coef_treatment.and.post,
      treatment.effects_temp = (
        coef_treatment.and.post.by.hdd * hdd +
          (
            coef_treatment.and.post.by.hdd.knot *
              (hdd - knot_in.numeric) *
               (hdd > knot_in.numeric)
          )
      )
    )
  ]

  col.pal_custom <- unikn::usecol(c("firebrick", "gold", "steelblue"))
  space_ <- if (case_in.vector[1] == TRUE) {
    "Electric Heating for Space"
  } else {
    "Non-Electric Heating for Space"
  }
  water_ <- if (case_in.vector[2] == TRUE) {
    "Electric Heating for Water"
  } else {
    "Non-Electric Heating for Water"
  }
  subtitle <- paste(space_, water_, sep = " & ")
  plot_treatment.effects <-
    ggplot(data = dt_for.plot) +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.4) +
      geom_vline(
        xintercept = knot_in.numeric, linetype = "dotdash", alpha = 0.4
      ) +
      geom_line(
        aes(x = hdd, y = treatment.effects_non.temp),
        color = "black", alpha = 0.25, lwd = 0.8
      ) +
      geom_line(
        aes(x = hdd, y = treatment.effects_non.temp),
        color = col.pal_custom[1], lwd = 0.5
      ) +
      geom_point(
        aes(x = hdd, y = treatment.effects_non.temp),
        color = "black", size = 2.5, alpha = 0.25
      ) +
      geom_point(
        aes(x = hdd, y = treatment.effects_non.temp),
        color = col.pal_custom[1], size = 2.0
      ) +
      geom_line(
        aes(x = hdd, y = treatment.effects_temp),
        color = "black", alpha = 0.25, lwd = 0.8
      ) +
      geom_line(
        aes(x = hdd, y = treatment.effects_temp),
        color = col.pal_custom[2], lwd = 0.5
      ) +
      geom_point(
        aes(x = hdd, y = treatment.effects_temp),
        color = "black", size = 2.5, alpha = 0.25
      ) +
      geom_point(
        aes(x = hdd, y = treatment.effects_temp),
        color = col.pal_custom[2], size = 2.0
      ) +
      scale_x_continuous(breaks = seq(0, 30, by = 5)) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
      scale_shape_manual(values = c(16, 18)) +
      labs(
        x = "Heating Degree Days  (HDDs)",
        y = TeX(r'(Treatment Effects  $(\Delta kWh)$)'),
        subtitle = subtitle
      ) +
      theme_linedraw() +
      theme(
        strip.text = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 12)),
        legend.position = "bottom"
      )
  return (plot_treatment.effects)
}

# # 3.2. To create a DT for predicted load profile
get_dt.for.plotting <- function (felm.obj, knot_in.numeric) {
  dt_fes <- getfe(felm.obj) %>% setDT(.)
  fes_id <- dt_fes[fe %like% "^id"]$effect %>% mean(.)
  fes_day <- dt_fes[fe %like% "^day"]$effect %>% mean(.)
  # fes_month <- dt_fes[fe %like% "^month"]$effect %>% mean(.)

  dt_plot <- data.table(hdd = seq(0, 30, by = 1))
  dt_plot[
    ,
    hourly.kwh_initial := (
      hdd * felm.obj$coefficients["hdd_all_60f",] +
        hdd * felm.obj$coefficients["treatment_by_hdd",] +
        felm.obj$coefficients["is_treatment.periodTRUE",] +
        hdd * felm.obj$coefficients["treatment.period_by_hdd",] +
        ((hdd - knot_in.numeric) * as.numeric(hdd > knot_in.numeric) *
          felm.obj$coefficients[paste0("hdd.knot", knot_in.numeric),]) +
        (
          (hdd - knot_in.numeric) *
            as.numeric(hdd > knot_in.numeric) *
            felm.obj$coefficients[
              paste0("treatment_by_hdd.knot", knot_in.numeric),
            ]
        ) +
        (
          (hdd - knot_in.numeric) *
            as.numeric(hdd > knot_in.numeric) *
            felm.obj$coefficients[
              paste0("treatment.period_by_hdd.knot", knot_in.numeric),
            ]
        ) +
        fes_id + fes_day
        # + fes_month
    )
  ]
  dt_plot[
    ,
    hourly.kwh_add.non.temp := (
      hourly.kwh_initial + felm.obj$coefficients["is_treatment.and.postTRUE",]
    )
  ]
  dt_plot[
    ,
    hourly.kwh_final := (
      hourly.kwh_add.non.temp +
        hdd * felm.obj$coefficients["treatment.and.post_by_hdd",] +
        (hdd - knot_in.numeric) *
          as.numeric(hdd > knot_in.numeric) *
          felm.obj$coefficients[
            paste0("treatment.and.post_by_hdd.knot", knot_in.numeric),
          ]
    )
  ]

  tmp_dt_1 <- dt_plot[, .(hdd, hourly.kwh_initial, hourly.kwh_add.non.temp)]
  tmp_dt_2 <- dt_plot[, .(hdd, hourly.kwh_add.non.temp, hourly.kwh_final)]
  names_new <- c("hdd", "hourly.kwh_min", "hourly.kwh_max")
  names(tmp_dt_1) <- names_new
  names(tmp_dt_2) <- names_new
  dt_plot_melted <- rbind(
    tmp_dt_1[, category := "Non-Temperature"],
    tmp_dt_2[, category := "Temperature"]
  )
}

# # 3.3. To generate a ggplot object for predicted load profile
get_ggplot.obj_load.profile <- function (
  data.table_dt, knot_in.numeric, case_in.vector
) {
  col.pal_custom <- unikn::usecol(c("firebrick", "gold", "steelblue"))

  space_ <- if (case_in.vector[1] == TRUE) {
    "Electric Heating for Space"
  } else {
    "Non-Electric Heating for Space"
  }
  water_ <- if (case_in.vector[2] == TRUE) {
    "Electric Heating for Water"
  } else {
    "Non-Electric Heating for Water"
  }
  subtitle <- paste(space_, water_, sep = " & ")

  ggplot.obj <-
    ggplot() +
      geom_vline(
        xintercept = knot_in.numeric, alpha = 0.3, linetype = "dotdash"
      ) +
      geom_ribbon(
        data = data.table_dt,
        aes(
          x = hdd,
          ymin = hourly.kwh_min,
          ymax = hourly.kwh_max,
          fill = category
        ),
        alpha = 0.3
      ) +
      geom_line(
        data = data.table_dt[category == "Non-Temperature"],
        aes(x = hdd, y = hourly.kwh_min),
        linetype = "dotted", lwd = 0.3
      ) +
      geom_line(
        data = data.table_dt[category == "Non-Temperature"],
        aes(x = hdd, y = hourly.kwh_max),
        linetype = "dotdash", lwd = 0.3
      ) +
      geom_line(
        data = data.table_dt[category == "Temperature"],
        aes(x = hdd, y = hourly.kwh_max),
        color = col.pal_custom[3]
      ) +
      scale_x_continuous(breaks = seq(0, 45, by = 5)) +
      scale_y_continuous(labels = scales::comma_format(accuracy = 0.01)) +
      scale_fill_manual(values = col.pal_custom[1:2]) +
      labs(
        x = "Heating Degree Days  (HDDs)",
        y = "kWh/Hour",
        fill = "",
        subtitle = subtitle
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
dt_for.reg[
  ,
  `:=` (
    treatment_by_hdd = as.numeric(is_treated_r) * hdd_all_60f,
    treatment.period_by_hdd = as.numeric(is_treatment.period) * hdd_all_60f,
    treatment.and.post_by_hdd = as.numeric(is_treatment.and.post) * hdd_all_60f,
    hdd2 = (hdd_all_60f^2),
    treatment_by_hdd2 = as.numeric(is_treated_r) * (hdd_all_60f^2),
    treatment.period_by_hdd2 =
      as.numeric(is_treatment.period) * (hdd_all_60f^2),
    treatment.and.post_by_hdd2 =
      as.numeric(is_treatment.and.post) * (hdd_all_60f^2)
  )
]


# ------------------------------------------------------------------------------
# Run regression(s) to estimate ATEs by heating type
# ------------------------------------------------------------------------------
# # ------- Run regression(s) -------
# # 1. Create object(s) that will be used later
# # 1.1. A list of cases
list_cases <- list(
  elec_and_elec = c(TRUE, TRUE),
  # elec_and_non.elec = c(TRUE, FALSE),
  # ## Note: There is only one household in this case.
  non.elec_and_elec = c(FALSE, TRUE),
  non.elec_and_non.elec = c(FALSE, FALSE)
)
# # 1.2. A list containing the number of households, by heating type
list_number.of.households_by.heating.type <-
  lapply(list_cases, get_number.of.households)


# # 2. Run regression(s)
list_reg.results_ate_fes.m <- lapply(
  list_cases,
  get_reg.result,
  formula = model_ate_hourly.in.peak_iw.dw.m
)
list_reg.results_ate_fes.none <- lapply(
  list_cases,
  get_reg.result,
  formula = model_ate_hourly.in.peak_iw.dw
)


# # ------- Export regression result(s) -------
# # 1. Create objects that will be utilized to generate stargazer object(s)
title_ <- paste0(
  "Average Treatment Effects in the Peak Rate Period: ",
  "By Heating Type, Including/Excluding Month of Year FEs"
)
out.header <- FALSE
column.labels <- NULL
column.separate <- NULL
covariate.labels_text <- "1[Treatment and Post]"
covariate.labels_latex <- "$\\mathbb{1}$[Treatment \\& Post]"
dep.var.caption <- "Dependent Variable"
dep.var.labels <- "Hourly Electricity Consumption  (kWh/Hour)"
add.lines <- list(
  c(
    "Electrical Heating for Space",
    rep(c("True", "False", "False"), times = 2)
  ),
  c(
    "Electrical Heating for Water",
    rep(c("True", "True", "False"), times = 2)
  ),
  c("FEs: Household by Half-Hourly Time Window", rep("Yes", times = 6)),
  c("FEs: Day of Week by Half-Hourly Time Window", rep("Yes", times = 6)),
  c("FEs: Month of Year", rep(c("Yes", "No"), each = 3)),
  c(
    "Number of Households",
    as.vector(list_number.of.households_by.heating.type, mode = "numeric") %>%
      format(., big.mark = ",") %>%
      rep(., times = 2)
  )
)
column.sep.width <- "20pt"
font.size <- "small"
header <- FALSE
label <- paste0(
  "Table:Average-Treatment-Effects-in-the-Peak-Rate-Period_By-Heating-Type_",
  "Model-without-Interaction-Terms_Month-of-Year-FEs"
)
model.names <- FALSE
omit.stat <- c("ser", "rsq")
omit.table.layout <- "n"
order <- NULL


# # 2. Export regression result(s)
# # 2.1. Export regression result(s) in the form of stargazer object
# # 2.1.1. Print regression table(s)
stargazer(
  c(list_reg.results_ate_fes.m, list_reg.results_ate_fes.none),
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
# # 2.1.2. Export regression table(s) in .tex format
stargazer(
  c(list_reg.results_ate_fes.m, list_reg.results_ate_fes.none),
  type = "latex",
  title = title_,
  out = paste(
    DIR_TO.SAVE_LATEX_ATE,
    paste0(
      "ATEs_Hourly-in-the-Peak-Rate-Period_By-Heating-Type_",
      "Including-vs-Excluding-Month-of-Year-FEs.tex"
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
# Run regression(s) to break by-heating-type ATEs down with FEs model(s)
# ------------------------------------------------------------------------------
# # ------- Run regression(s), by heating type -------
# # 1. Run regression(s) to break ATEs down, with FEs model(s)
reg.result_by.heating.type_fes.mw <- lapply(
  list_cases,
  get_reg.result,
  formula = model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version1
)
reg.result_by.heating.type_fes.m <- lapply(
  list_cases,
  get_reg.result,
  formula = model_breakdown.of.ate_hourly.in.peak_iw.dw.m
)
reg.result_by.heating.type_fes.none <- lapply(
  list_cases,
  get_reg.result,
  formula = model_breakdown.of.ate_hourly.in.peak_iw.dw
)


# # 2. Export regression result(s)
# # 2.1. Create object(s) that will be used later to make stargazer object(s)
title_ <- paste0(
  "Breakdown of Average Treatment Effects in the Peak Rate Period: ",
  "By Heating Type, With different Month-related FEs"
)
covariate.labels_text <- c(
  "HDDs",
  "1[Treatment] x HDDs",
  "1[Post]",
  "1[Post] x HDDs",
  "1[Treatment and Post]",
  "1[Treatment and Post] x HDDs"
)
covariate.labels_latex <- c(
  "HDDs",
  "$\\mathbb{1}$[Treatment] $\\times$ HDDs",
  "$\\mathbb{1}$[Post]",
  "$\\mathbb{1}$[Post] $\\times$ HDDs",
  "$\\mathbb{1}$[Treatment \\& Post]",
  "$\\mathbb{1}$[Treatment \\& Post] $\\times$ HDDs"
)
add.lines <- list(
  c(
    "Electrical Heating for Space",
    rep(c("True", "False", "False"), times = 3)
  ),
  c(
    "Electrical Heating for Water",
    rep(c("True", "True", "False"), times = 3)
  ),
  c("FEs: Household by Half-Hourly Time Window", rep("Yes", times = 9)),
  c("FEs: Day of Week by Half-Hourly Time Window", rep("Yes", times = 9)),
  c(
    "FEs: Month of Year by Half-Hourly Time Window",
    c(rep("Yes", times = 3), rep("No", times = 6))
  ),
  c(
    "FEs: Month of Year",
    c(rep("No", times = 3), rep("Yes", times = 3), rep("No", times = 3))
  ),
  c(
    "Number of Households",
    as.vector(list_number.of.households_by.heating.type, mode = "numeric") %>%
      format(., big.mark = ",") %>%
      rep(., times = 3)
  )
)
label <- paste0(
  "Table:Breakdown-of-Average-Treatment-Effects-in-the-Peak-Rate-Period_",
  "By-Heating-Type_With-Different-Month-related-FEs"
)

# # 2.2. Create stargazer object(s)
# # 2.2.1. Print regression table(s)
stargazer(
  c(
    reg.result_by.heating.type_fes.mw,
    reg.result_by.heating.type_fes.m,
    reg.result_by.heating.type_fes.none
  ),
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
# # 2.2.2. Export regression table(s) in .tex format
stargazer(
  c(
    reg.result_by.heating.type_fes.mw,
    reg.result_by.heating.type_fes.m,
    reg.result_by.heating.type_fes.none
  ),
  type = "latex",
  title = title_,
  out = paste(
    DIR_TO.SAVE_LATEX_BREAKDOWN,
    paste0(
      "Breakdown-of-ATEs_Hourly-in-the-Peak-Rate-Period_",
      "By-Heating-Type_With-Different-Month-related-FEs.tex"
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


# # ------- Run regression(s), by tariff and heating type -------
# # 1. Run regression(s)
# # 1.1. Run regression(s) and compute the number of households
for (tariff in LETTERS[1:4]) {
  # ## Create object(s) that will be used later
  obj.name_reg.result <-
    paste0("reg.result_by.heating.type.and.tariff_fes.mw_", tolower(tariff))
  obj.name_number.of.households <-
    paste0("number.of.households_", tolower(tariff))

  # ## Run regression(s)
  assign(
    obj.name_reg.result,
    lapply(
      list_cases,
      get_reg.result_by.tariff,
      formula = model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version1,
      tariff_in.str = tariff
    )
  )

  # ## Compute the number of households
  assign(
    obj.name_number.of.households,
    lapply(
      list_cases,
      get_number.of.households_by.tariff,
      tariff_in.str = tariff
    )
  )
}
for (tariff in LETTERS[1:4]) {
  # ## Create object(s) that will be used later
  obj.name_reg.result <-
    paste0("reg.result_by.heating.type.and.tariff_fes.none_", tolower(tariff))

  # ## Run regression(s)
  assign(
    obj.name_reg.result,
    lapply(
      list_cases,
      get_reg.result_by.tariff,
      formula = model_breakdown.of.ate_hourly.in.peak_iw.dw,
      tariff_in.str = tariff
    )
  )
}

# # 1.2. Create a list of household numbers
list_number.of.households_by.tariff.and.heating.type <- c(
  as.vector(number.of.households_a, mode = "numeric") %>%
    format(., big.mark = ","),
  as.vector(number.of.households_b, mode = "numeric") %>%
    format(., big.mark = ","),
  as.vector(number.of.households_c, mode = "numeric") %>%
    format(., big.mark = ","),
  as.vector(number.of.households_d, mode = "numeric") %>%
    format(., big.mark = ",")
)


# # 2. Export regression result(s)
# # 2.1. Create object(s) that will be used later
covariate.labels_text <- c(
  "HDDs",
  "1[Treatment] x HDDs",
  "1[Post]",
  "1[Post] x HDDs",
  "1[Treatment and Post]",
  "1[Treatment and Post] x HDDs"
)
covariate.labels_latex <- c(
  "HDDs",
  "$\\mathbb{1}$[Treatment] $\\times$ HDDs",
  "$\\mathbb{1}$[Post]",
  "$\\mathbb{1}$[Post] $\\times$ HDDs",
  "$\\mathbb{1}$[Treatment \\& Post]",
  "$\\mathbb{1}$[Treatment \\& Post] $\\times$ HDDs"
)

# # 2.2. Create stargazer object(s)
# # 2.2.1. Print regression table(s)
# # 2.2.1.1. For Tariff A
stargazer(
  c(
    reg.result_by.heating.type.and.tariff_fes.mw_a,
    reg.result_by.heating.type.and.tariff_fes.none_a
  ),
  type = "text",
  title = paste0(
    "Breakdown of Average Treatment Effects in the Peak Rate Period: ",
    "For Tariff A, By Heating Type, With different Month-related FEs"
  ),
  out = NULL,
  out.header = out.header,
  column.labels = column.labels,
  column.separate = column.separate,
  covariate.labels = covariate.labels_text,
  dep.var.caption = dep.var.caption,
  dep.var.labels = dep.var.labels,
  add.lines = add.lines <- list(
    c("Tariff", rep("A", each = 6)),
    c(
      "Electrical Heating for Space",
      rep(c("True", "False", "False"), times = 2)
    ),
    c(
      "Electrical Heating for Water",
      rep(c("True", "True", "False"), times = 2)
    ),
    c("FEs: Household by Half-Hourly Time Window", rep("Yes", times = 6)),
    c("FEs: Day of Week by Half-Hourly Time Window", rep("Yes", times = 6)),
    c(
      "FEs: Month of Year by Half-Hourly Time Window",
      rep(c("Yes", "No"), each = 3)
    ),
    c(
      "Number of Households",
      rep(list_number.of.households_by.tariff.and.heating.type[1:3], times = 2)
    )
  ),
  column.sep.width = column.sep.width,
  font.size = font.size,
  header = header,
  label = label <- paste0(
    "Table:Breakdown-of-Average-Treatment-Effects-in-the-Peak-Rate-Period_",
    "For-Tariff-A_By-Heating-Type_With-Different-Month-related-FEs"
  ),
  model.names = model.names
)
# # 2.2.1.2. For Tariff B
stargazer(
  c(
    reg.result_by.heating.type.and.tariff_fes.mw_b,
    reg.result_by.heating.type.and.tariff_fes.none_b
  ),
  type = "text",
  title = paste0(
    "Breakdown of Average Treatment Effects in the Peak Rate Period: ",
    "For Tariff B, By Heating Type, With different Month-related FEs"
  ),
  out = NULL,
  out.header = out.header,
  column.labels = column.labels,
  column.separate = column.separate,
  covariate.labels = covariate.labels_text,
  dep.var.caption = dep.var.caption,
  dep.var.labels = dep.var.labels,
  add.lines = add.lines <- list(
    c("Tariff", rep("B", each = 6)),
    c(
      "Electrical Heating for Space",
      rep(c("True", "False", "False"), times = 2)
    ),
    c(
      "Electrical Heating for Water",
      rep(c("True", "True", "False"), times = 2)
    ),
    c("FEs: Household by Half-Hourly Time Window", rep("Yes", times = 6)),
    c("FEs: Day of Week by Half-Hourly Time Window", rep("Yes", times = 6)),
    c(
      "FEs: Month of Year by Half-Hourly Time Window",
      rep(c("Yes", "No"), each = 3)
    ),
    c(
      "Number of Households",
      rep(list_number.of.households_by.tariff.and.heating.type[4:6], times = 2)
    )
  ),
  column.sep.width = column.sep.width,
  font.size = font.size,
  header = header,
  label = label <- paste0(
    "Table:Breakdown-of-Average-Treatment-Effects-in-the-Peak-Rate-Period_",
    "For-Tariff-B_By-Heating-Type_With-Different-Month-related-FEs"
  ),
  model.names = model.names
)
# # 2.2.1.3. For Tariff C
stargazer(
  c(
    reg.result_by.heating.type.and.tariff_fes.mw_c,
    reg.result_by.heating.type.and.tariff_fes.none_c
  ),
  type = "text",
  title = paste0(
    "Breakdown of Average Treatment Effects in the Peak Rate Period: ",
    "For Tariff C, By Heating Type, With different Month-related FEs"
  ),
  out = NULL,
  out.header = out.header,
  column.labels = column.labels,
  column.separate = column.separate,
  covariate.labels = covariate.labels_text,
  dep.var.caption = dep.var.caption,
  dep.var.labels = dep.var.labels,
  add.lines = add.lines <- list(
    c("Tariff", rep("C", each = 6)),
    c(
      "Electrical Heating for Space",
      rep(c("True", "False", "False"), times = 2)
    ),
    c(
      "Electrical Heating for Water",
      rep(c("True", "True", "False"), times = 2)
    ),
    c("FEs: Household by Half-Hourly Time Window", rep("Yes", times = 6)),
    c("FEs: Day of Week by Half-Hourly Time Window", rep("Yes", times = 6)),
    c(
      "FEs: Month of Year by Half-Hourly Time Window",
      rep(c("Yes", "No"), each = 3)
    ),
    c(
      "Number of Households",
      rep(list_number.of.households_by.tariff.and.heating.type[7:9], times = 2)
    )
  ),
  column.sep.width = column.sep.width,
  font.size = font.size,
  header = header,
  label = label <- paste0(
    "Table:Breakdown-of-Average-Treatment-Effects-in-the-Peak-Rate-Period_",
    "For-Tariff-C_By-Heating-Type_With-Different-Month-related-FEs"
  ),
  model.names = model.names
)
# # 2.2.1.4. For Tariff D
stargazer(
  c(
    reg.result_by.heating.type.and.tariff_fes.mw_d,
    reg.result_by.heating.type.and.tariff_fes.none_d
  ),
  type = "text",
  title = paste0(
    "Breakdown of Average Treatment Effects in the Peak Rate Period: ",
    "For Tariff D, By Heating Type, With different Month-related FEs"
  ),
  out = NULL,
  out.header = out.header,
  column.labels = column.labels,
  column.separate = column.separate,
  covariate.labels = covariate.labels_text,
  dep.var.caption = dep.var.caption,
  dep.var.labels = dep.var.labels,
  add.lines = add.lines <- list(
    c("Tariff", rep("D", each = 6)),
    c(
      "Electrical Heating for Space",
      rep(c("True", "False", "False"), times = 2)
    ),
    c(
      "Electrical Heating for Water",
      rep(c("True", "True", "False"), times = 2)
    ),
    c("FEs: Household by Half-Hourly Time Window", rep("Yes", times = 6)),
    c("FEs: Day of Week by Half-Hourly Time Window", rep("Yes", times = 6)),
    c(
      "FEs: Month of Year by Half-Hourly Time Window",
      rep(c("Yes", "No"), each = 3)
    ),
    c(
      "Number of Households",
      rep(
        list_number.of.households_by.tariff.and.heating.type[10:12],
        times = 2
      )
    )
  ),
  column.sep.width = column.sep.width,
  font.size = font.size,
  header = header,
  label = label <- paste0(
    "Table:Breakdown-of-Average-Treatment-Effects-in-the-Peak-Rate-Period_",
    "For-Tariff-D_By-Heating-Type_With-Different-Month-related-FEs"
  ),
  model.names = model.names
)


# ------------------------------------------------------------------------------
# Break by-heating-type ATEs down by using spline regression(s)
# ------------------------------------------------------------------------------
# # ------- Run regression(s), by heating type -------
# # 1. Preparing for running regression(s)
# # 1.1. Add data field(s)
knots <- c(10, 15)
for (knot in knots) {
  var.name_hdd.diff <- paste0("hdd.diff_", knot)
  var.name_indicator <- paste0("indicator_", knot)
  var.name_hdd.knot <- paste0("hdd.knot", knot)

  dt_for.reg[, (var.name_hdd.diff) := hdd_all_60f - knot]
  dt_for.reg[, (var.name_indicator) := hdd_all_60f > knot]
  dt_for.reg[
    ,
    (var.name_hdd.knot) :=
      get(var.name_hdd.diff) * as.numeric(get(var.name_indicator))
  ]
  dt_for.reg[
    ,
    (paste0("treatment_by_hdd.knot", knot)) :=
      as.numeric(is_treated_r) * get(var.name_hdd.knot)
  ]
  dt_for.reg[
    ,
    (paste0("treatment.period_by_hdd.knot", knot)) :=
      as.numeric(is_treatment.period) * get(var.name_hdd.knot)
  ]
  dt_for.reg[
    ,
    (paste0("treatment.and.post_by_hdd.knot", knot)) :=
      as.numeric(is_treatment.and.post) * get(var.name_hdd.knot)
  ]
}


# # 2. Run regression(s), and then export the results
# # 2.1. Run regression(s), create stargazer object(s), and export the
# #      regression results
for (knot in knots) {
  # ## Create object(s) that will be utilized later
  # ### Object(s) to which regression result will be stored
  obj.name <- paste0("list_reg.results_by.heating.type_fes.mw_knot", knot)
  # ### Modify formula(s) for regression(s)
  modified.formula <- get_change.terms.in.formula(
    model_breakdown.of.ate_hourly.in.peak_splines_iw.dw.mw,
    term_old_in.str = "hdd.knot", term_new_in.str = paste0("hdd.knot", knot)
  )
  # ## Run regression(s)
  assign(
    obj.name,
    lapply(
      list_cases,
      get_reg.result,
      formula = modified.formula
    )
  )
}
for (knot in knots) {
  # ## Create object(s) that will be utilized later
  # ### Object(s) to which regression result will be stored
  obj.name <- paste0("list_reg.results_by.heating.type_fes.none_knot", knot)
  # ### Modify formula(s) for regression(s)
  modified.formula <- get_change.terms.in.formula(
    model_breakdown.of.ate_hourly.in.peak_splines_iw.dw,
    term_old_in.str = "hdd.knot", term_new_in.str = paste0("hdd.knot", knot)
  )
  # ## Run regression(s)
  assign(
    obj.name,
    lapply(
      list_cases,
      get_reg.result,
      formula = modified.formula
    )
  )
}
covariate.labels_text <- c(
  "HDDs",
  "(HDDs - Knot) x 1[HDDs > Knot]",
  "1[Treatment] x HDDs",
  "1[Treatment] x (HDDs - Knot) x 1[HDDs > Knot]",
  "1[Post]",
  "1[Post] x HDDs",
  "1[Post] x (HDDs - Knot) x 1[HDDs > Knot]",
  "1[Treatment and Post]",
  "1[Treatment and Post] x HDDs",
  "1[Treatment and Post] x (HDDs - Knot) x 1[HDDs > Knot]"
)
covariate.labels_latex <- c(
  "HDDs",
  "(HDDs - Knot) $\\times$ $\\mathbb{1}$[HDDs $>$ Knot]",
  "$\\mathbb{1}$[Treatment] $\\times$ HDDs",
  "$\\mathbb{1}$[Treatment] $\\times$ (HDDs - Knot) $\\times$ $\\mathbb{1}$[HDDs $>$ Knot]",
  "$\\mathbb{1}$[Post]",
  "$\\mathbb{1}$[Post] $\\times$ HDDs",
  "$\\mathbb{1}$[Post] $\\times$ (HDDs - Knot) $\\times$ $\\mathbb{1}$[HDDs $>$ Knot]",
  "$\\mathbb{1}$[Treatment \\& Post]",
  "$\\mathbb{1}$[Treatment \\& Post] $\\times$ HDDs",
  "$\\mathbb{1}$[Treatment \\& Post] $\\times$ (HDDs - Knot) $\\times$ $\\mathbb{1}$[HDDs $>$ Knot]"
)
for (knot in knots) {
  obj.name_fes.mw <-
    paste0("list_reg.results_by.heating.type_fes.mw_knot", knot)
  obj.name_fes.none <-
    paste0("list_reg.results_by.heating.type_fes.none_knot", knot)
  stargazer(
    c(get(obj.name_fes.mw), get(obj.name_fes.none)),
    type = "text",
    title = paste0(
      "Breakdown of Average Treatment Effects in the Peak Rate Period, ",
      "By Heating Type, For Knot = ", knot
    ),
    out = NULL,
    out.header = out.header,
    column.labels = column.labels,
    column.separate = column.separate,
    covariate.labels = covariate.labels_text,
    dep.var.caption = dep.var.caption,
    dep.var.labels = dep.var.labels,
    add.lines = list(
      c(
        "Electrical Heating for Space",
        rep(c("True", "False", "False"), times = 2)
      ),
      c(
        "Electrical Heating for Water",
        rep(c("True", "True", "False"), times = 2)
      ),
      c("FEs: Household by Half-Hourly Time Window", rep("Yes", times = 6)),
      c("FEs: Day of Week by Half-Hourly Time Window", rep("Yes", times = 6)),
      c(
        "FEs: Month of Year by Half-Hourly Time Window",
        rep(c("Yes", "No"), each = 3)
      ),
      c(
        "Number of Households",
        as.vector(
          list_number.of.households_by.heating.type,
          mode = "numeric"
        ) %>%
          format(., big.mark = ",") %>%
          rep(., times = 2)
      )
    ),
    column.sep.width = column.sep.width,
    font.size = font.size,
    header = header,
    label = paste0(
      "Table:Breakdown-of-Average-Treatment-Effects-in-the-Peak-Rate-Period_",
      "By-Heating-Type_Knot-", knot
    ),
    model.names = model.names
  )
}

# # 2.2. Create ggplot object(s) that show treatment effects
# # 2.2.1. Create ggplot object(s)
list_plots_treatment.effects_knot10 <- mapply(
  get_ggplot.obj_treatment.effects,
  list_reg.results_by.heating.type_fes.none_knot10,
  list_cases,
  knot = 10,
  SIMPLIFY = FALSE
)
list_plots_treatment.effects_knot15 <- mapply(
  get_ggplot.obj_treatment.effects,
  list_reg.results_by.heating.type_fes.none_knot15,
  list_cases,
  knot = 15,
  SIMPLIFY = FALSE
)
# # 2.2.2. Export the ggplot object(s) in .PNG format
plot_treatment.effects <- grid.arrange(
  grobs = c(
    list_plots_treatment.effects_knot10,
    list_plots_treatment.effects_knot15
  ),
  nrow = 2
)
export_figure.in.png(
  plot_treatment.effects,
  filename_str = paste(
    PATH_TO.SAVE_FIGURE ,
    paste0(
      "Figure_Breakdown-of-Hourly-ATEs-in-the-Peak-Rate-Period_",
      "Treatment-Effect-by-Heating-Type_By-using-Spline-Regressions",
      "With-Different-Month-related-FEs.png"
    ),
    sep = "/"
  ),
  width_numeric = 45, height_numeric = 25
)

# # # 2.3. Create ggplot object(s) that show predicted load profile(s)
# # # 2.3.1. Create DT(s)
list_dt.for.plot_knot10 <- lapply(
  list_reg.results_by.heating.type_fes.none_knot10,
  get_dt.for.plotting,
  knot = 10
)
list_dt.for.plot_knot15 <- lapply(
  list_reg.results_by.heating.type_fes.none_knot15,
  get_dt.for.plotting,
  knot = 15
)
# # # 2.3.2. Create ggplot object(s)
list_plots_load.profile_knot10 <- mapply(
  get_ggplot.obj_load.profile,
  list_dt.for.plot_knot10,
  list_cases,
  knot_in.numeric = 10,
  SIMPLIFY = FALSE
)
list_plots_load.profile_knot15 <- mapply(
  get_ggplot.obj_load.profile,
  list_dt.for.plot_knot15,
  list_cases,
  knot_in.numeric = 15,
  SIMPLIFY = FALSE
)
# # # 2.3.3. Export the ggplot object(s) in .PNG format
plot_load.profiles <- grid.arrange(
  grobs = c(list_plots_load.profile_knot10, list_plots_load.profile_knot15),
  nrow = 2
)
export_figure.in.png(
  plot_load.profiles,
  filename_str = paste(
    PATH_TO.SAVE_FIGURE ,
    paste0(
      "Figure_Breakdown-of-Hourly-ATEs-in-the-Peak-Rate-Period_",
      "Load-Profile-by-Heating-Type_By-using-Spline-Regressions_",
      "With-Different-Month-related-FEs.png"
    ),
    sep = "/"
  ),
  width_numeric = 45, height_numeric = 25
)


# ------------------------------------------------------------------------------
# Estimate ATEs for Household using non-electric heating only
# ------------------------------------------------------------------------------
# # ------- Run regression(s), and create stargazer object(s) -------
# # 1. Create object(s) that will be used later
# # 1.1. Create subsetting condition(s)
condition_pre <- paste(
  "is_in.sample_incl.control_base.only.second.half == TRUE",
  "as.character(rate.period) == 'Peak'",
  "alloc_r_tariff %in% LETTERS[1:5]",
  "is_elec.heating_space_pre == FALSE",
  "is_elec.heating_water_pre == FALSE",
  sep = " & "
)
condition_stay <- paste(
  "is_in.sample_incl.control_base.only.second.half == TRUE",
  "as.character(rate.period) == 'Peak'",
  "alloc_r_tariff %in% LETTERS[1:5]",
  "is_elec.heating_space_pre == FALSE",
  "is_elec.heating_water_pre == FALSE",
  "is_elec.heating_space_post == FALSE",
  "is_elec.heating_water_post == FALSE",
  sep = " & "
)
condition_post <- paste(
  "is_in.sample_incl.control_base.only.second.half == TRUE",
  "as.character(rate.period) == 'Peak'",
  "alloc_r_tariff %in% LETTERS[1:5]",
  "is_elec.heating_space_post == FALSE",
  "is_elec.heating_water_post == FALSE",
  sep = " & "
)

# # 1.2. Modify econometric specification(s)
modified.formula_fes.mw <- get_change.terms.in.formula(
  model_breakdown.of.ate_hourly.in.peak_splines_iw.dw.m,
  term_old_in.str = "hdd.knot", term_new_in.str = paste0("hdd.knot", 15)
)
modified.formula_fes.none <- get_change.terms.in.formula(
  model_breakdown.of.ate_hourly.in.peak_splines_iw.dw,
  term_old_in.str = "hdd.knot", term_new_in.str = paste0("hdd.knot", 15)
)

# # 1.3. Run regression(s)
# # 1.3.1. Model(s) including Month-related FEs
reg.result_fes.mw_pre <- felm(
  formula = modified.formula_fes.mw,
  data = dt_for.reg[eval(parse(text = condition_pre))]
)
reg.result_fes.mw_stay <- felm(
  formula = modified.formula_fes.mw,
  data = dt_for.reg[eval(parse(text = condition_stay))]
)
reg.result_fes.mw_post <- felm(
  formula = modified.formula_fes.mw,
  data = dt_for.reg[eval(parse(text = condition_post))]
)
# # 1.3.2. Model(s) not including Month-related FEs
reg.result_fes.none_pre <- felm(
  formula = modified.formula_fes.none,
  data = dt_for.reg[eval(parse(text = condition_pre))]
)
reg.result_fes.none_stay <- felm(
  formula = modified.formula_fes.none,
  data = dt_for.reg[eval(parse(text = condition_stay))]
)
reg.result_fes.none_post <- felm(
  formula = modified.formula_fes.none,
  data = dt_for.reg[eval(parse(text = condition_post))]
)


# # 2. Create stargazer object(s)
# # 2.1. Create object(s) that will be used to make stargazer object(s)
n_households <- c(
  dt_for.reg[eval(parse(text = condition_pre)), .N, by = .(id)][, .N],
  dt_for.reg[eval(parse(text = condition_stay)), .N, by = .(id)][, .N],
  dt_for.reg[eval(parse(text = condition_post)), .N, by = .(id)][, .N]
)
covariate.labels_text <- c(
  "HDDs",
  "(HDDs - Knot) x 1[HDDs > Knot]",
  "1[Treatment] x HDDs",
  "1[Treatment] x (HDDs - Knot) x 1[HDDs > Knot]",
  "1[Post]",
  "1[Post] x HDDs",
  "1[Post] x (HDDs - Knot) x 1[HDDs > Knot]",
  "1[Treatment and Post]",
  "1[Treatment and Post] x HDDs",
  "1[Treatment and Post] x (HDDs - Knot) x 1[HDDs > Knot]"
)
covariate.labels_latex <- c(
  "HDDs",
  "(HDDs - Knot) $\\times$ $\\mathbb{1}$[HDDs $>$ Knot]",
  "$\\mathbb{1}$[Treatment] $\\times$ HDDs",
  "$\\mathbb{1}$[Treatment] $\\times$ (HDDs - Knot) $\\times$ $\\mathbb{1}$[HDDs $>$ Knot]",
  "$\\mathbb{1}$[Post]",
  "$\\mathbb{1}$[Post] $\\times$ HDDs",
  "$\\mathbb{1}$[Post] $\\times$ (HDDs - Knot) $\\times$ $\\mathbb{1}$[HDDs $>$ Knot]",
  "$\\mathbb{1}$[Treatment \\& Post]",
  "$\\mathbb{1}$[Treatment \\& Post] $\\times$ HDDs",
  "$\\mathbb{1}$[Treatment \\& Post] $\\times$ (HDDs - Knot) $\\times$ $\\mathbb{1}$[HDDs $>$ Knot]"
)

# # 2.2. Create stargazer object(s)
stargazer(
  list(
    reg.result_fes.mw_pre, reg.result_fes.mw_stay, reg.result_fes.mw_post,
    reg.result_fes.none_pre, reg.result_fes.none_stay, reg.result_fes.none_post
  ),
  type = "text",
  title = paste0(
    "Breakdown of Average Treatment Effects in the Peak Rate Period, ",
    "By Heating Type, For Knot = ", 15
  ),
  out = NULL,
  out.header = out.header,
  column.labels = column.labels,
  column.separate = column.separate,
  covariate.labels = covariate.labels_text,
  dep.var.caption = dep.var.caption,
  dep.var.labels = dep.var.labels,
  add.lines = list(
    c("Survey", rep(c("Pre", "Stay", "Post"), times = 2)),
    c("Electrical Heating for Space", rep("False", times = 6)),
    c("Electrical Heating for Water", rep("False", times = 6)),
    c("FEs: Household by Half-Hourly Time Window", rep("Yes", times = 6)),
    c("FEs: Day of Week by Half-Hourly Time Window", rep("Yes", times = 6)),
    c(
      "FEs: Month of Year by Half-Hourly Time Window",
      rep(c("Yes", "No"), each = 3)
    ),
    c(
      "Number of Households",
      rep(n_households, times = 2) %>% format(., big.mark = ",")
    )
  ),
  column.sep.width = column.sep.width,
  font.size = font.size,
  header = header,
  label = paste0(
    "Table:Breakdown-of-Average-Treatment-Effects-in-the-Peak-Rate-Period_",
    "By-Heating-Type_Knot-", 15
  ),
  model.names = model.names
)
