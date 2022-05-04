# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Regression-Analysis
# #
# > Script Number(s)
# # : A-02-05A
# #
# > Purpose of the script(s)
# # : 1) Run regressions, by heating type, to breakdown hourly ATEs.
# #      In this script, two econometric specifications are used:
# #      linear and quadratic models.
# #   2) Save regression table(s) in .tex format.
# #   3) Make figure(s), which show predicted load profiles, by exploiting
# #      the estimates.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(arrow)
library(stargazer)
library(lfe)
library(ggplot2)
library(gridExtra)
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


# # 2. Path(s) to which regression results will be stored
# # 2.1. For regression table(s)
DIR_TO.SAVE_LATEX <- paste(
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
# # 1. Function(s) for running regression(s)
get_reg.results <- function (data_in.DT, formula, case_in.vector) {
  func.name <- paste0(
    "get_subsetting.condition.in.str_",
    "breakdown.of.ate_hourly.in.peak_by.heating.type"
  )
  subsetting.condition <- get(func.name)(case_in.vector)
  reg.results <- felm(
    data = data_in.DT[eval(parse(text = subsetting.condition))],
    formula = formula
  )
  return (reg.results)
}


# # 2. To create ggplot object(s) demonstrating predicted load profiles
# # 2.1. Create a DT from a felm object
get_dt.for.plotting <- function (felm.obj, model.type = "Linear") {
  dt_fes <- getfe(felm.obj) %>% setDT(.)

  fes_id <- dt_fes[fe %like% "^id"]$effect %>% mean(.)
  fes_day <- dt_fes[fe %like% "^day"]$effect %>% mean(.)
  fes_month <- dt_fes[fe %like% "^month"]$effect %>% mean(.)

  dt_plot <- data.table(hdd = seq(0, 30, by = 1))
  if (model.type == "Quadratic") {
    dt_plot[
      ,
      hourly.kwh_initial := (
        hdd * felm.obj$coefficients["hdd_all_60f",] +
          hdd^2 * felm.obj$coefficients["hdd2",] +
          hdd * felm.obj$coefficients["treatment_by_hdd",] +
          hdd^2 * felm.obj$coefficients["treatment_by_hdd2",] +
          felm.obj$coefficients["is_treatment.periodTRUE",] +
          hdd * felm.obj$coefficients["treatment.period_by_hdd",] +
          hdd^2 * felm.obj$coefficients["treatment.period_by_hdd2",] +
          fes_id + fes_day + fes_month
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
          hdd^2 * felm.obj$coefficients["treatment.and.post_by_hdd2",]
      )
    ]
  } else {
    dt_plot[
      ,
      hourly.kwh_initial := (
        hdd * felm.obj$coefficients["hdd_all_60f",] +
          hdd * felm.obj$coefficients["treatment_by_hdd",] +
          felm.obj$coefficients["is_treatment.periodTRUE",] +
          hdd * felm.obj$coefficients["treatment.period_by_hdd",] +
          fes_id + fes_day + fes_month
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
          hdd * felm.obj$coefficients["treatment.and.post_by_hdd",]
      )
    ]
  }

  tmp_dt_1 <- dt_plot[, .(hdd, hourly.kwh_initial, hourly.kwh_add.non.temp)]
  tmp_dt_2 <- dt_plot[, .(hdd, hourly.kwh_add.non.temp, hourly.kwh_final)]
  names_new <- c("hdd", "hourly.kwh_min", "hourly.kwh_max")
  names(tmp_dt_1) <- names_new
  names(tmp_dt_2) <- names_new
  dt_plot_melted <- rbind(
    tmp_dt_1[, category := "Non-Temperature"],
    tmp_dt_2[, category := "Temperature"]
  )

  return (dt_plot_melted)
}

# # 2.2. Generate a ggplot object
get_ggplot.obj <- function (data.table_dt, subtitle_str) {
  col.pal_custom <- unikn::usecol(c("firebrick", "gold", "steelblue"))

  ggplot.obj <-
    ggplot() +
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
      scale_x_continuous(breaks = seq(0, 30, by = 5)) +
      scale_y_continuous(labels = scales::comma_format(accuracy = 0.1)) +
      scale_fill_manual(values = col.pal_custom[1:2]) +
      labs(
        x = "Heating Degree Days  (HDDs)",
        y = "kWh/Hour",
        fill = "",
        subtitle = subtitle_str
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
# Run regression(s) to estimate hourly ATEs in the peak rate period
# ------------------------------------------------------------------------------
# ------- Load the DT for regression analysis -------
# # 1. Load required DT(s) and/or script(s) for regression analysis
# # 1.1. Load the DT for regression analysis
dt_for.reg <- read_parquet(PATH_TO.LOAD_CER_FOR.REG)
setDT(dt_for.reg)

# # 1.2. Load the R script including model specification(s)
source(PATH_TO.LOAD_CER_MODELS)


# # 2. Modify the DT loaded
# # 2.1. Add column(s)
# # 2.1.1. A column contenting hourly electricity consumption
dt_for.reg[
  ,
  kwh_per.hour := lapply(.SD, sum, na.rm = TRUE), .SDcols = "kwh",
  by = .(id, date, interval_hour)
]
# # 2.1.2. Add data fields that will be used when running regression(s)
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


# ------- Create object(s) required to run regression(s) -------
# # 1. Create a list including values for subsetting the DT
# # 1.1. Create a DT including values for subsetting the DT
dt_cases <-
  expand.grid(
    sample = "Base",
    range = "Only Second Half",
    rate.period = "Peak",
    heating.type = c("Space", "Water", "Both"),
    is_electric.heating = c(TRUE, FALSE),
    stringsAsFactors = FALSE
  ) %>%
    setDT(.)

# # 1.2. Convert the DT into a list
list_cases <- list()
for (row in 1:dt_cases[, .N]) {
  sample_ <- dt_cases[row]$sample
  range <- dt_cases[row]$range
  rate.period <- dt_cases[row]$rate.period
  heating.type <- dt_cases[row]$heating.type
  is_electric.heating <- dt_cases[row]$is_electric.heating

  names <- paste(
    sample_, range, rate.period, heating.type, is_electric.heating,
    sep = "_"
  )
  list_ <-
    list(c(sample_, range, rate.period, heating.type, is_electric.heating))
  names(list_) <- names
  list_cases <- c(list_cases, list_)
}


# # 2. Create a list including regression model(s)
list_models <- list(
  model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version1 =
    model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version1,
  model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_quadratic =
    model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_quadratic
)


# ------- Create object(s) required to make regression table(s) -------
# # 1. Create objects that will be utilized to generate stargazer object(s)
title_ <- paste0(
  "Breakdown of Average Treatment Effects in the Peak Rate Period, ",
  "By Heating Type"
)
out.header <- FALSE
column.labels <- c("Linear Model", "Quadratic Model")
column.separate <- c(6, 6)
covariate.labels_text <- c(
  "HDDs",
  "HDDs\\^2",
  "1[Treatment] x HDDs",
  "1[Treatment] x HDDs\\^2",
  "1[Post]",
  "1[Post] x HDDs",
  "1[Post] x HDDs\\^2",
  "1[Treatment and Post]",
  "1[Treatment and Post] x HDDs",
  "1[Treatment and Post] x HDDs\\^2"
)
covariate.labels_latex <- c(
  "HDDs",
  "HDDs$^{2}$",
  "$\\mathbb{1}$[Treatment] $\\times$ HDDs",
  "$\\mathbb{1}$[Treatment] $\\times$ HDDs$^{2}$",
  "$\\mathbb{1}$[Post]",
  "$\\mathbb{1}$[Post] $\\times$ HDDs",
  "$\\mathbb{1}$[Post] $\\times$ HDDs$^{2}$",
  "$\\mathbb{1}$[Treatment \\& Post]",
  "$\\mathbb{1}$[Treatment \\& Post] $\\times$ HDDs",
  "$\\mathbb{1}$[Treatment \\& Post] $\\times$ HDDs$^{2}$"
)
dep.var.caption <- "Dependent Variable"
dep.var.labels <- "Hourly Electricity Consumption  (kWh/Hour)"
add.lines <- list(
  c(
    "Heating Type",
    rep(rep(c("Electric", "Non-Electric"), each = 3), times = 2)
  ),
  c("Heating Purpose", rep(c("Space", "Water", "Space and Water"), times = 4)),
  c("FEs: ID by Half-Hourly Time Window", rep("Yes", times = 12)),
  c("FEs: Day of Week by Half-Hourly Time Window", rep("Yes", times = 12)),
  c("FEs: Month of Year by Half-Hourly Time Window", rep("Yes", times = 12))
)
column.sep.width <- "20pt"
font.size <- "small"
header <- FALSE
label <- paste0(
  "Table:Breakdown-of-Average-Treatment-Effects-in-the-Peak-Rate-Period_",
  "By-Heating-Type_Linear-and-Quadratic-Models"
)
model.names <- FALSE
omit.stat <- c("ser", "rsq")
omit.table.layout <- "n"
order <- c(
  "hdd_all_60f$",
  "hdd2$",
  "treatment_by_hdd$",
  "treatment_by_hdd2$",
  "is_treatment.period$",
  "treatment.period_by_hdd$",
  "treatment.period_by_hdd2$",
  "is_treatment.and.post$",
  "treatment.and.post_by_hdd$",
  "treatment.and.post_by_hdd2$"
)


# ------- Run regression(s), and then save results in .tex format -------
# # 1. Run regression(s)
n_models <- length(list_models)
for (idx in 1:n_models) {
  # ## Create temporary objects that will be used later
  model.name <- if (str_detect(names(list_models)[idx], "quadratic")) {
    "quadratic"
  } else {
    "linear"
  }
  obj.name_results <- paste0("reg.results_", model.name)
  obj.name_estimates <- paste0("estimates_", model.name)

  # ## Run regression(s)
  print("Be Running Regressions ...")
  assign(
    obj.name_results,
    lapply(
      list_cases,
      FUN = get_reg.results,
      data_in.DT = dt_for.reg,
      formula = list_models[[idx]]
    )
  )

  # ## Extract estimates
  assign(
    obj.name_estimates,
    lapply(
      get(obj.name_results),
      get_estimates_from.felm,
      level = 0.95,
      fe = FALSE,
      se.type = "cluster"
    )
  )

  # ## Show the current work progress
  print(
    paste0("Estimation is completed : Model ", idx, " out of ", n_models)
  )
}


# # 2. Export regression result(s)
# # 2.1. Regression table(s)
# # 2.1.1. Print regression table(s)
stargazer(
  reg.results_linear, reg.results_quadratic,
  type = "text",
  title = title_,
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
  model.names = model.names,
  omit.stat = NULL,
  omit.table.layout = omit.table.layout
)
# # 2.1.2. Export regression table(s) in .tex format
stargazer(
  reg.results_linear, reg.results_quadratic,
  type = "latex",
  title = title_,
  out = paste(
    DIR_TO.SAVE_LATEX,
    paste0(
      "Breakdown-of-ATEs_Hourly-in-the-Peak-Rate-Period_",
      "By-Heating-Type_Linear-and-Quadratic-Models.tex"
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

# # 2.2. Export the estimates in .RData format
obj.to.save_estimates <- ls()[str_detect(ls(), "^estimates_")]
save(
  list = obj.to.save_estimates,
  file = paste(
    DIR_TO.SAVE_REG.RESULTS,
    paste0(
      "CER_Estimates_Breakdown-of-ATEs_",
      "Hourly-in-the-Peak-Rate-Period_",
      "By-Heating-Type_Linear-and-Quadratic-Models.RData"
    ),
    sep = "/"
  )
)


# # 3. Export figure(s), showing predicted load profiles, in .PNG format
# # 3.1. Create object(s) that will be used later
subtitles <- c(
  paste(
    "Electric Heating", c("Space", "Water", "Space and Water"),
    sep = ", For "
  ),
  paste(
    "Non-Electric Heating", c("Space", "Water", "Space and Water"),
    sep = ", For "
  )
) %>%
  rep(., times = 2)

# # 3.2. Run regressions
list_reg.results_linear <-
  lapply(reg.results_linear, get_dt.for.plotting, "Linear")
list_reg.results_quadratic <-
  lapply(reg.results_quadratic, get_dt.for.plotting, "Quadratic")

# # 3.3. Create ggplot object(s)
ggplot.objs <- mapply(
  get_ggplot.obj,
  c(list_reg.results_linear, list_reg.results_quadratic),
  subtitle_str = subtitles,
  SIMPLIFY = FALSE
)
plot_by.heating.type.and.model <- grid.arrange(grobs = ggplot.objs, nrow = 2)

# # 3.4. Export the ggplot object(s) in .PNG format
export_figure.in.png(
  plot_by.heating.type.and.model,
  filename_str = paste(
    PATH_TO.SAVE_FIGURE,
    paste0(
      "Figure_Breakdown-of-Hourly-ATEs-in-the-Peak-Rate-Period_",
      "By-Heating-Type_Linear-and-Quadratic-Models.png"
    ),
    sep = "/"
  ),
  width_numeric = 70, height_numeric = 35
)
