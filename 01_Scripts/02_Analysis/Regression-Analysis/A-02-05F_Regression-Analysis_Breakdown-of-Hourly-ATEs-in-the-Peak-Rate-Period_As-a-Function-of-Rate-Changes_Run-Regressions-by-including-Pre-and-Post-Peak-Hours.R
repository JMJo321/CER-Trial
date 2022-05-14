# < Description > *
# > Script Group Indicator Number and Name
# # : A-02, Regression-Analysis
# #
# > Script Number(s)
# # : A-02-05F
# #
# > Purpose of the script(s)
# #   The key purpose of this script is to break the ATEs down in two parts,
# #   temperature- and non-temperature-control, as a function of rate changes.
# #   1) Run regressions
# #      - By including Pre- or Post-peak-hours, whose length are 2 hours
# #        : pre-peak-hours only, peak-hours only, post-peak-hours only,
# #          pre-peak-hours and peak-hours, peak-hours and post-peak-hours, and
# #          from pre-peak-hours to post-peak-hours
# #          (Note that the rate changes in those 6 cases are fixed with the
# #           rate changes in the peak rate period.)
# #      - With a econometic specification
# #        : Model with ID-related FEs
# #      - For three different knots
# #        : 10, 12, and 14
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
DIR_TO.SAVE_FIGURE <- "Dynamic-Behavior"
PATH_TO.SAVE_FIGURE <- paste(PATH_OUTPUT_FIGURE, DIR_TO.SAVE_FIGURE, sep = "/")


# ------- Define parameter(s) -------
# # 1. Parameters exploited non only to run regressions but also to create plots
# # 1.1. Intervals of hours
list_intervals <- list(
  `15 to 16` = c(15:16),
  `17 to 18` = c(17:18),
  `19 to 20` = c(19:20),
  `15 to 18` = c(15:18),
  `17 to 20` = c(17:20),
  `15 to 20` = c(15:20)
)

# # 1.2. Rate changes
RATE.CHANGES <- seq(6, 24, by = 6)


# ------- Define function(s) -------
# # 1. Function(s) for running regressions
help_run.reg <- function (formula, interval.hours_in.array) {
  reg.result <- felm(
    formula = formula,
    data = dt_for.reg[
      is_in.sample_incl.control_base.only.second.half == TRUE &
        interval_hour %in% interval.hours_in.array
    ]
  )
  return (reg.result)
}


# # 2. Function(s) for extracting info. from felm object(s)
# # 2.1. Function(s) for computing ATEs
# # 2.1.1. To computing ATEs for a given knot and a givne rate change
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
# # 2.1.2. To computing ATEs for a given interval
help_get.dt.incl.ates.for.given.interval <- function (
  felm.object, knot_in.numeric
) {
  dt_ates <- lapply(
    seq(6, 24, by = 6),
    get_dt_estimated.ates,
    felm.obj = felm.object,
    knot_in.numeric = knot_in.numeric
  ) %>%
    rbindlist(.)
  dt_ates[
    ,
    rate.change := factor(rate.change, levels = RATE.CHANGES)
  ]
  return (dt_ates)
}
# # 2.1.2. To computing ATEs
get.dt.incl.ates <- function (list.of.felm.objects_in.list, knot_in.numeric) {
  dt_ates <- mapply(
    help_get.dt.incl.ates.for.given.interval,
    list.of.felm.objects_in.list,
    MoreArgs = list(
      knot_in.numeric = knot_in.numeric
    ),
    SIMPLIFY = FALSE
  ) %>%
    rbindlist(., idcol = "interval")
  dt_ates[, interval := factor(interval, levels = names(list_intervals))]
  return (dt_ates)
}

# # 2.2. Function(s) for computing load profiles
# # 2.2.1. To computing load profile(s) for a given knot and a givne rate change
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
# # 2.2.2. To computing load profile(s) for a given interval
help_get.dt.incl.load.profile.for.given.interval <- function (
  felm.object, knot_in.numeric
) {
  dt_load.profiles <- lapply(
    RATE.CHANGES,
    get_dt_load.profiles,
    felm.obj = felm.object,
    knot_in.numeric = knot_in.numeric
  ) %>%
    rbindlist(.)
  dt_load.profiles[
    ,
    rate.change := factor(
      rate.change,
      levels = as.character(RATE.CHANGES),
      labels = paste0("Rate Change  =  ", RATE.CHANGES, " Cents per kWh")
    )
  ]
  return (dt_load.profiles)
}
# # 2.2.3. To computing load profile(s)
get_dt.incl.load.profiles <- function (
  list.of.felm.objects_in.list, knot_in.numeric
) {
  dt_load.profiles <- mapply(
    help_get.dt.incl.load.profile.for.given.interval,
    list.of.felm.objects_in.list,
    MoreArgs = list(
      knot_in.numeric = knot_in.numeric
    ),
    SIMPLIFY = FALSE
  ) %>%
    rbindlist(., idcol = "interval")
  dt_load.profiles[
    ,
    interval := factor(interval, levels = names(list_intervals))
  ]
  return(dt_load.profiles)
}


# # 3. Function(s) for making plot(s)
# # 3.1. To make ggplot object(s) showing ATEs
get_ggplot.obj_treatment.effects <- function (dt) {
  dt_knots <- dt[
    , .N, by = .(knot)
  ][
    ,
    knot_in.numeric := str_extract(knot, "[0-9].+?$") %>%
      as.numeric(.)
  ]
  ggplot.obj <-
    ggplot() +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.4) +
      geom_vline(
        data = dt_knots,
        aes(xintercept = knot_in.numeric),
        linetype = "dotdash", alpha = 0.4
      ) +
      geom_line(
        data = dt[category == "Non-Temperature"],
        aes(x = hdd, y = ates, group = rate.change),
        color = "black", alpha = 0.25, lwd = 0.8
      ) +
      geom_line(
        data = dt[category == "Non-Temperature"],
        aes(x = hdd, y = ates, color = rate.change),
        lwd = 0.5
      ) +
      geom_point(
        data = dt[category == "Non-Temperature"],
        aes(x = hdd, y = ates, shape = category),
        color = "black", alpha = 0.25, size = 2.5
      ) +
      geom_point(
        data = dt[category == "Non-Temperature"],
        aes(x = hdd, y = ates, shape = category, color = rate.change),
        size = 2.0
      ) +
      geom_line(
        data = dt[category == "Temperature"],
        aes(x = hdd, y = ates, group = rate.change),
        color = "black", alpha = 0.25, lwd = 0.8
      ) +
      geom_line(
        data = dt[category == "Temperature"],
        aes(x = hdd, y = ates, color = rate.change),
        lwd = 0.5
      ) +
      geom_point(
        data = dt[category == "Temperature"],
        aes(x = hdd, y = ates, shape = category),
        color = "black", alpha = 0.25, size = 2.5
      ) +
      geom_point(
        data = dt[category == "Temperature"],
        aes(x = hdd, y = ates, shape = category, color = rate.change),
        size = 2.0
      ) +
      facet_grid(interval ~ knot, scales = "free_y") +
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

# # 3.2. To make ggplot object(s) showing the load profile(s)
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
      facet_grid(interval ~ rate.change, scales = "free_y") +
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
gc(reset = TRUE)


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
names(KNOTS) <- KNOTS
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
for (KNOT in KNOTS) {
  tmp_list_felm.obj <- paste0("list_reg.results_knot", KNOT)
  assign(
    tmp_list_felm.obj,
    lapply(
      list_intervals,
      help_run.reg,
      formula = get_change.terms.in.formula(
        model_breakdown.of.ate_hourly.in.peak_rate.change.in.spline_dw,
        term_old_in.str = "hdd.knot", term_new_in.str = paste0("hdd.knot", KNOT)
      )
    )
  )
}


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
  "(HDDs - Knot) x 1[HDDs > Knot]",
  "1[Treatment]",
  "1[Treatment] x R.C.",
  "1[Treatment] x HDDs",
  "1[Treatment] x (HDDs - Knot) x 1[HDDs > Knot]",
  "1[Treatment] x HDDs x R.C.",
  "1[Treatment] x (HDDs - Knot) x 1[HDDs > Knot] x R.C.",
  "1[Post]",
  "1[Post] x HDDs",
  "1[Post] x (HDDs - Knot) x 1[HDDs > Knot]",
  "1[Treatment and Post]",
  "1[Treatment and Post] x R.C.",
  "1[Treatment and Post] x HDDs",
  "1[Treatment and Post] x (HDDs - Knot) x 1[HDDs > Knot]",
  "1[Treatment and Post] x HDDs x R.C.",
  "1[Treatment and Post] x (HDDs - Knot) x 1[HDDs > Knot] x R.C."
)
covariate.labels_latex <- c(
  "HDDs",
  "(HDDs - Knot) $\\times$ $\\mathbb{1}$[HDDs $>$ Knot]",
  "$\\mathbb{1}$[Treatment]",
  "$\\mathbb{1}$[Treatment] $\\times$ $\\Delta$Price",
  "$\\mathbb{1}$[Treatment] $\\times$ HDDs",
  "$\\mathbb{1}$[Treatment] $\\times$ (HDDs - Knot) $\\times$ $\\mathbb{1}$[HDDs $>$ Knot]",
  "$\\mathbb{1}$[Treatment] $\\times$ HDDs $\\times$ $\\Delta$Price",
  "$\\mathbb{1}$[Treatment] $\\times$ (HDDs - Knot) $\\times$ $\\mathbb{1}$[HDDs $>$ Knot] $\\times$ $\\Delta$Price",
  "$\\mathbb{1}$[Post]",
  "$\\mathbb{1}$[Post] $\\times$ HDDs",
  "$\\mathbb{1}$[Post] $\\times$ (HDDs - Knot) $\\times$ $\\mathbb{1}$[HDDs $>$ Knot]",
  "$\\mathbb{1}$[Treatment \\& Post]",
  "$\\mathbb{1}$[Treatment \\& Post] $\\times$ $\\Delta$Price",
  "$\\mathbb{1}$[Treatment \\& Post] $\\times$ HDDs",
  "$\\mathbb{1}$[Treatment \\& Post] $\\times$ (HDDs - Knot) $\\times$ $\\mathbb{1}$[HDDs $>$ Knot]",
  "$\\mathbb{1}$[Treatment \\& Post] $\\times$ HDDs $\\times$ $\\Delta$Price",
  "$\\mathbb{1}$[Treatment \\& Post] $\\times$ (HDDs - Knot) $\\times$ $\\mathbb{1}$[HDDs $>$ Knot] $\\times$ $\\Delta$Price"
)
dep.var.caption <- "Dependent Variable"
dep.var.labels <- "Hourly Electricity Consumption  (kWh/Hour)"
add.lines <- NULL
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
for (KNOT in KNOTS) {
  # ## Print regression table(s)
  stargazer(
    get(paste0("list_reg.results_knot", KNOT)),
    type = "text",
    title = title_,
    out = NULL,
    out.header = out.header,
    column.labels = column.labels,
    column.separate = column.separate,
    covariate.labels = covariate.labels_text,
    dep.var.caption = dep.var.caption,
    dep.var.labels = dep.var.labels,
    add.lines = list(
      c("Interval of Hours", names(list_intervals)),
      c("Knot", rep(KNOT, times = length(list_intervals))),
      c(
        "FEs: Household by Half-Hourly Time Window",
        rep("No", times = length(list_intervals))
      ),
      c(
        "FEs: Day of Week by Half-Hourly Time Window",
        rep("Yes", times = length(list_intervals))
      )
    ),
    column.sep.width = column.sep.width,
    font.size = font.size,
    header = header,
    label = label,
    model.names = model.names
  )

  # ## In .tex format
  stargazer(
    get(paste0("list_reg.results_knot", KNOT)),
    type = "latex",
    title = title_,
    out = paste(
      DIR_TO.SAVE_LATEX_BREAKDOWN,
      paste0(
        "Breakdown-of-ATEs_Hourly-in-the-Peak-Rate-Period_",
        "As-a-Function-of-Rate-Changes_For-Different-Intervals_Knot-", KNOT,
        ".tex"
      ),
      sep = "/"
    ),
    out.header = out.header,
    column.labels = column.labels,
    column.separate = column.separate,
    covariate.labels = covariate.labels_latex,
    dep.var.caption = dep.var.caption,
    dep.var.labels = dep.var.labels,
    add.lines = list(
      c("Interval of Hours", names(list_intervals)),
      c("Knot", rep(KNOT, times = length(list_intervals))),
      c(
        "FEs: Household by Half-Hourly Time Window",
        rep("No", times = length(list_intervals))
      ),
      c(
        "FEs: Day of Week by Half-Hourly Time Window",
        rep("Yes", times = length(list_intervals))
      )
    ),
    column.sep.width = column.sep.width,
    font.size = font.size,
    header = header,
    label = label,
    model.names = model.names,
    omit.stat = omit.stat,
    omit.table.layout = omit.table.layout
  )
}


# ------------------------------------------------------------------------------
# Make plots by using the regression result(s) above
# ------------------------------------------------------------------------------
# ------- Make plots by using the regression result(s) above -------
# # 1. Create object(s) used later
# # 1.1. Create a list including lists that containing felm objects
list_reg.results <- list(
  list_reg.results_knot10,
  list_reg.results_knot12,
  list_reg.results_knot14
)
names(list_reg.results) <- paste0("Knot ", KNOTS)

# # 1.2. Create a DT including ATEs
dt_ates <- mapply(
  get.dt.incl.ates,
  list_reg.results,
  KNOTS,
  SIMPLIFY = FALSE
) %>%
  rbindlist(., idcol = "knot")
# ## Note:
# ## A DT including load profile(s) will be generated just before creating
# ## ggplot object(s).


# # 2. Create ggplot object(s)
# # 2.1. For ATEs
plot_ates <- get_ggplot.obj_treatment.effects(dt_ates)

# # 2.2. For load profile(s)
for (KNOT in KNOTS) {
  obj.name_reg.results <- paste0("list_reg.results_knot", KNOT)
  obj.name_dt <- paste0("dt_load.profiles_knot", KNOT)
  obj.name_plot <- paste0("plot_load.profiles_knot", KNOT)
  assign(
    obj.name_dt,
    get_dt.incl.load.profiles(get(obj.name_reg.results), KNOT)
  )
  assign(
    obj.name_plot,
    get_ggplot.obj_load.profile(get(obj.name_dt), KNOT)
  )
}


# ------- Export plot(s) created above in .PNG format -------
# # 1. Plot(s) showing ATEs
export_figure.in.png(
  plot_ates,
  filename_str = paste(
    PATH_TO.SAVE_FIGURE,
    paste0(
      "Figure_Breakdown-of-Hourly-ATEs-in-the-Peak-Rate-Period_",
      "As-a-Function-of-Rate-Changes_ATEs_For-Different-Knots-and-Intervals.png"
    ),
    sep = "/"
  ),
  width_numeric = 55, height_numeric = 33
)


# # 2. Plot(s) showing load profile(s)
for (KNOT in KNOTS) {
  obj.name_to.export <- paste0("plot_load.profiles_knot", KNOT)
  export_figure.in.png(
    get(obj.name_to.export),
    filename_str = paste(
      PATH_TO.SAVE_FIGURE,
      paste0(
        "Figure_Breakdown-of-Hourly-ATEs-in-the-Peak-Rate-Period_",
        "As-a-Function-of-Rate-Changes_Load-Profiles_For-Different-Intervals_",
        "Knot-", KNOT, ".png"
      ),
      sep = "/"
    ),
    width_numeric = 55, height_numeric = 30
  )
}
