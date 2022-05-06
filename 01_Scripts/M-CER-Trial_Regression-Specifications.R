# < Description > *
# > Script Group Indicator Number and Name
# # : M, Regression-Specifications
# #
# > Script Number(s)
# # : M-CER-Trial
# #
# > Purpose of the script(s)
# # : Define regression models.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Set working directory, and run header script
# ------------------------------------------------------------------------------
# ------- Set project name -------
# (Not Applicable)


# ------- Set working directory -------
# (Not Applicable)


# ------- Run the header script -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Define path(s), parameter(s) and function(s)
# ------------------------------------------------------------------------------
# ------- Define path(s) -------
# (Not Applicable)


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Define Regression Specification(s) to estimate ATEs
# ------------------------------------------------------------------------------
# ------- Define econometric models: Half-hourly ATEs -------
# # 1. Define dependent and independent variables commonly used
# # 1.1. Dependent variable
dep.var_ate_half.hourly <- "kwh"

# # 1.2. Independent variables
# # 1.2.1. Covariate(s)
indep.vars_covariates_ate_half.hourly <- paste(
  paste0("is_treatment.and.post_30min_", seq(1, 48, by = 1)),
  collapse = " + "
)
# # 1.2.2. Fixed effect(s)
# (Defined in each econometric model.)
# # 1.2.3. Instrument variable(s)
indep.vars_ivs_ate_half.hourly <- "0"
# # 1.2.4. Clustered standard error(s)
indep.vars_clustered.ses_ate_half.hourly <- "id_in.factor + day_in.factor"


# # 2. Econometric models for half-hourly ATEs
model_ate_half.hourly_iw.dw.m <- get_formula_felm(
  dep.var = dep.var_ate_half.hourly,
  indep.vars_covariates = indep.vars_covariates_ate_half.hourly,
  indep.vars_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week.and.30min.interval_in.factor",
    "month_in.factor",
    sep = " + "
  ),
  indep.vars_ivs = indep.vars_ivs_ate_half.hourly,
  indep.vars_clustered.ses = indep.vars_clustered.ses_ate_half.hourly
)


# ------- Define econometric models: Hourly ATEs in the peak rate period -------
# # 1. Define dependent and independent variables commonly used
# # 1.1. Dependent variable
dep.var_ate_hourly.in.peak <- "kwh_per.hour"

# # 1.2. Independent variables
# # 1.2.1. Covariate(s)
indep.vars_covariates_ate_hourly.in.peak <- "is_treatment.and.post"
# # 1.2.2. Fixed effect(s)
# (Defined in each econometric model.)
# # 1.2.3. Instrument variable(s)
indep.vars_ivs_ate_hourly.in.peak <- "0"
# # 1.2.4. Clustered standard error(s)
indep.vars_clustered.ses_ate_hourly.in.peak <- "id_in.factor + day_in.factor"


# # 2. Econometric models for half-hourly ATEs
model_ate_hourly.in.peak_iw.dw.m <- get_formula_felm(
  dep.var = dep.var_ate_hourly.in.peak,
  indep.vars_covariates = indep.vars_covariates_ate_hourly.in.peak,
  indep.vars_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week.and.30min.interval_in.factor",
    "month_in.factor",
    sep = " + "
  ),
  indep.vars_ivs = indep.vars_ivs_ate_hourly.in.peak,
  indep.vars_clustered.ses = indep.vars_clustered.ses_ate_hourly.in.peak
)
model_ate_hourly.in.peak_iw.dw <- get_formula_felm(
  dep.var = dep.var_ate_hourly.in.peak,
  indep.vars_covariates = indep.vars_covariates_ate_hourly.in.peak,
  indep.vars_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week.and.30min.interval_in.factor",
    sep = " + "
  ),
  indep.vars_ivs = indep.vars_ivs_ate_hourly.in.peak,
  indep.vars_clustered.ses = indep.vars_clustered.ses_ate_hourly.in.peak
)


# ------------------------------------------------------------------------------
# Define Regression Specification(s) to breakdown hourly ATEs in the peak
# rate period
# ------------------------------------------------------------------------------
# ------- Define econometric models:                               -------
# ------- Breakdown of Hourly ATEs in the peak rate period, Linear -------
# # 1. Define dependent and independent variables commonly used
# # 1.1. Dependent variable
dep.var_breakdown.of.ate_hourly.in.peak <- "kwh_per.hour"

# # 1.2. Independent variables
# # 1.2.1. Covariate(s)
indep.vars_covariates_breakdown.of.ate_hourly.in.peak_dw.mw <- paste(
  "hdd_all_60f",
  "is_treated_r",
  "treatment_by_hdd",
  "is_treatment.period",
  "treatment.period_by_hdd",
  "is_treatment.and.post",
  "treatment.and.post_by_hdd",
  sep = " + "
)
# ## Note:
# ## The last three variables must be created before running regressions.
indep.vars_covariates_breakdown.of.ate_hourly.in.peak_iw.dw.mw <-
  paste(
    "hdd_all_60f",
    "is_treatment.and.post",
    "treatment.and.post_by_hdd",
    sep = " + "
  )
indep.vars_covariates_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version1 <-
  paste(
    "hdd_all_60f",
    "treatment_by_hdd",
    "is_treatment.period",
    "treatment.period_by_hdd",
    "is_treatment.and.post",
    "treatment.and.post_by_hdd",
    sep = " + "
  )
indep.vars_covariates_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version2 <-
  paste(
    "hdd_all_60f",
    "is_treatment.period",
    "treatment.period_by_hdd",
    "is_treatment.and.post",
    "treatment.and.post_by_hdd",
    sep = " + "
  )
indep.vars_covariates_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version3 <-
  paste(
    "hdd_all_60f",
    "treatment.period_by_hdd",
    "is_treatment.and.post",
    "treatment.and.post_by_hdd",
    sep = " + "
  )
indep.vars_covariates_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version4 <-
  paste(
    "hdd_all_60f",
    "treatment_by_hdd",
    "is_treatment.period",
    "is_treatment.and.post",
    "treatment.and.post_by_hdd",
    sep = " + "
  )
indep.vars_covariates_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version5 <-
  paste(
    "hdd_all_60f",
    "treatment_by_hdd",
    "treatment.period_by_hdd",
    "is_treatment.and.post",
    "treatment.and.post_by_hdd",
    sep = " + "
  )
indep.vars_covariates_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version6 <-
  paste(
    "hdd_all_60f",
    "treatment_by_hdd",
    "is_treatment.and.post",
    "treatment.and.post_by_hdd",
    sep = " + "
  )
indep.vars_covariates_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version7 <-
  paste(
    "hdd_all_60f",
    "is_treatment.period",
    "is_treatment.and.post",
    "treatment.and.post_by_hdd",
    sep = " + "
  )
indep.vars_covariates_breakdown.of.ate_hourly.in.peak_iw.dw.m <-
  paste(
    "hdd_all_60f",
    "treatment_by_hdd",
    "is_treatment.period",
    "treatment.period_by_hdd",
    "is_treatment.and.post",
    "treatment.and.post_by_hdd",
    sep = " + "
  )
indep.vars_covariates_breakdown.of.ate_hourly.in.peak_iw.dw <-
  paste(
    "hdd_all_60f",
    "treatment_by_hdd",
    "is_treatment.period",
    "treatment.period_by_hdd",
    "is_treatment.and.post",
    "treatment.and.post_by_hdd",
    sep = " + "
  )
# ## Note:
# ##
# # 1.2.2. Fixed effect(s)
# (Defined in each econometric model.)
# # 1.2.3. Instrument variable(s)
indep.vars_ivs_breakdown.of.ate_hourly.in.peak <- "0"
# # 1.2.4. Clustered standard error(s)
indep.vars_clustered.ses_breakdown.of.ate_hourly.in.peak <-
  "id_in.factor + day_in.factor"


# # 2. Econometric models to breakdown hourly ATEs in the peak rate period
# # 2.1. DID Model without ID FEs
model_breakdown.of.ate_hourly.in.peak_dw.mw <- get_formula_felm(
  dep.var =
    dep.var_breakdown.of.ate_hourly.in.peak,
  indep.vars_covariates =
    indep.vars_covariates_breakdown.of.ate_hourly.in.peak_dw.mw,
  indep.vars_fes = paste(
    "day.of.week.and.30min.interval_in.factor",
    "month.and.30min.interval_in.factor",
    sep = " + "
  ),
  indep.vars_ivs =
    indep.vars_ivs_breakdown.of.ate_hourly.in.peak,
  indep.vars_clustered.ses =
    indep.vars_clustered.ses_breakdown.of.ate_hourly.in.peak
)

# # 2.2. DID Model with ID FEs
model_breakdown.of.ate_hourly.in.peak_iw.dw.mw <-
  get_formula_felm(
    dep.var =
      dep.var_breakdown.of.ate_hourly.in.peak,
    indep.vars_covariates =
      indep.vars_covariates_breakdown.of.ate_hourly.in.peak_iw.dw.mw,
    indep.vars_fes = paste(
      "id.and.30min.interval_in.factor",
      "day.of.week.and.30min.interval_in.factor",
      "month.and.30min.interval_in.factor",
      sep = " + "
    ),
    indep.vars_ivs =
      indep.vars_ivs_breakdown.of.ate_hourly.in.peak,
    indep.vars_clustered.ses =
      indep.vars_clustered.ses_breakdown.of.ate_hourly.in.peak
  )
model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version1 <-
  get_formula_felm(
    dep.var =
      dep.var_breakdown.of.ate_hourly.in.peak,
    indep.vars_covariates =
      indep.vars_covariates_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version1,
    indep.vars_fes = paste(
      "id.and.30min.interval_in.factor",
      "day.of.week.and.30min.interval_in.factor",
      "month.and.30min.interval_in.factor",
      sep = " + "
    ),
    indep.vars_ivs =
      indep.vars_ivs_breakdown.of.ate_hourly.in.peak,
    indep.vars_clustered.ses =
      indep.vars_clustered.ses_breakdown.of.ate_hourly.in.peak
  )
model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version2 <-
  get_formula_felm(
    dep.var =
      dep.var_breakdown.of.ate_hourly.in.peak,
    indep.vars_covariates =
      indep.vars_covariates_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version2,
    indep.vars_fes = paste(
      "id.and.30min.interval_in.factor",
      "day.of.week.and.30min.interval_in.factor",
      "month.and.30min.interval_in.factor",
      sep = " + "
    ),
    indep.vars_ivs =
      indep.vars_ivs_breakdown.of.ate_hourly.in.peak,
    indep.vars_clustered.ses =
      indep.vars_clustered.ses_breakdown.of.ate_hourly.in.peak
  )
model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version3 <-
  get_formula_felm(
    dep.var =
      dep.var_breakdown.of.ate_hourly.in.peak,
    indep.vars_covariates =
      indep.vars_covariates_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version3,
    indep.vars_fes = paste(
      "id.and.30min.interval_in.factor",
      "day.of.week.and.30min.interval_in.factor",
      "month.and.30min.interval_in.factor",
      sep = " + "
    ),
    indep.vars_ivs =
      indep.vars_ivs_breakdown.of.ate_hourly.in.peak,
    indep.vars_clustered.ses =
      indep.vars_clustered.ses_breakdown.of.ate_hourly.in.peak
  )
model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version4 <-
  get_formula_felm(
    dep.var =
      dep.var_breakdown.of.ate_hourly.in.peak,
    indep.vars_covariates =
      indep.vars_covariates_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version4,
    indep.vars_fes = paste(
      "id.and.30min.interval_in.factor",
      "day.of.week.and.30min.interval_in.factor",
      "month.and.30min.interval_in.factor",
      sep = " + "
    ),
    indep.vars_ivs =
      indep.vars_ivs_breakdown.of.ate_hourly.in.peak,
    indep.vars_clustered.ses =
      indep.vars_clustered.ses_breakdown.of.ate_hourly.in.peak
  )
model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version5 <-
  get_formula_felm(
    dep.var =
      dep.var_breakdown.of.ate_hourly.in.peak,
    indep.vars_covariates =
      indep.vars_covariates_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version5,
    indep.vars_fes = paste(
      "id.and.30min.interval_in.factor",
      "day.of.week.and.30min.interval_in.factor",
      "month.and.30min.interval_in.factor",
      sep = " + "
    ),
    indep.vars_ivs =
      indep.vars_ivs_breakdown.of.ate_hourly.in.peak,
    indep.vars_clustered.ses =
      indep.vars_clustered.ses_breakdown.of.ate_hourly.in.peak
  )
model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version6 <-
  get_formula_felm(
    dep.var =
      dep.var_breakdown.of.ate_hourly.in.peak,
    indep.vars_covariates =
      indep.vars_covariates_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version6,
    indep.vars_fes = paste(
      "id.and.30min.interval_in.factor",
      "day.of.week.and.30min.interval_in.factor",
      "month.and.30min.interval_in.factor",
      sep = " + "
    ),
    indep.vars_ivs =
      indep.vars_ivs_breakdown.of.ate_hourly.in.peak,
    indep.vars_clustered.ses =
      indep.vars_clustered.ses_breakdown.of.ate_hourly.in.peak
  )
model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version7 <-
  get_formula_felm(
    dep.var =
      dep.var_breakdown.of.ate_hourly.in.peak,
    indep.vars_covariates =
      indep.vars_covariates_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version7,
    indep.vars_fes = paste(
      "id.and.30min.interval_in.factor",
      "day.of.week.and.30min.interval_in.factor",
      "month.and.30min.interval_in.factor",
      sep = " + "
    ),
    indep.vars_ivs =
      indep.vars_ivs_breakdown.of.ate_hourly.in.peak,
    indep.vars_clustered.ses =
      indep.vars_clustered.ses_breakdown.of.ate_hourly.in.peak
  )
model_breakdown.of.ate_hourly.in.peak_iw.dw.mw.s <-
  get_formula_felm(
    dep.var =
      dep.var_breakdown.of.ate_hourly.in.peak,
    indep.vars_covariates =
      indep.vars_covariates_breakdown.of.ate_hourly.in.peak_iw.dw.mw,
    indep.vars_fes = paste(
      "id.and.30min.interval_in.factor",
      "day.of.week.and.30min.interval_in.factor",
      "month.and.30min.interval_in.factor",
      "week.of.sample_in.factor",
      sep = " + "
    ),
    indep.vars_ivs =
      indep.vars_ivs_breakdown.of.ate_hourly.in.peak,
    indep.vars_clustered.ses =
      indep.vars_clustered.ses_breakdown.of.ate_hourly.in.peak
  )
model_breakdown.of.ate_hourly.in.peak_iw.dw.mw.sw <-
  get_formula_felm(
    dep.var =
      dep.var_breakdown.of.ate_hourly.in.peak,
    indep.vars_covariates =
      indep.vars_covariates_breakdown.of.ate_hourly.in.peak_iw.dw.mw,
    indep.vars_fes = paste(
      "id.and.30min.interval_in.factor",
      "day.of.week.and.30min.interval_in.factor",
      "month.and.30min.interval_in.factor",
      "week.of.sample.and.30min.interval_in.factor",
      sep = " + "
    ),
    indep.vars_ivs =
      indep.vars_ivs_breakdown.of.ate_hourly.in.peak,
    indep.vars_clustered.ses =
      indep.vars_clustered.ses_breakdown.of.ate_hourly.in.peak
  )

# # 2.3. DID Model with ID FEs, Different Month-related FEs
model_breakdown.of.ate_hourly.in.peak_iw.dw.m <- get_formula_felm(
  dep.var =
    dep.var_breakdown.of.ate_hourly.in.peak,
  indep.vars_covariates =
    indep.vars_covariates_breakdown.of.ate_hourly.in.peak_iw.dw.m,
  indep.vars_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week.and.30min.interval_in.factor",
    "month_in.factor",
    sep = " + "
  ),
  indep.vars_ivs =
    indep.vars_ivs_breakdown.of.ate_hourly.in.peak,
  indep.vars_clustered.ses =
    indep.vars_clustered.ses_breakdown.of.ate_hourly.in.peak
)
model_breakdown.of.ate_hourly.in.peak_iw.dw <- get_formula_felm(
  dep.var =
    dep.var_breakdown.of.ate_hourly.in.peak,
  indep.vars_covariates =
    indep.vars_covariates_breakdown.of.ate_hourly.in.peak_iw.dw,
  indep.vars_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week.and.30min.interval_in.factor",
    sep = " + "
  ),
  indep.vars_ivs =
    indep.vars_ivs_breakdown.of.ate_hourly.in.peak,
  indep.vars_clustered.ses =
    indep.vars_clustered.ses_breakdown.of.ate_hourly.in.peak
)


# ------- Define econometric models:                                  -------
# ------- Breakdown of Hourly ATEs in the peak rate period, Nonlinear -------
# # 1. Define dependent and independent variables commonly used
# # 1.1. Dependent variable
# (Same as the linear model)

# # 1.2. Independent variables
# # 1.2.1. Covariate(s)
indep.vars_covariates_breakdown.of.ate_hourly.in.peak_quadratic <- paste(
  "hdd_all_60f",
  "hdd2",
  # "is_treated_r",
  "treatment_by_hdd",
  "treatment_by_hdd2",
  "is_treatment.period",
  "treatment.period_by_hdd",
  "treatment.period_by_hdd2",
  "is_treatment.and.post",
  "treatment.and.post_by_hdd",
  "treatment.and.post_by_hdd2",
  sep = " + "
)
indep.vars_covariates_breakdown.of.ate_hourly.in.peak_cubic <- paste(
  "hdd_all_60f",
  "hdd2",
  "hdd3",
  # "is_treated_r",
  "treatment_by_hdd",
  "treatment_by_hdd2",
  "treatment_by_hdd3",
  "is_treatment.period",
  "treatment.period_by_hdd",
  "treatment.period_by_hdd2",
  "treatment.period_by_hdd3",
  "is_treatment.and.post",
  "treatment.and.post_by_hdd",
  "treatment.and.post_by_hdd2",
  "treatment.and.post_by_hdd3",
  sep = " + "
)
# ## Note:
# ## The last three variables must be created before running regressions.
# # 1.2.2. Fixed effect(s)
# (Defined in each econometric model.)
# # 1.2.3. Instrument variable(s)
# (Same as the linear model)
# # 1.2.4. Clustered standard error(s)
# (Same as the linear model)


# # 2. Econometric models to breakdown hourly ATEs in the peak rate period
# # 2.1. Quadratic Model(s)
model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_quadratic <- get_formula_felm(
  dep.var =
    dep.var_breakdown.of.ate_hourly.in.peak,
  indep.vars_covariates =
    indep.vars_covariates_breakdown.of.ate_hourly.in.peak_quadratic,
  indep.vars_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week.and.30min.interval_in.factor",
    "month.and.30min.interval_in.factor",
    sep = " + "
  ),
  indep.vars_ivs =
    indep.vars_ivs_breakdown.of.ate_hourly.in.peak,
  indep.vars_clustered.ses =
    indep.vars_clustered.ses_breakdown.of.ate_hourly.in.peak
)

# # 2.2. Cubic Model(s)
model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_cubic <- get_formula_felm(
  dep.var =
    dep.var_breakdown.of.ate_hourly.in.peak,
  indep.vars_covariates =
    indep.vars_covariates_breakdown.of.ate_hourly.in.peak_cubic,
  indep.vars_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week.and.30min.interval_in.factor",
    "month.and.30min.interval_in.factor",
    sep = " + "
  ),
  indep.vars_ivs =
    indep.vars_ivs_breakdown.of.ate_hourly.in.peak,
  indep.vars_clustered.ses =
    indep.vars_clustered.ses_breakdown.of.ate_hourly.in.peak
)


# ------- Define econometric models:                                  -------
# ------- Breakdown of Hourly ATEs in the peak rate period, Splinw    -------
# ## Note:
# ## Several objects are defined above.

# # 1. Models for spline regression(s)
# # 1.1. Model including month-of-year-by-30-minute-window FEs
model_breakdown.of.ate_hourly.in.peak_splines_iw.dw.mw <- get_formula_felm(
  dep.var =
    dep.var_breakdown.of.ate_hourly.in.peak,
  indep.vars_covariates = paste(
    "hdd_all_60f", "hdd.knot",
    "treatment_by_hdd", "treatment_by_hdd.knot",
    "is_treatment.period",
    "treatment.period_by_hdd", "treatment.period_by_hdd.knot",
    "is_treatment.and.post",
    "treatment.and.post_by_hdd", "treatment.and.post_by_hdd.knot",
    sep = " + "
  ),
  indep.vars_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week.and.30min.interval_in.factor",
    "month.and.30min.interval_in.factor",
    sep = " + "
  ),
  indep.vars_ivs =
    indep.vars_ivs_breakdown.of.ate_hourly.in.peak,
  indep.vars_clustered.ses =
    indep.vars_clustered.ses_breakdown.of.ate_hourly.in.peak
)

# # 1.2. Model including month-of-year FEs
model_breakdown.of.ate_hourly.in.peak_splines_iw.dw.m <- get_formula_felm(
  dep.var =
    dep.var_breakdown.of.ate_hourly.in.peak,
  indep.vars_covariates = paste(
    "hdd_all_60f", "hdd.knot",
    "treatment_by_hdd", "treatment_by_hdd.knot",
    "is_treatment.period",
    "treatment.period_by_hdd", "treatment.period_by_hdd.knot",
    "is_treatment.and.post",
    "treatment.and.post_by_hdd", "treatment.and.post_by_hdd.knot",
    sep = " + "
  ),
  indep.vars_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week.and.30min.interval_in.factor",
    "month_in.factor",
    sep = " + "
  ),
  indep.vars_ivs =
    indep.vars_ivs_breakdown.of.ate_hourly.in.peak,
  indep.vars_clustered.ses =
    indep.vars_clustered.ses_breakdown.of.ate_hourly.in.peak
)

# # 1.3. Model including no month-related FEs
model_breakdown.of.ate_hourly.in.peak_splines_iw.dw <- get_formula_felm(
  dep.var =
    dep.var_breakdown.of.ate_hourly.in.peak,
  indep.vars_covariates = paste(
    "hdd_all_60f", "hdd.knot",
    "treatment_by_hdd", "treatment_by_hdd.knot",
    "is_treatment.period",
    "treatment.period_by_hdd", "treatment.period_by_hdd.knot",
    "is_treatment.and.post",
    "treatment.and.post_by_hdd", "treatment.and.post_by_hdd.knot",
    sep = " + "
  ),
  indep.vars_fes = paste(
    "id.and.30min.interval_in.factor",
    "day.of.week.and.30min.interval_in.factor",
    sep = " + "
  ),
  indep.vars_ivs =
    indep.vars_ivs_breakdown.of.ate_hourly.in.peak,
  indep.vars_clustered.ses =
    indep.vars_clustered.ses_breakdown.of.ate_hourly.in.peak
)
