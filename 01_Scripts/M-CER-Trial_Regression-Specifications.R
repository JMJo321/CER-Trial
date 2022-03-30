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
