# < Description > *
# > Script Group Indicator Number and Name
# # : A-01, Descriptive-Analysis
# #
# > Script Number(s)
# # : A-01-03A
# #
# > Purpose of the script(s)
# # : Make figure(s) showing estimates of non-temperature- and
# #   temperature-control treatment effects, respectively.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(latex2exp)
library(ggplot2)
library(arrow)
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
# # 1. Path(s) from which dataset(s) and/or script(s) are loaded
# # 1.1. For the DT for regression analysis
FILE_TO.LOAD_CER_FOR.REG <-
  "CER_DT-for-Regressions-with-Survey-Data_Electricity.parquet"
PATH_TO.LOAD_CER_FOR.REG <-
  paste(PATH_DATA_INTERMEDIATE_CER, FILE_TO.LOAD_CER_FOR.REG, sep = "/")

# # 1.2. For the estimates from regression(s)
DIR_TO.LOAD_CER_ESTIMATES <- "Breakdown-of-Hourly-ATEs/DID-Model-with-ID-FEs"
FILE_TO.LOAD_CER_ESTIMATES <-
  "CER_Estimates_Breakdown-of-ATEs_Hourly-in-the-Peak-Rate-Period.RData"
PATH_TO.LOAD_CER_ESTIMATES <- paste(
  PATH_DATA_ANALYSIS_REG.RESULTS,
  DIR_TO.LOAD_CER_ESTIMATES, FILE_TO.LOAD_CER_ESTIMATES, sep = "/"
)


# # 2. Path(s) to which output will be saved
# # 2.1. For figure(s)
DIR_TO.SAVE_FIGURE <- "Breakdown-of-Hourly-ATEs"
PATH_TO.SAVE_FIGURE <- paste(PATH_OUTPUT_FIGURE, DIR_TO.SAVE_FIGURE, sep = "/")


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Create figure(s)
# ------------------------------------------------------------------------------
# ------- Load dataset(s) required -------
# # 1. Load DT(s)
# # 1.1. DT for regression analysis
dt_for.reg <-
  read_parquet(PATH_TO.LOAD_CER_FOR.REG) %>%
    setDT(.)

# # 1.2. DT including estimates
load(PATH_TO.LOAD_CER_ESTIMATES)


# ------- Make DT(s) that will be used to create figure(s) -------
# # 1. DT for figure showing the distribution of HDDs
dt_for.plot_hdds <- dt_for.reg[
  month(date) >= 7,
  .N,
  by = .(date, period, hdd_all_60f)
][
  ,
  `:=` (
    N = NULL,
    date = NULL
  )
]


# # 2. DT for figure showing treatment effects across HDDs
# # 2.1. Create objects that will be used later
# # 2.1.1. Extract estimates
dt_estimates <- estimates_base_only.second.half_peak[[
  "model_breakdown.of.ate_hourly.in.peak_iw.dw.mw_version1"
]]
# # 2.1.2. Create objects incl. ATEs
estimate_non.temp <- dt_estimates[term == "is_treatment.and.postTRUE"]$estimate
estimate_temp <- dt_estimates[term == "treatment.and.post_by_hdd"]$estimate

# # 2.2. Create a DT
dt_for.plot_coefs <-
  data.table(hdd_all_60f = seq(0, dt_for.plot_hdds$hdd_all_60f %>%
    max(.) %>%
    ceiling(.), by = 2))
dt_for.plot_coefs[, treatment.effect_non.temp := estimate_non.temp]
dt_for.plot_coefs[, treatment.effect_temp := estimate_temp * hdd_all_60f]

# # 2.3. Melt the DT
dt_for.plot_coefs_melted <- melt(
  dt_for.plot_coefs,
  id.vars = "hdd_all_60f",
  measure.vars = c("treatment.effect_non.temp", "treatment.effect_temp")
)


# ------- Create figure(s) -------
# # 1. Create object(s) that will be used later
# # 1.1. Define plot option(s)
plot.options <- list(
  theme_linedraw(),
  theme(
    strip.text = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 12), hjust = 0.2),
    legend.text = element_text(margin = margin(r = 10, unit = "pt")),
    legend.position = "bottom"
  )
)

# # 1.2. Define color pallette
col.pal_signal <- unikn::usecol(pal = pal_signal, n = 3)
col.pal_unikn <- unikn::usecol(pal = pal_unikn)
col.pal_custom <- unikn::usecol(c("firebrick", "gold", "steelblue"))


# # 2. Create ggplot object(s)
plot_treatment.effects <-
  ggplot() +
    geom_histogram(
      data = dt_for.plot_hdds,
      aes(
        x = hdd_all_60f, y = ..density..,
        fill = period
      ),
      binwidth = 2, alpha = 0.6, position = "dodge"
    ) +
    geom_hline(yintercept = 0, linetype = "solid", lwd = 0.3) +
    geom_line(
      data = dt_for.plot_coefs_melted,
      aes(x = hdd_all_60f, y = value, group = variable),
      color = "black", alpha = 0.25, lwd = 0.8
    ) +
    geom_point(
      data = dt_for.plot_coefs_melted,
      aes(x = hdd_all_60f, y = value, shape = variable),
      color = "black", alpha = 0.25, size = 2.5
    ) +
    geom_line(
      data = dt_for.plot_coefs_melted,
      aes(x = hdd_all_60f, y = value, color = variable),
      lwd = 0.5
    ) +
    geom_point(
      data = dt_for.plot_coefs_melted,
      aes(x = hdd_all_60f, y = value, shape = variable, color = variable),
      size = 2.0
    ) +
    geom_vline(
      xintercept = round(estimate_non.temp / estimate_temp, 1),
      linetype = "dotdash", alpha = 0.4
    ) +
    scale_x_continuous(breaks = seq(0, 40, by = 5)) +
    scale_color_manual(
      values = col.pal_custom[1:2],
      labels = c("Non-Temperature Control", "Temperature Control")
    ) +
    scale_fill_manual(values = col.pal_unikn[c(3, 7)]) +
    scale_shape_manual(
      values = c(16, 18),
      labels = c("Non-Temperature Control", "Temperature Control")
    ) +
    labs(
      x = "Heating Degree Days",
      y = TeX(r'(Treatment Effects  $(\Delta kWh)$)'),
      color = "",
      fill = "",
      shape = ""
    ) +
    plot.options


# ------- Export figure(s) in .PNG format -------
export_figure.in.png(
  plot_treatment.effects,
  filename_str = paste(
    PATH_TO.SAVE_FIGURE,
    "Figure_Breakdown-of-Hourly-ATEs-in-the-Peak-Rate-Period.png",
    sep = "/"
  ),
  width_numeric = 23, height_numeric = 17
)
