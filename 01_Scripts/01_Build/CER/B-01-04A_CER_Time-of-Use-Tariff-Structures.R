# < Description > *
# > Script Group Indicator Number and Name
# # : B-01, Descriptive Analysis
# #
# > Script Number(s)
# # : B-01-04A
# #
# > Purpose of the script(s)
# # : Time-of-Use Tariff Structures, excluding the Weekend Tariff.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(stringr)
library(zoo)
library(latex2exp)
library(ggplot2)
library(data.table)


# ------------------------------------------------------------------------------
# Set working directory, and run header script
# ------------------------------------------------------------------------------
# ------- Set project name -------
PROJ.NAME <- "CER-Trial"


# ------- Set working directory -------
PATH_PROJ <- paste("/Users/jmjo/Dropbox/00_JMJo/Projects", PROJ.NAME, sep = "/")
setwd(PATH_PROJ)


# ------- Run the header script -------
PATH_HEADER <- paste0("01_Scripts/H-", PROJ.NAME, ".R")
source(PATH_HEADER)


# --------------------------------------------------
# Define path(s), parameter(s) and function(s)
# --------------------------------------------------
# ------- Define path(s) -------
# # 1. Path(s) to which Outputs will be stored
# # 1.1.
FILE_TO.SAVE_CER_TOU <- "CER_Time-Of-Use-Tariffs.parquet"
PATH_TO.SAVE_CER_TOU <-
  paste(PATH_DATA_INTERMEDIATE_CER, FILE_TO.SAVE_CER_TOU, sep = "/")

# # 1.2. Path(s) to which Plot(s) will be stored
DIR_TO.SAVE_PLOT <- paste(
  PATH_OUTPUT_FIGURE,
  "TOU-Tariff-Structures",
  sep = "/"
)


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# # 1. Create a DT that includes a tariff structure
gen_tariff.structure <- function (
  alloc_r_tariff_, alloc_r_tariff_desc_,
  rate_cents.per.kwh_night,
  rate_cents.per.kwh_day,
  rate_cents.per.kwh_peak
) {
  # ## Create an empty DT
  dt <- data.table(
    interval_hour = seq(0, 24, by = 1),
    alloc_r_tariff = rep(alloc_r_tariff_, 25),
    alloc_r_tariff_desc = rep(alloc_r_tariff_desc_, 25)
  )
  # ## Add information
  dt[
    interval_hour %in% c(seq(0, 7, by = 1), seq(23, 24, by = 1)),
    `:=` (
      rate_desc = "Night",
      rate_cents.per.kwh = rate_cents.per.kwh_night
    )
  ]
  dt[
    interval_hour %in% c(seq(8, 16, by = 1), seq(19, 22, by = 1)),
    `:=` (
      rate_desc = "Day",
      rate_cents.per.kwh = rate_cents.per.kwh_day
    )
  ]
  dt[
    interval_hour %in% seq(17, 18, by = 1),
    `:=` (
      rate_desc = "Peak",
      rate_cents.per.kwh = rate_cents.per.kwh_peak
    )
  ]
  # ## Return the DT created
  return (dt)
}


# ------------------------------------------------------------------------------
# Create a DT that contain TOU Tariff Structures
# ------------------------------------------------------------------------------
# ------- Create a DT that contain TOU Tariff Structure -------
# # 1. Create a DT
dt_tou <- rbind(
  gen_tariff.structure(
    alloc_r_tariff_ = "E",
    alloc_r_tariff_desc_ = "Control",
    rate_cents.per.kwh_night = 14.1,
    rate_cents.per.kwh_day = 14.1,
    rate_cents.per.kwh_peak = 14.1
  ),
  gen_tariff.structure(
    alloc_r_tariff_ = "A",
    alloc_r_tariff_desc_ = "Tariff A",
    rate_cents.per.kwh_night = 12.0,
    rate_cents.per.kwh_day = 14.0,
    rate_cents.per.kwh_peak = 20.0
  ),
  gen_tariff.structure(
    alloc_r_tariff_ = "B",
    alloc_r_tariff_desc_ = "Tariff B",
    rate_cents.per.kwh_night = 11.0,
    rate_cents.per.kwh_day = 13.5,
    rate_cents.per.kwh_peak = 26.0
  ),
  gen_tariff.structure(
    alloc_r_tariff_ = "C",
    alloc_r_tariff_desc_ = "Tariff C",
    rate_cents.per.kwh_night = 10.0,
    rate_cents.per.kwh_day = 13.0,
    rate_cents.per.kwh_peak = 32.0
  ),
  gen_tariff.structure(
    alloc_r_tariff_ = "D",
    alloc_r_tariff_desc_ = "Tariff D",
    rate_cents.per.kwh_night = 9.0,
    rate_cents.per.kwh_day = 12.5,
    rate_cents.per.kwh_peak = 38.0
  )
)


# # 2. Modify the DT created above
# # 2.1. Add column(s)
# # 2.1.1. Add a column, in factor type, that shows tariff description
dt_tou[
  ,
  alloc_r_tariff_desc_in.factor := factor(
    alloc_r_tariff_desc,
    levels = c("Control", "Tariff A", "Tariff B", "Tariff C", "Tariff D")
  )
]


# # 3. Save the DT created above in Parquet Format
arrow::write_parquet(
  dt_tou,
  sink = PATH_TO.SAVE_CER_TOU,
  compression = "snappy",
  use_dictionary = TRUE
)


# ------------------------------------------------------------------------------
# Create and Save Plot(s)
# ------------------------------------------------------------------------------
# ------- Create Objects that will be used to make Plot(s) -------
# # 1. Create DTs that include annotation-related information
# # 1.1. For annotations with respect to rate periods
dt_annotate_rate.priod <- data.table(
  x = c(3.5, 12.5, 18, 21, 24),
  y = rep(41, times = 5),
  label = c("Night", "Day", "Peak", "Day", "Night")
)

# # 1.2. For annotations with respect to rates
# # 1.2.1. Create DTs that include rates
rates_night <- dt_tou[
  rate_desc == "Night", .N, by = .(rate_cents.per.kwh)
][
  , N := NULL
] %>%
  .$rate_cents.per.kwh
rates_day <- dt_tou[
  rate_desc == "Day", .N, by = .(rate_cents.per.kwh)
][
  , N := NULL
] %>%
  .$rate_cents.per.kwh
rates_peak <- dt_tou[
  rate_desc == "Peak", .N, by = .(rate_cents.per.kwh)
][
  , N := NULL
] %>%
  .$rate_cents.per.kwh
# # 1.2.2. Create DTs that contain annotation-related information
dt_annotate_rate_night <- data.table(
  x = rep(23.5, times = 5),
  y = rates_night + 0.3,
  label = rates_night
)
dt_annotate_rate_day <- data.table(
  x = rep(17.5, times = 4),
  y = rates_day[-1] - 0.1,
  label = rates_day[-1]
)
dt_annotate_rate_peak <- data.table(
  x = rep(18.5, times = 5),
  y = rates_peak + 0.4,
  label = rates_peak
)


# ------- Create ggplot Object(s) -------
# # 1. Plot for TOU Tariff Structures
plot_tou <-
  ggplot() +
    geom_vline(
      data = data.table(xintercept = c(8, 17, 19, 23)),
      aes(xintercept = xintercept),
      color = "grey", linetype = "dotdash"
    ) +
    geom_step(
      data = dt_tou,
      aes(
        x = interval_hour,
        y = rate_cents.per.kwh,
        color = alloc_r_tariff_desc_in.factor
      ),
      lwd = 0.7
    ) +
    annotate(
      geom = "text",
      x = dt_annotate_rate.priod$x, y = dt_annotate_rate.priod$y,
      label = dt_annotate_rate.priod$label,
      color = "black", alpha = 0.5, size = 3.5
    ) +
    annotate(
      geom = "text",
      x = dt_annotate_rate_night$x, y = dt_annotate_rate_night$y,
      label = dt_annotate_rate_night$label,
      color = "black", alpha = 0.5, size = 2.5
    ) +
    annotate(
      geom = "text",
      x = dt_annotate_rate_day$x, y = dt_annotate_rate_day$y,
      label = dt_annotate_rate_day$label,
      color = "black", alpha = 0.5, size = 2.5
    ) +
    annotate(
      geom = "text",
      x = dt_annotate_rate_peak$x, y = dt_annotate_rate_peak$y,
      label = dt_annotate_rate_peak$label,
      color = "black", alpha = 0.5, size = 2.5
    ) +
    scale_x_continuous(breaks = seq(0, 24, by = 1), minor_breaks = NULL) +
    scale_y_continuous(breaks = seq(5, 45, by = 5), labels = scales::comma) +
    scale_color_viridis_d() +
    labs(
      x = "\nHour of Day",
      y = "Rates  (Cents per kWh)\n",
      color = "Tariff Groups"
    ) +
    theme_linedraw()


# ------- Save plot(s) created above in PNG format -------
# # 1. Plot(s) for TOU Tariff Structures
export_figure.in.png(
  plot_tou,
  filename_str = paste(
    DIR_TO.SAVE_PLOT, "Figure_Time-of-Use-Tariff-Structures.png", sep = "/"
  ),
  width_numeric = 30, height_numeric = 17, units_str = "cm"
)
