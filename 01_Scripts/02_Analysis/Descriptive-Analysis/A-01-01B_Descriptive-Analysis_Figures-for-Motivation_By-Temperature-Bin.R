# < Description > *
# > Script Group Indicator Number and Name
# # : A-01, Descriptive Analysis
# #
# > Script Number(s)
# # : A-01-01B
# #
# > Purpose of the script(s)
# # : To make figure(s) for motivation, by using temperature bins.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(arrow)
library(ggplot2)
library(gridExtra)
library(latex2exp)
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
FILE_TO.LOAD_CER_FOR.REG <- "CER_DT-for-Regressions_Electricity.parquet"
PATH_TO.LOAD_CER_FOR.REG <-
  paste(PATH_DATA_INTERMEDIATE_CER, FILE_TO.LOAD_CER_FOR.REG, sep = "/")


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# # 1. To generate a ggplot object showing both household average daily
# #    consumption and differences in household average daily consumption
# # 1.1. Define help functions
# # 1.1.1. To create a DT containing household average daily consumption by
# #        temperature bin
help_compute.avg.kwh <- function (
  condition_str,
  lowest_numeric, highest_numeric, step_numeric
) {
  dt_avg.kwh <- dt_for.reg[
    ,
    range := cut(
      mean.temp_all_f,
      breaks = seq(lowest_numeric, highest_numeric, by = step_numeric),
      include.lowest = TRUE
    )
  ][
    parse(text = condition_str) %>% eval(.)
  ][
    ,
    lapply(.SD, sum, na.rm = TRUE), .SDcols = "kwh",
    by = .(id, group, period, range, date)
  ][
    ,
    lapply(.SD, mean, na.rm = TRUE), .SDcols = "kwh",
    by = .(id, group, period, range)
  ][
    ,
    lapply(.SD, mean, na.rm = TRUE), .SDcols = "kwh",
    by = .(group, period, range)
  ]
  dt_avg.kwh[
    ,
    range_mid.point := lapply(
      range,
      function (x) {
        str_extract(x, "[0-9].+[0-9]") %>%
          str_split(., ",", simplify = TRUE) %>%
          as.numeric(.) %>%
          mean(.)
      }
    ) %>%
      as.vector(., mode = "numeric")
  ]
  dt_avg.kwh[, category := "Average Daily Consumption"]

  return(dt_avg.kwh)
}
# # 1.1.2. To create a DT containing differences in household average daily
# #        consumption by temperature bin
help_compute.diff.in.kwh <- function (data.table_dt) {
  dt_control <- merge(
    x = data.table_dt[group == "Control" & period == "Baseline"],
    y = data.table_dt[group == "Control" & period == "Treatment"],
    by = c("group", "range", "range_mid.point"),
    suffixes = c("_baseline", "_treatment")
  )
  dt_treatment <- merge(
    x = data.table_dt[group == "Treatment" & period == "Baseline"],
    y = data.table_dt[group == "Treatment" & period == "Treatment"],
    by = c("group", "range", "range_mid.point"),
    suffixes = c("_baseline", "_treatment")
  )
  dt_merged <- rbind(dt_control, dt_treatment)
  dt_merged[, kwh := kwh_treatment - kwh_baseline]
  dt_to.return <- melt(
    dt_merged,
    id.vars = c("group", "range", "range_mid.point", "kwh"),
    measure.vars = c("period_baseline", "period_treatment"),
    value.name = "period"
  )
  dt_to.return[, category := "Difference in Average Daily Consumption"]

  return(
    dt_to.return[, .(group, period, range, kwh, range_mid.point, category)]
  )
}
# # 1.1.3. To generate a ggplot object from the DTs
help_create.ggplot.obj <- function (
  data.table.for.avg_dt,
  data.table.for.diff_dt
) {
  # ## Create objects that will be used to generate a ggplot object
  plot.options <- list(
    scale_y_continuous(labels = scales::comma),
    theme_linedraw(),
    theme(
      strip.text = element_text(face = "bold"),
      axis.title.y = element_text(margin = margin(r = 12)),
      legend.position = "bottom"
    )
  )
  col.pal_signal <- unikn::usecol(pal = pal_signal, n = 3)
  col.pal_custom <- unikn::usecol(c("firebrick", "gold", "steelblue"))
  label_group <- c("Control Group", "Treatment Group")
  names(label_group) <- str_replace(label_group, " Group", "")
  # ## Create a ggplot object incl. plot for household average daily consumption
  plot_avg.kwh <-
    ggplot(
      data = data.table.for.avg_dt[category == "Average Daily Consumption"]
    ) +
      geom_vline(xintercept = 60, linetype = "dotdash", alpha = 0.5) +
      geom_line(
        aes(x = range_mid.point, y = kwh, group = period),
        color = "black", alpha = 0.25, lwd = 0.8
      ) +
      geom_point(
        aes(x = range_mid.point, y = kwh, shape = period),
        color = "black", alpha = 0.25, size = 2.0
      ) +
      geom_line(
        aes(x = range_mid.point, y = kwh, color = period),
        lwd = 0.5
      ) +
      geom_point(
        aes(x = range_mid.point, y = kwh, color = period, shape = period),
        size = 1.7
      ) +
      facet_grid(. ~ group, labeller = labeller(group = label_group)) +
      scale_color_manual(
        values = col.pal_custom[c(1, 2)],
        labels = c("Baseline Period", "Treatment Period")
      ) +
      scale_shape_manual(
        values = c(16, 18),
        labels = c("Baseline Period", "Treatment Period")
      ) +
      labs(
        x = "",
        y = "Average Daily Consumption  (kWh/Day)",
        color = "",
        shape = "",
        subtitle = "Panel A: Household Average Daily Electricity Consumption"
      ) +
      plot.options +
      theme(
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()
      )
  # ## Create a ggplot object incl. plot for differences in household average
  # ## daily consumption
  plot_diff.in.kwh <-
    ggplot(
      data = data.table.for.diff_dt[
        category == "Difference in Average Daily Consumption" &
          period == "Baseline"
      ]
    ) +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
      geom_vline(xintercept = 60, linetype = "dotdash", alpha = 0.5) +
      geom_line(
        aes(x = range_mid.point, y = kwh),
        color = "black", alpha = 0.25, lwd = 0.8
      ) +
      geom_point(
        aes(x = range_mid.point, y = kwh),
        color = "black", alpha = 0.25, size = 1.7, shape = 15
      ) +
      geom_line(
        aes(x = range_mid.point, y = kwh),
        color = col.pal_signal[3], lwd = 0.5
      ) +
      geom_point(
        aes(x = range_mid.point, y = kwh),
        color = col.pal_signal[3], size = 1.4, shape = 15
      ) +
      facet_grid(. ~ group, labeller = labeller(group = label_group)) +
      labs(
        x = TeX(r'(Temperature $ (\degree F)$)'),
        y = "Differences in Household Average Daily Consumption  (kWh/Day)",
        color = "",
        shape = "",
        subtitle = "Panel B: Differences in Household Average Daily Consumption"
      ) +
      plot.options +
      theme(axis.title.x = element_text(margin = margin(t = 10)))
  # ## Combine the two ggplot objects
  plot <- grid.arrange(plot_avg.kwh, plot_diff.in.kwh)

  return(plot)
}

# # 1.2. Define functions by using the help functions
get_ggplot.obj <- function (
  condition_str, lowest_numeric, highest_numeric, step_numeric
) {
  dt_avg.kwh <- help_compute.avg.kwh(
    condition_str = condition_str,
    lowest_numeric = lowest_numeric,
    highest_numeric = highest_numeric,
    step_numeric = step_numeric
  )
  dt_diff.in.kwh <- help_compute.diff.in.kwh(data.table_dt = dt_avg.kwh)
  list_plots <- help_create.ggplot.obj(
    data.table.for.avg_dt = dt_avg.kwh,
    data.table.for.diff_dt = dt_diff.in.kwh
  )
  return(list_plots)
}


# ------------------------------------------------------------------------------
# Create DT(s) including ggplot object(s)
# ------------------------------------------------------------------------------
# ------- Load the DT for regression analysis -------
dt_for.reg <- read_parquet(PATH_TO.LOAD_CER_FOR.REG)


# ------- Conduct preliminary analysis -------
# # 1. Check household average daily consumption
dt_for.reg[  # To aggregate consumption at ID-by-Date level
  is_in.sample_incl.control_base == TRUE &
    period == "Baseline",
  lapply(.SD, sum, na.rm = TRUE), .SDcols = "kwh",
  by = .(id, date)
][  # To compute average daily consumption at ID level
  ,
  lapply(.SD, mean, na.rm = TRUE), .SDcols = "kwh",
  by = .(id)
][  # To compute household average daily consumption
  ,
  lapply(.SD, mean, na.rm = TRUE), .SDcols = "kwh"
]
# ## Note:
# ## This result indicates that household average daily consumption was about
# ## 24 kWh.


# # 2. Get info. about temperature distribution
dt_for.reg[, .N, by = .(mean.temp_all_f)]$mean.temp_all_f %>%
  summary(.)

temp.bin_lowest <-
  dt_for.reg[, .N, by = .(mean.temp_all_f)]$mean.temp_all_f %>%
    min(., na.rm = TRUE) %>%
    floor(.)

temp.bin_highest <-
  dt_for.reg[, .N, by = .(mean.temp_all_f)]$mean.temp_all_f %>%
    max(., na.rm = TRUE) %>%
    ceiling(.)


# ------- Create figures, and then export them -------
# # 1. Create ggplot objects
# # 1.0. Define conditions to subset `dt_for.reg`
conditions <- list(
  base =
    "is_in.sample_incl.control_base == TRUE",
  base.only.second.half =
    "is_in.sample_incl.control_base.only.second.half == TRUE",
  case1 =
    "is_in.sample_incl.control_case1 == TRUE",
  case1.only.second.half =
    "is_in.sample_incl.control_case1.only.second.half == TRUE"
)

# # 1.1. For step size 1
list_plots_step.size.1 <- lapply(
  conditions,
  get_ggplot.obj,
  lowest_numeric = temp.bin_lowest,
  highest_numeric = temp.bin_highest,
  step_numeric = 1
)

# # 1.2. For step size 2
list_plots_step.size.2 <- lapply(
  conditions,
  get_ggplot.obj,
  lowest_numeric = temp.bin_lowest,
  highest_numeric = temp.bin_highest,
  step_numeric = 2
)


# # 2. Export the ggplot objects in PNG format
# # 2.1. Make objects that will be used to export the ggplot objects
# # 2.1.1. Make a list incl. file names
list_filenames <- list(
  "Base",
  "Base-only-the-Second-Half",
  "Case-1",
  "Case-1-only-the-Second-Half"
)
# # 2.1.2. Make lists incl. paths to where figures will be saved
list_paths_step.size.1 <- lapply(
  list_filenames,
  function (filename_str) {
    paste(
      PATH_OUTPUT_FIGURE,
      paste0(
        "Figure_For-Motivation_Daily-Consumption_",
        paste(filename_str, paste0("Step-Size-", 1), sep = "_"),
        ".png"
      ),
      sep = "/"
    )
  }
)
list_paths_step.size.2 <- lapply(
  list_filenames,
  function (filename_str) {
    paste(
      PATH_OUTPUT_FIGURE,
      paste0(
        "Figure_For-Motivation_Daily-Consumption_",
        paste(filename_str, paste0("Step-Size-", 2), sep = "_"),
        ".png"
      ),
      sep = "/"
    )
  }
)
# # 2.1.3. Make a list containing plot options
list_args <- list(
  width_numeric = 45,
  height_numeric = 30,
  dpi_int = 700
)

# # 2.2. Export ggplot objects in PNG format
# # 2.2.1. For step size 1
mapply(
  export_figure.in.png,
  list_plots_step.size.1, list_paths_step.size.1,
  MoreArgs = list_args
)
# # 2.2.2. For step size 2
mapply(
  export_figure.in.png,
  list_plots_step.size.2, list_paths_step.size.2,
  MoreArgs = list_args
)
