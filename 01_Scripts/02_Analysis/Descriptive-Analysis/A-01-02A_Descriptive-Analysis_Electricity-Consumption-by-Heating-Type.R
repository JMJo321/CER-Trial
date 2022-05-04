# < Description > *
# > Script Group Indicator Number and Name
# # : A-01, Descriptive Analysis
# #
# > Script Number(s)
# # : A-01-02A
# #
# > Purpose of the script(s)
# # : To make figure(s) showing how daily average consumption varies with
# #   temperatures. In addition, make a table demonstrating the distribution of
# #   how households allocated in terms of space and water heating types.

# ## Note:
# ## The pre-trial survey was conducted in April/May 2010.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(arrow)
library(ggplot2)
library(gridExtra)
library(latex2exp)
library(huxtable)
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


# # 2. Path(s) to which dataset(s) and script(s) are saved
# # 2.1. For figure(s)
PATH_OUTPUT_FIGURE_HEATING.TYPE <-
  paste(PATH_OUTPUT_FIGURE, "Average-kWh-by-Heating-Type", sep = "/")


# ------- Define parameter(s) -------
# (Not Applicable)


# ------- Define function(s) -------
# # 1. To generate a ggplot object showing both household average daily
# #    consumption and differences in household average daily consumption
# # 1.1. Define help functions
# # 1.1.1. To create a DT containing household average daily consumption by
# #        temperature bin
help_compute.avg.kwh <- function (
  condition_str, var.names_str, var.name.for.facetting_str,
  lowest_numeric, highest_numeric, step_numeric
) {
  cols_daily.kwh <-
    c("id", "group", "period", "range", "date", var.names_str)
  cols_daily.average.kwh <-
    c("id", "group", "period", "range", var.names_str)
  cols_household.daily.average.kwh <-
    c("group", "period", "range", var.names_str)

  dt_avg.kwh <- dt_for.reg[
    ,
    range := cut(
      mean.temp_all_f,
      breaks = seq(lowest_numeric, highest_numeric, by = step_numeric),
      include.lowest = TRUE
    )
  ][
    alloc_r_tariff %in% LETTERS[1:5]
  ][
    parse(text = condition_str) %>% eval(.)
  ][
    ,
    lapply(.SD, sum, na.rm = TRUE), .SDcols = "kwh",
    by = cols_daily.kwh
  ][
    ,
    lapply(.SD, mean, na.rm = TRUE), .SDcols = "kwh",
    by = cols_daily.average.kwh
  ][
    ,
    lapply(.SD, mean, na.rm = TRUE), .SDcols = "kwh",
    by = cols_household.daily.average.kwh
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

  dt_avg.kwh[
    get(var.names_str[1]) == "Yes" & get(var.names_str[2]) == "Yes",
    (var.name.for.facetting_str) := "Electric"
  ]
  dt_avg.kwh[
    get(var.names_str[1]) == "Yes" & get(var.names_str[2]) == "No",
    (var.name.for.facetting_str) := "Electric to Non-Electric"
  ]
  dt_avg.kwh[
    get(var.names_str[1]) == "No" & get(var.names_str[2]) == "Yes",
    (var.name.for.facetting_str) := "Non-Electric to Electric"
  ]
  dt_avg.kwh[
    get(var.names_str[1]) == "No" & get(var.names_str[2]) == "No",
    (var.name.for.facetting_str) := "Non-Electric"
  ]

  return(dt_avg.kwh)
}
# # 1.1.2. To create a DT containing differences in household average daily
# #        consumption by temperature bin
help_compute.diff.in.kwh <- function (
  data.table_dt, var.names_str, var.name.for.facetting_str
) {
  dt_control <- merge(
    x = data.table_dt[group == "Control" & period == "Baseline"],
    y = data.table_dt[group == "Control" & period == "Treatment"],
    by = c(
      "group", "range", "range_mid.point",
      var.names_str, var.name.for.facetting_str
    ),
    suffixes = c("_baseline", "_treatment")
  )
  dt_treatment <- merge(
    x = data.table_dt[group == "Treatment" & period == "Baseline"],
    y = data.table_dt[group == "Treatment" & period == "Treatment"],
    by = c(
      "group", "range", "range_mid.point",
      var.names_str, var.name.for.facetting_str
    ),
    suffixes = c("_baseline", "_treatment")
  )
  dt_merged <- rbind(dt_control, dt_treatment)
  dt_merged[
    ,
    `:=` (
      kwh = kwh_treatment - kwh_baseline,
      percentage.change = (kwh_treatment / kwh_baseline) - 1
    )
  ]
  dt_to.return <- melt(
    dt_merged,
    id.vars =
      c(
        "group", "range", "range_mid.point", "kwh", "percentage.change",
        var.names_str, var.name.for.facetting_str
      ),
    measure.vars = c("period_baseline", "period_treatment"),
    value.name = "period"
  )
  dt_to.return[, category := "Percentage Changes in Average Daily Consumption"]

  cols_to.extract <- c(
    "group", "period", "range", "kwh", "percentage.change", "range_mid.point",
    "category", var.names_str, var.name.for.facetting_str
  )
  return(dt_to.return[, .SD, .SDcols = cols_to.extract])
}
# # 1.1.3. To generate a ggplot object from the DTs
help_create.ggplot.obj <- function (
  data.table.for.avg_dt,
  data.table.for.diff_dt,
  var.names.for.subsetting_str,
  var.name.for.facetting_str
) {
  # ## Create objects that will be used to generate a ggplot object
  condition_avg.kwh <- paste(
    c(
      "category == 'Average Daily Consumption'",
      paste0(var.names.for.subsetting_str, " %in% c('Yes', 'No')")
    ),
    collapse = " & "
  )
  condition_diff.in.kwh <- paste(
    c(
      "category == 'Percentage Changes in Average Daily Consumption'",
      "period == 'Baseline'",  # `period == "Treatment"` gives the same plot.
      paste0(var.names.for.subsetting_str, " %in% c('Yes', 'No')")
    ),
    collapse = " & "
  )
  plot.options <- list(
    theme_linedraw(),
    theme(
      strip.text = element_text(face = "bold"),
      axis.title.x = element_text(margin = margin(t = 10)),
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
      data = data.table.for.avg_dt[eval(parse(text = condition_avg.kwh))]
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
      facet_grid(
        get(var.name.for.facetting_str) ~ group,
        labeller = labeller(group = label_group),
        scales = "free_y"
      ) +
      # scale_y_continuous(labels = scales::comma) +
      scale_color_manual(
        values = col.pal_custom[c(1, 2)],
        labels = c("Baseline Period", "Treatment Period")
      ) +
      scale_shape_manual(
        values = c(16, 18),
        labels = c("Baseline Period", "Treatment Period")
      ) +
      labs(
        x = TeX(r'(Temperature $ (\degree F)$)'),
        y = "Average Daily Consumption  (kWh/Day)",
        color = "",
        shape = "",
        subtitle = "Panel A: Household Average Daily Electricity Consumption"
      ) +
      plot.options
  # ## Create a ggplot object incl. plot for differences in household average
  # ## daily consumption
  plot_diff.in.kwh <-
    ggplot(
      data = data.table.for.diff_dt[eval(parse(text = condition_diff.in.kwh))]
    ) +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
      geom_vline(xintercept = 60, linetype = "dotdash", alpha = 0.5) +
      geom_line(
        aes(x = range_mid.point, y = percentage.change),
        color = "black", alpha = 0.25, lwd = 0.8
      ) +
      geom_point(
        aes(x = range_mid.point, y = percentage.change),
        color = "black", alpha = 0.25, size = 1.7, shape = 15
      ) +
      geom_line(
        aes(x = range_mid.point, y = percentage.change),
        color = col.pal_signal[3], lwd = 0.5
      ) +
      geom_point(
        aes(x = range_mid.point, y = percentage.change),
        color = col.pal_signal[3], size = 1.4, shape = 15
      ) +
      facet_grid(
        get(var.name.for.facetting_str) ~ group,
        labeller = labeller(group = label_group),
        scales = "free_y"
      ) +
      scale_y_continuous(labels = scales::percent) +
      labs(
        x = TeX(r'(Temperature $ (\degree F)$)'),
        y =
          "Percentage Changes in Household Average Daily Consumption  (kWh/Day)",
        color = "",
        shape = "",
        subtitle =
          "Panel B: Percentage Changes in Household Average Daily Consumption"
      ) +
      plot.options
  # ## Combine the two ggplot objects
  plot <- grid.arrange(plot_avg.kwh, plot_diff.in.kwh)

  return(plot)
}

# # 1.2. Define functions by using the help functions
get_ggplot.obj <- function (
  condition_str, var.names_str, var.name.for.facetting_str,
  lowest_numeric, highest_numeric, step_numeric
) {
  dt_avg.kwh <- help_compute.avg.kwh(
    condition_str = condition_str,
    var.names_str = var.names_str,
    var.name.for.facetting_str = var.name.for.facetting_str,
    lowest_numeric = lowest_numeric,
    highest_numeric = highest_numeric,
    step_numeric = step_numeric
  )
  dt_diff.in.kwh <- help_compute.diff.in.kwh(
    data.table_dt = dt_avg.kwh,
    var.names_str = var.names_str,
    var.name.for.facetting_str = var.name.for.facetting_str
  )
  list_plots <- help_create.ggplot.obj(
    data.table.for.avg_dt = dt_avg.kwh,
    data.table.for.diff_dt = dt_diff.in.kwh,
    var.names.for.subsetting_str = var.names_str,
    var.name.for.facetting_str = var.name.for.facetting_str
  )
  return(list_plots)
}


# ------------------------------------------------------------------------------
# Create ggplot object(s), and then export the object(s) in .PNG format
# ------------------------------------------------------------------------------
# ------- Load the DT for regression analysis -------
# # 1. Load the DT for regression analysis
dt_for.reg <- read_parquet(PATH_TO.LOAD_CER_FOR.REG)
setDT(dt_for.reg)


# # 2. Modify the DT loaded
# # 2.1. Change values of data fields
vars <- names(dt_for.reg)[str_detect(names(dt_for.reg), "^is_elec.heating")]
for (var in vars) {
  dt_for.reg[,  (var) := get(var) %>% as.character(.)]
  dt_for.reg[get(var) == "TRUE", (var) := "Yes"]
  dt_for.reg[get(var) == "FALSE", (var) := "No"]
  dt_for.reg[get(var) %>% is.na(.), (var) := "Unk"]
  dt_for.reg[
    ,
    (var) := get(var) %>% factor(., levels = c("Yes", "No", "Unk"))
  ]
}


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
list_var.names <- list(
  space = c("is_elec.heating_space_pre", "is_elec.heating_space_post"),
  water = c("is_elec.heating_water_pre", "is_elec.heating_water_post")
)
list_plots <- lapply(
  list_var.names,
  get_ggplot.obj,
  condition_str = "is_in.sample_incl.control_base.only.second.half == TRUE",
  # condition_str = "is_in.sample_incl.control_base == TRUE",
  var.name.for.facetting_str = "heating.type",
  lowest_numeric = temp.bin_lowest,
  highest_numeric = temp.bin_highest,
  step_numeric = 2
)


# # 2. Export the ggplot objects in PNG format
# # 2.1. Make objects that will be used to export the ggplot objects
# # 2.1.1. Make a list incl. file names
list_filenames <- list(
  "Space-Heating",
  "Water-Heating"
)
# # 2.1.2. Make lists incl. paths to where figures will be saved
list_paths <- lapply(
  list_filenames,
  function (filename_str) {
    paste(
      PATH_OUTPUT_FIGURE, "Average-kWh-by-Heating-Type",
      paste0(
        "Figure_Daily-Consumption-with-Percentage-Changes_",
        filename_str,
        ".png"
      ),
      sep = "/"
    )
  }
)
# # 2.1.3. Make a list containing plot options
list_args <- list(
  width_numeric = 35,
  height_numeric = 40,
  dpi_int = 350
)

# # 2.2. Export ggplot objects in PNG format
mapply(
  export_figure.in.png,
  list_plots, list_paths,
  MoreArgs = list_args
)


# ------------------------------------------------------------------------------
# Create huxtable object(s)
# ------------------------------------------------------------------------------
# ------- Create DT(s) utilized to make huxtable object(s) -------
# # 1. Create DTs by reshaping the DT for regression analysis
dt_1by1 <- dcast(
  dt_for.reg[
    is_in.sample_incl.control_base.only.second.half == TRUE &
      alloc_r_tariff %in% LETTERS[1:5],
    .N,
    keyby = .(id, is_elec.heating_space_pre, is_elec.heating_water_pre)
  ][
    , N := NULL
  ],
  formula = is_elec.heating_water_pre ~ is_elec.heating_space_pre,
  fun.aggregate = length
)
dt_2by1 <- dcast(
  dt_for.reg[
    is_in.sample_incl.control_base.only.second.half == TRUE &
      alloc_r_tariff %in% LETTERS[1:5],
    .N,
    keyby = .(id, is_elec.heating_space_pre, is_elec.heating_water_post)
  ][
    , N := NULL
  ],
  formula = is_elec.heating_water_post ~ is_elec.heating_space_pre,
  fun.aggregate = length
)
dt_1by2 <- dcast(
  dt_for.reg[
    is_in.sample_incl.control_base.only.second.half == TRUE &
      alloc_r_tariff %in% LETTERS[1:5],
    .N,
    keyby = .(id, is_elec.heating_space_post, is_elec.heating_water_pre)
  ][
    , N := NULL
  ],
  formula = is_elec.heating_water_pre ~ is_elec.heating_space_post,
  fun.aggregate = length
)
dt_2by2 <- dcast(
  dt_for.reg[
    is_in.sample_incl.control_base.only.second.half == TRUE &
      alloc_r_tariff %in% LETTERS[1:5],
    .N,
    keyby = .(id, is_elec.heating_space_post, is_elec.heating_water_post)
  ][
    , N := NULL
  ],
  formula = is_elec.heating_water_post ~ is_elec.heating_space_post,
  fun.aggregate = length
)


# # 2. Combine the DTs created above
dt_two.way <- rbind(
  cbind(
    dt_1by1,
    dt_1by2[, Total := Yes + No + Unk]
  ),
  cbind(
    janitor::adorn_totals(
      dat = as.data.frame(dt_2by1),
      where = "row"
    ),
    janitor::adorn_totals(
      dat = as.data.frame(dt_2by2[, Total := Yes + No + Unk]),
      where = "row"
    )
  ) %>% setDT(.),
  use.names = FALSE
)


# ------- Create huxtable object(s) -------
# # 1. Create huxtable object(s)
# # 1.1. Create huxtable object(s) from the DT created above
ht_tmp <- hux(dt_two.way)
contents(ht_tmp)[, 5] <- NULL  # To drop a unnecessary column

# # 1.2. Modify the huxtable object(s) generated above
# # 1.2.1. Add column(s)
ht_interim_cols <- insert_column(
  ht_tmp,
  c(
    "", "Pre-Survey", rep("", times = 2), "Post-Survey", rep("", times = 2),
    "Total"
  )
) %>%
  insert_column(
    .,
    c("", "Water Heating", rep("", times = 6))
  )
# # 1.2.2. Add row(s)
ht <- insert_row(
  ht_interim_cols,
  c(
    rep("", times = 3), "Pre-Survey", rep("0", times = 2), "Post-Survey",
    rep("0", times = 2), "Total"
  )
) %>%
  insert_row(
    .,
    c(rep("", times = 3), "Space Heating", rep("0", times = 6))
  )

# # 1.2.3. Merge cells
colspan(ht)[1, 4] <- 10 - 4 + 1
colspan(ht)[2, 4] <- 6 - 4 + 1
colspan(ht)[2, 7] <- 9 - 7 + 1
# # 1.2.4. Align texts
align(ht)[1, 4] <- "center"
align(ht)[2, 4] <- "center"
align(ht)[2, 7] <- "center"
align(ht)[3, ] <- "center"
# # 1.2.5. Change contents in cells
contents(ht)[3, 3] <- ""
contents(ht)[3, 10] <- ""
contents(ht)[10, 3] <- ""
# # 1.2.6. Add border lines
top_border(ht)[1:2, 4:10] <- brdr(thickness = 1)
top_border(ht)[3, 4:9] <- brdr(thickness = 1)
bottom_border(ht)[3, ] <- brdr(thickness = 1)
bottom_border(ht)[6, 2:10] <- brdr(thickness = 1)
bottom_border(ht)[9, 2:10] <- brdr(thickness = 1)
bottom_border(ht)[10, ] <- brdr(thickness = 1)
left_border(ht)[4:10, 1] <- brdr(thickness = 1)
right_border(ht)[4:10, 1] <- brdr(thickness = 1)
right_border(ht)[4:9, 2] <- brdr(thickness = 1)
right_border(ht)[, 3] <- brdr(thickness = 1)
right_border(ht)[2:10, 6] <- brdr(thickness = 1)
right_border(ht)[2:10, 9] <- brdr(thickness = 1)
right_border(ht)[, 10] <- brdr(thickness = 1)
# # 1.2.7. Change the format of numbers
number_format(ht)[4:10, 4:10] <- fmt_pretty()


# # 3. Print the huxtable(s)
ht
# ## Note:
# ## The table is a snapshot only
# ## (i.e., some number = stayed + inflow - outflow.)
