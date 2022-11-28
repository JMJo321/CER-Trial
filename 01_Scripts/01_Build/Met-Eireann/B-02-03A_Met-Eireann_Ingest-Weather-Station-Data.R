# < Description > *
# > Script Group Indicator Number and Name
# # : B-02, Met-Eireann
# #
# > Script Number(s)
# # : B-02-03A
# #
# > Purpose of the script(s)
# # : Make a plot showing the location of weather stations.

# ------------------------------------------------------------------------------
# Load required libraries
# ------------------------------------------------------------------------------
library(arrow)
library(stringr)
library(data.table)
library(ggplot2)
library(ggmap)


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
# # 1.1. For the DT including hourly weather data
FILE_TO.LOAD_WEATHER_HOURLY <-
  "Met-Eireann_Weather-Data_Hourly.parquet"
PATH_TO.LOAD_WEATHER_HOURLY <-
  paste(PATH_DATA_INTERMEDIATE_WEATHER, FILE_TO.LOAD_WEATHER_HOURLY, sep = "/")

# # 1.2. For a CSV including weather stations' location information
FILE_TO.LOAD_LOCATION <- "StationDetails.csv"
PATH_TO.LOAD_LOCATION <- paste(
  PATH_DATA_RAW_USE, "Met-Eireann/Weather-Station-Data", FILE_TO.LOAD_LOCATION,
  sep = "/"
)


# ------- Define parameter(s) -------
# # 1. Set a list of weather stations that will be excluded from the table.
STATIONS_TO.EXCLUDE <- c(
  "Casement Aerodrome", "Claremorris", "Dunsany", "Roches Point SWS",
  "Phoenix Park", # Close to other stations
  "Athenry", "Finner", # Less observations
  "Clones" # Close at 2008
)


# # 2. Path(s) to which regression results will be stored
# # 2.1. For figure(s)
DIR_TO.SAVE_FIGURE_FOR.DISSERTATION <- "For-Dissertation_Chapter-2"
PATH_TO.SAVE_FIGURE_FOR.DISSERTATION <-
  paste(PATH_OUTPUT_FIGURE, DIR_TO.SAVE_FIGURE_FOR.DISSERTATION, sep = "/")


# ------- Define function(s) -------
# (Not Applicable)


# ------------------------------------------------------------------------------
# Create DT(s) to generate ggplot object(s)
# ------------------------------------------------------------------------------
# ------- Make DT(s) -------
# # 1. Ingest data containing each weather station's longitude and latitude
# # 1.1. Ingest a CSV file
dt_stations <- fread(file = PATH_TO.LOAD_LOCATION)

# # 1.2. Modify the ingested data
# # 1.2.1. Rename columns
names(dt_stations) <- c(
  "county", "station_code", "station", "height_meter",
  "easting", "northing", "latitude", "longitude",
  "year_open", "year_close"
)
# # 1.2.2. Update values
dt_stations[str_detect(year_open, "null"), year_open := NA]
dt_stations[, year_open := as.integer(year_open)]
dt_stations[str_detect(year_close, "null"), year_close := NA]
dt_stations[, year_close := as.integer(year_close)]


# # 2. Make a list of weather stations
dt_weather_hourly <- read_parquet(PATH_TO.LOAD_WEATHER_HOURLY)
dt_stations_from.hourly <- dt_weather_hourly[
  !station %in% STATIONS_TO.EXCLUDE, .(station, station_code)
][
  , .N, keyby = .(station, station_code)
][
  , N := NULL
]


# # 3. Create a DT for making ggplot object(s)
dt_for.plot <-
  dt_stations[station_code %in% dt_stations_from.hourly$station_code] %>%
    merge(
      x = .,
      y = dt_stations_from.hourly,
      by = "station_code",
      all.x = TRUE
    )


# ------------------------------------------------------------------------------
# Create ggplot object(s)
# ------------------------------------------------------------------------------
# ------- Create objects for setting plot options -------
# # 1. Define common plot options
# # 1.1. Create a list including common options
plot.options <- list(
  theme_linedraw(),
  theme(
    strip.text = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 12)),
    legend.text = element_text(margin = margin(r = 10, unit = "pt")),
    legend.position = "bottom",
    legend.margin = margin(-0.2, 0, 0, 0, unit = "cm"),
    legend.box.just = "left",
    legend.box = "vertical",
    legend.box.margin = margin(0.5, 0, 0.1, 0, unit = "cm")
  )
)


# ------- Create ggplot object(s) -------
plot_map <-
  ggplot() +
    geom_polygon(
      data = map_data(map = "world", region = "Ireland"),
      aes(x = long, y = lat),
      alpha = 0.4
    ) +
    geom_label(
      data = dt_for.plot,
      aes(x = longitude, y = latitude, label = station.y),
      size = 3.5
    ) +
    labs(x = "Longitude", y = "Latitude") +
    plot.options


# ------- Export the ggplot object(s) created above -------
export_figure.in.png(
  plot_map,
  filename_str = paste(
    PATH_TO.SAVE_FIGURE_FOR.DISSERTATION,
    "Figure_Location-of-Weather-Stations.png",
    sep = "/"
  ),
  width_numeric = 20, height_numeric = 30
)
