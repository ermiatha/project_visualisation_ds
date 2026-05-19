required_packages <- c(
  "shiny", "DT", "tidyverse", "lubridate", "scales", "fmsb",
  "plotly", "htmltools", "bslib", "shinyjs", "reticulate"
)
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Install missing packages first: ", paste(missing_packages, collapse = ", "), call. = FALSE)
}

library(shiny)
library(DT)
library(tidyverse)
library(lubridate)
library(scales)
library(fmsb)
library(plotly)
library(htmltools)
library(bslib)
library(shinyjs)
library(reticulate)

app_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
data_dir <- file.path(app_dir, "data")

read_app_rds <- function(file) readRDS(file.path(data_dir, file))

aqi_joined <- read_app_rds("aqi_joined.RDS") |>
  mutate(
    pollutant = as.factor(pollutant),
    year = as.integer(as.character(year)),
    month = sprintf("%02d", as.integer(as.character(month))),
    day = sprintf("%02d", as.integer(as.character(day)))
  )

madrid_complete <- read_app_rds("madrid_complete.RDS") |>
  mutate(year = as.integer(as.character(year)))

pollutant_choices <- c("All", sort(unique(as.character(aqi_joined$pollutant))))
year_choices <- sort(unique(stats::na.omit(madrid_complete$year)))

source(file.path(app_dir, "R", "helpers.R"), local = TRUE)
source(file.path(app_dir, "R", "plots", "heatmap_plot.R"), local = TRUE)
source(file.path(app_dir, "R", "plots", "line_plot.R"), local = TRUE)
source(file.path(app_dir, "R", "plots", "radar_plot.R"), local = TRUE)
source(file.path(app_dir, "R", "plots", "python_component.R"), local = TRUE)
