filter_aqi_data <- function(pollutant = "All") {
  df <- aqi_joined
  if (!is.null(pollutant) && pollutant != "All") df <- df |> filter(pollutant == !!pollutant)
  df
}

build_heatmap_plot <- function(df) {
  if (nrow(df) == 0) return(empty_plot())
  aqi_cols <- c("#00E400", "#FFFF00", "#FF7E00", "#FF0000", "#8F3F97", "#7E0023")
  df <- df |>
    filter(year %in% c(2001, 2005, 2009, 2013, 2017), !is.na(year)) |>
    mutate(
      day = factor(day, levels = sprintf("%02d", 1:31)),
      month = factor(month, levels = sprintf("%02d", 12:1)),
      year = factor(year)
    )
  if (nrow(df) == 0) return(empty_plot("No AQI rows match this pollutant."))
  ggplot(df, aes(day, month, fill = AQI)) +
    geom_tile() +
    facet_wrap(~year, ncol = 5) +
    scale_fill_gradientn(colors = aqi_cols) +
    scale_x_discrete(breaks = c("01", "10", "20", "30")) +
    labs(x = "Day", y = "Month", fill = "AQI") +
    theme_minimal(base_size = 14) +
    theme(panel.grid = element_blank(), strip.text = element_text(face = "bold"), legend.position = "right")
}
