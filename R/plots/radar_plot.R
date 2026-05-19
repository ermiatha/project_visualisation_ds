build_radar_plot <- function(df, selected_year = 2008) {
  gases <- intersect(c("NO_2", "O_3", "PM10", "CO", "SO_2"), names(df))
  if (length(gases) < 3) return(empty_plot("Not enough pollutant columns for radar plot."))
  base_stations <- df |>
    filter(year == 2008) |>
    group_by(name) |>
    summarise(across(all_of(gases), ~mean(.x, na.rm = TRUE)), .groups = "drop") |>
    mutate(score = rowMeans(across(all_of(gases)), na.rm = TRUE)) |>
    arrange(desc(score)) |>
    slice_head(n = 6) |>
    pull(name)

  radar_prep <- df |>
    filter(year == selected_year, name %in% base_stations) |>
    group_by(name) |>
    summarise(across(all_of(gases), ~mean(.x, na.rm = TRUE)), .groups = "drop") |>
    mutate(across(all_of(gases), ~scales::rescale(.x, to = c(0, 100))))

  if (nrow(radar_prep) == 0) return(empty_plot("No radar data for this year."))
  radar_df <- as.data.frame(t(radar_prep[, gases, drop = FALSE]))
  colnames(radar_df) <- radar_prep$name
  radar_final <- rbind(rep(100, ncol(radar_df)), rep(0, ncol(radar_df)), radar_df)
  gas_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#A65628")[seq_len(ncol(radar_df))]
  gas_fill <- scales::alpha(gas_colors, 0.18)
  fmsb::radarchart(
    radar_final, axistype = 1, pcol = gas_colors, pfcol = gas_fill, plwd = 3, plty = 1,
    cglcol = "grey80", cglty = 1, axislabcol = "grey30", caxislabels = seq(0, 100, 25),
    cglwd = 0.8, vlcex = 0.85,
    title = paste("Pollutant Footprint Across Top Stations -", selected_year)
  )
  legend("topright", legend = colnames(radar_df), bty = "n", pch = 20, col = gas_colors, cex = 0.8, pt.cex = 1.5)
}
