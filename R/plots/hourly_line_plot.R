build_hourly_line_plot <- function(df) {
    priority_gases <- intersect(c("CO", "PM10", "O_3", "NO_2", "SO_2", "PM25"), names(df))
    if (length(priority_gases) == 0 || nrow(df) == 0) return(empty_plot())
    # 1. Use the verified Top 5 Stations and 6 Priority Gases
    target_names <- df |>
        filter(year == max(year, na.rm = TRUE)) |>
        group_by(name) |>
        summarise(across(all_of(priority_gases), ~mean(.x, na.rm = TRUE)), .groups = "drop") |>
        mutate(across(all_of(priority_gases), ~scales::rescale(.x))) |>
        rowwise() |>
        mutate(Pollution_Score = mean(c_across(all_of(priority_gases)), na.rm = TRUE)) |>
        ungroup() |>
        arrange(desc(Pollution_Score)) |>
        slice_head(n = 5) |>
        pull(name)
    
    # 2. Process data for hourly averages
    hourly_variation <- df %>%
        # Filter only for the 5 verified top stations
        filter(name %in% target_names) %>%
        group_by(hour) %>%
        # Aggregate averages for the six gases
        summarise(across(all_of(priority_gases), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
        pivot_longer(-hour, names_to = "Gas", values_to = "Concentration")
    
    # 3. Generate Figure
    ggplot(hourly_variation, aes(x = hour, y = Concentration, color = Gas)) +
        geom_line(linewidth = 1.5) +
        geom_point(size = 2) +
        # Detailed X-axis for every hour of the day
        scale_x_continuous(breaks = seq(0, 23, by = 1)) +
        theme_minimal() +
        labs(
            title = "Hourly Variation of Priority Gases (Top 5 Stations)",
            subtitle = "Daily pollution aggregated across the most polluted areas (2001-2018)",
            x = "Hour of Day", 
            y = "Concentration (µg/m³; CO in mg/m³)",
            caption = "Data Source: Madrid Air Quality Study"
        ) +
        theme(
            panel.grid.minor = element_blank(),
            legend.position = "right",
            axis.text.x = element_text(angle = 0),
            plot.title = element_text(face = "bold", size = 14)
        )
    
}

