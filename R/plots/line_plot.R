build_line_plot <- function(df) {
  priority_gases <- intersect(c("CO", "PM10", "O_3", "NO_2", "SO_2", "PM25"), names(df))
  if (length(priority_gases) == 0 || nrow(df) == 0) return(empty_plot())

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

  data_p1 <- df |>
    filter(name %in% target_names) |>
    mutate(name = factor(name, levels = target_names)) |>
    group_by(name, year) |>
    summarise(across(all_of(priority_gases), ~mean(.x, na.rm = TRUE)), .groups = "drop") |>
    group_by(name) |>
    mutate(across(all_of(priority_gases), ~scales::rescale(.x))) |>
    ungroup() |>
    pivot_longer(cols = all_of(priority_gases), names_to = "Gas", values_to = "value")

  ggplot(data_p1, aes(x = year, y = value, color = Gas)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.7) +
    geom_vline(xintercept = 2018, linetype = "dashed", color = "grey40") +
    facet_wrap(~name, ncol = 1, strip.position = "left") +
    theme_bw(base_size = 13) +
    labs(title = "Historical Trends of Priority Gases in Top 5 Polluted Stations", x = "Year", y = "Normalised Concentration") +
    theme(strip.background = element_rect(fill = "#eaf6ef"), plot.title = element_text(face = "bold"))
}
