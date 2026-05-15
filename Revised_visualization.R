
# SETUP & LIBRARIES
install.packages("fmsb")
install.packages("lubridate")
install.packages("scales")
library(tidyverse)
library(lubridate)
library(scales)
library(fmsb)

# DATA IMPORT & MERGING
folder_path <- "/Users/tmbalu/Downloads/R_WD/VDS2526_Madrid"

# Import measurement files [cite: 13]
file_list <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
# Filter out the stations file from the measurement list
measurement_files <- file_list[!str_detect(file_list, "stations")]

madrid_measurements <- measurement_files %>%
  set_names(basename(.)) %>% 
  map(~read_csv(.x, show_col_types = FALSE)) %>%
  list_rbind(names_to = "origin_file")

# Import station metadata [cite: 13]
stations_info <- read_csv(file.path(folder_path, "stations.csv"), show_col_types = FALSE)

# Join datasets and filter for the six priority gases [cite: 15-25]
madrid_complete <- madrid_measurements %>%
  left_join(stations_info, by = c("station" = "id")) %>%
  mutate(
    year  = year(date),
    month = month(date, label = TRUE),
    hour  = hour(date)
  ) %>%
  select(date, year, month, hour, name, lon, lat, 
         CO, PM10, O_3, NO_2, SO_2, PM25)

# VERIFY TOP POLLUTED STATIONS (2018)
# Define the six priority pollutants [cite: 13, 15]
priority_gases <- c("CO", "PM10", "O_3", "NO_2", "SO_2", "PM25")
# Calculate the verified top stations for 2018
verified_top_stations <- madrid_complete %>%
  filter(year == 2018) %>%
  group_by(name) %>%
  # Calculate mean for each of the six priority gases
  summarise(across(all_of(priority_gases), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
  # Normalise each pollutant [0, 1] so they contribute equally to the score
  mutate(across(all_of(priority_gases), ~ scales::rescale(.x))) %>%
  # Create the composite Pollution Score
  rowwise() %>%
  mutate(Pollution_Score = mean(c_across(all_of(priority_gases)), na.rm = TRUE)) %>%
  arrange(desc(Pollution_Score))

# View the Top 5 Stations to confirm the list
target_stations_list <- head(verified_top_stations, 5)
print(target_stations_list)

# VISUALIZATION 1: FACETED HISTORICAL TRENDS
# 1. Extract the specific 5 names from verified list
target_names <- target_stations_list$name
priority_gases <- c("CO", "PM10", "O_3", "NO_2", "SO_2", "PM25")

# 2. Generate the plot
madrid_complete %>%
  # Filter only for these 5 stations
  filter(name %in% target_names) %>%
  # Force R to forget about the other stations so they don't appear in the plot
  mutate(name = factor(name, levels = target_names)) %>%
  group_by(name, year) %>%
  summarise(across(all_of(priority_gases), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
  # Normalize trends per station (0 to 1 scale)
  group_by(name) %>%
  mutate(across(all_of(priority_gases), ~ scales::rescale(.x))) %>%
  pivot_longer(cols = all_of(priority_gases), names_to = "Gas") %>%
  ggplot(aes(x = year, y = value, color = Gas)) +
  geom_line(linewidth = 1) +
  # Reference line for the 2018 council inquiry year
  geom_vline(xintercept = 2018, linetype = "dashed", color = "grey40") +
  # Facet by name - only our 5 stations will show up now
  facet_wrap(~name, ncol = 1, strip.position = "left") +
  theme_bw() +
  scale_y_continuous(breaks = c(0.1, 0.5, 1.0)) +
  labs(
    title = "Historical Trends of 6 Priority Gases in Top 5 Polluted Stations",
    subtitle = "Normalized Concentration (2001-2018) | Verified by 2018 Pollution Score",
    x = "Year", 
    y = "Normalised Concentration"
  ) +
  theme(
    strip.background = element_blank(), 
    strip.text.y.left = element_text(angle = 0, face = "bold", hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

# VISUALIZATION 2 - Hourly variations
# 1. Use the verified Top 5 Stations and 6 Priority Gases
target_names <- target_stations_list$name
priority_gases <- c("CO", "PM10", "O_3", "NO_2", "SO_2", "PM25")

# 2. Process data for hourly averages
hourly_variation <- madrid_complete %>%
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
    subtitle = "Daily pollution pulse aggregated across the most polluted areas (2001-2018)",
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

# VISUALIZATION 3: RADAR PLOT
# 1. Define the verified target stations from your previous ranking
target_names <- target_stations_list$name

# 2. Prepare data for the radar plot
radar_prep <- madrid_complete %>%
  filter(name %in% target_names) %>%
  group_by(name) %>%
  summarise(
    NO_2 = mean(NO_2, na.rm = TRUE),
    O_3  = mean(O_3, na.rm = TRUE),
    PM10 = mean(PM10, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Rescale pollutants 0-100 for visual comparison
  mutate(across(-name, ~ scales::rescale(.x, to = c(0, 100))))

# 3. Format the data frame for fmsb
# fmsb needs pollutants as rows and stations as columns for the web axes
radar_df <- as.data.frame(t(radar_prep[,-1]))
colnames(radar_df) <- radar_prep$name

# Add the required max (100) and min (0) rows for fmsb scaling
radar_final <- rbind(rep(100, 5), rep(0, 5), radar_df)

# 4. Generate the Radar Plot
colors_border <- c("#E41A1C", "#4DAF4A", "#377EB8") # Distinct Red, Green, Blue
colors_in     <- c(alpha("#E41A1C", 0.3), alpha("#4DAF4A", 0.3), alpha("#377EB8", 0.3))

radarchart(radar_final, 
           axistype = 1,
           pcol = colors_border, 
           pfcol = colors_in, 
           plwd = 4, 
           plty = 1,
           cglcol = "grey", 
           cglty = 1, 
           axislabcol = "grey", 
           caxislabels = seq(0, 100, 25), 
           cglwd = 0.8,
           vlcex = 0.8,
           title = "Top 3 Pollutants Patterns Across Top 5 Stations")

# Add legend to identify the 3 pollutants
legend(x = "topright", 
       inset = c(-0.25, 0),    
       legend = rownames(radar_df), 
       bty = "n",              
       pch = 20, 
       col = gas_colors, 
       cex = 0.9, 
       pt.cex = 2,
       xpd = TRUE)             

# RADAR plot for all the six gases
# 1. Ensure fmsb is ready
library(fmsb)

# 2. Extract verified top names and define the 6 gases
target_names <- target_stations_list$name
priority_gases <- c("CO", "PM10", "O_3", "NO_2", "SO_2", "PM25")

# 3. Prepare data: Mean of all 6 gases across the 5 stations
radar_prep <- madrid_complete %>%
  filter(name %in% target_names) %>%
  group_by(name) %>%
  summarise(across(all_of(priority_gases), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
  # Rescale to 0-100 within each gas for fair visual comparison
  mutate(across(all_of(priority_gases), ~ scales::rescale(.x, to = c(0, 100))))

# 4. Transpose for fmsb: Pollutants as Rows, Stations as Axis Columns
radar_df <- as.data.frame(t(radar_prep[,-1]))
colnames(radar_df) <- radar_prep$name

# Add required max (100) and min (0) rows for fmsb scaling
radar_final <- rbind(rep(100, 5), rep(0, 5), radar_df)

# 5. Define colors for the 6 gases
gas_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#A65628")
gas_fill   <- alpha(gas_colors, 0.2)

# 6. Generate the Radar Plot
radarchart(radar_final, 
           axistype = 1,
           pcol = gas_colors, 
           pfcol = gas_fill, 
           plwd = 3, 
           plty = 1,
           cglcol = "grey", 
           cglty = 1, 
           axislabcol = "grey", 
           caxislabels = seq(0, 100, 25), 
           cglwd = 0.8,
           vlcex = 0.8,
           title = "Pollutant Footprint: 6 Priority Gases Across Top Stations")

# 7. Add Legend to the side
legend(x = 1.1, y = 1.2, 
       legend = rownames(radar_df), 
       bty = "n", pch = 20, 
       col = gas_colors, 
       cex = 0.9, pt.cex = 2)

#To include all "full sensor" stations for the gases
# 1. Setup - Ensuring clean margins for the legend
par(mar = c(1, 1, 2, 1)) # Adjust margins: bottom, left, top, right

# 2. Extract verified names and define the 6 gases
target_names <- target_stations_list$name
priority_gases <- c("CO", "PM10", "O_3", "NO_2", "SO_2", "PM25")

# 3. Prepare data with Zero-Imputation for missing sensors
radar_prep <- madrid_complete %>%
  filter(name %in% target_names) %>%
  group_by(name) %>%
  summarise(across(all_of(priority_gases), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
  # FIX: Replace NaN/NA with 0 so the radar lines can close/connect
  mutate(across(all_of(priority_gases), ~ replace_na(., 0))) %>%
  # Rescale to 0-100 within each gas for fair visual comparison
  mutate(across(all_of(priority_gases), ~ scales::rescale(.x, to = c(0, 100))))

# 4. Transpose for fmsb: Pollutants as Rows (Webs), Stations as Axis Columns
radar_df <- as.data.frame(t(radar_prep[,-1]))
colnames(radar_df) <- radar_prep$name

# Add required max (100) and min (0) rows for fmsb scaling
radar_final <- rbind(rep(100, 5), rep(0, 5), radar_df)

# 5. Define high-contrast colors for the 6 gases
gas_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#A65628")
gas_fill   <- alpha(gas_colors, 0.2)

# 6. Generate the Radar Plot
radarchart(radar_final, 
           axistype = 1,
           pcol = gas_colors, 
           pfcol = gas_fill, 
           plwd = 3, 
           plty = 1,
           cglcol = "grey80", 
           cglty = 1, 
           axislabcol = "grey30", 
           caxislabels = seq(0, 100, 25), 
           cglwd = 0.8,
           vlcex = 0.8,
           title = "Pollutant Footprint: 6 Priority Gases Across Top Stations")

# 7. Add Legend - Positioned to the right with no double-printing
legend(x = "topright", 
       inset = c(-0.15, 0), # Move slightly outside the plot area
       legend = rownames(radar_df), 
       bty = "n", 
       pch = 20, 
       col = gas_colors, 
       cex = 0.85, 
       pt.cex = 1.5,
       text.font = 2)

# VISUALIZATION - Hourly City-wide pollution 2001-2018
# 1. Define the six priority gases
priority_gases <- c("CO", "PM10", "O_3", "NO_2", "SO_2", "PM25")

# 2. Process data: Global hourly averages (2001-2018)
global_hourly <- madrid_complete %>%
  group_by(hour) %>%
  # Calculate mean across all stations for each hour
  summarise(across(all_of(priority_gases), \(x) mean(x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(-hour, names_to = "Gas", values_to = "Concentration")

# 3. Generate the Final Plot
ggplot(global_hourly, aes(x = hour, y = Concentration, color = Gas)) +
  geom_line(linewidth = 1.5) +
  geom_point(size = 2) +
  # Detailed X-axis for 24 hours
  scale_x_continuous(breaks = 0:23) +
  theme_minimal() +
  labs(
    title = "City-Wide Hourly Variation of Priority Gases (2001-2018)",
    subtitle = "Historical mean concentrations aggregated across all monitoring stations",
    x = "Hour of Day", 
    y = "Concentration (µg/m³; CO in mg/m³)",
    caption = "Madrid Air Quality Dataset Analysis"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right",
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  )
