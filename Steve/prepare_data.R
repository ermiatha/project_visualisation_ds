
# DATA IMPORT & MERGING
folder_path <- "./data/VDS2526_Madrid"

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
        year  = lubridate::year(date),
        month = lubridate::month(date, label = TRUE),
        hour  = lubridate::hour(date)
    ) %>%
    select(date, year, month, hour, name, lon, lat, 
           CO, PM10, O_3, NO_2, SO_2, PM25)


saveRDS(madrid_complete, "./data/madrid_complete.RDS")

