options(shiny.error = browser)

# File Name: app.R
# Visualisation of Pollution in Madrid over Time

# Set Up ----------------------------------------------------------------------
# Install packages if needed:
# install.packages(c("shiny", "dplyr", "ggplot2", "DT"))
library(shiny)
library(DT)
library(tidyverse)
library(lubridate)
library(scales)
library(fmsb)

# Data ------------------------------------------------------------------------
aqi_joined <- readRDS("./data/aqi_joined.RDS")
aqi_joined <- aqi_joined %>%
    mutate(
        pollutant = as.factor(pollutant)
    )

madrid_complete <- readRDS("./data/madrid_complete.RDS")
madrid_complete <- madrid_complete %>%
    mutate(
        year = as.factor(year)
    )

radar_prep_2008 <- madrid_complete %>%
    filter(year == 2008) %>%
    #filter(name %in% target_names) %>%
    group_by(name) %>%
    summarise(
        NO_2 = mean(NO_2, na.rm = TRUE),
        O_3  = mean(O_3, na.rm = TRUE),
        PM10 = mean(PM10, na.rm = TRUE),
        CO   = mean(CO, na.rm  = TRUE),
        SO_2 = mean(SO_2, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    # Rescale pollutants 0-100 for visual comparison
    mutate(across(-name, ~ scales::rescale(.x, to = c(0, 100))))

stations_2008 <- radar_prep_2008 %>% select(name) %>% drop_na() %>%
    pull(name) %>% unique()


plt_lineplot_stations <- readRDS("./Steve/figures/lineplot_stations.rds")

# User interface --------------------------------------------------------------
ui <- fluidPage(
    
    titlePanel("Air Pollution in Madrid"),
    
    br(),
    
    h3("Summary"),
    verbatimTextOutput("summary_text"),
    
    br(),
    
    h3("Dataset"),
    DTOutput("pollutant_table"),
    
    br(),
    
    # ---------------------------------------------------
    # Visualisation 1: Heatmap
    # ---------------------------------------------------
    
    h2("AQI Levels over Time"),
    
    fluidRow(
        column(
            width = 3,
            
            selectInput(
                inputId = "pollutant",
                label = "Choose Pollutant",
                choices = c("All", levels(aqi_joined$pollutant)),
                selected = "All"
            )
        )
    ),
    
    plotOutput(
        "plt_heatmap_over_time",
        height = "650px"
    ),
    
    br(),
    br(),
    
    # ---------------------------------------------------
    # Visualisation 2: line plot
    # ---------------------------------------------------
    
    
    h2("Line Plot: Pollution Evolution over the Years"),
    
    plotOutput(
        "plt_lineplot_stations",
        height = "650px"
    ),
    
    br(),
    br(),
    
    # ---------------------------------------------------
    # Visualisation 3: Radar plot
    # ---------------------------------------------------
    
    h2("Radar Plot: Pollution across Stations"),
    
    fluidRow(
        column(
            width = 3,
            
            sliderInput(
                inputId = "year",
                label = "Choose Year",
                min = min(as.numeric(as.character(madrid_complete$year)), na.rm = TRUE),
                max = max(as.numeric(as.character(madrid_complete$year)), na.rm = TRUE),
                value = 2008,
                step = 1,
                sep = ""
            )
        )
    ),
    
    plotOutput(
        "plt_radar_over_time",
        height = "850px"
    )
    
)

# Server ----------------------------------------------------------------------
server <- function(input, output) {
    
    output$pollutant_table <- renderDT({
        data <- aqi_joined
        
        if (input$pollutant != "All") {
            data <- data |>
                filter(pollutant == input$pollutant)
        }
        
        datatable(
            data,
            options = list(pageLength = 4),
            rownames = FALSE
        )
    })
    
    output$summary_text <- renderPrint({
        data <- aqi_joined
        
        if (input$pollutant != "All") {
            data <- data %>% 
                filter(pollutant == input$pollutant)
        }
        
        
        cat("Rows:", nrow(data), "\n")
        cat("Pollutants:", paste(unique(data$pollutant), collapse = ", "), "\n")
        cat("Years:", paste(unique(data$year), collapse = ", "), "\n")
        cat("Average Pollution Concentration:", round(mean(data$conc, 
                                             na.rm = TRUE), 3), "\n")
    })
    
    output$plt_heatmap_over_time <- renderPlot(
        {
            aqi_cols <- c(
                "Good" = "#00E400",
                "Moderate" = "#FFFF00",
                "Unhealthy for Sensitive Groups" = "#FF7E00",
                "Unhealthy" = "#FF0000",
                "Very Unhealthy" = "#8F3F97",
                "Hazardous" = "#7E0023"
            )
            
        
        ### receive user input
        data <- aqi_joined
        if (input$pollutant != "All") {
            data <- data %>% 
                filter(pollutant == input$pollutant)
        }
        
        # prepare dataset
        data <- data %>%
            filter(
                year %in% c("2001", "2005", "2009", "2013", "2017")
            ) %>%
            filter(
                !is.na(year),
            ) %>%
            mutate(
                day = factor(
                    day,
                    levels = sprintf("%02d", 1:31)
                ),
                
                # reverse month order (12 at bottom)
                month = factor(
                    month,
                    levels = sprintf("%02d", 12:1)
                ),
                
                year = factor(year)
            )
        
        
        p <- ggplot(data,
                    aes(day, month, fill = AQI)) +
            geom_tile() +
            facet_wrap(~ year, ncol = 5) +
            
            scale_fill_gradientn(colors = unname(aqi_cols)) +
            scale_x_discrete(breaks = c("01", "10", "20", "30")) +
            
            labs(
                x = "Day",
                y = "Month",
                fill = "AQI") +
            
            theme_minimal(base_size = 20) +
            theme(
                panel.grid = element_blank(),
                strip.text = element_text( size = 18, face = "bold"),
                axis.text.x = element_text(angle = 0, vjust = 0.5),
                legend.position = "right",
                panel.spacing = unit(1.5, "lines")
            )
        p        
        }, width = 1400, height = 400)
    
    
    ### Line plot ############################################################
    # no interactions
    output$plt_lineplot_stations <- renderPlot({
        plt_lineplot_stations
    })
    
    ### Radar plot ############################################################
    output$plt_radar_over_time <- renderPlot({

        
        ### receive user input
        data <- madrid_complete
        if (input$year != "All") {
            data <- data %>% 
                filter(year == input$year)
        }
        
        radar_prep_2018 <- data %>%
            filter(name %in% stations_2008) %>%
            group_by(name) %>%
            summarise(
                NO_2 = mean(NO_2, na.rm = TRUE),
                O_3  = mean(O_3, na.rm = TRUE),
                PM10 = mean(PM10, na.rm = TRUE),
                CO   = mean(CO, na.rm  = TRUE),
                SO_2 = mean(SO_2, na.rm = TRUE),
                .groups = "drop"
            ) %>%
            # Rescale pollutants 0-100 for visual comparison
            mutate(across(-name, ~ scales::rescale(.x, to = c(0, 100))))
        
        radar_df <- as.data.frame(t(radar_prep_2018[,-1]))
        colnames(radar_df) <- radar_prep_2018$name
        
        
        radar_final <- rbind(rep(100, ncol(radar_df)), rep(0, ncol(radar_df)), radar_df)
        
        gas_colors <- c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#A65628")
        gas_fill   <- alpha(gas_colors, 0.2)
        
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
        
        legend(x = 1.1, y = 1.2,
               #x = "topright", 
               #inset = c(-0.15, 0), # Move slightly outside the plot area
               legend = rownames(radar_df), 
               bty = "n", 
               pch = 20, 
               col = gas_colors, 
               cex = 0.85, 
               pt.cex = 1.5,
               text.font = 2)
        
    })
    
}

# Run app ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)
