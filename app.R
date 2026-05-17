options(shiny.error = browser)

# File Name: app.R
# Visualisation of Pollution in Madrid over Time

# Set Up ----------------------------------------------------------------------
# Install packages if needed:
# install.packages(c("shiny", "dplyr", "ggplot2", "DT"))
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

# Data ------------------------------------------------------------------------
aqi_joined <- readRDS("./data/aqi_joined.RDS")
aqi_joined <- aqi_joined %>%
    mutate(
        pollutant = as.factor(pollutant)
    )



# User interface --------------------------------------------------------------
ui <- fluidPage(
    titlePanel("Air Pollution in Madrid"),

    
    sidebarLayout(
        sidebarPanel(
            width = 2,
            
            selectInput(
                inputId = "pollutant",
                label = "Choose Pollutant",
                choices = c("All", levels(aqi_joined$pollutant)),
                selected = "All"
                )


        ),
        
        mainPanel(
            width = 10,
            h3("Summary"),
            verbatimTextOutput("summary_text"),
            
            h3("Dataset"),
            DTOutput("pollutant_table"),
            
            h3("AQI Levels over Time"),
            plotOutput("plt_heatmap_over_time", height = "600px")
        )
        
        
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
    
}

# Run app ---------------------------------------------------------------------
shinyApp(ui = ui, server = server)
