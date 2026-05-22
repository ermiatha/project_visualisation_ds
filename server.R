server <- function(input, output, session) {
  py_html <- prepare_python_component()

  filtered_aqi <- reactive({ filter_aqi_data(input$pollutant %||% "All") })

  output$kpi_cards <- renderUI({
    df <- filtered_aqi()
    div(
      class = "kpi-row",
      div(class = "kpi-card green", h3(scales::comma(nrow(df))), p("Records")),
      div(class = "kpi-card blue", h3(scales::comma(round(mean(df$conc, na.rm = TRUE), 2))), p("Average concentration")),
      div(class = "kpi-card purple", h3(length(unique(stats::na.omit(df$pollutant)))), p("Pollutants")),
      div(class = "kpi-card orange", h3(length(unique(stats::na.omit(df$year)))), p("Years"))
    )
  })



  output$plt_heatmap_over_time <- renderPlot({
    build_heatmap_plot(filtered_aqi())
  }, res = 100)

  output$plt_lineplot_stations <- renderPlot({
    build_line_plot(madrid_complete)
  }, res = 100)

  output$plt_radar_over_time <- renderPlot({
    selected_year <- suppressWarnings(as.integer(input$radar_year))
    if (is.na(selected_year)) selected_year <- 2008
    build_radar_plot(madrid_complete, selected_year)
  }, res = 100)

  output$madrid_pollution_plot <- renderUI({
    tags$iframe(src = "generated/madrid_pollution.html", width = "100%", height = "650px", style = "border:0;background:white;border-radius:16px;")
  })
  
  output$plt_hourly_lineplot <- renderPlot({
      build_hourly_line_plot(madrid_complete)
  }, res = 100)
  

  output$members_table <- DT::renderDT({
    DT::datatable(team_members, options = list(pageLength = 8, dom = "tip"), rownames = FALSE)
  })

  output$download_filtered <- downloadHandler(
    filename = function() paste0("madrid_pollution_filtered_", Sys.Date(), ".csv"),
    content = function(file) readr::write_csv(filtered_aqi(), file)
  )
}
