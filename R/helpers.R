`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b

make_plot_card <- function(id, title, body, extra_controls = NULL) {
  tags$div(
    id = paste0("card_", id),
    class = "plot-card",
    tags$div(
      class = "plot-card-header",
      tags$h4(class = "plot-card-title", title),
      tags$button(type = "button", class = "maximize-btn", "Maximize")
    ),
    tags$div(class = "plot-card-extra max-only", extra_controls),
    tags$div(class = "plot-card-body", body)
  )
}

empty_plot <- function(message = "No data available for the selected filters.") {
  plot.new(); text(0.5, 0.5, message, cex = 1.3, col = "#52616b")
}

team_members <- data.frame(
  Name = c("Silas Ooko", "Ermioni", "Steve Omollo", "Pallavi"),
  StudentNumber = c("2501260", "ADMN", "ADMN", "ADMN"),
  stringsAsFactors = FALSE
)
