prepare_python_component <- function() {
  html_file <- file.path(app_dir, "www", "generated", "madrid_pollution.html")
  if (file.exists(html_file)) return(html_file)
  py_dir <- file.path(app_dir, "pycodes")
  data_path <- file.path(app_dir, "VDS2526_Madrid")
  dir.create(dirname(html_file), recursive = TRUE, showWarnings = FALSE)
  ok <- TRUE
  tryCatch({
    reticulate::py_require(c("pandas", "plotly", "numpy"))
    viz <- reticulate::import_from_path("madrid_pollution_plot", path = py_dir, convert = TRUE)
    viz$save_figure_html(data_dir = data_path, output_html = html_file)
  }, error = function(e) {
    ok <<- FALSE
    writeLines(paste0("<html><body style='font-family:Arial;padding:30px'><h3>Python visualization not generated</h3><p>", htmltools::htmlEscape(conditionMessage(e)), "</p><p>Install Python packages from requirements.txt and restart the app.</p></body></html>"), html_file)
  })
  html_file
}
