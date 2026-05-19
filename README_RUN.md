# Madrid Pollution Dashboard

Final Shiny dashboard package for the Madrid air pollution visualisation project.

## Run

1. Open this folder in RStudio.
2. Install required R packages if prompted:

```r
install.packages(c("shiny", "DT", "tidyverse", "lubridate", "scales", "fmsb", "plotly", "htmltools", "bslib", "shinyjs", "reticulate"))
```

3. Run:

```r
shiny::runApp()
```

## Edit group members

Edit `R/helpers.R`, then update the `team_members` data frame.

## Main files

- `ui.R` - dashboard layout, sidebar, tabs, and maximizer UI.
- `server.R` - output orchestration.
- `global.R` - packages, data loading, and sourcing plot scripts.
- `R/plots/` - separate plot code.
- `www/app.css` - visual styling.
