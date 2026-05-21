nav_item <- function(label, icon, section, active = FALSE) {
  tags$a(
    class = paste("sidebar-link", if (active) "active" else ""),
    href = "#", `data-section` = section,
    onclick = sprintf("return goToSection(this, '%s');", section),
    tags$span(icon), tags$span(label)
  )
}

hero_card <- function(icon, title, text) {
  tags$div(class = "hero-card", tags$span(icon), tags$div(tags$strong(title), tags$p(text)))
}

ui <- bslib::page_fillable(
  theme = bslib::bs_theme(version = 5),
  shinyjs::useShinyjs(),

  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$link(rel = "stylesheet", href = "app.css"),
    tags$title("Madrid Air Pollution Visualization Platform"),
    tags$script(HTML('
      function setActiveSidebar(el){
        document.querySelectorAll(".sidebar-link").forEach(x => x.classList.remove("active"));
        if(el) el.classList.add("active");
      }
      function closeMaximizedCards(){
        document.querySelectorAll(".plot-card.maximized").forEach(card => {
          card.classList.remove("maximized");
          const btn = card.querySelector(".maximize-btn");
          if(btn) btn.innerText = "Maximize";
        });
        document.body.classList.remove("has-maximized");
      }
      function goToSection(el, section){
        setActiveSidebar(el); closeMaximizedCards();
        if(window.Shiny) Shiny.setInputValue("nav_to", section, {priority: "event"});
        const main = document.querySelector(".app-main");
        if(main) main.scrollTop = 0;
        window.scrollTo(0, 0);
        setTimeout(() => window.dispatchEvent(new Event("resize")), 300);
        return false;
      }
      document.addEventListener("click", ev => {
        const btn = ev.target.closest(".maximize-btn");
        if(!btn) return;
        const card = btn.closest(".plot-card");
        if(!card) return;
        const active = card.classList.toggle("maximized");
        document.body.classList.toggle("has-maximized", active);
        btn.innerText = active ? "Minimize" : "Maximize";
        setTimeout(() => window.dispatchEvent(new Event("resize")), 300);
      });
      document.addEventListener("keydown", ev => {
        if(ev.key === "Escape"){
          closeMaximizedCards();
          setTimeout(() => window.dispatchEvent(new Event("resize")), 300);
        }
      });
    '))
  ),

  tags$div(
    class = "app-shell",

    tags$aside(
      class = "app-sidebar",
      tags$div(class = "sidebar-brand", tags$img(src = "img/logo.png", alt = "Logo")),
      tags$div(class = "sidebar-user", tags$span("👤"), tags$span("Pollution Platform")),
      tags$nav(
        class = "sidebar-nav",
        nav_item("Home", "🏠", "home", TRUE),
        nav_item("Indicators", "📊", "indicators"),
        nav_item("Python Map", "🗺️", "python"),
        nav_item("Group Members", "👥", "members")
      ),
      tags$div(
        class = "sidebar-footer",
        tags$a(
          href = "#", class = "sidebar-back-home",
          onclick = "return goToSection(document.querySelector('.sidebar-link'), 'home');",
          tags$span("↩"), tags$span("Back Home")
        )
      )
    ),

    tags$main(
      class = "app-main",
      tags$div(class = "topbar", tags$h3("Madrid Air Pollution")),
      tags$div(
        class = "app-content",

        conditionalPanel(
          condition = "!input.nav_to || input.nav_to == 'home'",
          tags$section(
            class = "home-section",
            tags$div(
              class = "hero",
              tags$div(class = "hero-overlay"),
              tags$div(
                class = "hero-content",
                tags$span(class = "badge-pill", "Madrid Air Quality"),
                tags$h1("Air Pollution Visualization Platform"),
                tags$p("Explore AQI patterns, station trends, radar comparisons, and spatial views."),
                tags$div(
                  class = "hero-cards",
                  hero_card("📊", "Visualize Indicators", "Compare pollutants and monitor changes over time."),
                  hero_card("🔎", "Filter and Explore", "Focus on pollutant, year, station, and AQI patterns."),
                  hero_card("🧭", "Maximize Views", "Open any chart in full screen for deeper interpretation.")
                )
              )
            )
          )
        ),

        conditionalPanel(
            condition = "input.nav_to == 'indicators'",
            tags$section(
                class = "content-section",
                
                # Top filter panel
                tags$div(
                    class = "filter-panel",
                    tags$div(
                        class = "filter-item",
                        selectInput("pollutant", "Choose Pollutant",
                                    pollutant_choices,
                                    selected = "All")
                    ),
                   
                    tags$div(
                        class = "filter-item download-filter",
                        downloadButton("download_filtered",
                                       "Download Filtered Data",
                                       class = "btn btn-success")
                    )
                ),
                
                uiOutput("kpi_cards"),
                
                tags$div(
                    class = "plot-grid two",
                    
                    make_plot_card(
                        "heatmap",
                        "AQI Calendar Heatmap",
                        plotOutput("plt_heatmap_over_time", height = "430px"),
                        tagList(helpText("Use pollutant filters to update the heatmap."))
                    ),
                    
                    make_plot_card(
                        "lineplot",
                        "Pollution Evolution Over the Years",
                        plotOutput("plt_lineplot_stations", height = "430px"),
                        tagList(helpText("Displays the most polluted stations over time."))
                    )
                ),
                
                # Radar section
                tags$div(
                    class = "plot-grid one",
                    
                    make_plot_card(
                        "radar",
                        "Pollution Across Stations",
                        
                        tagList(
                            tags$div(
                                class = "filter-panel",
                                tags$div(
                                    class = "filter-item",
                                    sliderInput(
                                        inputId = "radar_year",
                                        label = "Radar Year",
                                        min = min(as.numeric(as.character(madrid_complete$year)), na.rm = TRUE),
                                        max = max(as.numeric(as.character(madrid_complete$year)), na.rm = TRUE),
                                        value = 2008,
                                        step = 1,
                                        sep = ""
                                    )
                                )
                            ),
                            
                            plotOutput("plt_radar_over_time", height = "560px"),
                            
                            helpText("Change the year from the filter panel.")
                        )
                    )
                )
            )
        ),

        conditionalPanel(
          condition = "input.nav_to == 'python'",
          tags$section(
            class = "content-section",
            tags$div(class = "section-title", tags$h2("Python Integrated Spatial Visualization"), tags$p("Spatial analysis")),
            make_plot_card("pythonmap", "Madrid Pollution Spatial Visualization", uiOutput("madrid_pollution_plot"), tagList(helpText("Use internal controls to switch pollutant variables.")))
          )
        ),

        conditionalPanel(
          condition = "input.nav_to == 'members'",
          tags$section(
            class = "content-section",
            tags$div(class = "section-title", tags$h2("Group Members")),
            tags$div(class = "data-card", DTOutput("members_table"))
          )
        )
      )
    )
  )
)
