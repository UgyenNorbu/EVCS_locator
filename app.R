library(shiny)
library(leaflet)
library(readr)
library(dplyr)
library(bslib)

# ── Colour tokens ────────────────────────────────────────────────────────────
COL_GREEN        <- "#1B5E35"
COL_GREEN_LIGHT  <- "#E8F5EE"
COL_AMBER        <- "#D4890A"
COL_GREY_MARKER  <- "#9EADB0"
COL_BG           <- "#F7F8F5"
COL_SIDEBAR_BG   <- "#FFFFFF"
COL_TEXT         <- "#2C3A3A"
COL_TEXT_MUTED   <- "#6B7C7C"
COL_BORDER       <- "#DDE4E4"

# ── Custom CSS ────────────────────────────────────────────────────────────────
custom_css <- "
  @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');

  * { box-sizing: border-box; }

  body {
    font-family: 'Inter', sans-serif;
    background-color: #F7F8F5;
    color: #2C3A3A;
    font-size: 14px;
  }

  /* ── App header ── */
  .app-header {
    background-color: #1B5E35;
    color: #FFFFFF;
    padding: 14px 24px;
    display: flex;
    align-items: center;
    gap: 12px;
    border-bottom: 3px solid #145228;
  }
  .app-header .header-icon {
    font-size: 20px;
  }
  .app-header h1 {
    font-size: 17px;
    font-weight: 600;
    margin: 0;
    letter-spacing: 0.01em;
  }
  .app-header .header-sub {
    font-size: 11px;
    font-weight: 400;
    opacity: 0.75;
    margin-left: auto;
    letter-spacing: 0.03em;
    text-transform: uppercase;
  }

  /* ── Layout ── */
  .app-body {
    display: flex;
    height: calc(100vh - 55px);
    overflow: hidden;
  }

  /* ── Sidebar ── */
  .app-sidebar {
    width: 280px;
    min-width: 280px;
    background: #FFFFFF;
    border-right: 1px solid #DDE4E4;
    display: flex;
    flex-direction: column;
    padding: 20px 16px;
    gap: 20px;
    overflow-y: auto;
  }

  .sidebar-section-label {
    font-size: 10px;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.08em;
    color: #6B7C7C;
    margin-bottom: 8px;
  }

  /* ── Selector ── */
  .selectize-control .selectize-input {
    border: 1px solid #DDE4E4 !important;
    border-radius: 6px !important;
    font-family: 'Inter', sans-serif !important;
    font-size: 13px !important;
    color: #2C3A3A !important;
    padding: 8px 10px !important;
    box-shadow: none !important;
  }
  .selectize-control .selectize-input.focus {
    border-color: #1B5E35 !important;
    box-shadow: 0 0 0 3px rgba(27,94,53,0.12) !important;
  }
  .selectize-dropdown {
    font-family: 'Inter', sans-serif !important;
    font-size: 13px !important;
    border: 1px solid #DDE4E4 !important;
    border-radius: 6px !important;
    box-shadow: 0 4px 12px rgba(0,0,0,0.08) !important;
  }
  .selectize-dropdown .option.selected,
  .selectize-dropdown .option:hover {
    background-color: #E8F5EE !important;
    color: #1B5E35 !important;
  }
  label {
    font-size: 10px !important;
    font-weight: 600 !important;
    text-transform: uppercase !important;
    letter-spacing: 0.08em !important;
    color: #6B7C7C !important;
    margin-bottom: 6px !important;
  }

  /* ── Stat card ── */
  .stat-card {
    background: #F7F8F5;
    border: 1px solid #DDE4E4;
    border-left: 4px solid #1B5E35;
    border-radius: 8px;
    padding: 16px;
  }
  .stat-card .stat-label {
    font-size: 10px;
    font-weight: 600;
    text-transform: uppercase;
    letter-spacing: 0.08em;
    color: #6B7C7C;
    margin-bottom: 6px;
  }
  .stat-card .stat-number {
    font-size: 38px;
    font-weight: 700;
    color: #1B5E35;
    line-height: 1;
    margin-bottom: 2px;
  }
  .stat-card .stat-sublabel {
    font-size: 12px;
    color: #6B7C7C;
  }

  /* ── Status badge row ── */
  .status-row {
    display: flex;
    flex-direction: column;
    gap: 8px;
  }
  .status-item {
    display: flex;
    align-items: center;
    gap: 8px;
    font-size: 12px;
    color: #2C3A3A;
  }
  .status-dot {
    width: 10px;
    height: 10px;
    border-radius: 50%;
    flex-shrink: 0;
  }
  .status-dot.working      { background-color: #1B5E35; }
  .status-dot.construction { background-color: #D4890A; }
  .status-dot.other        { background-color: #9EADB0; }
  .status-count {
    margin-left: auto;
    font-weight: 600;
    color: #2C3A3A;
  }

  /* ── Divider ── */
  .sidebar-divider {
    height: 1px;
    background: #DDE4E4;
    margin: 0;
  }

  /* ── About block ── */
  .about-block {
    font-size: 12px;
    color: #6B7C7C;
    line-height: 1.6;
  }
  .about-block strong {
    color: #2C3A3A;
    font-weight: 500;
  }

  /* ── Legend ── */
  .legend-block {
    display: flex;
    flex-direction: column;
    gap: 6px;
  }

  /* ── Map panel ── */
  .app-main {
    flex: 1;
    position: relative;
  }
  #map {
    width: 100% !important;
    height: 100% !important;
  }

  /* ── Leaflet popup polish ── */
  .leaflet-popup-content-wrapper {
    border-radius: 8px !important;
    font-family: 'Inter', sans-serif !important;
    font-size: 13px !important;
    box-shadow: 0 4px 16px rgba(0,0,0,0.12) !important;
  }
  .leaflet-popup-content {
    margin: 12px 14px !important;
    line-height: 1.6 !important;
  }
  .popup-name {
    font-weight: 600;
    font-size: 14px;
    color: #2C3A3A;
    margin-bottom: 4px;
  }
  .popup-row {
    font-size: 12px;
    color: #6B7C7C;
  }
  .popup-status-working      { color: #1B5E35; font-weight: 500; }
  .popup-status-construction { color: #D4890A; font-weight: 500; }

  /* Override bslib defaults */
  .container-fluid { padding: 0 !important; }
  .shiny-html-output { margin: 0; }
"

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
    tags$head(
        tags$style(HTML(custom_css))
    ),
    # Header
    tags$div(class = "app-header",
             tags$span(class = "header-icon", "\U26A1"),
             tags$h1("EV Charging Station Locator"),
             tags$span(class = "header-sub", "Department of Surface Transport")
    ),
    # Body
    tags$div(class = "app-body",
             # Sidebar
             tags$div(class = "app-sidebar",
                      # Dzongkhag selector
                      tags$div(
                          tags$div(class = "sidebar-section-label", "Filter by location"),
                          selectInput("dzongkhag", NULL,
                                      choices  = c("All Dzongkhags" = "All"),
                                      selected = "All",
                                      width    = "100%")
                      ),
                      # Stat card
                      tags$div(class = "stat-card",
                               tags$div(class = "stat-label", "Charging stations"),
                               uiOutput("station_count"),
                               tags$div(class = "stat-sublabel", uiOutput("station_location_label"))
                      ),
                      # # Status breakdown
                      # tags$div(
                      #   tags$div(class = "sidebar-section-label", "Status breakdown"),
                      #   uiOutput("status_breakdown")
                      # ),
                      tags$div(class = "sidebar-divider"),
                      # About
                      tags$div(class = "about-block",
                               tags$strong("About this map"), tags$br(),
                               "Live locations of public EV charging stations across Bhutan. Use the dropdown to filter by Dzongkhag."
                      )
             ),
             # Map
             tags$div(class = "app-main",
                      leafletOutput("map", height = "100%", width = "100%")
             )
    )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {
    
    # Load data
    stations <- reactive({
        read_csv("./EVCS_coordinates - EVCS Coordinates.csv", show_col_types = FALSE)
    })
    
    # Populate selector
    observe({
        df <- stations()
        choices <- c("All Dzongkhags" = "All", sort(unique(df$Dzongkhag)))
        updateSelectInput(session, "dzongkhag", choices = choices, selected = "All")
    })
    
    # Filtered data for selected Dzongkhag
    filtered <- reactive({
        df  <- stations()
        sel <- input$dzongkhag
        if (is.null(sel) || sel == "All") df else filter(df, Dzongkhag == sel)
    })
    
    # Station count
    output$station_count <- renderUI({
        n <- nrow(filtered())
        tags$div(class = "stat-number", n)
    })
    
    output$station_location_label <- renderUI({
        sel <- input$dzongkhag
        if (is.null(sel) || sel == "All") {
            "across all Dzongkhags"
        } else {
            paste0("in ", sel, " Dzongkhag")
        }
    })
    
    # Status breakdown
    # output$status_breakdown <- renderUI({
    #   df <- filtered()
    #   working      <- sum(df$Status == "Working",              na.rm = TRUE)
    #   construction <- sum(df$Status == "Under construction",   na.rm = TRUE)
    #   other        <- nrow(df) - working - construction
    #   
    #   items <- list(
    #     tags$div(class = "status-item",
    #              tags$div(class = "status-dot working"),
    #              tags$span("Operational"),
    #              tags$span(class = "status-count", working)
    #     ),
    #     tags$div(class = "status-item",
    #              tags$div(class = "status-dot construction"),
    #              tags$span("Under construction"),
    #              tags$span(class = "status-count", construction)
    #     )
    #   )
    #   if (other > 0) {
    #     items <- c(items, list(
    #       tags$div(class = "status-item",
    #                tags$div(class = "status-dot other"),
    #                tags$span("Other / unknown"),
    #                tags$span(class = "status-count", other)
    #       )
    #     ))
    #   }
    #   tags$div(class = "status-row", items)
    # })
    # 
    # Popup builder (vectorised — called with ~ inside leaflet)
    make_popup <- function(name, facility, status) {
        status_class <- ifelse(status == "Working", "popup-status-working", "popup-status-construction")
        paste0(
            "<div class='popup-name'>", name, "</div>",
            "<div class='popup-row'>", facility, "</div>",
            "<div class='popup-row'>Status: <span class='", status_class, "'>", status, "</span></div>"
        )
    }
    
    # Base map
    output$map <- renderLeaflet({
        df <- stations()
        leaflet(df) %>%
            addProviderTiles(providers$CartoDB.Positron) %>%
            setView(lng = mean(df$long, na.rm = TRUE),
                    lat = mean(df$lat,  na.rm = TRUE),
                    zoom = 8)
    })
    
    # Update markers on selection change
    observe({
        df  <- stations()
        sel <- input$dzongkhag
        
        proxy <- leafletProxy("map", data = df) %>% clearMarkers()
        
        if (is.null(sel) || sel == "All") {
            # All stations, colour by status
            working      <- filter(df, Status == "Working")
            construction <- filter(df, Status == "Under construction")
            
            if (nrow(working) > 0) {
                proxy <- addAwesomeMarkers(proxy,
                                           data   = working, lng = ~long, lat = ~lat,
                                           icon   = awesomeIcons(icon = 'fa-plug', iconColor = 'white',
                                                                 markerColor = 'green', library = 'fa'),
                                           popup  = ~make_popup(Station_name, Facility, Status),
                                           label  = ~Station_name
                )
            }
            if (nrow(construction) > 0) {
                proxy <- addAwesomeMarkers(proxy,
                                           data   = construction, lng = ~long, lat = ~lat,
                                           icon   = awesomeIcons(icon = 'fa-wrench', iconColor = 'white',
                                                                 markerColor = 'orange', library = 'fa'),
                                           popup  = ~make_popup(Station_name, Facility, Status),
                                           label  = ~Station_name
                )
            }
            proxy %>% setView(lng = mean(df$long, na.rm = TRUE),
                              lat = mean(df$lat,  na.rm = TRUE), zoom = 8)
            
        } else {
            non_sel <- filter(df, Dzongkhag != sel)
            sel_df  <- filter(df, Dzongkhag == sel)
            
            # Greyed-out background markers
            if (nrow(non_sel) > 0) {
                proxy <- addCircleMarkers(proxy,
                                          data = non_sel, lng = ~long, lat = ~lat,
                                          radius = 5, fillColor = "#9EADB0", color = "#9EADB0",
                                          fillOpacity = 0.3, opacity = 0.3, weight = 1,
                                          popup = ~make_popup(Station_name, Facility, Status),
                                          label = ~Station_name
                )
            }
            
            # Highlighted selected markers
            if (nrow(sel_df) > 0) {
                sel_working      <- filter(sel_df, Status == "Working")
                sel_construction <- filter(sel_df, Status == "Under construction")
                
                if (nrow(sel_working) > 0) {
                    proxy <- addAwesomeMarkers(proxy,
                                               data  = sel_working, lng = ~long, lat = ~lat,
                                               icon  = awesomeIcons(icon = 'fa-plug', iconColor = 'white',
                                                                    markerColor = 'green', library = 'fa'),
                                               popup = ~make_popup(Station_name, Facility, Status),
                                               label = ~Station_name
                    )
                }
                if (nrow(sel_construction) > 0) {
                    proxy <- addAwesomeMarkers(proxy,
                                               data  = sel_construction, lng = ~long, lat = ~lat,
                                               icon  = awesomeIcons(icon = 'fa-wrench', iconColor = 'white',
                                                                    markerColor = 'orange', library = 'fa'),
                                               popup = ~make_popup(Station_name, Facility, Status),
                                               label = ~Station_name
                    )
                }
                
                # Pan / zoom to selection
                if (nrow(sel_df) == 1) {
                    proxy %>% setView(lng = sel_df$long[1], lat = sel_df$lat[1], zoom = 14)
                } else {
                    bounds <- sel_df %>%
                        summarise(min_lat = min(lat), max_lat = max(lat),
                                  min_lng = min(long), max_lng = max(long))
                    pad_lat <- max((bounds$max_lat - bounds$min_lat) * 0.25, 0.05)
                    pad_lng <- max((bounds$max_lng - bounds$min_lng) * 0.25, 0.05)
                    proxy %>% fitBounds(
                        lng1 = bounds$min_lng - pad_lng, lat1 = bounds$min_lat - pad_lat,
                        lng2 = bounds$max_lng + pad_lng, lat2 = bounds$max_lat + pad_lat
                    )
                }
            }
        }
    })
}

shinyApp(ui = ui, server = server) 