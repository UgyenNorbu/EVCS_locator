library(shiny)
library(leaflet)
library(readr)
library(dplyr)

ui <- fluidPage(
    theme = bslib::bs_theme(bootswatch = "cerulean"),
    br(),
    titlePanel("EV Charging Station Locator"),
    br(),
    
    # Add selector for Dzongkhag
    sidebarLayout(
        sidebarPanel(
            selectInput("dzongkhag",
                        "Select Dzongkhag:",
                        choices = c("All", ""),  # Will be populated from data
                        selected = "All",
                        width = "250px"),
            br(),
            tabPanel("About",
                     h5("About this App"),
                     p("This application was created to provide reliable locations of public charging stations in the Bhutan.")
            )
        ),

        mainPanel(
            leafletOutput("map", height = 600)
        )
    )
)

server <- function(input, output, session) {
    # Read the CSV file
    stations <- reactive({
        df <- read_csv("./EVCS_coordinates - EVCS Coordinates.csv")
        return(df)
    })
    
    # Update Dzongkhag choices when data is loaded
    observe({
        df <- stations()
        dzongkhag_choices <- c("All", sort(unique(df$Dzongkhag)))
        updateSelectInput(session, "dzongkhag", 
                          choices = dzongkhag_choices,
                          selected = "All")
    })
    
    output$map <- renderLeaflet({
        df <- stations()
        
        # Create base map
        leaflet(df) %>%
            addTiles() %>%
            setView(lng = mean(df$long), lat = mean(df$lat), zoom = 8)
    })
    
    # Update markers when Dzongkhag selection changes
    observe({
        df <- stations()
        selected_dzongkhag <- input$dzongkhag
        
        leafletProxy("map", data = df) %>%
            clearMarkers() %>%
            {
                if (selected_dzongkhag == "All" || is.null(selected_dzongkhag)) {
                    # Show all markers with color coding
                    working_stations <- df %>% filter(Status == "Working")
                    construction_stations <- df %>% filter(Status == "Under construction")
                    
                    # Add working stations (green)
                    if (nrow(working_stations) > 0) {
                        addAwesomeMarkers(.,
                                          data = working_stations,
                                          lng = ~long, 
                                          lat = ~lat,
                                          icon = awesomeIcons(
                                              icon = 'fa-plug',
                                              iconColor = 'white',
                                              markerColor = 'green',
                                              library = 'fa'
                                          ),
                                          popup = ~paste0(
                                              "<strong>", Station_name, "</strong><br/>",
                                              "Facility: ", Facility, "<br/>",
                                              "Status: <span style='color: green;'>", Status, "</span><br/>"
                                          ),
                                          label = ~Station_name
                        )
                    }
                    
                    # Add construction stations (red)
                    if (nrow(construction_stations) > 0) {
                        addAwesomeMarkers(.,
                                          data = construction_stations,
                                          lng = ~long, 
                                          lat = ~lat,
                                          icon = awesomeIcons(
                                              icon = 'fa-wrench',
                                              iconColor = 'white',
                                              markerColor = 'red',
                                              library = 'fa'
                                          ),
                                          popup = ~paste0(
                                              "<strong>", Station_name, "</strong><br/>",
                                              "Facility: ", Facility, "<br/>",
                                              "Status: <span style='color: red;'>", Status, "</span><br/>"
                                          ),
                                          label = ~Station_name
                        )
                    }
                    
                    setView(., lng = mean(df$long), lat = mean(df$lat), zoom = 8)
                } else {
                    # Filter stations
                    non_selected <- df %>% filter(Dzongkhag != selected_dzongkhag)
                    selected <- df %>% filter(Dzongkhag == selected_dzongkhag)
                    
                    # Calculate bounds for selected stations
                    if (nrow(selected) > 0) {
                        bounds <- selected %>%
                            summarise(
                                min_lat = min(lat),
                                max_lat = max(lat),
                                min_lng = min(long),
                                max_lng = max(long)
                            )
                        
                        # Add padding to bounds
                        lat_padding <- (bounds$max_lat - bounds$min_lat) * 0.2
                        lng_padding <- (bounds$max_lng - bounds$min_lng) * 0.2
                        
                        # If only one station, use fixed zoom
                        if (nrow(selected) == 1) {
                            center_lat <- selected$lat[1]
                            center_lng <- selected$long[1]
                            zoom_level <- 14
                        } else {
                            center_lat <- mean(selected$lat)
                            center_lng <- mean(selected$long)
                            zoom_level <- NULL  # Will use fitBounds instead
                        }
                    }
                    
                    # First add grey markers for non-selected
                    if (nrow(non_selected) > 0) {
                        addCircleMarkers(.,
                                         data = non_selected,
                                         lng = ~long, 
                                         lat = ~lat,
                                         radius = 6,
                                         fillColor = "grey",
                                         color = "grey",
                                         fillOpacity = 0.3,
                                         opacity = 0.3,
                                         popup = ~paste0(
                                             "<strong>", Station_name, "</strong><br/>",
                                             "Facility: ", Facility, "<br/>",
                                             "Status: ", Status, "<br/>"
                                         ),
                                         label = ~Station_name
                        )
                    }
                    
                    # Then add highlighted markers for selected
                    if (nrow(selected) > 0) {
                        # Separate by status
                        selected_working <- selected %>% filter(Status == "Working")
                        selected_construction <- selected %>% filter(Status == "Under construction")
                        
                        # Add working stations (green)
                        if (nrow(selected_working) > 0) {
                            addAwesomeMarkers(.,
                                              data = selected_working,
                                              lng = ~long, 
                                              lat = ~lat,
                                              icon = awesomeIcons(
                                                  icon = 'fa-plug',
                                                  iconColor = 'white',
                                                  markerColor = 'green',
                                                  library = 'fa'
                                              ),
                                              popup = ~paste0(
                                                  "<strong>", Station_name, "</strong><br/>",
                                                  "Facility: ", Facility, "<br/>",
                                                  "Status: <span style='color: green;'>", Status, "</span><br/>"
                                              ),
                                              label = ~Station_name
                            )
                        }
                        
                        # Add construction stations (red)
                        if (nrow(selected_construction) > 0) {
                            addAwesomeMarkers(.,
                                              data = selected_construction,
                                              lng = ~long, 
                                              lat = ~lat,
                                              icon = awesomeIcons(
                                                  icon = 'fa-wrench',
                                                  iconColor = 'white',
                                                  markerColor = 'red',
                                                  library = 'fa'
                                              ),
                                              popup = ~paste0(
                                                  "<strong>", Station_name, "</strong><br/>",
                                                  "Facility: ", Facility, "<br/>",
                                                  "Status: <span style='color: red;'>", Status, "</span><br/>"
                                              ),
                                              label = ~Station_name
                            )
                        }
                        
                        # Set view
                        if (nrow(selected) == 1) {
                            # For single station, use setView
                            setView(., lng = center_lng, lat = center_lat, zoom = zoom_level)
                        } else {
                            # For multiple stations, use fitBounds
                            fitBounds(.,
                                      lng1 = bounds$min_lng - lng_padding,
                                      lat1 = bounds$min_lat - lat_padding,
                                      lng2 = bounds$max_lng + lng_padding,
                                      lat2 = bounds$max_lat + lat_padding
                            )
                        }
                    }
                }
            }
    })
}

shinyApp(ui = ui, server = server)