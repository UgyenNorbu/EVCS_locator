library(shiny)

ui <- fluidPage(
  navbarPage(
    'EV Charging Station Locator', 
    column(width = 3, 
           selectInput('dz', "Select Dzongkhag", choices = c("A", "B")), 
           h5("About this App"),
           p("This application was created to provide reliable locations of public charging stations in the Bhutan."), 
           tabPanel("About",
                    h5("About this App"),
                    p("This application was created to provide reliable locations of public charging stations in the Bhutan.")
           )), 
    column(width = 9, 
           plotOutput('plot')), 
  )
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)