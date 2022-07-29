library(shiny)
library(unpdata)
library(suncalc)
library(plotrix)
library(sonicscrewdriver)


sites <- sites()
year <- as.POSIXlt(Sys.Date())$year + 1900

ui <- fluidPage(
  titlePanel("UNP sites: Daylight Information"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("date",
                  "Date:",
                  min = as.Date(paste0(year,"-01-01"),"%Y-%m-%d"),
                  max = as.Date(paste0(year,"-12-31"),"%Y-%m-%d"),
                  value=Sys.Date(),
                  timeFormat="%Y-%m-%d",
                  step = 3,
                  animate = animationOptions(interval = 250,loop=TRUE)
      ),
      selectInput("loc",
                  "Location:",
                  sites$names,
                  selected = sites$names[1],
                  multiple = FALSE,
                  selectize = TRUE,
                  width = NULL,
                  size = NULL
      ),
      checkboxGroupInput("times",
                         "Time of day:",
                         choices = c(
                           "Sunrise",
                           "Sunset",
                           "Solar Noon",
                           "Civil Twilight",
                           "Nautical Twilight",
                           "Astronomical Twilight",
                           "Night",
                           "Nadir"
                         ),
                         selected = c(
                           "Sunrise",
                           "Sunset",
                           "Civil Twilight",
                           "Nautical Twilight",
                           "Astronomical Twilight",
                           "Night"
                         ),
      ),
      selectInput("display",
                  "Display:",
                  c("Main", "Core", 'Ring'),
                  selected = "Main",
                  multiple = FALSE,
                  selectize = TRUE,
                  width = NULL,
                  size = NULL
      ),
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    lat <- as.numeric(sites[sites$names==input$loc,]$lat)
    lon <- as.numeric(sites[sites$names==input$loc,]$lon)

    if (input$display == "Main") {
      inner = 0
      outer = 2
    }
    if (input$display == "Core") {
      inner = 0
      outer = 1
    }
    if (input$display == "Ring") {
      inner = 1.75
      outer = 2
    }
    dielPlot(as.POSIXct(input$date),lat,lon,c(inner,outer),input$times)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
