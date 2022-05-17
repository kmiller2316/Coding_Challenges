library(dygraphs)
library(lubridate)
library(xts)
library(readxl)
library(shiny)
library(dplyr)
Soccer_Scaled <- read_xlsx("Fall_Soccer_Scaled.csv")
Soccer_Scaled$Date <- ymd(Soccer_Scaled$Date)
Soccer_Scaled$Athlete <- "filler"
for (i in 1:length(unique(Soccer_Scaled$Player.Name))) {
    athlete <- unique(Soccer_Scaled$Player.Name)[i]
    Soccer_Scaled$Athlete[Soccer_Scaled$Player.Name == athlete] <- paste("Athlete", i, sep="_")
}

# Define UI for application that displays created player metrics and output
ui <- fluidPage(

    # Application title
    titlePanel("Player Performance"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            dateRangeInput("daterange",
                           "Choose the Date Range:",
                           start = "2020-07-01",
                           end = "2020-12-01",
                           min = "2020-03-01",
                           max = "2020-12-01",
                           format = "mm/dd/yyyy"),
            selectInput("player",
                        "Select Player to Review:",
                        choices = unique(Soccer_Scaled$Athlete)),
            submitButton()
            # ,checkboxInput("variables",
            #               "Include All Variables in the Table?")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           h4("This dashboard showcases Player Performance for a Women's Soccer Team. The data is scaled to show individual 
              player performance, performance compared to their teammates for the day, and a rolling average. The summary statistics
              below the graph will update via the date range selection on the sidebar.", align = "center"),
           h5("Directions: To use this app, you should adjust the date range preferred and select the athlete you would like to view.
              Once this is selected, press 'Apply Changes' and the app will update to show the new filters. The graph is dynamic and 
              will provide additional information via hovering with the mouse. Rolling Average can be adjusted via the number on the 
              bottom left of the graph and graph date range can be motified via the sliders on the bottom of the graph.", 
              align = "center"),
           dygraphOutput("performancePlot"),
           verbatimTextOutput("summaryStats")
        )
    )
)

# Define server logic required to display graphs to make output
server <- function(input, output) {
    
    output$performancePlot <- renderDygraph({
        # generate player subset based on inputs from ui
        sub <- subset(Soccer_Scaled, Athlete == input$player)
        don <- xts(x = sub[,69:71], order.by = sub$Date)
        # adjust dates if needed
        date1 <- input$daterange[1] 
        date2 <- input$daterange[2]
        date <- paste(date1,date2,sep = "/")
        don <- don[date]

        # generate the plot based on the code
        p <- dygraph(don) %>%
            dyOptions(labelsUTC = TRUE, fillGraph=TRUE, fillAlpha=0.1, drawGrid = FALSE, colors=c("#FF8200", "#58595B", "#006C93")) %>%
            dyLegend(show = "onmouseover", labelsSeparateLines = T) %>%
            dyRangeSelector() %>%
            dyCrosshair(direction = "vertical") %>%
            dyHighlight(highlightCircleSize = 5, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
            dyRoller(rollPeriod = 1)
        p
    })
    
    V <- reactive({
        p <- input$player
        d1 <- input$daterange[1] 
        d2 <- input$daterange[2]
        s1 <- subset(Soccer_Scaled, Athlete == p)
        s2 <- subset(s1, Date >= d1 & Date <= d2)
        return(s2)
    })
    
    output$summaryStats <- renderPrint({
        summary(V()[,69:71])
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
