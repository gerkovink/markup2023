#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggplot2)
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("binwidth",
                        "Number of bins:",
                        min = 1,
                        max = 10,
                        value = 30)
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
        # generate bins based on input$bins from ui.R
        #x    <- faithful[, 2]
        #bins <- seq(min(faithful[, 2]), max(faithful[, 2]), length.out = input$binwidth + 1)

        # draw the histogram with the specified number of bins
        #hist(x, breaks = bins, col = 'darkgray', border = 'white',
           #  xlab = 'Waiting time to next eruption (in mins)',
            # main = 'Histogram of waiting times')
        
        ggplot(faithful, (aes(x = waiting))) + 
          geom_histogram(binwidth = input$binwidth, fill = "darkgray", col = "white") +
          labs(x = 'Waiting time to next eruption (in mins)', title = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
