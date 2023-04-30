#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for the app
ui <- fluidPage(
  titlePanel("Binomial Distribution"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "n:", min = 1, max = 50, value = 10),
      sliderInput("p", "p:", min = 0, max = 1, value = 0.5, step = 0.1)
    ),
    mainPanel(
      plotOutput("plot"),
      verbatimTextOutput("text")
    )
  )
)

# Define server logic for the app
server <- function(input, output) {
  # Generate binomial distribution with parameters n and p
  output$plot <- renderPlot({
    x <- 0:input$n
    y <- dbinom(x, size = input$n, prob = input$p)
    barplot(y, names.arg = x, xlab = "X", ylab = "Probability", main = "Binomial Distribution", col = "#BCCCBA", border = "black")
  })
  
  # Calculate mean and variance of the binomial distribution
  output$text <- renderPrint({
    mean <- input$n * input$p
    var <- input$n * input$p * (1 - input$p)
    cat("Mean: ", mean, "\n")
    cat("Variance: ", var, "\n")
  })
}

# Run the app
shinyApp(ui = ui, server = server)

