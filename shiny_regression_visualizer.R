library(shiny)
library(ggplot2)
library(plotly)

ui <- fluidPage(
  titlePanel("Simple Linear Regression"),
  
  fluidRow(
    column(3,
           selectInput("xvar", "X Variable:", choices = names(mtcars), selected = "wt")),
    column(3,
           selectInput("yvar", "Y Variable:", choices = names(mtcars), selected = "mpg")),
    column(3,
           checkboxInput("showLine", "Regression Line", TRUE)),
    column(3,
           sliderInput("alpha", "Point Opacity", min = 0.1, max = 1, value = 0.8))
  ),
  
  fluidRow(
    column(4,
           radioButtons("lineType", "Line Type:",
                        choices = c("Linear" = "lm", "LOESS" = "loess"),
                        selected = "lm"))
  ),
  
  br(),
  
  fluidRow(
    column(12,
           plotlyOutput("regPlot", height = "500px"))
  ),
  
  br(),
  
  fluidRow(
    column(12, align = "center",
           h4(textOutput("regSummary")))
  )
)

server <- function(input, output) {
  
  model <- reactive({
    formula <- as.formula(paste(input$yvar, "~", input$xvar))
    lm(formula, data = mtcars)
  })
  
  output$regPlot <- renderPlotly({
    p <- ggplot(mtcars, aes_string(x = input$xvar, y = input$yvar)) +
      geom_point(color = "green", alpha = input$alpha) +
      labs(title = paste("Regression:", input$yvar, "vs", input$xvar),
           x = input$xvar, y = input$yvar)
    
    if (input$showLine) {
      p <- p + geom_smooth(method = input$lineType, color = "red", se = TRUE)
    }
    
    ggplotly(p)
  })
  
  output$regSummary <- renderText({
    fit <- model()
    coef <- round(coef(fit), 2)
    r2 <- round(summary(fit)$r.squared, 3)
    paste("y = ", coef[1], "+", coef[2], "* x", "   |   R² =", r2)
  })
}

shinyApp(ui = ui, server = server)
