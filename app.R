library(shiny)
library(tidyverse)
library(patchwork)

ui <- fluidPage(
    
    # Application title
    titlePanel("Models of Discounting"),
    
    # sidebarLayout(
    #     sidebarPanel(
    #         numericInput("r",
    #                      "Exponential Discount Rate (r):",
    #                      min = 0,
    #                      max = 1,
    #                      value = .01,
    #                      step = .001),
    #         numericInput("k",
    #                      "Hyperbolic Discount Rate (k)",
    #                      min = 0,
    #                      max = 1,
    #                      value = .01,
    #                      step = .001),
    #         numericInput("LL",
    #                      "Larger Later",
    #                      min = 0,
    #                      max = 10000,
    #                      value = 100,
    #                      step = 10),
    #         numericInput("beta",
    #                      "beta",
    #                      min = 0,
    #                      max = 1,
    #                      value = .98,
    #                      step = .001),
    #         numericInput("delta",
    #                      "delta",
    #                      min = 0,
    #                      max = 1,
    #                      value = .98,
    #                      step = .001),
    #         numericInput("a",
    #                      "a",
    #                      min = 0,
    #                      max = 100,
    #                      value = .5,
    #                      step = .25),
    #         numericInput("b",
    #                      "b",
    #                      min = 0,
    #                      max = 10,
    #                      value = 1,
    #                      step = .05)
    #         
    #     ),
    #     
    #     # Show a plot of the generated distribution
    #     mainPanel(
    #         plotOutput("scatPlot")
    #     )
    # )
    
    fluidRow(
        column(3,
               h4("Exponential"),
               withMathJax(
                   '$$\\frac{1}{(1 + r)^D}$$'),
               numericInput("r",
                            "Exponential Discount Rate (r):",
                            min = 0,
                            max = 1,
                            value = .01,
                            step = .001),               
               checkboxInput('include_exponential', 'Include', value = TRUE)
        ),
        column(3,
               h4("Hyperbolic"),
               numericInput("k",
                            "Hyperbolic Discount Rate (k)",
                            min = 0,
                            max = 1,
                            value = .01,
                            step = .001),
               checkboxInput('include_hyperbolic', 'Include', value = TRUE)
        ),
        column(3,
               h4("Quasi-Hyperbolic"),
               numericInput("beta",
                            "beta",
                            min = 0,
                            max = 1,
                            value = .98,
                            step = .001),
               numericInput("delta",
                            "delta",
                            min = 0,
                            max = 1,
                            value = .98,
                            step = .001),
               checkboxInput('include_quasi', 'Include', value = TRUE)
        ),
        column(3,
               h4("Constant-Sensitivity"),
               numericInput("a",
                            "a",
                            min = 0,
                            max = 100,
                            value = .5,
                            step = .25),
               numericInput("b",
                            "b",
                            min = 0,
                            max = 10,
                            value = 1,
                            step = .05),
               checkboxInput('include_constant', 'Include', value = TRUE)
        )
    ),
    hr(),
    
    plotOutput("model_plot")
    
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    calc_exponential <- function(V, D, r) {
        V / (1 + r)^D
    }
    
    # hyperbolic
    calc_hyperbolic <- function(V, D, k) {
        V / (1 + k*D)
    }
    
    calc_quasi_hyperbolic <- function(V, D, beta, delta) {
        V * (beta * delta^D)
    }
    
    constant_sensitivity <- function(V, D, a, b) {
        V * exp(-(a*D)^b)
    }
    
    
    stat_exponential <- reactive(stat_function(mapping = aes(color = 'Exponential'), fun = calc_exponential, size = 1.5, args = list(V = 100, r = input$r), alpha = .5))
    stat_hyperbolic <- reactive(stat_function(mapping = aes(color = 'Hyperbolic'), fun = calc_hyperbolic, size = 1.5, args = list(V = 100, k = input$k), alpha = .5))
    stat_quasihyp <- reactive(stat_function(mapping = aes(color = 'Quasi-Hyperbolic'), fun = calc_quasi_hyperbolic, size = 1.5, args = list(V = 100, beta = input$beta, delta = input$delta), alpha = .5))
    stat_cs <- reactive(stat_function(mapping = aes(color = 'Constant Sensitivity'), fun = constant_sensitivity, size = 1.5, args = list(V = 100, a = input$a, b = input$b), alpha = .5))
    
    output$model_plot <- renderPlot({
        ggplot(data.frame(x=c(0, 360)), aes(x = x)) +
            stat_exponential() +
            stat_hyperbolic() +
            stat_quasihyp() + 
            stat_cs() +
            scale_x_continuous('Delay of Reward (D)') +
            scale_y_continuous('Present-Day Value') +
            theme_bw() +
            theme(text = element_text(size = 16, face = 'bold')) +
            labs(title = 'Hyperbolic Discounting: Value of $100 as a function of delay (D) for different discount models')
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
