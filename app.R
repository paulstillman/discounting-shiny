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
               h3(checkboxInput('include_exponential', 'Exponential', value = TRUE)),
        ),
        column(3,
               h3(checkboxInput('include_hyperbolic', "Hyperbolic", value = TRUE)),
        ),
        column(3,
               h3(checkboxInput('include_quasi', 'Quasi-Hyperbolic', value = TRUE)),
        ),
        column(3,
               # h4("Constant-Sensitivity"),
               h3(checkboxInput('include_constant', 'Constant-Sensitivity', value = TRUE)),
        )
    ),
    
    fluidRow(
        column(3,
               withMathJax('$$e^{-r \\cdot D}$$'),
        ),
        column(3,
               withMathJax('$$\\frac{1}{1 + k \\cdot D}$$'),
        ),
        column(3,
               withMathJax('$$\\beta \\cdot \\delta^D$$'),
        ),
        column(3,
               withMathJax('$$ e^{-(a \\cdot D)^b}$$'),
               # checkboxInput('include_constant', 'Include', value = TRUE)
        )
    ),
    
    fluidRow(
        column(3,
               numericInput("r",
                            "Exponential Discount Rate (r)",
                            min = 0,
                            max = 1,
                            value = .01,
                            step = .001)               
        ),
        column(3,
               numericInput("k",
                            "Hyperbolic Discount Rate (k)",
                            min = 0,
                            max = 1,
                            value = .01,
                            step = .001)
        ),
        column(3,
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
                            step = .001)
        ),
        column(3,
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
        )
    ),
    
    
    hr(),
    
    plotOutput("model_plot")
    
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    calc_exponential <- function(V, D, r) {
        V / exp(r*D)
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
        
        # stat_exponential <- reactive(stat_function(mapping = aes(color = 'Exponential'), fun = calc_exponential, size = 1.5, args = list(V = 100, r = input$r), alpha = .5))
        # stat_hyperbolic <- reactive(stat_function(mapping = aes(color = 'Hyperbolic'), fun = calc_hyperbolic, size = 1.5, args = list(V = 100, k = input$k), alpha = .5))
        # stat_quasihyp <- reactive(stat_function(mapping = aes(color = 'Quasi-Hyperbolic'), fun = calc_quasi_hyperbolic, size = 1.5, args = list(V = 100, beta = input$beta, delta = input$delta), alpha = .5))
        # stat_cs <- reactive(stat_function(mapping = aes(color = 'Constant Sensitivity'), fun = constant_sensitivity, size = 1.5, args = list(V = 100, a = input$a, b = input$b), alpha = .5))
        
        
        p_base <- ggplot(data.frame(x=c(0, 360)), aes(x = x)) +
            scale_x_continuous('Delay of Reward (D)') +
            scale_y_continuous('Present-Day Value') +
            theme_bw() +
            theme(text = element_text(size = 16, face = 'bold'),
                  legend.position = 'right') +
            labs(title = 'Hyperbolic Discounting: Value of $100 as a function of delay (D) for different discount models')
        
        if(input$include_exponential) p_base <- p_base + stat_exponential()
        if(input$include_hyperbolic) p_base <- p_base + stat_hyperbolic()
        if(input$include_quasi) p_base <- p_base + stat_quasihyp()
        if(input$include_constant) p_base <- p_base + stat_cs()
        
        p_base + scale_color_manual(values = c('Exponential' = rainbow(4)[1], 'Hyperbolic' = rainbow(4)[2], 'Quasi-Hyperbolic' = rainbow(4)[3], 'Constant Sensitivity' = rainbow(4)[4]))
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
