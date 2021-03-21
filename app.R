library(shiny)
library(tidyverse)
library(patchwork)

ui <- fluidPage(
    
    # Application title
    titlePanel("Prospect Theory Parameters"),
    
    sidebarLayout(
        sidebarPanel(
            numericInput("r",
                         "Exponential Discount Rate (r):",
                         min = 0,
                         max = 1,
                         value = .01,
                         step = .001),
            numericInput("k",
                         "Hyperbolic Discount Rate (k)",
                         min = 0,
                         max = 1,
                         value = .01,
                         step = .001),
            numericInput("LL",
                         "Larger Later",
                         min = 0,
                         max = 10000,
                         value = 100,
                         step = 10),
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
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("scatPlot")
        )
    )
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
    
    stat_exponential <- reactive(stat_function(mapping = aes(color = 'Exponential'), fun = calc_exponential, size = 1.5, args = list(V = input$LL, r = input$r), alpha = .5))
    stat_hyperbolic <- reactive(stat_function(mapping = aes(color = 'Hyperbolic'), fun = calc_hyperbolic, size = 1.5, args = list(V = input$LL, k = input$k), alpha = .5))
    stat_quasihyp <- reactive(stat_function(mapping = aes(color = 'Quasi-Hyperbolic'), fun = calc_quasi_hyperbolic, size = 1.5, args = list(V = input$LL, beta = input$beta, delta = input$delta), alpha = .5))
    
    output$scatPlot <- renderPlot({
        ggplot(data.frame(x=c(0, 360)), aes(x = x)) +
            stat_exponential() +
            stat_hyperbolic() +
            stat_quasihyp() +
            scale_x_continuous('Delay of Reward (D)') +
            scale_y_continuous('Present-Day Value') +
            theme_bw() +
            theme(text = element_text(size = 16, face = 'bold')) +
            labs(title = 'Hyperbolic Discounting: Value of $50 as a\nfunction of delay (D) and discount rate (r)')
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
