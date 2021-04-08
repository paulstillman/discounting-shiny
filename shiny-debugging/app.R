library(shiny)
library(tidyverse)
library(patchwork)

ui <- fluidPage(
    checkboxInput('include_exponential', 'Exponential', value = TRUE),
    checkboxInput('include_hyperbolic', "Hyperbolic", value = TRUE),
    checkboxInput('include_quasi', 'Quasi-Hyperbolic', value = TRUE),
    checkboxInput('include_constant', 'Constant-Sensitivity', value = TRUE),
    
    plotOutput("model_plot")
)

server <- function(input, output) {
    # model functions
    calc_exponential <- function(V, D, r) {
        V / exp(r*D)
    }
    calc_hyperbolic <- function(V, D, k) {
        V / (1 + k*D)
    }
    calc_quasi_hyperbolic <- function(V, D, beta, delta) {
        V * (beta * delta^D)
    }
    constant_sensitivity <- function(V, D, a, b) {
        V * exp(-(a*D)^b)
    }
    
    # functions to add to ggplot (placed within `reactive` since they usually also vary as a function of user input)
    # stat_exponential <- reactive(stat_function(color = 'red', fun = calc_exponential, args = list(V = 100, r = .02)))
    # stat_hyperbolic <- reactive(stat_function(color = 'blue', fun = calc_hyperbolic,args = list(V = 100, k = .02)))
    # stat_quasihyp <- reactive(stat_function(color = 'green', fun = calc_quasi_hyperbolic, args = list(V = 100, beta = .99, delta = .99)))
    # stat_cs <- reactive(stat_function(color = 'black', fun = constant_sensitivity, args = list(V = 100, a = .5, b = 1)))    
    
    stat_exponential <- reactive(stat_function(mapping = aes(color = 'Exponential'), fun = calc_exponential, args = list(V = 100, r = .02)))
    stat_hyperbolic <- reactive(stat_function(mapping = aes(color = 'Hyperbolic'), fun = calc_hyperbolic,args = list(V = 100, k = .02)))
    stat_quasihyp <- reactive(stat_function(mapping = aes(color = 'Quasi-Hyperbolic'), fun = calc_quasi_hyperbolic, args = list(V = 100, beta = .99, delta = .99)))
    stat_cs <- reactive(stat_function(mapping = aes(color = 'Constant Sensitivity'), fun = constant_sensitivity, args = list(V = 100, a = .5, b = 1)))
    
    
    output$model_plot <- renderPlot({
        p_base <- ggplot(data.frame(x=c(0, 360)), aes(x = x)) +
            theme_bw() +
            theme(text = element_text(size = 16, face = 'bold'))+
            labs(title = 'Value of $100 as a function of delay (D) for different discount models')
        
        if(input$include_exponential) p_base <- p_base + stat_exponential()
        if(input$include_hyperbolic) p_base <- p_base + stat_hyperbolic()
        if(input$include_quasi) p_base <- p_base + stat_quasihyp()
        if(input$include_constant) p_base <- p_base + stat_cs()
        
        p_base + scale_color_manual(values = c('Exponential' = rainbow(4)[1], 'Hyperbolic' = rainbow(4)[2], 'Quasi-Hyperbolic' = rainbow(4)[3], 'Constant Sensitivity' = rainbow(4)[4]))
    })
}

# Define server logic required to draw a histogram
# server <- function(input, output) {
#     # model functions
#     calc_exponential <- function(V, D, r) {
#         V / exp(r*D)
#     }
#     calc_hyperbolic <- function(V, D, k) {
#         V / (1 + k*D)
#     }
#     calc_quasi_hyperbolic <- function(V, D, beta, delta) {
#         V * (beta * delta^D)
#     }
#     constant_sensitivity <- function(V, D, a, b) {
#         V * exp(-(a*D)^b)
#     }
#     
#     # functions to add to ggplot (placed within `reactive` since they usually also vary as a function of user input)
#     # stat_exponential <- reactive(stat_function(mapping = aes(color = 'Exponential'), fun = calc_exponential, args = list(V = 100, r = .02)))
#     # stat_hyperbolic <- reactive(stat_function(mapping = aes(color = 'Hyperbolic'), fun = calc_hyperbolic,args = list(V = 100, k = .02)))
#     # stat_quasihyp <- reactive(stat_function(mapping = aes(color = 'Quasi-Hyperbolic'), fun = calc_quasi_hyperbolic, args = list(V = 100, beta = .99, delta = .99)))
#     # stat_cs <- reactive(stat_function(mapping = aes(color = 'Constant Sensitivity'), fun = constant_sensitivity, args = list(V = 100, a = .5, b = 1)))
#     
#     stat_exponential <- reactive(stat_function(color = 'red', fun = calc_exponential, args = list(V = 100, r = .02)))
#     stat_hyperbolic <- reactive(stat_function(color = 'blue', fun = calc_hyperbolic,args = list(V = 100, k = .02)))
#     stat_quasihyp <- reactive(stat_function(color = 'green', fun = calc_quasi_hyperbolic, args = list(V = 100, beta = .99, delta = .99)))
#     stat_cs <- reactive(stat_function(color = 'black', fun = constant_sensitivity, args = list(V = 100, a = .5, b = 1)))
#     
#     
#     output$model_plot <- renderPlot({
#         
#         p_base <- ggplot(data.frame(x=c(0, 360)), aes(x = x)) +
#             theme_bw() +
#             theme(text = element_text(size = 16, face = 'bold'))+
#             labs(title = 'Value of $100 as a function of delay (D) for different discount models')
#         
#         if(input$include_exponential) p_base <- p_base + stat_exponential()
#         if(input$include_hyperbolic) p_base <- p_base + stat_hyperbolic()
#         if(input$include_quasi) p_base <- p_base + stat_quasihyp()
#         if(input$include_constant) p_base <- p_base + stat_cs()
#         
#         p_base
#     })
# }

# Run the application 
shinyApp(ui = ui, server = server)
