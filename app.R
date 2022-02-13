library(shiny)
library(shinydashboard)
library(tidyverse)
library(deSolve)

## Methods
h0_nocleave <- function (t, x, params) {
    ## extract current state
    conc_uc1 <- x[1]
    
    ## rate
    d_uc1 <- params['k_tx'] - conc_uc1*params['k_dg']
    
    list(d_uc1)
}

h1_singleutr <- function (t, x, params) {
    ## extract current state
    conc_uc1  <- x[1]
    conc_mrna <- x[2]
    
    ## params
    k_tx <- params['k_tx']
    k_dg_uc <- params['k_dg_uc']
    k_pa <- params['k_pa']
    k_dg_mrna <- params['k_dg_mrna']
    
    ## rates
    d_uc1  <- k_tx - conc_uc1*(k_pa + k_dg_uc)
    d_mrna <- conc_uc1*k_pa - conc_mrna*k_dg_mrna
    
    list(c(d_uc1, d_mrna))
}

ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
        menuItem("Parameters", 
                 sliderInput("slider_tx", "k_transcribe", 
                             min=0, max=4, step=0.1, value=2,
                             width="100%"),
                 sliderInput("slider_dg_uc","k_degrade_UC", 
                             min=0, max=2, step=0.1, value=0.5,
                             width="100%"), 
                 sliderInput("slider_pa", "k_pA", 
                             min=0, max=4, step=0.1, value=1,
                             width="100%"),
                 sliderInput("slider_dg_mrna","k_degrade_mRNA", 
                             min=0, max=2, step=0.1, value=0.2,
                             width="100%")),
        menuItem("Initial Concentrations",
                 sliderInput("slider_uc1_0", "Initial [UC1]", 
                             min=0, max=10, step=0.1, value=0,
                             width="100%"),
                 sliderInput("slider_mrna_0","Initial [mRNA]", 
                             min=0, max=10, step=0.1, value=0,
                             width="100%")), 
        menuItem("Plot Parameters", icon=icon("stats"),
                 sliderInput("slider_time_max", "Final Time", 
                             min=1, max=20, step=1, value=10),
                            # width="100%"),
                 sliderInput("slider_range_y","y-Axis Range", 
                             min=0, max=10, step=0.1, value=c(0,10))),
                             #width="100%")), 
        width="40%"),
    dashboardBody(
        fluidRow(column(10, plotOutput('tx_plot')))
    ))

server <- function(input, output, session) { 
    output$tx_plot <- renderPlot({
        ## times
        ts <- seq(0, input$slider_time_max, length.out=500)
        
        df_sample <- ode(
            func=h1_singleutr,
            y=c(conc_UC1=input$slider_uc1_0, 
                conc_mRNA=input$slider_mrna_0),
            times=ts,
            parms=c(k_tx=input$slider_tx, 
                    k_dg_uc=input$slider_dg_uc, 
                    k_pa=input$slider_pa, 
                    k_dg_mrna=input$slider_dg_mrna)
        ) %>% 
            as.data.frame()
        
        #print(head(df_sample))
        
        df_sample %>%
            pivot_longer(-1, names_to="variable", values_to="values") %>%
            ggplot(aes(x=time, y=values, color=variable, group=variable)) +
            geom_line() + 
            scale_x_continuous() +
            scale_y_continuous(limits=input$slider_range_y) +
            theme_bw()
    })
}

shinyApp(ui, server)