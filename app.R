library(shiny)
library(shinydashboard)
library(tidyverse)
library(deSolve)

## Methods
m0_nocleave <- function (t, x, params) {
    ## extract current state
    conc_uc1 <- x[1]
    
    ## rate
    d_uc1 <- params['k_tx'] - conc_uc1*params['k_dg']
    
    list(d_uc1)
}

m1_singleutr <- function (t, x, params) {
    ## extract current state
    conc_uc1  <- x[1]
    conc_mrna <- x[2]
    conc_total <- x[3]
    
    ## params
    k_tx <- params['k_tx']
    k_dg_uc <- params['k_dg_uc']
    k_pa <- params['k_pa']
    k_dg_mrna <- params['k_dg_mrna']
    
    ## rates
    d_uc1   <- k_tx - conc_uc1*(k_pa + k_dg_uc)
    d_mrna  <- conc_uc1*k_pa - conc_mrna*k_dg_mrna
    d_total <- k_tx
    
    list(c(d_uc1, d_mrna, d_total))
}

m2_twoutr <- function (t, x, params) {
    ## extract current state
    conc_uc1   <- x[1]
    conc_uc2   <- x[2]
    conc_su    <- x[3]
    conc_lu    <- x[4]
    conc_total <- x[5]
    
    ## params
    k_tx <- params['k_tx']
    k_uc1_uc2 <- params['k_uc1_uc2']
    k_su <- params['k_su']
    k_lu <- params['k_lu']
    k_dg_uc <- params['k_dg_uc']
    k_dg_su <- params['k_dg_su']
    k_dg_lu <- params['k_dg_lu']
    
    ## rates
    d_uc1   <- k_tx - conc_uc1*(k_su + k_uc1_uc2 + k_dg_uc)
    d_uc2   <- conc_uc1*k_uc1_uc2 - conc_uc2*(k_su + k_lu + k_dg_uc)
    d_su  <- conc_uc1*k_su + conc_uc2*k_su - conc_su*k_dg_su
    d_lu  <- conc_uc2*k_lu - conc_lu*k_dg_lu
    d_total <- k_tx
    
    list(c(d_uc1, d_uc2, d_su, d_lu, d_total))
}

ui <- dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
        menuItem("Parameters", 
                 sliderInput("slider_tx", "k_transcribe", 
                             min=0, max=4, step=0.1, value=2,
                             width="100%"),
                 sliderInput("slider_uc1_uc2", "k_UC1_UC2", 
                             min=0, max=10, step=0.1, value=1,
                             width="100%"),
                 sliderInput("slider_su", "k_SU", 
                             min=0, max=4, step=0.1, value=1,
                             width="100%"),
                 sliderInput("slider_lu", "k_LU", 
                             min=0, max=4, step=0.1, value=1,
                             width="100%"),
                 sliderInput("slider_dg_uc","k_degrade_UC", 
                             min=0, max=2, step=0.1, value=0.5,
                             width="100%"), 
                 sliderInput("slider_dg_su","k_degrade_SU", 
                             min=0, max=2, step=0.1, value=0.5,
                             width="100%"), 
                 sliderInput("slider_dg_lu","k_degrade_LU", 
                             min=0, max=2, step=0.1, value=0.5,
                             width="100%")),
        menuItem("Initial Concentrations",
                 sliderInput("slider_uc1_0", "Initial [UC1]", 
                             min=0, max=10, step=0.1, value=0,
                             width="100%"),
                 sliderInput("slider_uc2_0", "Initial [UC2]", 
                             min=0, max=10, step=0.1, value=0,
                             width="100%"),
                 sliderInput("slider_su_0","Initial [SU]", 
                             min=0, max=10, step=0.1, value=0,
                             width="100%"),
                 sliderInput("slider_lu_0","Initial [LU]", 
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
    # dashboardSidebar( # single utr
    #     menuItem("Parameters", 
    #              sliderInput("slider_tx", "k_transcribe", 
    #                          min=0, max=4, step=0.1, value=2,
    #                          width="100%"),
    #              sliderInput("slider_dg_uc","k_degrade_UC", 
    #                          min=0, max=2, step=0.1, value=0.5,
    #                          width="100%"), 
    #              sliderInput("slider_pa", "k_pA", 
    #                          min=0, max=4, step=0.1, value=1,
    #                          width="100%"),
    #              sliderInput("slider_dg_mrna","k_degrade_mRNA", 
    #                          min=0, max=2, step=0.1, value=0.2,
    #                          width="100%")),
    #     menuItem("Initial Concentrations",
    #              sliderInput("slider_uc1_0", "Initial [UC1]", 
    #                          min=0, max=10, step=0.1, value=0,
    #                          width="100%"),
    #              sliderInput("slider_mrna_0","Initial [mRNA]", 
    #                          min=0, max=10, step=0.1, value=0,
    #                          width="100%")), 
    #     menuItem("Plot Parameters", icon=icon("stats"),
    #              sliderInput("slider_time_max", "Final Time", 
    #                          min=1, max=20, step=1, value=10),
    #              # width="100%"),
    #              sliderInput("slider_range_y","y-Axis Range", 
    #                          min=0, max=10, step=0.1, value=c(0,10))),
    #     #width="100%")), 
    #     width="40%"),
    dashboardBody(
        fluidRow(column(10, plotOutput('tx_plot'))),
        fluidRow(column(10, plotOutput('ratio_plot'))))
    )

server <- function(input, output, session) { 
    ode_sample <- reactive({
        ## times
        ts <- seq(0, input$slider_time_max, length.out=500)
        
        df_sample <- ode(
            func=m2_twoutr,
            y=c(conc_UC1=input$slider_uc1_0, 
                conc_UC2=input$slider_uc2_0, 
                conc_SU=input$slider_su_0, 
                conc_LU=input$slider_lu_0, 
                conc_total=0),
            times=ts,
            parms=c(k_tx=input$slider_tx, 
                    k_uc1_uc2=input$slider_uc1_uc2, 
                    k_su=input$slider_su, 
                    k_lu=input$slider_lu, 
                    k_dg_uc=input$slider_dg_uc,
                    k_dg_su=input$slider_dg_su,
                    k_dg_lu=input$slider_dg_lu)
        ) %>% 
            as.data.frame()
    })
    
    output$tx_plot <- renderPlot({
        ode_sample() %>%
            pivot_longer(-1, names_to="variable", values_to="values") %>%
            ggplot(aes(x=time, y=values, color=variable, group=variable)) +
            geom_line() + 
            scale_x_continuous() +
            scale_y_continuous(limits=input$slider_range_y) +
            theme_bw()
    })
    
    output$ratio_plot <- renderPlot({
        ode_sample() %>%
            ggplot(aes(x=time)) +
            geom_line(aes(y=conc_SU/(conc_SU+conc_LU))) + 
            scale_x_continuous() +
            scale_y_continuous(limits=c(0,1)) +
            labs(x="Time (AU)", y="SUI") +
            theme_bw()
    })
}

shinyApp(ui, server)