#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggplot2)
library(plotly)
library(shiny)
library(ggplot2)
library(plotly)
library(shiny)
library(tidyverse)
library(deSolve)
library(lubridate)
library(ggpubr)
library(shinycssloaders)
library(shinyWidgets)

# options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)
# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    
    # Application title
    
    titlePanel("SIR Modeling of Covid19 in Malaysia."),
    
    sidebarLayout(

        sidebarPanel(
            h4("Contributors:"),
            p("Chan Weng Howe (UTM), Wan Nor Arifin (USM)"),
            p(paste0("updated: ",today())),
            p(HTML("<br/>")),

            width="3",
            selectInput("period",
                        "Analysis period:",
                        c(paste0("MCO to date (18-Mar -",format(today()-1,"%d-%b"),")"),
                          "MCO Phase 2 (01-Apr - 14-Apr)", 
                          "MCO Phase 1 (18-Mar - 31-Mar)",
                          "Pre MCO (1-Mar - 17-Mar)")),
            # uiOutput("dateSelect"),
            # uiOutput('enddateSelect'),
            selectInput("scale",
                        "Display scale:",
                        c("Normal", "Log")),
            # sliderInput("N",
            #             "Susceptible (% of Malaysian population):",
            #             min = 1,
            #             max = 100,
            #             value = 30),
            # selectInput("type",
            #             "Estimation type:",
            #             c("Optimization", "Custom")),
            # sliderInput("betaInit",
            #             "Initial contact rate (beta):",
            #             min = round(1/100,3),
            #             max = round(1,3),
            #             value = round(1/2,3)),
            # sliderInput("gammaInit",
            #             "Initial recovery rate (gamma):",
            #             min = round(1/49,3),
            #             max = round(1,3),
            #             value = round(1/14,3)),
            # uiOutput("gammaSlider"),
            # uiOutput("betaSlider"),
            # numericInput('gammaUpper',
            #           'Upper Limit of recovery days:',
            #           value = 11)
            dateInput("projectPeriod",
                      "Projection until:",
                      min=today()+15,
                        value=today()+30)
        ),

        # Show a plot of the generated distribution

        mainPanel(
            width=9,
            # shinycssloaders::withSpinner(plotOutput('sirplot'), type = 3)
            addSpinner(shiny::plotOutput("sirplot"))
            # plotlyOutput("sirplot") # UNCOMMENT IF WANT TO OUTPUT AS PLOTLY
        )
    )
))
