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

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("SIR Modeling of Covid19 in Malaysia"),

    sidebarLayout(
        
        sidebarPanel(
            h4("Contributors:"),
            p("Chan Weng Howe (UTM), Wan Nor Arifin (USM)"),
            p("updated: 13-Apr-2020"),
            p(HTML("<br/>")),
            
            width="3",
            selectInput("period",
                        "Analysis period:",
                        c("Pre MCO", "MCO Phase 1", "MCO Phase 2")),
            sliderInput("N",
                        "Susceptible (% of Malaysian population):",
                        min = 1,
                        max = 100,
                        value = 30),
            sliderInput("betaInit",
                        "Initial contact rate (beta):",
                        min = round(1/100,3),
                        max = round(1,3),
                        value = round(1/2,3)),
            sliderInput("gammaInit",
                        "Initial recovery rate (gamma):",
                        min = round(1/49,3),
                        max = round(1/11,3),
                        value = round(1/14,3)),
            # uiOutput("gammaSlider"),
            numericInput('gammaUpper',
                      'Upper Limit of recovery days:',
                      value = 11)
        ),

        # Show a plot of the generated distribution

        mainPanel(
            plotOutput("sirplot")
        )
    )
))
