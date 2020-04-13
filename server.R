#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggplot2)
library(plotly)
library(shiny)
library(tidyverse)
library(deSolve)
library(lubridate)
library(ggpubr)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$gammaSlider <- renderUI({
        sliderInput("gammaInit",
                    "Initial recovery rate (gamma):",
                    min = round(1/42,3),
                    max = round(1/input$gammaUpper,3),
                    value = round(1/14,3))
    })
    
   
    output$sirplot <- renderPlot({
        
        generateSIR(input$N, input$betaInit, input$gammaInit, input$gammaUpper, input$period)

    },height=450)
    

    
    generateSIR <- function(susceptible, initBeta, initGamma,gammaUpper, period) {
        
        # SIR model
        # S - Susceptible, I - Infected, R - Removed (recovered + death)
        SIR <- function(time, state, parameters) {
            par <- as.list(c(state, parameters))
            with(par, {
                dS <- (-beta * I * S) / N
                dI <- ((beta * I * S) / N) - (gamma * I)
                dR <- gamma * I
                list(c(dS, dI, dR))
            })
        }
        
        # RSS 
        RSS <- function(parameters) {
            names(parameters) <- c("beta","gamma")
            out <- ode(y=init, times=Day, func=SIR, parms=parameters)
            fit <- out[,3]
            # sum(Active) - log(fit)^2)  # find min RSS on original scale 
            sum((log(Active) - log(fit))^2)  # find min RSS on log scale
        }
        
        mys_data <- read_csv("https://wnarifin.github.io/covid-19-malaysia/covid-19_my_full.csv") %>% select(1:10) %>% rename(Date = "date")  # uncomment
        
        # Apply the chosen percentage for the susceptible population
        N <- 32.68E6
        N <- N * (susceptible/100)
        
        # Based on the chosen period, set the start date and end date
        if(period=="Pre MCO") {
            name = "Pre MCO"
            start_date  <- "2020-03-01"
            end_date    <- "2020-03-17"
        } else if(period == "MCO Phase 1") {
            name = "MCO 1"
            start_date  <- "2020-03-18"
            end_date    <- "2020-03-31"
        } else if(period == "MCO Phase 2") {
            name = "MCO 2"
            start_date  <- "2020-04-01" 
            end_date    <- "2020-04-14"
        }

        
        Infected   <- mys_data %>% filter(Date>=ymd(start_date), Date<=ymd(end_date)) %>% pull(total_cases)
        Recovered  <- mys_data %>% filter(Date>=ymd(start_date), Date<=ymd(end_date)) %>% pull(total_recover)
        Death      <- mys_data %>% filter(Date>=ymd(start_date), Date<=ymd(end_date)) %>% pull(total_deaths)
        Active     <- Infected - Recovered - Death
        Date       <- mys_data %>% filter(Date>=ymd(start_date), Date<=ymd(end_date)) %>% pull(Date)
        Day        <- 1:(length(Date))
        
        # Initialization
        init <- c(S=N-Infected[1]-Recovered[1]-Death[1], I=Infected[1]-Death[1]-Recovered[1], R=Recovered[1]+Death[1])
        
        parameters_values <- c(initBeta, initGamma)  # seem no longer stuck at local minima if take middle values?
        parameters_values_lower <- c(1/14, 1/42)
        parameters_values_upper <- c(1, (1/gammaUpper))
        
        Opt <- optim(parameters_values, RSS, method = "L-BFGS-B", lower = parameters_values_lower, upper = parameters_values_upper)
        Opt_par <- setNames(Opt$par, c("beta", "gamma"))
        R0 <- (Opt_par['beta']/Opt_par['gamma']); names(R0) <- "R0"
        
        # time in days for predictions
        t = 1:max(Day)
        # get the fitted values from our SIR model
        fitted_projected <- data.frame(ode(y=init, times=t, func=SIR, parms=Opt_par))
        # add a Date & Active
        fitted_projected$Date = Date
        fitted_projected$A = Active
        tss = sum((log(fitted_projected$A) - mean(log(fitted_projected$A)))^2); tss
        rss = sum((log(fitted_projected$A) - log(fitted_projected$I))^2); rss
        R2 = 1 - (rss / tss); R2
        
        # last date to project
        last_date = "2020-09-30"
        # time in days for predictions
        t = 1:as.integer(ymd(last_date) + 1 - ymd(start_date))
        # get the fitted values from our SIR model
        fitted_projected <- data.frame(ode(y=init, times=t, func=SIR, parms=Opt_par))
        # add a Date & Active
        fitted_projected$Date = ymd(start_date) + days(t - 1)
        fitted_projected$A = c(Active, rep(NA, length(t) - length(Active)))
        head(fitted_projected, 10); tail(fitted_projected, 10)
        # date peak
        max_I = which(round(fitted_projected$I) == round(max(fitted_projected$I)))
        max_date = fitted_projected$Date[max_I]

        # color settings
        colors = c("Susceptible" = "black", "Recovered" = "#74C365", "Infectious" = "red", "Observed Active" = "orange")

        theme_set(theme_gray(base_size = 14))
        # plot projection data
        sirplot1 = ggplot(fitted_projected, aes(x = Date)) +
            geom_line(aes(y = I, color = "Infectious"),size=1) +
            geom_line(aes(y = S, color = "Susceptible"),size=1) +
            geom_line(aes(y = R, color = "Recovered"),size=1) +
            geom_point(aes(y = A, color = "Observed Active")) +
            scale_y_continuous(labels = scales::comma) +
            scale_x_date(date_breaks = "14 day", date_labels = "%d/%m/%y") +
            scale_colour_manual(values = colors) +
            labs(y = "Active Cases", title = paste("COVID-19 SIR model Malaysia,", name),
                 color = paste0("R square = ", round(R2,3), "\n",
                                "R0 = ", round(R0, 3), "\n",
                                "beta = ", round(Opt_par[1], 3), "\n",
                                "gamma = ", round(Opt_par[2], 3), "\n",
                                "Max Active = ", round(max(fitted_projected$I)), "\n")) +
            geom_vline(xintercept = as.numeric(as.Date(max_date)), linetype = "dotted") +
            annotate(geom = "text", x = as.Date(max_date)+20, y = N,
                     label = paste0("Peak on ", format(max_date, "%d/%m/%y")), angle = 0) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        # plot projection data, in log10
        sirplot1_log = ggplot(fitted_projected, aes(x = Date)) + 
            geom_line(aes(y = I, color = "Infectious"),size=1) + 
            geom_line(aes(y = S, color = "Susceptible"),size=1) + 
            geom_line(aes(y = R, color = "Recovered"),size=1) +
            geom_point(aes(y = A, color = "Observed Active")) +
            scale_y_log10(labels = scales::comma) +
            scale_x_date(date_breaks = "14 day", date_labels = "%d/%m/%y") + 
            scale_colour_manual(values = colors) +
            labs(y = "Active Cases", title = paste("COVID-19 SIR model Malaysia,", name, "log10"), 
                 color = paste0("R square = ", round(R2,3), "\n",
                                "R0 = ", round(R0, 3), "\n",
                                "beta = ", round(Opt_par[1], 3), "\n",
                                "gamma = ", round(Opt_par[2], 3), "\n",
                                "Max Active = ", round(max(fitted_projected$I)), "\n")) +
            geom_vline(xintercept = as.numeric(as.Date(max_date)), linetype = "dotted") +
            annotate(geom = "text", x = as.Date(max_date)+20, y = 1E5, 
                     label = paste0("Peak on ", format(max_date, "%d/%m/%y")), angle = 0) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
        
        
        ggarrange(sirplot1_log,sirplot1)
    }
   
})
