#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    mys_data <- read_csv("https://wnarifin.github.io/covid-19-malaysia/covid-19_my_full.csv") %>% select(1:10) %>% rename(Date = "date")  # uncomment
    
    # output$gammaSlider <- renderUI({
    #     if(input$type=="Optimization") {
    #         sliderInput("gammaInit",
    #                 "Initial recovery rate (gamma):",
    #                 min = round(1/49,4),
    #                 max = round(1.000,4),
    #                 value = round(1/14,4))
    #     } else {
    #         sliderInput("gammaInit",
    #                 "Recovery rate (gamma):",
    #                 min = round(1/49,4),
    #                 max = round(1,4),
    #                 value = round(1/14,4))
    #     }
    # })
    # 
    # output$betaSlider <- renderUI({
    #     if(input$type=="Optimization") {
    #         sliderInput("betaInit",
    #                     "Initial contact rate (beta):",
    #                     min = round(1/100,4),
    #                     max = round(1,3),
    #                     value = round(1/2,4))
    #     } else {
    #         sliderInput("betaInit",
    #                     "Contact rate (beta):",
    #                     min = round(1/100,4),
    #                     max = round(1,4),
    #                     value = round(1/2,4))
    #     }
    # })
    # 
    # output$dateSelect <- renderUI({
    #     if(input$period=="Custom") {
    #         dateInput("startDate",
    #                   "Start Date",
    #                   min = "2020-03-01",
    #                   value="2020-03-01")
    #     }
    # })
    # 
    # output$enddateSelect <- renderUI({
    #     if(input$period=="Custom") {
    #         dateInput("endDate",
    #                   "Start Date",
    #                   min = "2020-03-07",
    #                   value="2020-03-07")
    #     }
    # })
   
    # UNCOMMENT IF WANT TO DISPLAY AS PLOTLY
    # output$sirplot <- renderPlotly({
    #     
    #     generateSIR(input$N, input$betaInit, input$gammaInit, input$gammaUpper, input$period, input$scale, input$startDate, input$endDate, input$type)
    # 
    # })
    
    output$sirplot <- renderPlot({
        
        # generateSIR(input$N, input$betaInit, input$gammaInit, input$gammaUpper, input$period, input$scale, input$startDate, input$endDate, input$type, input$projectPeriod)
        generateSIR(input$period, input$scale, input$projectPeriod)
        
    }, height=800)
    

    # generateSIR <- function(susceptible, initBeta, initGamma,gammaUpper, period, scale, startDate, endDate, type, projectEndDate) {
    generateSIR <- function(period, scale, projectEndDate) {
            
        # Definition of SIR model
        # S - Susceptible, I - Infected, R - Removed (recovered + death)
        SIR <- function(time, state, parameters) {
            par <- as.list(c(state, parameters))
            with(par, {
                dS <- (-beta * I * S) / n
                dI <- ((beta * I * S) / n) - (gamma * I)
                dR <- gamma * I
                list(c(dS, dI, dR))
            })
        }
        
        # RSS 
        RSS <- function(parameters) {
            names(parameters) <- c("beta","gamma")
            out <- ode(y=init, times=Day, func=SIR, parms=parameters)
            # fit <- out[,3]
            # sum(Active) - log(fit)^2)  # find min RSS on original scale 
            
            fit1 <- out[,3]
            fit2 <- out[,4]
            w1 = 4
            w2 = 1
            
            w1*sum((log(Active) - log(fit1))^2) + w2*sum((log(Removed)-log(fit2))^2)  # find min RSS on log scale
            # give weight to less well predicted curve.
        }
        
        # mys_data <- read_csv("https://wnarifin.github.io/covid-19-malaysia/covid-19_my_full.csv") %>% select(1:10) %>% rename(Date = "date")  # uncomment
        
        # Apply the chosen percentage for the susceptible population
        # N <- 6536
        # N <- N * (susceptible/100)
        
        # Based on the chosen period, set the start date and end date
        if(period=="Pre MCO (1-Mar - 17-Mar)") {
            name = "Pre MCO (1-Mar - 17-Mar)"
            start_date  <- "2020-03-01"
            end_date    <- "2020-03-17"
        } else if(period == "MCO Phase 1 (18-Mar - 31-Mar)") {
            name = "MCO Phase 1 (18-Mar - 31-Mar)"
            start_date  <- "2020-03-18"
            end_date    <- "2020-03-31"
        } else if(period == "MCO Phase 2 (01-Apr - 14-Apr)") {
            name = "MCO Phase 2 (01-Apr - 14-Apr)"
            start_date  <- "2020-04-01" 
            end_date    <- "2020-04-14"
        } else if(period == "Custom") {
            name = "Custom Range"
            start_date <- startDate
            end_date   <- endDate
        } else if(period == paste0("MCO to date (18-Mar -",format(today()-1,"%d-%b"),")")) {
            name = paste0("MCO to date (18-Mar -",format(today()-1,"%d-%b"),")")
            start_date <- "2020-03-18"
            end_date <- "2020-04-18"
        }

        
        Infected   <- mys_data %>% filter(Date>=ymd(start_date), Date<=ymd(end_date)) %>% pull(total_cases)
        Recovered  <- mys_data %>% filter(Date>=ymd(start_date), Date<=ymd(end_date)) %>% pull(total_recover)
        Death      <- mys_data %>% filter(Date>=ymd(start_date), Date<=ymd(end_date)) %>% pull(total_deaths)
        Active     <- Infected - Recovered - Death
        Removed    <- Recovered + Death
        Date       <- mys_data %>% filter(Date>=ymd(start_date), Date<=ymd(end_date)) %>% pull(Date)
        Day        <- 1:(length(Date))
        
        parameters_values       <- c(1/2, 1/14)  # seem no longer stuck at local minima if take middle values?
        parameters_values_lower <- c(1/100, 1/19) # days recover 6 weeks,  reduce bound bcs not many severe, observed show quick recovery
        parameters_values_upper <- c(1, 1/11) # updated to min 11 days
        
        # Placeholder to ind optimal susceptible population
        N_in_steps = data.frame(step=1:7, N=rep(NA,7), Loc=rep(NA,7))
        
        # Initial values;
        N           = 32.68E6/2  # 4th quarter, 2019
        Opt_min_loc = 1  # Optimum minimum n location in output vector
        step_i      = 1  # initialize step
        
        # Step 1-7
        for (step_i in 1:7) {
            cat("=== Step ", step_i, ": Finding optimal values of beta, gamma and n ===\n", sep ="")
            N = N[Opt_min_loc]  # Susceptible population from previous Step
            # initial susceptible population of Malaysia
            p_start = 0.1
            p_end = 2  # max p of population, also include up direction
            p_step = 0.1
            susceptible = seq(p_start, p_end, p_step)
            N = N * susceptible
            inits = data.frame(S=N-Infected[1]-Recovered[1]-Death[1], I=Infected[1]-Death[1]-Recovered[1], R=Recovered[1]+Death[1])
            Opts = vector("list", length(N))
            for (i in 1:length(N)) {
                n = N[i]
                init = setNames(as.numeric(inits[i,]), c("S", "I", "R"))
                Opt_ = optim(parameters_values, RSS, method = "L-BFGS-B", lower = parameters_values_lower, upper = parameters_values_upper)
                Opts[i]  = list(Opt_)
            }
            Opt_value = sapply(Opts, function(x) x$value)
            Opt_min_loc = which(Opt_value == min(Opt_value))
            N_in_steps[step_i, "N"] = N[Opt_min_loc]
            N_in_steps[step_i, "Loc"] = Opt_min_loc
            cat("=== Found n = ", N[Opt_min_loc], " at location ", Opt_min_loc, " in vector N ===\n\n", sep = "")
            step_i = step_i + 1
            if (step_i == 8) {
                cat("=== Finalizing results =====\n")
                cat("============================\n")
                print(N_in_steps)
            }
        }
        
        # Saving optimized parameters
        Opt = Opts[[Opt_min_loc]]
        Opt$message  # make sure converge
        Opt_par = setNames(Opt$par, c("beta", "gamma"))
        R0 = (Opt_par['beta']/Opt_par['gamma']); names(R0) = "R0"
        parameters_values_lower; parameters_values; parameters_values_upper  # just to check whether values in range
        cat("beta = ", Opt_par[['beta']], ", infectious contact rate (/person/day)\n",
            "gamma = ", Opt_par[['gamma']], ", recovery rate (/day)\n",
            "R_0 = ", R0, " number infected/person\n",
            "Recovery days = ", 1/Opt_par[['gamma']], " days",
            sep = "")
        
        
        
        # # Initialization
        # init <- c(S=N-Infected[1]-Recovered[1]-Death[1], I=Infected[1]-Death[1]-Recovered[1], R=Recovered[1]+Death[1])
        # 
        # 
        # 
        # if(type=="Optimization") {
        #     Opt <- optim(parameters_values, RSS, method = "L-BFGS-B", lower = parameters_values_lower, upper = parameters_values_upper)
        #     Opt_par <- setNames(Opt$par, c("beta", "gamma"))
        #     R0 <- (Opt_par['beta']/Opt_par['gamma']); names(R0) <- "R0"
        # } else {
        #     Opt_par <- vector()
        #     Opt_par['beta'] <- initBeta
        #     Opt_par['gamma'] <- initGamma
        #     R0 <- (Opt_par['beta']/Opt_par['gamma']); names(R0) <- "R0"
        # }
        
        # time in days for fitting
        t = 1:max(Day)
        n = N[Opt_min_loc]
        init = setNames(as.numeric(inits[Opt_min_loc,]), c("S", "I", "R"))
        # get the fitted values from our SIR model
        fitted_projected = data.frame(ode(y=init, times=t, func=SIR, parms=Opt_par))
        # add Date, Active, Removed
        fitted_projected$Date = Date
        fitted_projected$A = Active
        fitted_projected$Rm = Removed
        
        
        
        
        # # time in days for predictions
        # t = 1:max(Day)
        # # get the fitted values from our SIR model
        # fitted_projected <- data.frame(ode(y=init, times=t, func=SIR, parms=Opt_par))
        # # add a Date & Active
        # fitted_projected$Date = Date
        # fitted_projected$A = Active
        # tss = sum((log(fitted_projected$A) - mean(log(fitted_projected$A)))^2); tss
        # rss = sum((log(fitted_projected$A) - log(fitted_projected$I))^2); rss
        # R2 = 1 - (rss / tss); R2
        # 
        # # last date to project
        # last_date = projectEndDate
        # # time in days for predictions
        # t = 1:as.integer(ymd(last_date) + 1 - ymd(start_date))
        # # get the fitted values from our SIR model
        # fitted_projected <- data.frame(ode(y=init, times=t, func=SIR, parms=Opt_par))
        # # add a Date & Active
        # fitted_projected$Date = ymd(start_date) + days(t - 1)
        # fitted_projected$A = c(Active, rep(NA, length(t) - length(Active)))
        # head(fitted_projected, 10); tail(fitted_projected, 10)
        # # date peak
        # max_I = which(round(fitted_projected$I) == round(max(fitted_projected$I)))
        # max_date = fitted_projected$Date[max_I]

        tss1 = sum((log(fitted_projected$A) - mean(log(fitted_projected$A)))^2); tss1
        ## [1] 0.3902107
        rss1 = sum((log(fitted_projected$A) - log(fitted_projected$I))^2); rss1
        ## [1] 0.02557835
        R2_1 = 1 - (rss1 / tss1); R2_1
        ## [1] 0.9344499
        tss2 = sum((log(fitted_projected$Rm) - mean(log(fitted_projected$Rm)))^2); tss2
        ## [1] 17.12206
        rss2 = sum((log(fitted_projected$Rm) - log(fitted_projected$R))^2); rss2
        ## [1] 1.139206
        R2_2 = 1 - (rss2 / tss2); R2_2
        
        
        # last date to project
        last_date = projectEndDate
        # time in days for predictions
        t = 1:as.integer(ymd(last_date) + 1 - ymd(start_date))
        # get the fitted values from our SIR model
        fitted_projected = data.frame(ode(y=init, times=t, func=SIR, parms=Opt_par))
        # add add Date, Active, Removed
        fitted_projected$Date = ymd(start_date) + days(t - 1)
        fitted_projected$A = c(Active, rep(NA, length(t) - length(Active)))
        fitted_projected$Rm = c(Removed, rep(NA, length(t) - length(Active)))
        head(fitted_projected, 10); tail(fitted_projected, 10)
        
        # date of peak active cases
        # max_I = which(round(fitted_projected$I) == round(max(fitted_projected$I)))  # at times this works better
        max_I = which(fitted_projected$I == max(fitted_projected$I))
        max_date = fitted_projected$Date[max_I]
        # add cumulative infected cases
        fitted_projected$total_infected = fitted_projected$I + fitted_projected$R
        # predicted new cases today
        new_today = (fitted_projected[fitted_projected$Date == today(), ] - fitted_projected[fitted_projected$Date == today()-1, ])$total_infected
        # maximum cumulative cases, date. May add to plot.
        fitted_projected$Date[min(which(round(fitted_projected$total_infected) == max(round(fitted_projected$total_infected))))]
        
        # color settings
        colors = c("Susceptible" = "black", "Recovered" = "green", "Infectious" = "red", "Observed Active" = "orange", "Observed Recovered" = "blue")
        theme_set(theme_gray(base_size = 14))
        if(scale=="Normal") {
        # plot projection data
        
            # plot fit the data
            sirplot1 = ggplot(fitted_projected, aes(x = Date)) + 
                geom_line(aes(y = I, color = "Infectious")) + 
                geom_line(aes(y = S, color = "Susceptible")) + 
                geom_line(aes(y = R, color = "Recovered")) +
                geom_point(aes(y = A, color = "Observed Active")) +
                geom_point(aes(y = Rm, color = "Observed Recovered")) +
                scale_y_continuous(labels = scales::comma) +
                scale_x_date(date_breaks = "14 day", date_labels = "%d/%m/%y") + 
                scale_colour_manual(values = colors) +
                labs(y = "Number of cases", title = paste("COVID-19 SIR model Malaysia,", name), 
                     subtitle = paste("Projection from data:", start_date, "to", fitted_projected$Date[max(Day)]),
                     color = paste0("R square 1 = ", round(R2_1,3), "\n",
                                    "R square 2 = ", round(R2_2,3), "\n",
                                    "R0 = ", round(R0, 3), "\n",
                                    "beta = ", round(Opt_par[1], 3), "\n",
                                    "gamma = ", round(Opt_par[2], 3), "\n",
                                    "Susceptible = ", round(n), "\n",
                                    "Peak Active = ", round(max(fitted_projected$I)), "\n",
                                    "Maximum Total Infected = ", round(max(fitted_projected$total_infected)))) +
                geom_vline(xintercept = as.numeric(as.Date(max_date)), linetype = "dotted") +
                annotate(geom = "text", x = as.Date(max_date)+20, y = n*1.3, 
                         label = paste0("Peak on ", format(max_date, "%d/%m/%y")), angle = 0) +
                geom_vline(xintercept = as.numeric(as.Date(today())), linetype = "dotted", color = "red") +
                annotate(geom = "text", x = as.Date(today())+25, y = n*1.2, 
                         label = paste0("Today's Prediction (", format(today(), "%d/%m/%y"), ")\n",
                                        "Total Cases = ", round(fitted_projected[fitted_projected$Date == today(), "total_infected"]),
                                        "\nNew Cases = ", round(new_today)), angle = 0) +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
            
        # sirplot1 = ggplot(fitted_projected, aes(x = Date)) +
        #     geom_line(aes(y = I, color = "Infectious"),size=1) +
        #     geom_line(aes(y = S, color = "Susceptible"),size=1) +
        #     geom_line(aes(y = R, color = "Recovered"),size=1) +
        #     geom_point(aes(y = A, color = "Observed Active")) +
        #     scale_y_continuous(labels = scales::comma) +
        #     scale_x_date(date_breaks = "14 day", date_labels = "%d/%m/%y") +
        #     scale_colour_manual(values = colors) +
        #     labs(y = "Active Cases", title = paste("COVID-19 SIR model Malaysia,", name),
        #          color = paste0("R square = ", round(R2,3), "\n",
        #                         "R0 = ", round(R0, 3), "\n",
        #                         "beta = ", round(Opt_par[1], 3), "\n",
        #                         "gamma = ", round(Opt_par[2], 3), "\n",
        #                         "Max Active = ", round(max(fitted_projected$I)), "\n")) +
        #     geom_vline(xintercept = as.numeric(as.Date(max_date)), linetype = "dotted") +
        #     annotate(geom = "text", x = as.Date(max_date)+20, y = N,
        #              label = paste0("Peak on ", format(max_date, "%d/%m/%y")), angle = 0) +
        #     theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
            
        # UNCOMMENT FOLLOW TO ENABLE PLOTLY TYPE OF PLOT
        # ggplotly(sirplot1, height=600) %>%
        #     layout(
        #         title = list(
        #             text = paste0("COVID-19 SIR model Malaysia,", name, "\n\n\n\n",
        #                           "<sup>\t\t\tR square = ", round(R2,3), "\n",
        #                           "\t\t\tR0 = ", round(R0, 3), "\n",
        #                           "\t\t\tbeta = ", round(Opt_par[1], 3), "\n",
        #                           "\t\t\tgamma = ", round(Opt_par[2], 3), "\n",
        #                           "\t\t\tMax Active = ", round(max(fitted_projected$I)), "</sup>\n")
        #     ),
        #     legend = list(
        #         orientation = "h",
        #         y = -0.2,
        #         text="test-----"
        #     )
        # ) %>% config(displayModeBar = FALSE)
        
            
            
        } else {
        # plot projection data, in log10
        sirplot1 = ggplot(fitted_projected, aes(x = Date)) + 
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
        
        # UNCOMMENT FOLLOW TO ENABLE PLOTLY TYPE OF PLOT
        # ggplotly(sirplot1, height=600) %>%
        #     layout(
        #         title = list(
        #             text = paste0("COVID-19 SIR model Malaysia,", name, " log10", "\n\n\n\n",
        #                           "<sup>\t\t\tR square = ", round(R2,3), "\n",
        #                           "\t\t\tR0 = ", round(R0, 3), "\n",
        #                           "\t\t\tbeta = ", round(Opt_par[1], 3), "\n",
        #                           "\t\t\tgamma = ", round(Opt_par[2], 3), "\n",
        #                           "\t\t\tMax Active = ", round(max(fitted_projected$I)), "</sup>\n")
        #     ),
        #     legend = list(
        #         orientation = "h",
        #         y = -0.2,
        #         x = 0.25
        #     )
        # ) %>% config(displayModeBar = FALSE)
        }
        
        # UNCOMMENT IF WANT TO DISPLAY AS PLOTLY TYPE
        sirplot1
    }
   
})
