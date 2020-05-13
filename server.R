
function(input, output, session) {
  power20 <- reactive({ # Function to get sample size and power
    rand <- 1
    delta <- 1
    sd <- 10
    ss_plan <- uniroot(solver, c(1, 1e6),
                       power_target = input$power20 / 100,
                       rand = rand,
                       delta = delta, sd = sd,
                       alpha = input$sig20)$root
    ss_actual <- ss_plan * input$frac20 / 100
    ss <- c(ss_plan, ss_actual)
    prop_trt <- rand / (rand + 1)
    prop_ctl <- 1 / (rand + 1)
    z <- delta / sd / sqrt((1 / prop_trt + 1 / prop_ctl) / ss)
    power <- 100 - pnorm(qnorm(1 - input$sig20), z, 1) * 100
    obs <- qnorm(1 - input$sig20) * sd * sqrt((1 / prop_trt + 1 / prop_ctl) / ss)
    data <- data.frame(ss, power, obs)
    return(data)
  })
  
  output$power_table20 <- renderTable({ # Table in Tab: Power Information
    data <- power20()[, -1]
    data[, 1] <- sprintf("%.1f", data[, 1])
    data[, 2] <- sprintf("%.2f", data[, 2])
    data <- data.frame(c("Original design", "Available"), data)
    colnames(data) <- c("", "Power (%)", "Observed treatment effect* reaching significance")
    return(data)
  }, striped = T, hover = T, bordered = T, rownames = FALSE, colnames = TRUE)
  
  power_sample20 <- reactive({ # Function to get power vs. proportion of data available
    rand <- 1
    delta <- 1
    sd <- 10
    ss_plan <- uniroot(solver, c(1, 1e6),
                       power_target = input$power20 / 100,
                       rand = rand,
                       delta = delta, sd = sd,
                       alpha = input$sig20)$root
    frac <- seq(floor(input$frac20 / 10) * 10, 100, 1)
    ss <- ss_plan * frac / 100
    prop_trt <- rand / (rand + 1)
    prop_ctl <- 1 / (rand + 1)
    z <- delta / sd / sqrt((1 / prop_trt + 1 / prop_ctl) / ss)
    power <- round(100 - pnorm(qnorm(1 - input$sig20), z, 1) * 100, 1)
    obs <- qnorm(1 - input$sig20) * sd * sqrt((1 / prop_trt + 1 / prop_ctl) / ss)
    data <- data.frame(frac, power, obs)
    return(data)
  })
  
  output$title_power_curve_sample20 <- renderText("Power vs. proportion of data available, under the hypothesized design effect")
  
  output$power_curve_sample20 <- renderPlotly({ # Plot in Tab: Power vs. Sample Size
    data <- power_sample20()[, c(1, 2)]
    colnames(data) <- c("Proportion", "Power")
    p <- ggplot(data, aes(x = Proportion, y = Power)) +
      geom_line(linetype = 1, size = 1, colour = "#0460A9") +
      xlab("Proportion (%) of data available") +
      ylab("Power (%)") +
      theme_bw() +
      theme(text = element_text(size = 12, face = "bold"),
            plot.title = element_text(colour = "black", size = 12, face = "bold", hjust = 0),
            plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"),
            legend.text = element_text(colour = "blue", size = 12, face = "bold")
      )
    ggplotly(p)
  })
  
  output$title_power_curve20 <- renderText(paste0("Power vs. effect size, given ",
                                                  input$frac20, "% of data available"))
  
  output$power_curve20 <- renderPlotly({ # Plot for Tab: Power vs. Effect Size
    req(!is.null(input$trt20))
    rand <- 1
    sd <- 10
    data <- power20()
    delta <- seq(input$trt20[1], input$trt20[2], 0.01)
    z_actual <- delta / sd /
      sqrt(((rand + 1) / rand + (rand + 1)) / data[2, 1])
    power_actual <- round(100 - pnorm(qnorm(1 - input$sig20), z_actual, 1) * 100, 1)
    z_plan <- delta / sd /
      sqrt(((rand + 1) / rand + (rand + 1)) / data[1, 1])
    power_plan <- round(100 - pnorm(qnorm(1 - input$sig20), z_plan, 1) * 100, 1)
    data_plot <- data.frame(rep(c("Original design", "Available"), each = length(delta)),
                            rep(delta, 2),
                            c(power_plan, power_actual)
    )
    colnames(data_plot) <- c("Data", "Effect", "Power")
    data_plot$Data <- factor(data_plot$Data,
                             levels = c("Original design", "Available"))
    p <- ggplot(data_plot, aes(x = Effect, y = Power, color = Data)) +
      geom_line(linetype = 1, size = 1) +
      xlab("Treatment effect relative to the hypothesized design effect") +
      ylab("Power (%)") +
      theme_bw() +
      scale_color_manual(values = c("Original design" = "#0460A9", "Available" = "#E74A21")) +
      theme(text = element_text(size = 12, face = "bold"),
            plot.title = element_text(colour = "black", size = 12, face = "bold", hjust = 0),
            legend.title = element_blank(),
            plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"),
            legend.text = element_text(colour = "black", size = 12, face = "bold")
      )
    ggplotly(p)
  })
  
  #********************************************************************************************************
  # GSD start
  #********************************************************************************************************
  value <- reactiveValues(etaGSD = 0) # create reactive variables for dilution effect eta
  value <- reactiveValues(psiGSD = 1) # create reactive variables for variance inflation psi
  
  output$dilutionSettings <- renderUI({ # show input for eta if box is unticked
    if(input$dilutionCheck == FALSE) {
      sliderInput("etaGSD", label=HTML("Dilution effect &eta;"), min = 0, max = 1, value = 0)
    }
  })
  observeEvent(input$etaGSD, { # update eta according to slider
    if(input$dilutionCheck == FALSE) value$etaGSD <- input$etaGSD
  })
  observeEvent(input$dilutionCheck, { # update eta if box is ticked again
    if(input$dilutionCheck == TRUE) value$etaGSD <- 0
  })
  
  output$psiSettings <- renderUI({ # show input for psi if box is unticked
    if(input$psiCheck == FALSE) {
      numericInput("psiGSD", label=HTML("Variance inflation &psi;"), min = 0.1, max = 5, value = 1, step = 0.1)
    }
  })
  observeEvent(input$psiGSD, { # update psi according to input
    if(input$psiCheck == FALSE) value$psiGSD <- input$psiGSD
  })
  observeEvent(input$psiCheck, { # update psi if box is ticked again
    if(input$psiCheck == TRUE) value$psiGSD <- 1
  })
  
 
  powerGSD <- reactive({ # Create table for the resulting values of the power (independent of input value for tau)
    # values for tau
    TAU = c(seq(0.05, 0.95, 0.05), 0.99)*100
    # matrix for results
    data.gsd = matrix(NA, ncol=4, nrow=length(TAU))
    for(tauGSD in TAU) {
      # get critical valus for GSD from package rpact
      crit = getDesignGroupSequential(
        kMax = 2,
        typeOfDesign = input$designSelect, 
        informationRates = c(tauGSD/100, 1),
        alpha = input$alphaGSD, 
        sided = 1)$criticalValues
      # calculate means for first stage and final stage test statistics
      mu.t0 = (qnorm(1-input$alphaGSD)+qnorm(input$powerGSD/100)) * sqrt(tauGSD/100)
      mu.t =  (qnorm(1-input$alphaGSD)+qnorm(input$powerGSD/100)) * (tauGSD/100+(1-tauGSD/100)*(1-value$etaGSD))/sqrt(tauGSD/100 + (1-tauGSD/100)*value$psiGSD)
      # calculate variance matrix for the test statistics
      SIG = matrix(c(1, 
                     sqrt(tauGSD/100*1/(tauGSD/100 + (1-tauGSD/100)*value$psiGSD)), 
                     sqrt(tauGSD/100*1/(tauGSD/100 + (1-tauGSD/100)*value$psiGSD)), 
                     1), ncol=2)
      # calculate power for fixed design and GSD (first stage and final stage)
      power.fix = 1-pnorm(qnorm(1-input$alphaGSD), mean = mu.t0 , sd=1)
      power.gsd.1 = 1-pnorm(crit[1], mean = mu.t0, sd=1)
      power.gsd.2 = 1-pmvnorm(lower = c(-Inf, -Inf), upper = c(crit[1], crit[2]), 
                              mean = c(mu.t0 , mu.t), 
                              sigma = SIG)[1]
      data.gsd[which(tauGSD==TAU), 1:4] = c(tauGSD, power.fix*100, power.gsd.1*100, power.gsd.2*100)
    }
    return(data.gsd)
  })
  
  powerGSDinput <- reactive({ # Add row for input value of tau
    # values for tau
    TAU = unique(sort(c(input$tauGSD, c(seq(0.05, 0.95, 0.05), 0.99)*100)))
    # matrix for results
    tauGSD = input$tauGSD
    # get critical valus for GSD from package rpact
    crit = getDesignGroupSequential(
      kMax = 2,
      typeOfDesign = input$designSelect, 
      informationRates = c(tauGSD/100, 1),
      alpha = input$alphaGSD, 
      sided = 1)$criticalValues
    # calculate means for first stage and final stage test statistics
    mu.t0 = (qnorm(1-input$alphaGSD)+qnorm(input$powerGSD/100)) * sqrt(tauGSD/100)
    mu.t =  (qnorm(1-input$alphaGSD)+qnorm(input$powerGSD/100)) * (tauGSD/100+(1-tauGSD/100)*(1-value$etaGSD))/sqrt(tauGSD/100 + (1-tauGSD/100)*value$psiGSD)
    # calculate variance matrix for the test statistics
    SIG = matrix(c(1, 
                   sqrt(tauGSD/100*1/(tauGSD/100 + (1-tauGSD/100)*value$psiGSD)), 
                   sqrt(tauGSD/100*1/(tauGSD/100 + (1-tauGSD/100)*value$psiGSD)), 
                   1), ncol=2)
    # calculate power for fixed design and GSD (first stage and final stage)
    power.fix = 1-pnorm(qnorm(1-input$alphaGSD), mean = mu.t0 , sd=1)
    power.gsd.1 = 1-pnorm(crit[1], mean = mu.t0, sd=1)
    power.gsd.2 = 1-pmvnorm(lower = c(-Inf, -Inf), upper = c(crit[1], crit[2]), 
                            mean = c(mu.t0 , mu.t), 
                            sigma = SIG)[1]
    data.gsd = rbind(powerGSD(),
                     c(tauGSD, power.fix*100, power.gsd.1*100, power.gsd.2*100))
    data.gsd = data.gsd[order(data.gsd[,1]),]
    data.gsd = data.gsd[!duplicated(data.gsd[,1]),]
    return(data.gsd)
  })

  output$power_plotlyGSD <- renderPlotly({ # GSD Plot in Tab: Power Information
    data.gsd = powerGSDinput()
    data_plot <- data.frame(Proportion=rep(data.gsd[, 1], 3), Power=as.vector(data.gsd[, 2:4]),
                            Type=rep(c("Available", "GSD (stage 1)", "GSD (overall)"), each=nrow(data.gsd)))
    data_plot$Type <- factor(data_plot$Type, levels=c("Available", "GSD (stage 1)", "GSD (overall)"))
    data_plot$Power <- round(data_plot$Power,2)
    p <- ggplot(data_plot, aes(x = Proportion, y = Power, color = Type)) +
      geom_line(linetype = 1, size = 1) +
      xlab("Proportion (%) of data available") +
      ylab("Power (%)") +
      xlim(0,100) +
      ylim(0,100) +
      theme_bw() +
      scale_color_manual(values = c("Available" = "#0460A9", "GSD (stage 1)" = "#EC9A1E", "GSD (overall)" = "#8D1F1B")) +
      theme(text = element_text(size = 12, face = "bold"),
            plot.title = element_text(colour = "black", size = 12, face = "bold", hjust = 0),
            legend.title = element_blank(),
            plot.margin = unit(c(0.3, 0.3, 0.3, 0.3), "cm"),
            legend.text = element_text(colour = "black", size = 12, face = "bold")
      )
    ggplotly(p)
    
  })
  
  output$power_tableGSD <- DT::renderDataTable({ # GSD Table in Tab: Power Information
    data.gsd = powerGSDinput()
    data.table = data.gsd[which(round(data.gsd[,1],0)%%10==0 | data.gsd[,1]==input$tauGSD | (data.gsd[,1]>=79 & round(data.gsd[,1],1)%%5==0)),]
    data.table[, 2:4] <- sprintf("%.2f",  data.table[, 2:4])
    data.table = data.frame(data.table)
    colnames(data.table) <- c("tau", "Available", "GSD Stage 1", "GSD overall")
    datatable(data.table, options = list(dom = 't', pageLength = 12)) %>% formatStyle(
      'tau',
      target = 'row',
      backgroundColor = styleEqual(input$tauGSD, 'lightblue')
    )
  }, server = TRUE)
  
  
  #********************************************************************************************************
  # GSD end
  #********************************************************************************************************
  
}
