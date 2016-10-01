library(TSA)
plotARMAacf <- function(ar = numeric(),
                        ma = numeric(),
                        lag.max = 15,
                        use.pacf = FALSE) {
  arOrder <- length(ar)
  maOrder <- length(ma)
  if (arOrder == 0) {
    plotTitle <- paste0("MA(", maOrder , ")")
  } else if (maOrder == 0) {
    plotTitle <- paste0("AR(", arOrder, ")")
  } else {
    plotTitle <- paste0("ARMA(", arOrder, ", ", maOrder,
                        ")")
  }
  acf_values <- ARMAacf(ar, (-1)*ma, lag.max, use.pacf)
  if (!use.pacf) {
    plotTitle <- paste("Theoretical ACF of", plotTitle)
    plot(as.integer(names(acf_values)),
         acf_values,
         type='h',
         xlim=c(1, lag.max),
         ylim=c(-1, 1),
         main=plotTitle,
         xlab="Lag",
         ylab=ifelse(use.pacf, "PACF", "ACF")) 
  } else {
    plotTitle <- paste("Theoretical PACF of", plotTitle)
    plot(1:length(acf_values),
         acf_values,
         type='h',
         xlim=c(1, lag.max),
         ylim=c(-1, 1),
         main=plotTitle,
         xlab="Lag",
         ylab=ifelse(use.pacf, "PACF", "ACF"))
  }
  abline(h=0)
}

shinyServer(
  function(input, output, session) {
    output$AR_sliders <- renderUI({
      num_AR_coefs <- as.integer(input$AR_order)
      lapply(1:num_AR_coefs, function(i) {
        output_name <- paste0("AR_coef", i)
        slider_name <- paste0("AR coefficient for lag ", i)
        sliderInput(output_name, slider_name,
                    min=-1, max=1, value=.1,
                    step=0.1,
                    animate=animationOptions(interval=300, loop=F))
      })
    })
    
    output$AR_eqn <- renderUI({
      num_AR_coefs <- as.integer(input$AR_order)
      eqn <- paste(sapply(1:num_AR_coefs, function(i){
        paste0("\\phi_", i, "Y_{t - ", i, "}")
      }), collapse=" + ")
      eqn <- paste0('$$Y_t = ', eqn, ' + e_t$$')
      withMathJax(helpText(eqn))
    })
    
    output$AR_ch_eqn <- renderUI({
      num_AR_coefs <- as.integer(input$AR_order)
      eqn <- paste(sapply(1:num_AR_coefs, function(i){
        if(i == 1) {
          paste0("\\phi_1 x")
        } else {
          paste0("\\phi_", i, "x^", i)
        }
      }), collapse=" - ")
      eqn <- paste0('AR characteristic equation: ', '\\(1 - ', eqn, ' = 0\\)')
      withMathJax(helpText(eqn))
    })
    
    output$AR_roots <- renderUI({
      AR_order <- as.integer(input$AR_order)
      if (sum(grepl(paste0("AR_coef", AR_order), names(input)))) {
        AR_coefs <- sapply(1:AR_order, function(i) {
          input[[paste0("AR_coef", i)]]
        })
        roots <- round(polyroot(c(1, -AR_coefs)), 4)
        roots_statement <- paste0("Roots: ", paste0(roots, collapse=', '))
        moduli <- round(Mod(roots), 4)
        moduli_statement <- paste0("Moduli: ", paste0(moduli, collapse=', '))
        withMathJax(helpText(roots_statement),
                    helpText(moduli_statement))
      }
    })
   
    output$theoretical_AR_plot <- renderPlot({
      AR_order <- as.integer(unlist(input$AR_order))
      if (sum(grepl(paste0("AR_coef", AR_order), names(input)))) {
        AR_coefs <- sapply(1:AR_order, function(i) {
          input[[paste0("AR_coef", i)]]
        })
        par(mfrow=c(1,2))
        plotARMAacf(AR_coefs, NULL, lag.max=10)
        plotARMAacf(AR_coefs, NULL, lag.max=10, use.pacf=T)
      }
    })
    
    output$sample_AR_plot <- renderPlot({
      AR_order <- as.integer(unlist(input$AR_order))
      if (sum(grepl(paste0("AR_coef", AR_order), names(input)))) {
        AR_coefs <- sapply(1:AR_order, function(i) {
          input[[paste0("AR_coef", i)]]
        })
        AR_sim <- arima.sim(list(ar=AR_coefs), input$AR_sample_size)
        par(mfrow=c(2,2))
        plot(AR_sim,
             main="Simulated Series",
             ylab="Simulated Values")
        stats::acf(AR_sim, ylim=c(-1,1), xlim=c(1, 10),
                   main="Sample ACF of the simulated series")
        stats::pacf(AR_sim, ylim=c(-1,1), xlim=c(1, 10),
                    main="Sample PACF of the simulated series")
      }
    })
    
    output$MA_sliders <- renderUI({
      num_MA_coefs <- as.integer(input$MA_order)
      lapply(1:num_MA_coefs, function(i) {
        output_name <- paste0("MA_coef", i)
        slider_name <- paste0("(negative) MA coefficient for lag ", i)
        sliderInput(output_name, slider_name,
                    min=-5, max=5, value=.1,
                    step=0.1,
                    animate=animationOptions(interval=300, loop=F))
      })
    })
    
    output$MA_eqn <- renderUI({
      num_MA_coefs <- as.integer(input$MA_order)
      eqn <- paste(sapply(1:num_MA_coefs, function(i){
        paste0("\\theta_", i, "e_{t - ", i, "}")
      }), collapse=" - ")
      eqn <- paste0('$$Y_t = e_t - ', eqn, '$$')
      withMathJax(helpText(eqn))
    })
    
    output$MA_ch_eqn <- renderUI({
      num_MA_coefs <- as.integer(input$MA_order)
      eqn <- paste(sapply(1:num_MA_coefs, function(i){
        if(i == 1) {
          paste0("\\theta_1 x")
        } else {
          paste0("\\theta_", i, "x^", i)
        }
      }), collapse=" - ")
      eqn <- paste0('MA characteristic equation: ', '\\(1 - ', eqn, ' = 0\\)')
      withMathJax(helpText(eqn))
    })
    
    output$MA_roots <- renderUI({
      MA_order <- as.integer(unlist(input$MA_order))
      if (sum(grepl(paste0("MA_coef", MA_order), names(input)))) {
        MA_coefs <- sapply(1:MA_order, function(i) {
          input[[paste0("MA_coef", i)]]
        })
        roots <- round(polyroot(c(1, -MA_coefs)), 4)
        roots_statement <- paste0("Roots: ", paste0(roots, collapse=', '))
        moduli <- round(Mod(roots), 4)
        moduli_statement <- paste0("Moduli: ", paste0(moduli, collapse=', '))
        withMathJax(helpText(roots_statement),
                    helpText(moduli_statement))
      }
    })
    
    output$theoretical_MA_plot <- renderPlot({
      MA_order <- as.integer(unlist(input$MA_order))
      if (sum(grepl(paste0("MA_coef", MA_order), names(input)))) {
        MA_coefs <- sapply(1:MA_order, function(i) {
          input[[paste0("MA_coef", i)]]
        })
        par(mfrow=c(1,2))
        plotARMAacf(NULL, MA_coefs, lag.max=10)
        plotARMAacf(NULL, MA_coefs, lag.max=10, use.pacf=T) 
      }
    })
    
    output$sample_MA_plot <- renderPlot({
      MA_order <- as.integer(unlist(input$MA_order))
      if (sum(grepl(paste0("MA_coef", MA_order), names(input)))) {
        MA_coefs <- sapply(1:MA_order, function(i) {
          input[[paste0("MA_coef", i)]]
        })
        par(mfrow=c(2,2))
        MA_sim <- arima.sim(list(ma=-MA_coefs), input$MA_sample_size)
        plot(MA_sim,
             main="Simulated Series",
             ylab="Simulated Values")
        stats::acf(MA_sim, ylim=c(-1,1), xlim=c(1, 10),
                   main="Sample ACF of the simulated series")
        stats::pacf(MA_sim, ylim=c(-1,1), xlim=c(1, 10),
                    main="Sample PACF of the simulated series")
      }
    })
    
    output$ARMA_AR_sliders <- renderUI({
      num_AR_coefs <- as.integer(input$ARMA_AR_order)
      lapply(1:num_AR_coefs, function(i) {
        output_name <- paste0("ARMA_AR_coef", i)
        slider_name <- paste0("AR coefficient for lag ", i)
        sliderInput(output_name, slider_name,
                    min=-1, max=1, value=.1,
                    step=0.1)
      })
    })
    
    output$ARMA_MA_sliders <- renderUI({
      num_MA_coefs <- as.integer(input$ARMA_MA_order)
      lapply(1:num_MA_coefs, function(i) {
        output_name <- paste0("ARMA_MA_coef", i)
        slider_name <- paste0("(negative) MA coefficient for lag ", i)
        sliderInput(output_name, slider_name,
                    min=-1, max=1, value=.1,
                    step=0.1)
      })
    })
    
    output$theoretical_ARMA_plot <- renderPlot({
      ARMA_AR_order <- as.integer(unlist(input$ARMA_AR_order))
      ARMA_MA_order <- as.integer(unlist(input$ARMA_MA_order))
      if (sum(grepl(paste0("ARMA_AR_coef", ARMA_AR_order), names(input))) > 0 &
          sum(grepl(paste0("ARMA_MA_coef", ARMA_MA_order), names(input))) > 0) {
        ARMA_AR_coefs <- sapply(1:ARMA_AR_order, function(i) {
          input[[paste0("ARMA_AR_coef", i)]]
        })
        ARMA_MA_coefs <- sapply(1:ARMA_MA_order, function(i) {
          input[[paste0("ARMA_MA_coef", i)]]
        })
        par(mfrow=c(1,2))
        plotARMAacf(ARMA_AR_coefs, ARMA_MA_coefs, lag.max=10) 
        plotARMAacf(ARMA_AR_coefs, ARMA_MA_coefs, lag.max=10, use.pacf=T) 
      }
    })
    
    output$sample_ARMA_plot <- renderPlot({
      ARMA_AR_order <- as.integer(unlist(input$ARMA_AR_order))
      ARMA_MA_order <- as.integer(unlist(input$ARMA_MA_order))
      if (sum(grepl(paste0("ARMA_AR_coef", ARMA_AR_order), names(input))) > 0 &
          sum(grepl(paste0("ARMA_MA_coef", ARMA_MA_order), names(input))) > 0) {
        ARMA_AR_coefs <- sapply(1:ARMA_AR_order, function(i) {
          input[[paste0("ARMA_AR_coef", i)]]
        })
        ARMA_MA_coefs <- sapply(1:ARMA_MA_order, function(i) {
          input[[paste0("ARMA_MA_coef", i)]]
        })
        par(mfrow=c(2,2))
        ARMA_sim <- arima.sim(list(ar=ARMA_AR_coefs,
                                   ma=-ARMA_MA_coefs),
                              input$ARMA_sample_size)
        plot(ARMA_sim,
             main="Simulated Series",
             ylab="Simulated Values")
        stats::acf(ARMA_sim, ylim=c(-1,1), xlim=c(1, 10),
                   main="Sample ACF of the simulated series")
        stats::pacf(ARMA_sim, ylim=c(-1,1), xlim=c(1, 10),
                    main="Sample PACF of the simulated series")
        if (length(ARMA_sim) > 100) {
          sim_EACF <- eacf(ARMA_sim)
          image(0:13, 0:7, t(sim_EACF$symbol=="x"),
                ylim=c(7, 0), xlab="MA Order",
                ylab="AR Order", main="Sample EACF")
        }
      }
    })
    
    
    output$ARMA_eqn <- renderUI({
      num_AR_coefs <- as.integer(input$ARMA_AR_order)
      eqn1 <- paste(sapply(1:num_AR_coefs, function(i){
        paste0("\\phi_", i, "Y_{t - ", i, "}")
      }), collapse=" + ")
      num_MA_coefs <- as.integer(input$ARMA_MA_order)
      eqn2 <- paste(sapply(1:num_MA_coefs, function(i){
        paste0("\\theta_", i, "e_{t - ", i, "}")
      }), collapse=" - ")
      eqn <- paste0('$$Y_t =', eqn1, ' + e_t - ', eqn2, '$$')
      withMathJax(helpText(eqn))
    })
    
    output$ARMA_AR_ch_eqn <- renderUI({
      num_AR_coefs <- as.integer(input$ARMA_AR_order)
      eqn <- paste(sapply(1:num_AR_coefs, function(i){
        if(i == 1) {
          paste0("\\phi_1 x")
        } else {
          paste0("\\phi_", i, "x^", i)
        }
      }), collapse=" - ")
      eqn <- paste0('AR characteristic equation: ', '\\(1 - ', eqn, ' = 0\\)')
      withMathJax(helpText(eqn))
    })
    
    output$ARMA_MA_ch_eqn <- renderUI({
      num_MA_coefs <- as.integer(input$ARMA_MA_order)
      eqn <- paste(sapply(1:num_MA_coefs, function(i){
        if(i == 1) {
          paste0("\\theta_1 x")
        } else {
          paste0("\\theta_", i, "x^", i)
        }
      }), collapse=" - ")
      eqn <- paste0('MA characteristic equation: ', '\\(1 - ', eqn, ' = 0\\)')
      withMathJax(helpText(eqn))
    })
    
    output$ARMA_AR_roots <- renderUI({
      ARMA_AR_order <- as.integer(input$ARMA_AR_order)
      if (sum(grepl(paste0("ARMA_AR_coef", ARMA_AR_order), names(input)))) {
        ARMA_AR_coefs <- sapply(1:ARMA_AR_order, function(i) {
          input[[paste0("ARMA_AR_coef", i)]]
        })
        roots <- round(polyroot(c(1, -ARMA_AR_coefs)), 4)
        roots_statement <- paste0("Roots: ", paste0(roots, collapse=', '))
        moduli <- round(Mod(roots), 4)
        moduli_statement <- paste0("Moduli: ", paste0(moduli, collapse=', '))
        withMathJax(helpText(roots_statement),
                    helpText(moduli_statement))
      }
    })
    
    output$ARMA_MA_roots <- renderUI({
      ARMA_MA_order <- as.integer(unlist(input$ARMA_MA_order))
      if (sum(grepl(paste0("ARMA_MA_coef", ARMA_MA_order), names(input)))) {
        ARMA_MA_coefs <- sapply(1:ARMA_MA_order, function(i) {
          input[[paste0("ARMA_MA_coef", i)]]
        })
        roots <- round(polyroot(c(1, -ARMA_MA_coefs)), 4)
        roots_statement <- paste0("Roots: ", paste0(roots, collapse=', '))
        moduli <- round(Mod(roots), 4)
        moduli_statement <- paste0("Moduli: ", paste0(moduli, collapse=', '))
        withMathJax(helpText(roots_statement),
                    helpText(moduli_statement))
      }
    })
    
  }
)