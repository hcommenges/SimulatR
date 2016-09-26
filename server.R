shinyServer(function(session, input, output) {
  
  # set data ----
  
  baseData <- reactiveValues(tailleval = dfExample$TAILLE, tval = NULL, meanval = NULL, chival = NULL)
  
  
  # mean estimation ----
  
  output$plotpopmean <- renderPlot(
    if(input$getonemean == 0){
      PlotPopMean(df = dfExample)
    } else {
      PlotOneSampleMean(df = dfExample, sampone = oneSampleMean()$SAMP, val = baseData$tailleval)
    }
  )
  
  output$plothist <- renderPlot(
    PlotHist(df = dfExample, sel = oneSampleMean()$SAMP, val = baseData$tailleval, sigma = input$varlevmean)
  )
  
  observe({
    baseData$tailleval <- c(rnorm(n = 500, mean = 100, sd = input$varlevmean), rnorm(n = 500, mean = 100, sd = input$varlevmean))
  })
  
  observe({
    baseData$tailleval <- c(rnorm(n = 500, mean = 100, sd = input$varlevcomp), rnorm(n = 500, mean = 100, sd = input$varlevcomp))
  })
  
  oneSampleMean <- eventReactive(input$getonemean, {
    GetOneSampleMean(df = dfExample, sampsize = input$sampsizemean, val = baseData$tailleval)
  })
  
  tenSampleMean <- eventReactive(input$gettenmean, {
    GetTenSampleMean(df = dfExample, sampsize = input$sampsizemean, val = baseData$tailleval)
  })
  
  hundredSampleMean <- eventReactive(input$gethundredmean, {
    GetHundredSampleMean(df = dfExample, sampsize = input$sampsizemean, val = baseData$tailleval)
  })
  
  observeEvent(input$resetmean, {
    baseData$meanval <- NULL
  })
  
  observeEvent(input$getonemean, {
    baseData$meanval <- append(x = baseData$meanval, values = oneSampleMean()$MEAN)
  })
  
  observeEvent(input$gettenmean, {
    baseData$meanval <- append(x = baseData$meanval, values = tenSampleMean()$MEAN)
  })
  
  observeEvent(input$gethundredmean, {
    baseData$meanval <- append(x = baseData$meanval, values = hundredSampleMean()$MEAN)
  })
  
  output$tabmean <- renderDataTable(options = list(pageLength = 10), expr = {
    if(length(baseData$meanval > 0)){
      ShowDataTableMean(val = baseData$meanval)
    }
  })
  
  output$meandistrib <- renderPlot(
    if(length(baseData$meanval > 0)){
      PlotMeanDistrib(baseData$meanval, levalpha = input$levalphamean, mu = mean(baseData$tailleval), sigma = input$varlevmean, sampsize = input$sampsizemean)
    })
  
  
  # mean comparison ----
  
  output$plotpopcomp <- renderPlot(
    if(input$getonecomp == 0){
      PlotPopComp(df = dfExample)
    } else {
      PlotOneSampleComp(df = dfExample, sampone = oneSampleComp()$SAMP, val = baseData$tailleval)
    }
  )
  
  output$plothistcomp <- renderPlot(
    PlotHistComp(df = dfExample, sel = oneSampleComp()$SAMP, val = baseData$tailleval)
  )
  
  oneSampleComp <- eventReactive(input$getonecomp, {
    GetOneSampleComp(df = dfExample, sampsize = input$sampsizecomp, val = baseData$tailleval)
  })
  
  tenSampleComp <- eventReactive(input$gettencomp, {
    GetTenSampleComp(df = dfExample, sampsize = input$sampsizecomp, val = baseData$tailleval)
  })
  
  hundredSampleComp <- eventReactive(input$gethundredcomp, {
    GetHundredSampleComp(df = dfExample, sampsize = input$sampsizecomp, val = baseData$tailleval)
  })
  
  observeEvent(input$resetcomp, {
    baseData$tval <- NULL
  })
  
  observeEvent(input$getonecomp, {
    baseData$tval <- append(x = baseData$tval, values = oneSampleComp()$TVAL)
  })
  
  observeEvent(input$gettencomp, {
    baseData$tval <- append(x = baseData$tval, values = tenSampleComp()$TVAL)
  })
  
  observeEvent(input$gethundredcomp, {
    baseData$tval <- append(x = baseData$tval, values = hundredSampleComp()$TVAL)
  })
  
  output$tabcomp <- renderDataTable(options = list(pageLength = 10), expr = {
    if(length(baseData$tval > 0)){
      ShowDataTableComp(val = baseData$tval)
    }
  })
  
  output$tdistrib <- renderPlot(
    if(length(baseData$tval > 0)){
      PlotTDistrib(tvalues = baseData$tval, levalpha = input$levalphacomp, sampsize = input$sampsizecomp)
    })
  
  
  
  # chi2 estimation ----
  
  output$plotpopchi <- renderPlot(
    if(is.null(baseData$chival)){
      PlotPopChi(df = dfExample)
    } else {
      PlotOneSampleChi(df = dfExample, sampone = oneSampleChi()$SAMP)
    }
  )
  
  output$plotmosaic <- renderPlot(
    if(input$getonechi == 0){
      PlotMosaic(varx = "FORME", vary = "COULEUR", df = dfExample)
    } else {
      PlotMosaic(varx = "FORME", vary = "COULEUR", df = dfExample[oneSampleChi()$SAMP, ])
    }
  )
  
  oneSampleChi <- eventReactive(input$getonechi, {
    GetOneSampleChi(df = dfExample, sampsize = input$sampsizechi)
  })
  
  tenSampleChi <- eventReactive(input$gettenchi, {
    GetTenSampleChi(df = dfExample, sampsize = input$sampsizechi)
  })
  
  hundredSampleChi <- eventReactive(input$gethundredchi, {
    GetHundredSampleChi(df = dfExample, sampsize = input$sampsizechi)
  })
  
  observeEvent(input$resetchi, {
    baseData$chival <- NULL
  })
  
  observeEvent(input$getonechi, {
    baseData$chival <- append(x = baseData$chival, values = oneSampleChi()$CHI)
  })
  
  observeEvent(input$gettenchi, {
    baseData$chival <- append(x = baseData$chival, values = tenSampleChi()$CHI)
  })
  
  observeEvent(input$gethundredchi, {
    baseData$chival <- append(x = baseData$chival, values = hundredSampleChi()$CHI)
  })
  
  output$tabchi <- renderDataTable(options = list(pageLength = 10), expr = {
    if(length(baseData$chival > 0)){
      ShowDataTableChi(val = baseData$chival)
    }
  })
  
  output$chidistrib <- renderPlot(
    if(length(baseData$chival > 0)){
      PlotChiDistrib(baseData$chival, levalpha = input$levalphachi)
    })
  
  
})
