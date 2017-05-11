

shinyServer(function(input, output, session) {
  
  ## close app when browser tab is closed
  session$onSessionEnded(function() { 
    stopApp() 
  })
  
  
  ####### New analysis / reload button ########
  output$reload <- renderUI({
    if (input$newanalysis > 0) {
      tags$script("window.location.reload();")
    }
  })
  
  
  ####### Ace Editor / lavaan input ########
  observe({
    shinyAce::updateAceEditor(session, "ace", theme=input$theme, 
                              fontSize=input$fontsize)
    
    if(!is.null(input$inpfile)){
      shinyAce::updateAceEditor(session, "ace", value=paste(inpFile(), collapse="\n"))
    }
  })
  
  ######## Reactive Data Input ########
  dataInput <- reactive({
    inFile <- input$file1
    exdata <- input$exdata
    
    if(is.null(inFile)){      
      if(exdata==""){
        return(NULL)        
      }else if(exdata=="HolzingerSwineford1939"){
        return(HolzingerSwineford1939)  
      }else if(exdata=="PoliticalDemocracy"){
        return(PoliticalDemocracy)
      }else if(exdata=="Demo.growth"){
        return(Demo.growth)
      }                        
    }
    
    if(!is.null(inFile)){
      
      return(EffectLiteR::elrReadData(file=inFile$datapath,
                         name=inFile$name,
                         header=input$header,
                         sep=input$sep,
                         dec=input$dec,
                         na.strings=input$na.strings,
                         use.value.labels=input$vallabels))
      
    }
  })
  
    
  ######## Reactive Input File ########
  inpFile <- reactive({
    inpfile <- input$inpfile
    selinpfile <- input$selinpfile
    
    if(is.null(inpfile) || selinpfile=="" || !(selinpfile %in% inpfile$name)){      
        return(NULL)        
    }else {
      ind <- which(inpfile$name == selinpfile) ## which one is selected
      return(readLines(inpfile$datapath[ind]))                
    }  
  })
  
  
  ###### Reactive select input files ###########
  selInpFile <- reactive({
    return(input$inpfile)
  })  
  
  ###### Update select input files ########
  observe({
    sel <- selInpFile()
    updateSelectizeInput(session, "selinpfile", choices = sel$name,
                         selected=sel$name[1])  
  })

  
  
  ###### Update Variable Selectors group and categorical ########
  observe({
    inFile <- input$file1
    exdata <- input$exdata
    
    if(is.null(inFile) & exdata=="")
      return(NULL)  
    
    d <- dataInput()
    
    updateSelectInput(session, "group", choices = c("", names(d)))
    updateSelectInput(session, "ordered", choices = c("", names(d)))
  })
  
  
  ###### Update group.label ########
  observe({
    group <- input$group
    
    if(is.null(group))
      return(NULL)  
    
    d <- dataInput()
    
    groupvariable <- d[[group]]
    updateSelectInput(session, "group.label", 
                      choices = unique(groupvariable),
                      selected = unique(groupvariable))
  })

  ###### Update whattolavinspect ########
  observe({
    topic <- input$lavinspecttopic

    if(topic == "All topics"){
      updateSelectInput(session, "whattolavinspect", 
                        choices = li_all,
                        selected="")
      
    }else if(topic == "All topics (+ alias)"){
      updateSelectInput(session, "whattolavinspect", 
                        choices = li_all_alias,
                        selected="")
      
    }else if(topic == "Information about data"){
      updateSelectInput(session, "whattolavinspect", 
                        choices = li_infdata,
                        selected="")
      
    }else if(topic == "Observed sample statistics"){
      updateSelectInput(session, "whattolavinspect", 
                        choices = li_obssampstat,
                        selected="")
      
    }else if(topic == "Model features"){
      updateSelectInput(session, "whattolavinspect", 
                        choices = li_modfeatures,
                        selected="")
      
    }else if(topic == "Model-implied sample statistics"){
      updateSelectInput(session, "whattolavinspect", 
                        choices = li_modimpliedstat,
                        selected="")
      
    }else if(topic == "Optimizer information"){
      updateSelectInput(session, "whattolavinspect", 
                        choices = li_optimizer,
                        selected="")
      
    }else if(topic == "Gradient, Hessian, Information matrices"){
      updateSelectInput(session, "whattolavinspect", 
                        choices = li_gradhessinfo,
                        selected="")
      
    }else if(topic == "Vcov of model parameters"){
      updateSelectInput(session, "whattolavinspect", 
                        choices = li_vcovmodpar,
                        selected="")
      
    }else if(topic == "Miscallaneous"){
      updateSelectInput(session, "whattolavinspect", 
                        choices = li_misc,
                        selected="")
      
    }
    
  })
  
    

  ###### Reactive lavaan Syntax input #########
  model <- reactive({
    
    input$ace
      
  })
  
  
  
  ###### Reactive Run Model #########
  m1 <- reactive({
        
    ## arguments for sem(), cfa(), growth
    model <- model()
    data <- dataInput()
    meanstructure <- input$meanstructure
    fixed.x <- input$fixed.x
    orthogonal <- input$orthogonal
    std.lv <- input$std.lv
    parameterization <- input$parameterization
    std.ov <- input$std.ov
    missing <- input$missing
    ordered <- input$ordered        
    if(input$group==""){group <- NULL}else{group <- input$group}      
    group.label <- input$group.label
    group.equal <- ifelse(is.null(input$group.equal),"",input$group.equal)
    group.partial <- ifelse(is.null(input$group.partial),"",input$group.partial)
    group.w.free <- input$group.w.free    
    estimator <- input$estimator
    se <- input$se
    test <- input$test
    bootstrap <- input$bootstrap
    mimic <- input$mimic
    start <- input$start
    verbose <- input$verbose
    warn <- input$warn
    debug <- input$debug
    
    if(input$mainfunc == "sem"){
      res <- tryCatch(
        sem(model=model,
            data=data,
            meanstructure=meanstructure,
            fixed.x=fixed.x,
            orthogonal=orthogonal,
            std.lv=std.lv,
            parameterization=parameterization,
            std.ov=std.ov,
            missing=missing,
            ordered=ordered,
            group=group,
            group.label=group.label,
            group.equal=group.equal,
            group.partial=group.partial,
            group.w.free=group.w.free,
            estimator=estimator,
            se=se,
            test=test,
            bootstrap=bootstrap,
            mimic=mimic,
            verbose=verbose,
            warn=warn,
            debug=debug
        )
      )
    }

    if(input$mainfunc == "growth"){
      res <- tryCatch(
        growth(model=model,
            data=data,
            fixed.x=fixed.x,
            orthogonal=orthogonal,
            parameterization=parameterization,
            std.ov=std.ov,
            missing=missing,
            ordered=ordered,
            group=group,
            group.label=group.label,
            group.equal=group.equal,
            group.partial=group.partial,
            group.w.free=group.w.free,
            estimator=estimator,
            se=se,
            test=test,
            bootstrap=bootstrap,
            mimic=mimic,
            verbose=verbose,
            warn=warn,
            debug=debug
        )
      )
    }
    

    if(input$mainfunc == "cfa"){
      res <- tryCatch(
        cfa(model=model,
            data=data,
            meanstructure=meanstructure,
            fixed.x=fixed.x,
            orthogonal=orthogonal,
            std.lv=std.lv,
            parameterization=parameterization,
            std.ov=std.ov,
            missing=missing,
            ordered=ordered,
            group=group,
            group.label=group.label,
            group.equal=group.equal,
            group.partial=group.partial,
            group.w.free=group.w.free,
            estimator=estimator,
            se=se,
            test=test,
            bootstrap=bootstrap,
            mimic=mimic,
            verbose=verbose,
            warn=warn,
            debug=debug
        )
      )
    }
    
    res
    
  })
  
  

  
  ###### Output Download Input Output #########  
  output$saveinput <- downloadHandler(      
    filename = function() {
      paste0('lavinput', '.lav')
    },
    content = function(file) {
      writeLines(model(), file)
    }
  )

  output$saveoutput <- downloadHandler(      
    filename = function() {
      paste0('lavoutput', '.out')
    },
    content = function(file) {
      capture.output(summary(m1(), 
                             standardized=TRUE, 
                             fit.measures=TRUE, 
                             rsquare=TRUE), 
                     file=file)
    }
  )  
  
  
  ###### Output Data Table #########  
  output$mytable1 = renderDataTable({ 
    d <- dataInput()
    dprint <- format(d, digits=3)
    dprint
  })


  ###### Output Path Diagram #########
  output$plotpd <- renderPlot({    
    
      m1 <- m1()      
      semPlot::semPaths(m1, what="path", ask=FALSE, style="lisrel",
                        intercepts=FALSE, thresholds=FALSE,
                        rotation=4, optimizeLatRes=TRUE,
                        exoVar=TRUE)
    
  })
  


  ###### Output Lavaan Results #########
  output$lavresults <- renderPrint({      
              
      m1 <- m1()
      summary(m1, fit.measures=TRUE)  
       
  })

  
  ###### Output Parameter Estimates #########
  output$parestimates <- renderPrint({      
    
    m1 <- m1()
    parameterEstimates(m1, standardized=TRUE)  
    
  })
  
  
  ###### Output Modification Indices #########
  output$modindices <- renderPrint({      
    
    m1 <- m1()
    modificationIndices(m1, power=TRUE, sort.=TRUE)  
    
  })
  
  ###### Output lavInspect #########
  output$lavinspect <- renderPrint({      
    
    if(input$whattolavinspect == ""){
      cat("choose what to inspect")
      
    }else{
      m1 <- m1()
      lavInspect(m1, what=input$whattolavinspect)  
    }
    
  })
  
  
  ###### Output Factor Scores Table #########
  output$fscores = renderDataTable({
      m1 <- m1()
      fs <- data.frame(lavPredict(m1, method=input$fsmethod))
      if(input$addytofs){
        ydata <- data.frame(lavInspect(m1, "data"))
        names(ydata) <- lavNames(m1)
        fs <- cbind(ydata,fs)
      }
      fsprint <- format(fs, digits=3)
      fsprint
  })
  
  ###### Download Data (Factor Scores Table) #######
  output$downloadFScores <- downloadHandler(
    filename = function() {
      paste('FactorScores-', Sys.Date(), '.txt', sep='')
    },
    content = function(con) {
      m1 <- m1()
      fs <- data.frame(lavPredict(m1, method=input$fsmethod))
      if(input$addytofs){
        ydata <- data.frame(lavInspect(m1, "data"))
        names(ydata) <- lavNames(m1)
        fs <- cbind(ydata,fs)
      }
      write.table(fs, con, row.names=F, col.names=T, 
                  quote=F)
    }
  )
  
  
  
  ###### Output Wald Test #########
  output$waldtest <- renderPrint({      
    
    if(input$add.syntax.wald == ""){
      
      cat("No user-defined Wald test specified")
      
    }else{
      
      m1 <- m1()
      con <- input$add.syntax.wald
      wtest <- data.frame(lavTestWald(m1, con)[1:3])  
      row.names(wtest) <- "Wald Test"  
      names(wtest) <- c("Wald Chi-Square", "df", "p-value")
      print(wtest, digits=3, print.gap=3)
      
    }  
  })
  
})