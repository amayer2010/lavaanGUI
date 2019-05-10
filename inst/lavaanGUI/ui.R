library("shinythemes")

shinyUI(fluidPage(
  titlePanel(title="lavaanGUI"),
  
  sidebarLayout(
  sidebarPanel(
    tabsetPanel(
      ######### Data ############
      tabPanel('Data',
               br(),
               actionButton("newanalysis","Start a New Analysis"),
               uiOutput("reload"),
               hr(),        
               selectizeInput(inputId="exdata", label="Example Data", selected="",
                              choices= c("","HolzingerSwineford1939", "PoliticalDemocracy",
                                         "Demo.growth"),
                              options = list(placeholder = 'choose example data'),
                              width='50%'),    
               # hr(),
               tryCatch(
                 fileInput("file1", "Data File", 
                           accept=c(".csv", ".txt", ".sav", ".xpt", 
                                    ".CSV", ".TXT", ".SAV", ".XPT",
                                    ".DAT", ".dat"))
               ),
               helpText('Select either a .csv, .dat, .txt, .sav or a .xpt file to be uploaded. The corresponding R function (read.csv, read.table, read.spss, or read.xport) will be chosen automatically with the default settings for arguments. Some default arguments can be overwritten (see additional options below).'),
               br(),
               h5(strong("Additional Options to Read Data")),
               br(),
               selectizeInput(inputId="vallabels", 
                              label=h5("Use value labels (SPSS data)"), 
                              selected="default",
                              choices= c("default","yes","no"),
                              width='90%'),
               selectizeInput(inputId="header", 
                              label=h5("File contains variable names (csv, dat, and txt data)"), 
                              selected="default",
                              choices= c("default","yes","no"),
                              width='90%'),
               selectizeInput(inputId="sep", 
                              label=h5("Character separating columns (csv, dat, and txt data)"), 
                              selected="default",
                              choices= c("default","semicolon","white space"),
                              width='90%'),
               selectizeInput(inputId="dec", 
                              label=h5("Decimal character (csv, dat, and txt data)"), 
                              selected="default",
                              choices= c("default","decimal point","decimal comma"),
                              width='90%'),
               textInput(inputId="na.strings", 
                         label=h5("Missing value code (csv, dat, and txt data)"), 
                         value = "NA",
                         width='90%'),
               br()
      ),
      ######### File / Ace Editor ############
      tabPanel('File',
               h5("Open Input Files"),
               fileInput("inpfile", "", 
                         accept=c(".lav", ".txt", ".LAV", ".TXT"),
                         multiple=TRUE),                              
              selectizeInput("selinpfile", "", "", 
                            selected=NULL, multiple=FALSE,
                            options = list(placeholder = 'select input file')),
              hr(),
              h5("Save Input and Output Files"),
              downloadButton("saveinput", label = "Save Input File"),
              downloadButton("saveoutput", label = "Save Main Output File"),
              hr(),
              h5("Editor Options"),              
               numericInput("fontsize", "Font Size", min=6, max=40, value=18),
               selectInput("theme", "Theme: ", choices=themes, selected="textmate"),
               br(),
               br(),
               br(),
               br(),
               br(),
               br()
      ),      
      ########## Estimation #############
      tabPanel('Estimation',
        br(),       
        selectizeInput("mainfunc", "workhorse function", 
                              choices=c("sem", "cfa", "growth"), 
                              selected = "sem"),
        hr(),
        selectizeInput("estimator", "estimator", 
                       choices=c("default", "ML", "GLS", "WLS", "ULS", "DWLS", "MLM", "MLMV", "MLMVS", "MLF", "MLR"), 
                       selected = "default"),
        selectizeInput("se", "se", 
                       choices=c("default", "standard", "first.order", "robust.sem", "robust.huber.white", "bootstrap", "none"), 
                       selected = "default"),
        conditionalPanel(
          condition = "input.se == 'bootstrap'",
          numericInput("bootstrap", "Number of bootstrap draws", 
                       min=1, max=5000, value=1000)
        ),
        selectizeInput("test", "test", 
                       choices=c("default", "standard", "Satorra.Bentler", "Yuan.Bentler", "mean.var.adjusted", "Satterthwaite", "scaled.shifted", "bootstrap", "Bollen.Stine"), 
                       selected = "default"),
        hr(),
        selectizeInput("missing", "missing", 
                       choices=c("default","listwise","fiml"), 
                       selected = "default"),
        selectizeInput("mimic", "mimic", 
                       choices=c("default","lavaan","Mplus", "EQS"), 
                       selected = "default"),
        br(),
        br(),
        br(),
        br(),
        br()
     ),
     ########## Multi-Group #############
     tabPanel('Multi-Group',
        br(),      
        selectizeInput("group", "group", "", selected=NULL,
              options = list(placeholder = 'select grouping variable')),              
        selectizeInput("group.label", "group.label", "", 
                       selected=NULL, multiple=TRUE,
                       options = list(placeholder = 'order of groups')),
        hr(),
        selectizeInput("group.equal", "group.equal", 
                       choices=c("loadings","intercepts","means",
                                 "thresholds", "regressions", "residuals",
                                 "residual.covariances", "lv.variances",
                                 "lv.covariances"), 
                       multiple = TRUE,
                       selected = NULL,
                       options = list(placeholder = 'equality constraints across groups')),
        selectizeInput("group.partial", "group.partial", 
                       choices=c("","loadings","intercepts","means",
                                 "thresholds", "regressions", "residuals",
                                 "residual.covariances", "lv.variances",
                                 "lv.covariances"), 
                       multiple = TRUE,
                       selected = NULL,
                       options = list(placeholder = 'free parameters across groups')),
        br(),
        checkboxInput("group.w.free", "group.w.free", FALSE),
        br(),
        br(),
        br(),
        br(),
        br()                              
     ),
     ########## Categorical #############
     tabPanel('Categorical',
        br(),      
        selectizeInput("ordered", "ordered", "", 
              multiple=TRUE, selected=NULL,
              options = list(placeholder = 'select ordered variables')),
        selectizeInput("parameterization", "parameterization", 
                       choices=c("default","theta","delta"), 
                       selected = "default"),
        br(),
        br(),
        br(),
        br(),
        br()
              
     ),
     ########## lavInspect Input #############
     tabPanel('lavInspect',
              br(),
              selectizeInput("lavinspecttopic", "lavInspect Topic", "", 
                             multiple=FALSE, selected="All",
                             choices=c("All topics",
                                       "All topics (+ alias)",
                                       "Information about data",
                                       "Observed sample statistics",
                                       "Model features",
                                       "Model-implied sample statistics",
                                       "Optimizer information",
                                       "Gradient, Hessian, Information matrices",
                                       "Vcov of model parameters",
                                       "Miscallaneous"
                                       )),
              selectizeInput("whattolavinspect", "What to lavInspect", 
                             choices=li_all, 
                             selected = "",
                             options = list(placeholder = 'choose what to inspect')),
              br()
              
     ),
     ########## lavPredict Input #############
     tabPanel('lavPredict',
              br(),
              selectizeInput("fsmethod", "Factor Score Method", "", 
                             multiple=FALSE, selected="EBM",
                             choices=c("EBM",
                                       "Bartlett",
                                       "regression"
                             )),
              checkboxInput("addytofs","Add observed variables to factor scores table",
                            value=TRUE),
              br()
              
     ),
     ########## Additional #############
      tabPanel('Additional',
        checkboxInput("meanstructure", "meanstructure", TRUE),
        checkboxInput("fixed.x", "fixed.x", TRUE),
        hr(),
        checkboxInput("std.lv", "std.lv", FALSE),
        checkboxInput("std.ov", "std.ov", FALSE),
        checkboxInput("orthogonal", "orthogonal", FALSE),
        hr(),
        checkboxInput("verbose", "verbose", FALSE),
        checkboxInput("warn", "warn", TRUE),
        checkboxInput("debug", "debug", FALSE)

    ),
    ########## Wald Test #############
    tabPanel('Wald Test',
             br(),
             h5("User-Specified Wald Test"),
             helpText('This text can be used to specify an additional (user-specified) Wald test based on names of model parameters.'),
             helpText('Example: par1 == 0 ; par2 == 0'),
             tags$textarea(id="add.syntax.wald", rows=5, cols=40, "")
    )

  )),
  
  mainPanel(
    tabsetPanel(
      ######### Data Table ##########
      tabPanel('Data', dataTableOutput("mytable1")),      
      
      ######### lavaan Input ##########
      tabPanel("lavaan Input", shinyAce::aceEditor("ace", value='', mode="r")),      
      
      ######### lavaan Results ##########
      tabPanel("lavaan Results", verbatimTextOutput("lavresults")),
      
      ######### Parameter Estimates ##########
      tabPanel("Parameter Estimates", verbatimTextOutput("parestimates")),
      
      ######### Modification Indices ##########
      tabPanel("Modification Indices", verbatimTextOutput("modindices")),
      
      ######### lavInspect ##########
      tabPanel("lavInspect", verbatimTextOutput("lavinspect")),
      
      ######### Factor Scores ##########
      tabPanel('Factor Scores', 
               downloadLink('downloadFScores', 
                            'Download Factor Scores Data'),
               br(),
               br(),
               dataTableOutput("fscores")),
      
      
      ######### User Specified Tests ##########
      tabPanel("Wald Test", verbatimTextOutput("waldtest")),
            
      ######### Path Diagram ##########
      tabPanel("Path Diagram", plotOutput("plotpd"))
            
    )    
  ))
  , shinythemes::themeSelector() # add a theme selector
  , theme = shinytheme("darkly") # set default theme as darkly
  
))