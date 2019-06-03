fluidPage(
  # theme selection during development.
  theme = shinytheme("yeti"),
  titlePanel("Ekimetrics graph transactions", windowTitle = "Kiehl's"),
  
  sidebarLayout(
    
    # Sidebar layout with a input and output definitions
    sidebarPanel(
      fileInput('file1', 'cliquez pour choisir un fichier (.csv)',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  'text/tab-separated-values',
                  'text/plain', 
                  '.csv',
                  '.tsv')
               
    ),
    tags$hr(),
    checkboxInput('header', 'Header', TRUE)
    ,
    uiOutput("slider_explic")
    ,
    tags$hr()
    ,
    sidebarPanel(sliderInput("support","choix du support",min=0.0005,max=1,value=0.01,step=0.0005)
    ,
    tags$hr()
    ,
    sliderInput("confidence","choix du niveau de confidence",min=0.05,max=1,value=0.7,step=0.05)
    ,
    tags$hr()
    ,
    sliderInput("length","choix du maxlength",min=1,max=5,value=2,step=1)
    ,
    tags$hr()
    ,
    actionButton(inputId = "goComputation", label = "Compute")
    
    )),
#Panel central

mainPanel(
  tabsetPanel(
    tabPanel(title = "Galaxy",
             br(),
  
  
  # delete errors
  tags$style(type="text/css",
            ".shiny-output-error { visibility: hidden; }",
            ".shiny-output-error:before { visibility: hidden; }"
  ),
  
  #affichage dans le "mainPanel"
  sidebarPanel(sliderInput("alpha1","intensite de liaisons du reseau",min=0.005,max=1,value=0.2,step=0.005)),
  
  
  
  
  # button computation:
  
  actionButton("store_position", "Store positions !"),
  downloadLink('downloadNetwork', 'Download network'),
  visNetworkOutput(outputId = "network1",height = 1000)
    ),
  tabPanel(title = "Dataset",
           br(),
           dataTableOutput(outputId = "table"),
           br(),
           br(),
           dataTableOutput("plot3"),
           br(),
           plotlyOutput("plot7", height = 600, width = 1000)
           ),
  
  tabPanel(title = "Cross-sell potential", 
           br(), 
           plotOutput(outputId = "crosspotential", height = 800, width = 1400)),
  


  tabPanel(title = "Specific Product",
           br(),
           uiOutput("slider_select"),
           br(),
           actionButton(inputId = "goComputation1", label = "launch computation"),
           br(),
           dataTableOutput(outputId = "table1"),
           br(),
           dataTableOutput(outputId = "table2"),
           br(),
           plotlyOutput("plot1", height = 600, width = 1000),
           br(),
           dataTableOutput("plot2"),
           br(),
           plotlyOutput("plot5", height = 600, width = 1000),
           br(),
           dataTableOutput("plot6"),
           br()
           )
  )
  ))
)