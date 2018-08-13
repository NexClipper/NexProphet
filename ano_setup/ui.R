source('../Source/load_package.R')
source('../Source/server_func.R')


dashboardPage(
  dashboardHeader(title = "", disable = T),
  dashboardSidebar(
    disable = T
  ),
  dashboardBody(
              tags$style(
                type = 'text/css', 
                '.bg-aqua {background-color: #97a6c1!important; } 
                 .box-header {background-color: #97a6c1!important; padding-left: 15px;  padding-top:15px;padding-bottom: 5px; }
                .box {border-color: #97a6c1!important;}
                .iconNexBig {color: white; margin-right: 20px}
                .iconNexSmall {color: white; margin-right: 10px}
                .iconNexButton {color: black; margin-right: 10px}
                .nexSideMenu {background-color: #fbfbfb!important; padding-left: 30px; padding-top: 20px; padding-bottom: 10px;}
                .nexBody8 {padding-left: 40px; padding-top: 20px; padding-bottom: 20px;}'

              ),
              
              useShinyjs(),

              fluidRow(    
               valueBoxOutput("TotalRules", width = 5),
               valueBoxOutput("AlertCount", width = 5)
              ), 
              
              
              # Step 0 : Select Define a new metric to alarm or redefine a existing rule -----------------------------------------------------------------------------------
              
              fluidRow(
                box(
                  width = 10, status = "info", solidHeader = TRUE,  collapsible = T, 
                  title = p(icon("adjust", class = 'iconNexSmall'), "Step 0 : Select Define a new metric to alarm or redefine a existing rule"),
                  
                  column(width = 4, class = "nexSideMenu",
                         radioButtons("setType", "Select anomaly detection type :", 
                                      c("Define a new metric", "Redefine", "Delete"), selected = "Define a new metric",
                                      inline = T, width = "100%")
                  ),
                  
                  column(width = 8, class = "nexBody8",
                         
                         selectizeInput("selMetricRule", "Selecte a existing Rule : ", choices = "", 1),
                         actionButton("actionViewMonitoring", "View realtime Chart", width = "250px", style = "margin-right: 20px;",
                                      icon = icon("image", class = "iconNexButton")),

                         actionButton("actionViewAllList", "View all Existing Rules", width = "250px", style = "margin-right: 20px;",
                                      icon = icon("list-ul", class = "iconNexButton")),
                         
                         actionButton("actionDelete", "Delete Selected Rules", width = "250px",
                                      icon = icon("eraser", class = "iconNexButton")),
                         
                         plotOutput("tempPlot", height = "20px"),

                         dataTableOutput("datatableAllList"),
                         br(),
                         h4(id='step1_comment', icon("arrow-right", class = "iconNexButton"), "Go to Next Step!!!")
                         # div(id='step1_comment', style = "text-align:center", "This application is based on Quandl Data")
                         
                  )
                  
                )
              ),
              
              # Step 1 : Select metric to dectect anomaly by algorithm -----------------------------------------------------------------------------------
              
              fluidRow(
                box(
                  width = 10, status = "info", solidHeader = TRUE,  collapsible = T,
                  title = p(icon("indent", class = 'iconNexSmall'),  "Step 1 : Select metric to dectect anomaly by algorithm"),
              
                  
                  column(width = 4, class = "nexSideMenu",
                    radioButtons("anomType", "Select anomaly detection type :", 
                                   c("Service" = 'service',
                                     "Host" = 'host',
                                     # "Task" = 'task',
                                     "Docker" = 'docker'), inline = T, width = "100%")
                  ),
                  
                  column(width = 8, class = "nexBody8",
                         
                         uiOutput("anomInput"),
                         selectizeInput("selMetric", "Select a Metric to Monitor : ", choices = NULL),
                         
                         br(),
                         
                         actionButton("actionViewMetric", "View Metric Trend Chart", width = "250px",
                                      icon = icon("image", class = "iconNexButton"))
                         
                  )
                  
                )
              ),
              
              
              
              # Step 2 : Set alert condition -----------------------------------------------------------------------------------
              fluidRow(
                
                box(
                  width = 10, status = "info", solidHeader = TRUE,  collapsible = T,
                  title = p(icon("exclamation-circle", class = 'iconNexSmall'),"Step 2 : Set alert conditions"),
                  
                  column(width = 4, class = "nexSideMenu",

                         radioButtons("selDirection", "Select anomaly directon :", 
                                      c("Above", "Below", "Both"), selected = "Above", inline = T, width = "100%")

                  ),
                  
                  column(width = 4, class = "nexBody8",
                         
                         selectizeInput("selDuration", "Select the time duration to alart : ",
                                        choices =  c("5 minutes", "10 minutes"), selected = "5 minutes", width = "250px")
                         
                         
                  ),
                  
                  column(width = 4, class = "nexBody8",
                         radioButtons("modelType", "Select anomaly model type :", 
                                      c("Basic", "Robust", "Agile"), selected = "Agile", inline = T, width = "100%"),
                         br(),
                         actionButton("adOptions", "Advanced Option", width = "250px",
                                      icon = icon("cog", class = "iconNexButton"))
                        
                  ), 
                  actionButton('save', 'save')
                )
                
              )
              

    )
)
