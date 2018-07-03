
# 개발된 함수를 불러 온다
source("Source/BIZSCAPE_FUNCTIONS.R", local = T, encoding="utf-8")

# 필요한 라이브러리가 설치 안되었으면 설치하고, 라이브러리로 등록한다  
# 샤이니 관련  
bs.Library(c("shiny", "shinythemes", "shinydashboard", "shinyAce", "shinyBS", "shinyjs", "shinyFiles", "shinyTree",
             "rhandsontable", "RColorBrewer", "colourpicker", "listviewer"))


tagList(
    # shinythemes::themeSelector(),
    navbarPage(
        # theme = "cerulean",  # <--- To use a theme, uncomment this
        NULL,

        tabPanel("Metric Association",
                     sidebarLayout(

                         # Define the sidebar with one input
                         sidebarPanel(width = 3,
                             wellPanel(
                                 selectizeInput("combo_MAsso_Host", "Select Host :",
                                                choices = c(""), selected = "")
                             ),
                             hr(),
                             wellPanel(
                                 selectizeInput("combo_MAsso_CM", "Select Custer Metric :",
                                                choices = c(""), selected = ""),
                                 selectizeInput("combo_MAsso_HM", "Select Host Metric :",
                                                choices = c(""), selected = ""),
                                 selectizeInput("combo_MAsso_TM", "Select Task Metric :",
                                                choices = c(""), selected = "")
                             )
                         ),

                         # Create a spot for the barplot
                         mainPanel(
                             plotOutput("phonePlot")
                         )

                     )
        ),
        tabPanel("Metric Asso(Time-Lag)", 
                 sidebarLayout(
                     
                     # Define the sidebar with one input
                     sidebarPanel(width = 3,
                                  wellPanel(
                                      selectizeInput("combo_MAssoTL_Host", "Select Host :",
                                                     choices = c(""), selected = "")
                                  ),
                                  hr(),
                                  wellPanel(
                                      selectizeInput("combo_MAssoTL_CM", "Select Custer Metric :",
                                                     choices = c(""), selected = ""),
                                      selectizeInput("combo_MAssoTL_HM", "Select Host Metric :",
                                                     choices = c(""), selected = ""),
                                      selectizeInput("combo_MAssoTL_TM", "Select Task Metric :",
                                                     choices = c(""), selected = "")
                                  )
                     ),
                     
                     # Create a spot for the barplot
                     mainPanel(
                         plotOutput("phonePlot")
                     )
                 ) 
        ),
        tabPanel("Event Association", 
                 "This panel is intentionally left blank"
        ),
        tabPanel("Anomaly Detection", 
                 "This panel is intentionally left blank"
        ),
        tabPanel("Forecasting", 
                 "This panel is intentionally left blank"
        )
        
    )

)








# 
# 
# fluidPage(    
#     # Generate a row with a sidebar
#     sidebarLayout(      
#         
#         # Define the sidebar with one input
#         sidebarPanel(width = 3,
#             wellPanel(
#                 selectizeInput("combo_MAsso_Host", "Select Host :", 
#                                choices = c(""), selected = "")
#             ),
#             hr(),
#             wellPanel(
#                 selectizeInput("combo_MAsso_CM", "Select Custer Metric :", 
#                                choices = c(""), selected = ""),
#                 selectizeInput("combo_MAsso_HM", "Select Host Metric :", 
#                                choices = c(""), selected = ""),
#                 selectizeInput("combo_MAsso_TM", "Select Take Metric :", 
#                                choices = c(""), selected = "")
#             )
#         ),
#         
#         # Create a spot for the barplot
#         mainPanel(
#             plotOutput("phonePlot")  
#         )
#         
#     )
# )