getMetricRule <- function(agent_id) {
  library(RMySQL)
  agent_id <- 27
  uid = "admin"
  password = "password"
  dbname = "defaultdb"
  host = "192.168.0.166"
  port = 27604
  
  con <- dbConnect(MySQL(), 
                   user = uid, 
                   password = password,
                   dbname = dbname,
                   host = host, 
                   port = port)
  
  query <- "select * from temp_monitoring_metric where agent_id = %s" %>% 
    sprintf(agent_id)
  
  # 룰셋을 읽어온다. 
  rs = dbSendQuery(con, query)
  anomalySet = fetch(rs, n = -1)
  
  dbDisconnect(con)
  
  list(
    table = anomalySet[,-(1:2)],
    textVector =  apply(anomalySet[, -(1:2)],1, paste, collapse=", ")
  )
}

getMetricResult <- function(agent_id) {
  library(RMySQL)
  agent_id <- 27
  uid = "admin"
  password = "password"
  dbname = "defaultdb"
  host = "192.168.0.166"
  port = 27604
  
  con <- dbConnect(MySQL(), 
                   user = uid, 
                   password = password,
                   dbname = dbname,
                   host = host, 
                   port = port)
  
  query <- "select * from temp_monitoring_results where agent_id = %s" %>% 
    sprintf(agent_id)
  
  # 룰셋을 읽어온다. 
  rs = dbSendQuery(con, query)
  anomalyResult = fetch(rs, n = -1)
  
  dbDisconnect(con)
  
  anomalyResult %>% return()
  
}

getAnomalyCount <- function(agent_id) {
  library(RMySQL)
  agent_id <- 27
  uid = "admin"
  password = "password"
  dbname = "defaultdb"
  host = "192.168.0.166"
  port = 27604
  
  con <- dbConnect(MySQL(), 
                   user = uid, 
                   password = password,
                   dbname = dbname,
                   host = host, 
                   port = port)
  
  query <- "select count(agent_id) as count
            from temp_monitoring_results
            where agent_id = %s" %>% 
    sprintf(agent_id)
  
  # 룰셋을 읽어온다. 
  rs = dbSendQuery(con, query)
  anomalyCount = fetch(rs, n = -1)
  
  dbDisconnect(con)
  
  anomalyCount$count %>% return()
  
}

server <- function(input, output, session){
  
  AGENT_ID <- reactive({
    
    url_search <- session$clientData$url_search
    
    agent <- str_extract(url_search, 'agent_id=\\d+') %>%
      strsplit('=') %>%
      unlist()
    
    agent[2]
    
  })
  
  # Session 내 global 변수 ----------------------------------------
  viewTalbeYN = F              # 테이블 보기를 하고 있는 상태인지
  metricRules <- getMetricRule(AGENT_ID())          # DB에서 가져온 룰셋 데이터 
  bRuleTable <- F              # 테이블에 룰셋이 들어가 있는지
  metricResult <- getMetricResult(AGENT_ID())

  # Modal Window  -- Metric Monitoring Chart ------------------------------------------------------------------
  metricMonitoringModal <- function(failed = FALSE) {
    modalDialog(size = "l", easyClose = T, 
                
                plotOutput("metricMonitoring", height = "600px"),
                
                if (failed)
                  div(tags$b("Invalid Metric Information", style = "color: red;")),
                
                footer = tagList(
                  modalButton("Close")
                )
    )
  }
  
  # Modal Window  -- Metric Chart ------------------------------------------------------------------
  metricChartModal <- function(failed = FALSE) {
    modalDialog(size = "l", easyClose = T, 
                
      fluidRow(style = "padding-left: 30px; padding-right: 30px, padding-bottom: 0px",
        
        br(),
        radioButtons("duringHour", "Select Chart Period : ", 
                     choices = c("1 hour", "3 hours", "6 hours"), inline = T)
          
      ),
      
      hr(),
      
      fluidRow(style ="padding-left: 30px; padding-right: 30px",
        renderDygraph({
          dygraph(iris, aes(Sepal.Length, Sepal.Width)) + geom_point(size = 3) + geom_line()
          
        })
      ), 
                

      if (failed)
        div(tags$b("Invalid Metric Information", style = "color: red;")),
      
      footer = tagList(
        modalButton("Close")
      )
    )
  }
  
  # Modal Window  -- Advanced Option ------------------------------------------------------------------
  showAdvancedOption <- function(failed = FALSE) {
    modalDialog(size = "l", easyClose = T, 
                
          h3("Define Advanced Option!"),
                
          fluidRow(style = "padding-left: 20px;",
            br(),
            column(width = 4, 
                   radioButtons("modelingInterval", "Select Modeling Interval :", 
                                c("Every Day", "Every Week"), inline = T, width = "100%")
            ),
            column(width = 4,
                   sliderInput("AlertConfidenceInterval", "Select Alert Condidence Interval :", 
                               min = 0.8, max = 0.999, value = 0.95, width = "90%")
            ),
            column(width = 4,
                   sliderInput("WarningConfidenceInterval", "Select Warning Condidence Interval :", 
                               min = 0.8, max = 0.999, value = 0.80, width = "90%")
            )
            
          ), 
          
          fluidRow(style = "padding-left: 20px;",
             br(),
             column(width = 4, 
                    radioButtons("monitoringInterval", "Select Monitoring Interval :", 
                                 c("10s", "30s", "1m", "5m"), inline = T, width = "100%")
             ),
             column(width = 4,
                    sliderInput("timeAt", "The model is learned at (o'clock): ", min = 0, max=23, value = 3,  width = "90%")
                    
             )
            
          ),
          

          footer = tagList(
            modalButton("Close")
          )
    )
  }
  
  
  output$TotalRules <- renderValueBox({
    valueBox(
      nrow(metricRules$table),
      subtitle = "Number of metrics defined",
      icon = icon("list-ul", class = "iconNexBig")
    )
  })
  
  output$AlertCount <- renderValueBox({
    valueBox(
      getAnomalyCount(AGENT_ID()),
      subtitle = "Number of alerts during last 1 hour",
      icon = icon("exclamation-triangle", class = "iconNexBig")
    )
  })
  
  
  # set type에 따라 ui가 변경되는 코드 -------------------------------------------------------
  
  
  observeEvent(input$setType, {
    
      if (input$setType == "Define a new metric") {

        shinyjs::hide("selMetricRule")
        shinyjs::hide("actionViewMonitoring")
        shinyjs::hide("actionViewAllList")
        shinyjs::hide("tempPlot")
        
        shinyjs::hide("datatableAllList")
        shinyjs::hide("actionDelete")
        
        shinyjs::show("step1_comment")

      } else if(input$setType == "Redefine") {
        
        if(is.null(metricRules)) metricRules <<- getMetricRule(AGENT_ID())
        
        shinyjs::show("selMetricRule")
        shinyjs::show("actionViewMonitoring")
        shinyjs::show("actionViewAllList")
        shinyjs::show("tempPlot")
        
        shinyjs::show("datatableAllList")
        shinyjs::hide("actionDelete")
        
        shinyjs::hide("step1_comment")        
       
      } else if (input$setType == "Delete") {
        
        if(is.null(metricRules)) metricRules <<- getMetricRule(AGENT_ID())
        
        shinyjs::show("selMetricRule")
        shinyjs::show("actionViewMonitoring")
        shinyjs::show("actionViewAllList")
        shinyjs::show("tempPlot")
        
        shinyjs::show("datatableAllList")
        shinyjs::show("actionDelete")
        
        shinyjs::hide("step1_comment")     
      }
    
})

  
  # output$anomInput0 <- renderUI({
  #   
  #   if (input$setType == "Define a new metric") {
  #     
  #      h4(icon("arrow-right", class = "iconNexButton"), "Go to Next Step!!!")
  # 
  #   } else if(input$setType == "Redefine") {
  #     
  #     if(is.null(metricRules)) metricRules <<- getMetricRule()
  #     
  #     tagList(
  #       selectizeInput("selMetricRule", "Selecte a exsisting Rule : ", choices = metricRules$textVector, 1),
  #       actionButton("actionViewMonitoring", "View realtime Chart", width = "250px", style = "margin-right: 20px;",
  #                    icon = icon("image", class = "iconNexButton")),
  # 
  #       actionButton("actionViewAllList", "View all Existing Rules", width = "250px",
  #                    icon = icon("list-ul", class = "iconNexButton")),
  #       br(), br(), br(),
  #       
  #       dataTableOutput("datatableAllList"),
  #       br(),
  #       verbatimTextOutput("redefineMent")
  #     ) 
  #   } else if (input$setType == "Delete") {
  #     
  #     if(is.null(metricRules)) metricRules <<- getMetricRule()
  #     
  #     tagList(
  #       selectizeInput("selMetricRule", "Selecte a exsisting Rule : ", choices = metricRules$textVector, 1),
  #       actionButton("actionDeleteRule", "Delete Seleted Rules", width = "250px",
  #                    icon = icon("trash", class = "iconNexButton")),
  #       br(), br()
  #     )
  #     
  #   }
  # })
  
  
  # View Existing All Rules --------------------------------------------------------------
  
  # Show modal when button is clicked.
  observeEvent(input$actionViewAllList, {
    
    if(viewTalbeYN == F) {
      
      # Mysql에서 룰 읽어와서 저장 ---------------------
      
      if(!bRuleTable) {
        output$datatableAllList <- renderDataTable(metricRules$table,
                                                   filter = 'bottom', class = "compact",
                                                   rownames = F,
                                                   selection = 'single',
                                                   options = list(
                                                     dom = 'rtp',
                                                     pageLength = 5,
                                                     columnDefs = list(list(className = 'dt-center', targets = 0:6))))
        
        bRuleTable <<- T
      }
                                                 
      shinyjs::show("datatableAllList")
      viewTalbeYN <<- T
      
      updateActionButton(session, "actionViewAllList", "Hide Existing Rules", 
                         icon = icon("eye-slash", class = "iconNexButton"))
      
    } else if(viewTalbeYN == T) {
      
      
      shinyjs::hide("datatableAllList")
      # output$datatableAllList <- renderDataTable(NULL, height ="0px")
      viewTalbeYN <<- F
      
      updateActionButton(session, "actionViewAllList", "View all Existing Rules", 
                   icon = icon("list-ul", class = "iconNexButton"))

    }
    
  })
    
  
  # datatable (룰)이 선택 될때마다 콤보 박스가 바뀌도록....
  observe({
    ss = input$datatableAllList_rows_selected
    # 선택된 룰에 따라 next step의 모든 정보 수정 한다 ------------------------------------------------------
    updateSelectizeInput(session, "selMetricRule", label =  "Selecte a exsisting Rule : ", 
                         choices = metricRules$textVector, 
                         selected = metricRules$textVector[ss])
    
    # anomaly type
    updateRadioButtons(session, "anomType", label =  "Select anomaly detection type : ", 
                       choices = c("Service", "Host", "Task", "Docker"), inline = T, 
                       selected = str_to_title(metricRules$table[ss, 1]))
    
  })
  
  observeEvent(input$selMetricRule, {
    
    ss <- match(input$selMetricRule, metricRules$textVector)

    # 선택된 룰에 따라 next step의 모든 정보 수정 한다 ------------------------------------------------------
    
    # metric rule set
    updateSelectizeInput(session, "selMetricRule", label =  "Selecte a exsisting Rule : ", 
                         choices = metricRules$textVector, 
                         selected = metricRules$textVector[ss])


    
    
    # anomaly type에 따라 ui가 변경되는 코드 -------------------------------------------------------
    output$anomInput <- renderUI({
      
      if(input$anomType == "Service") {
        tagList(
          selectizeInput("selService", "Select Service : ", choices = NULL, 1)
        )
        
      } else if (input$anomType == "Host") {
        tagList(
          selectizeInput("selHost", "Select Host : ", choices = NULL, 1)
        )
        
      } else if (input$anomType == "Task") {
        tagList(
          selectizeInput("selTask", "Select Task : ", choices = NULL, 1)
        )      
        
      } else if (input$anomType == "Docker") {
        tagList(
          selectizeInput("selDocker", "Select Docker : ", choices = NULL, 1)
        )      
        
      } 
      
    })
    

    if (!is.na(ss)) { 
      
      output$redefineMent <- renderText("Go to Next Step to revise metric monitoring conditions!")
      
      updateSelectizeInput(session, "selMetric", label =  "Select a Metric to Monior :", 
                           choices = metricRules$table[ss, 5])
      
      if(input$anomType == "Service") {
        updateSelectizeInput(session, "selService", label =  "Select Service : ",
                             choices = metricRules$table[ss, 6],
                             selected = metricRules$table[ss, 6])

      } else if(input$anomType == "Host") {
        
        updateSelectizeInput(session, "selHost", label =  "Select Host : ",
                             choices = metricRules$table[ss, 2])
      } else if(input$anomType == "Task") {
        

        print(metricRules$table[ss, 3])
        updateSelectizeInput(session, "selTask", label =  "Select Task : ",
                             choices = metricRules$table[ss, 3])

      } else if(input$anomType == "Docker") {
        updateSelectizeInput(session, "selDocker", label =  "Select Docker : ",
                             choices = metricRules$table[ss, 4],
                             selected = metricRules$table[ss, 4])
      }

      
    } else {
      

      # updateSelectizeInput(session, "selMetricRule", label =  "Selecte a exsisting Rule : ", 
      #                      choices = "")
      
      updateSelectizeInput(session, "selMetric", label =  "Select a Metric to Monior ----:", 
                           choices = "")
      
      updateSelectizeInput(session, "selMetric", "Select a Metric to Monior : ", choices = NULL, 1)

      output$redefineMent <- renderText("Choose the metric to revise")
      
    }
    
  })

  
  
  

  
  
  #  Model UI : Anomaly Monitoring Chart
  observeEvent(input$actionViewMonitoring, {
    showModal(metricMonitoringModal())
    jqui_draggable(ui = '.modal-content')
  })
  
   
  #  Model UI : Metric Chart 
  observeEvent(input$actionViewMetric, {
    showModal(metricChartModal())
    jqui_draggable(ui = '.modal-content')
    
  })
  
  # Model UI : Advanced Options
  observeEvent(input$adOptions, {
    showModal(showAdvancedOption())
    # jqui_draggable(selector = '.modal-content')
    
  })
  
  
  

}
