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
  
  query <- "select * from monitoring_metric where agent_id = %s" %>% 
    sprintf(agent_id)
  
  # 룰셋을 읽어온다. 
  rs = dbSendQuery(con, query)
  anomalySet = fetch(rs, n = -1)
  
  dbDisconnect(con)
  
  list(
    table = anomalySet[, -1:-2],
    textVector =  apply(anomalySet[, -1:-2], 1, paste, collapse=", ")
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
  
  query <- "select * from monitoring_results where agent_id = %s" %>% 
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
            from monitoring_results
            where agent_id = %s" %>% 
    sprintf(agent_id)
  
  # 룰셋을 읽어온다. 
  rs = dbSendQuery(con, query)
  anomalyCount = fetch(rs, n = -1)
  
  dbDisconnect(con)
  
  anomalyCount$count %>% return()
  
}

get_model_info <- function(resource, target, metric, agent_id) {
  # resource <- 'host';target <- '192.168.0.160';metric <- 'cpu_stolen_percent';agent_id <- 27
  db_info <- read_json('../Source/mysql_info.json')
  
  con <- dbConnect(MySQL(), 
                   user = db_info$user, 
                   password = db_info$password,
                   dbname = db_info$dbname,
                   host = db_info$host, 
                   port = db_info$port)
  
  query <- "select *
            from model_info
            where resource = '%s' and target = '%s' and metric = '%s' and agent_id = '%s'
            order by id desc
            limit 1" %>% 
    sprintf(resource, target, metric, agent_id)
  
  rs = dbSendQuery(con, query)
  
  result <- fetch(rs, n=-1)
  
  dbDisconnect(con)
  
  return(result)
  
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
  
  HOST_TAG_LIST <- NULL
  
  HOST_METRIC_LIST <- NULL
  
  TASK_TAG_LIST <- NULL
  
  TASK_METRIC_LIST <- NULL
  
  DOCKER_TAG_LIST <- NULL
  
  DOCKER_METRIC_LIST <- NULL
  
  observeEvent(AGENT_ID(), {
    
    HOST_TAG_LIST <<- load_tag_list('host', 27)
    
    TASK_TAG_LIST <<- load_tag_list('task', 27)
    
    DOCKER_TAG_LIST <<- load_tag_list('docker', 27)
    
    HOST_METRIC_LIST <<- load_metric_list('host')
    
    TASK_METRIC_LIST <<- load_metric_list('task')
    
    DOCKER_METRIC_LIST <<- load_metric_list('docker')
    
    # updateSelectizeInput(
    #   session = session,
    #   inputId = 'selHost',
    #   choices = HOST_TAG_LIST
    # )
    # 
    # updateSelectizeInput(
    #   session = session,
    #   inputId = 'selTask',
    #   choices = TASK_TAG_LIST
    # )
    # 
    # updateSelectizeInput(
    #   session = session,
    #   inputId = 'selDocker',
    #   choices = DOCKER_TAG_LIST
    # )
    
  })
  

  # Modal Window  -- Metric Monitoring Chart ------------------------------------------------------------------
  metricMonitoringModal <- function(failed = FALSE) {
    modalDialog(size = "l", easyClose = T, 
                
                # plotOutput("metricMonitoring", height = "600px"),
                renderPlot({
                  # browser()
                  resultRule <- input$selMetricRule %>%
                    strsplit(', ') %>%
                    unlist()
                  
                  model_info <- get_model_info(resultRule[1],
                                               resultRule[2],
                                               resultRule[3],
                                               27)
                  if (nrow(model_info) == 0)
                    return('Modeling not yet')
                  
                  resource <- model_info$resource
                  
                  host <- model_info$target
                  
                  metric <- model_info$metric
                  
                  period <- 3
                  
                  unit <- model_info$unit
                  
                  groupby <- '5m'
                  
                  mount <- model_info$mount
                  
                  developed_at <- model_info$developed_at
                  
                  developed_at <- gsub(':', '-', developed_at)
                  
                  if (mount == 'null') {
                    
                    dir.name <-  paste("../Model",
                                       paste0('agent_id_', 27),
                                       resource, host,
                                       paste0('unit_', unit),
                                       metric, gsub(':', '-', developed_at), sep = "/")
                    
                  } else {
                    
                    dir.name <-  paste("../Model",
                                       paste0('agent_id_', AGENT_ID()),
                                       resource, host,
                                       paste0('unit_', unit),
                                       metric,
                                       paste0('mount_', mount),
                                       gsub(':', '-', developed_at), sep = "/")
                  }
                  
                  modelFile.name <- paste(dir.name, "fcst.rdata", sep = "/")
                  
                  figFile.name <- paste(dir.name, "anomaly.png", sep = "/")
                  
                  series <- load_single_metric(resource, host, metric, period, groupby,
                                               unit, 27, mount) %>% 
                    as.data.table()
                  
                  load(modelFile.name)
                  
                  # Forecastring 결과에 따라 Anomaly 탐지
                  future <- data.frame(ds = seq(min(series$ds),
                                                max(series$ds),
                                                by = posixt_helper_func(str_sub(groupby, -1))))
                  
                  fcst <- predict(fcastModel, future)
                  
                  pData <- merge(series, fcst, by = "ds", all.x = T)[, .(ds, y, yhat_lower, yhat_upper)]
                  
                  pData[, anomaly := y]
                  
                  pData[, anomaly := ifelse(y > yhat_upper | y < yhat_lower, y, NA)]
                  
                  anom_count <- pData[, sum(anomaly, na.rm = T)]
                  
                  g <- ggplot(pData, aes(ds, y)) + geom_point() +
                    geom_ribbon(aes(ymin = yhat_lower,
                                    ymax = yhat_upper),
                                alpha = 0.1) + 
                    xlim(c(min(pData$ds), max(pData$ds))) +
                    ylab(metric) +
                    xlab("Time")
                  
                  if (anom_count > 0) {
                    
                    g <- g + geom_point(aes(y = anomaly),
                                        size = 5, alpha = 0.5, color = "red") 
                  } 
                  
                  g
                }),
                
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
                     choices = c("1 hour" = 1,
                                 "3 hours" = 3,
                                 "6 hours" = 6), inline = T)
          
      ),
      
      hr(),
      
      fluidRow(style ="padding-left: 30px; padding-right: 30px",
        renderDygraph({
          
          metric <- input$selMetric
          
          host <- switch(input$anomType,
                         'service' = input$selService,
                         'host' = input$selHost,
                         'task' = input$selTask,
                         'docker' = input$selDocker)
          
          # if (input$anomType == 'host') {
          #   
          #   host_tag_list <- load_host_tag_list(27)
          #   
          #   host <- host_tag_list[host][[1]][1]
          #   
          # }
          # browser()
          series <- load_single_metric(input$anomType, host, metric, input$duringHour, '5m',
                                       '1', 27)
          
          ts <- xts(series$y,
                    order.by = series$ds,
                    tzone = Sys.getenv("TZ"))
          
          dygraph(ts) %>%
            dyRangeSelector(height = 30)
          
        })
      ), 
                

      if (failed)
        div(tags$b("Invalid Metric Information", style = "color: red;")),
      
      footer = tagList(
        modalButton("Close")
      )
    )
  }
  
  observeEvent(input$save, {
    
    if (input$setType == "Define a new metric") {
      
      showAdvancedOption()
      
      target <- switch(input$anomType,
                      'service' = input$selService,
                      'host' = input$selHost,
                      'task' = input$selTask,
                      'docker' = input$selDocker)
      # browser()
      save_config <- data.frame('agent_id' = 27,
                               'resource' = input$anomType,
                               'target' = target,
                               'metric' = input$selMetric,
                               'modeling_period' = input$modelingInterval,
                               'modeling_hour' = input$timeAt)
      
      db_info <- read_json('../Source/mysql_info.json')
      
      con <- dbConnect(MySQL(), 
                       user = db_info$user, 
                       password = db_info$password,
                       dbname = db_info$dbname,
                       host = db_info$host, 
                       port = db_info$port)
      
      dbWriteTable(con, 
                   name = 'monitoring_metric', 
                   value = save_config,
                   row.names = F,
                   append = T)
      
      dbCommit(con)
      
      dbDisconnect(con)
      
      print('Success to save model configuration!')
      
      showModal(modalDialog(
        title = "Success to save model configuration",
        easyClose = TRUE,
        footer = tagList(
          modalButton("Close")
        )
      ))
      
    }
  })
  
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
  observeEvent(input$anomType, {
    
    metric_list <- switch(input$anomType,
                          'host' = HOST_METRIC_LIST,
                          'task' = TASK_METRIC_LIST,
                          'docker' = DOCKER_METRIC_LIST)
    
    updateSelectizeInput(session, 'selMetric', choices = metric_list)
    
  })
  
  observeEvent(input$setType, {
    
      if (input$setType == "Define a new metric") {

        shinyjs::hide("selMetricRule")
        shinyjs::hide("actionViewMonitoring")
        shinyjs::hide("actionViewAllList")
        shinyjs::hide("tempPlot")
        
        shinyjs::hide("datatableAllList")
        shinyjs::hide("actionDelete")
        
        shinyjs::show("step1_comment")
        
        updateRadioButtons(session, 'anomType', selected = 'host')
        
        updateSelectizeInput(session, 'selHost', selected = 1)
        
        updateSelectizeInput(session, 'selMetric', selected = 1)

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
        # browser()
        output$datatableAllList <- renderDataTable(metricRules$table,
                                                   filter = 'bottom', class = "compact",
                                                   rownames = F,
                                                   selection = 'single',
                                                   options = list(
                                                     dom = 'rtp',
                                                     pageLength = 5,
                                                     columnDefs = list(list(className = 'dt-center', targets = 0:4))))
        
        updateSelectizeInput(session, "selMetricRule", label =  "Selecte a exsisting Rule : ", 
                             choices = metricRules$textVector)
        
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
                         # choices = metricRules$textVector, 
                         selected = metricRules$textVector[ss])
    
    # anomaly type
    updateRadioButtons(session, "anomType", label =  "Select anomaly detection type : ", 
                       # choices = c("Service" = 'service',
                       #             "Host" = 'host',
                       #             "Task" = 'task',
                       #             "Docker" = 'docker'), inline = T, 
                       selected = metricRules$table[ss, 1])
    
  })
  
  observeEvent(input$selMetricRule, {
    
    ss <- match(input$selMetricRule, metricRules$textVector)

    # 선택된 룰에 따라 next step의 모든 정보 수정 한다 ------------------------------------------------------
    
    # metric rule set
    updateSelectizeInput(session, "selMetricRule", label =  "Selecte a exsisting Rule : ", 
                         # choices = metricRules$textVector, 
                         selected = metricRules$textVector[ss])


    
    
    # anomaly type에 따라 ui가 변경되는 코드 -------------------------------------------------------
    output$anomInput <- renderUI({
      
      if(input$anomType == "service") {
        tagList(
          selectizeInput("selService", "Select Service : ", choices = NULL)
        )
        
      } else if (input$anomType == "host") {
        tagList(
          selectizeInput("selHost", "Select Host : ", choices = HOST_TAG_LIST)
        )
        
        
      } else if (input$anomType == "task") {
        tagList(
          selectizeInput("selTask", "Select Task : ", choices = TASK_TAG_LIST)
        )
        
        
      } else if (input$anomType == "docker") {
        tagList(
          selectizeInput("selDocker", "Select Docker : ", choices = DOCKER_TAG_LIST)
        )
        
        
      } 
      
    })
    

    if (!is.na(ss)) { 
      
      output$redefineMent <- renderText("Go to Next Step to revise metric monitoring conditions!")
      
      # updateSelectizeInput(session, "selMetric", label =  "Select a Metric to Monitor :", 
      #                      selected = metricRules$table[ss, 3])
      
      if(input$anomType == "service") {
        updateSelectizeInput(session, "selService", label =  "Select Service : ",
                             # choices = metricRules$table[ss, 4],
                             selected = metricRules$table[ss, 4])

      } else if(input$anomType == "host") {
        
        updateSelectizeInput(session, "selHost", label =  "Select Host : ",
                             selected = metricRules$table[ss, 2])
        
        updateSelectizeInput(session, "selMetric", label =  "Select a Metric to Monitor :",
                             choices = HOST_METRIC_LIST,
                             selected = metricRules$table[ss, 3])
        
      } else if(input$anomType == "task") {
        
        updateSelectizeInput(session, "selTask", label =  "Select Task : ",
                             selected = metricRules$table[ss, 2])
        
        updateSelectizeInput(session, "selMetric", label =  "Select a Metric to Monitor :",
                             choices = TASK_METRIC_LIST,
                             selected = metricRules$table[ss, 3])

      } else if(input$anomType == "docker") {
        updateSelectizeInput(session, "selDocker", label =  "Select Docker : ",
                             # choices = metricRules$table[ss, 2],
                             selected = metricRules$table[ss, 2])
        
        updateSelectizeInput(session, "selMetric", label =  "Select a Metric to Monitor :",
                             choices = DOCKER_METRIC_LIST,
                             selected = metricRules$table[ss, 3])
      }

      
    } #else {
      

      # updateSelectizeInput(session, "selMetricRule", label =  "Selecte a exsisting Rule : ", 
      #                      choices = "")
      
      # updateSelectizeInput(session, "selMetric", label =  "Select a Metric to Monior ----:", 
      #                      choices = "")
      # 
    #   updateSelectizeInput(session, "selMetric", "Select a Metric to Monitor : ", choices = NULL, 1)
    # 
    #   output$redefineMent <- renderText("Choose the metric to revise")
    #   
    # }
    
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
