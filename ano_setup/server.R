

getMetricRule <- function(agent_id) {
  # agent_id <- 5
  uid = "admin"
  password = "password"
  dbname = "defaultdb"
  host = "mysql.marathon.l4lb.thisdcos.directory"
  port = 3306
  
  con <- dbConnect(MySQL(), 
                   user = uid, 
                   password = password,
                   dbname = dbname,
                   host = host, 
                   port = port)
  
  query <- "select *
            from monitoring_metric
            where agent_id = '%s' and
                  is_delete = 0" %>% 
    sprintf(agent_id)
  
  # 룰셋을 읽어온다. 
  anomalySet = dbGetQuery(con, query, n = -1)
  
  dbDisconnect(con)
  # print('##############')
  # print(dbListConnections(MySQL()) %>% length())
  # print('##############')
  list(
    table = anomalySet[, c(-2, -8)],
    textVector =  apply(anomalySet[, c(-1, -2, -8)], 1, paste, collapse=", ")
  )
}

getMetricResult <- function(agent_id) {
  # agent_id <- 5
  uid = "admin"
  password = "password"
  dbname = "defaultdb"
  host = "mysql.marathon.l4lb.thisdcos.directory"
  port = 3306
  
  con <- dbConnect(MySQL(), 
                   user = uid, 
                   password = password,
                   dbname = dbname,
                   host = host, 
                   port = port)
  
  query <- "select *
            from monitoring_results
            where agent_id = '%s'" %>% 
    sprintf(agent_id)
  
  # 룰셋을 읽어온다. 
  anomalyResult = dbGetQuery(con, query, n = -1)
  
  dbDisconnect(con)
  # print('##############')
  # print(dbListConnections(MySQL()) %>% length())
  # print('##############')
  anomalyResult %>% return()
  
}

getAnomalyCount <- function(agent_id) {
  # agent_id <- 5
  uid = "admin"
  password = "password"
  dbname = "defaultdb"
  host = "mysql.marathon.l4lb.thisdcos.directory"
  port = 3306
  
  con <- dbConnect(MySQL(), 
                   user = uid, 
                   password = password,
                   dbname = dbname,
                   host = host, 
                   port = port)
  
  query <- "select count(agent_id) as count
            from monitoring_results
            where agent_id = '%s'" %>% 
    sprintf(agent_id)
  
  # 룰셋을 읽어온다. 
  anomalyCount = dbGetQuery(con, query, n = -1)
  
  dbDisconnect(con)
  # print('##############')
  # print(dbListConnections(MySQL()) %>% length())
  # print('##############')
  anomalyCount$count %>% return()
  
}

get_model_info <- function(resource, target, metric, agent_id) {
  # resource <- 'host';target <- '192.168.0.160';metric <- 'cpu_stolen_percent';agent_id <- 27
  
  uid = "admin"
  password = "password"
  dbname = "defaultdb"
  host = "mysql.marathon.l4lb.thisdcos.directory"
  port = 3306
  
  con <- dbConnect(MySQL(), 
                   user = uid, 
                   password = password,
                   dbname = dbname,
                   host = host, 
                   port = port)
  
  query <- "select *
            from model_info
            where resource = '%s' and
                  target = '%s' and
                  metric = '%s' and
                  agent_id = '%s'
            order by id desc
            limit 1" %>% 
    sprintf(resource, target, metric, agent_id)
  
  result = dbGetQuery(con, query, n = -1)
  
  dbDisconnect(con)
  # print('##############')
  # print(dbListConnections(MySQL()) %>% length())
  # print('##############')
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
  # con <- NULL
  query <- NULL
  
  HOST_TAG_LIST <- NULL
  
  HOST_METRIC_LIST <- NULL
  
  # TASK_TAG_LIST <- NULL
  # 
  # TASK_METRIC_LIST <- NULL
  
  DOCKER_TAG_LIST <- NULL
  
  DOCKER_METRIC_LIST <- NULL
  
  observeEvent(AGENT_ID(), {
    
    HOST_TAG_LIST <<- load_tag_list('host', AGENT_ID())
    
    # TASK_TAG_LIST <<- load_tag_list('task', 5)
    
    DOCKER_TAG_LIST <<- load_tag_list('docker', AGENT_ID())
    
    HOST_METRIC_LIST <<- load_metric_list('host')
    
    # TASK_METRIC_LIST <<- load_metric_list('task')
    
    DOCKER_METRIC_LIST <<- load_metric_list('docker')
    
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
                                               AGENT_ID())
                  if (nrow(model_info) == 0) {
                    
                    default <- default_time_seqeunce(6, '1h')
                    
                    default %>% 
                      ggplot(aes(ds, y)) + geom_point() +
                      xlim(c(min(default$ds),
                             max(default$ds))) +
                      ylab(resultRule[3]) +
                      xlab("Time")
                    
                  } else {
                    
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
                                         paste0('agent_id_', AGENT_ID()),
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
                                                 unit, AGENT_ID(), mount) %>% 
                      as.data.table()
                    
                    load(modelFile.name)
                    
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
                  }
                  
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
                                      choices = c("3 hour" = 3,
                                                  "6 hours" = 6,
                                                  "12 hours" = 12,
                                                  '24 hours' = 24,
                                                  '36 hours' = 36), inline = T)
                         
                ),
                
                hr(),
                
                fluidRow(style = "padding-left: 30px; padding-right: 30px",
                         renderDygraph({
                           
                           metric <- input$selMetric
                           
                           host <- switch(input$anomType,
                                          'service' = input$selService,
                                          'host' = input$selHost,
                                          # 'task' = input$selTask,
                                          'docker' = input$selDocker)
                           
                           series <- load_single_metric(input$anomType, host, metric, input$duringHour, '5m',
                                                        '1', AGENT_ID())
                           
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
  
  observeEvent(input$confirm, {
    # browser()
    
    uid = "admin"
    password = "password"
    dbname = "defaultdb"
    host = "mysql.marathon.l4lb.thisdcos.directory"
    port = 3306
    
    con <- dbConnect(MySQL(), 
                     user = uid, 
                     password = password,
                     dbname = dbname,
                     host = host, 
                     port = port)
    
    dbGetQuery(con, query)
    
    removeModal()
    
    dbCommit(con)
    
    dbDisconnect(con)
    # print('##############')
    # print('confirm')
    # print(dbListConnections(MySQL()) %>% length())
    # print('##############')
    metricRules <<- getMetricRule(AGENT_ID())
    # browser()
    output$TotalRules <- renderValueBox({
      valueBox(
        nrow(metricRules$table),
        subtitle = "Number of metrics defined",
        icon = icon("list-ul", class = "iconNexBig")
      )
    })
    
    output$datatableAllList <- renderDataTable(metricRules$table[, -1],
                                               filter = 'bottom', class = "compact",
                                               rownames = F,
                                               selection = 'single',
                                               options = list(
                                                 dom = 'rtp',
                                                 pageLength = 5,
                                                 columnDefs = list(list(className = 'dt-center', targets = 0:4))))
    
    updateSelectizeInput(session, "selMetricRule", label =  "Selecte a exsisting Rule : ", 
                         choices = metricRules$textVector)
    
  })
  
  observeEvent(input$confirm_db, {
    
    removeModal()
    
    metricRules <<- getMetricRule(AGENT_ID())
    
    output$TotalRules <- renderValueBox({
      valueBox(
        nrow(metricRules$table),
        subtitle = "Number of metrics defined",
        icon = icon("list-ul", class = "iconNexBig")
      )
    })
    
    output$datatableAllList <- renderDataTable(metricRules$table[, -1],
                                               filter = 'bottom', class = "compact",
                                               rownames = F,
                                               selection = 'single',
                                               options = list(
                                                 dom = 'rtp',
                                                 pageLength = 5,
                                                 columnDefs = list(list(className = 'dt-center', targets = 0:4))))
    
    updateSelectizeInput(session, "selMetricRule", label =  "Selecte a exsisting Rule : ", 
                         choices = metricRules$textVector)
    
    print('Success to save model configuration! by dfwrite')
    
  })
  
  observeEvent(input$save, {
    
    target <- switch(input$anomType,
                     'service' = input$selService,
                     'host' = input$selHost,
                     # 'task' = input$selTask,
                     'docker' = input$selDocker)
    
    uid = "admin"
    password = "password"
    dbname = "defaultdb"
    host = "mysql.marathon.l4lb.thisdcos.directory"
    port = 3306
    
    con <- dbConnect(MySQL(), 
                     user = uid, 
                     password = password,
                     dbname = dbname,
                     host = host, 
                     port = port)
    
    if (is.null(input$modelingInterval)) {
      
      modelingInterval <- 'Every Day'
      
      timeAt <- 3
      
    } else {
      
      modelingInterval <- input$modelingInterval
      
      timeAt <- input$timeAt
      
    }
    
    if (input$setType == "Define a new metric") {
      
      # browser()
      query <- "select exists(
                  select *
                  from monitoring_metric
                  where agent_id = '%s' and
                  resource = '%s' and
                  target = '%s' and
                  metric = '%s' and
                  is_delete = 0)" %>% 
        sprintf(AGENT_ID(),
                input$anomType,
                target,
                input$selMetric)
      
      result <- dbGetQuery(con, query)
      
      if (result[1, 1] == 1) {
        
        query <- "select *
                  from monitoring_metric
                  where agent_id = '%s' and
                        resource = '%s' and
                        target = '%s' and
                        metric = '%s' and
                        is_delete = 0" %>%
          sprintf(AGENT_ID(),
                  input$anomType,
                  target,
                  input$selMetric)
        
        old <- dbGetQuery(con, query)
        # browser()
        query <- "update monitoring_metric
                  set modeling_period = '%s',
                      modeling_hour = %d
                  where id = '%s'"  %>% 
          sprintf(modelingInterval,
                  timeAt,
                  old$id)
        # browser()
        # dbGetQuery(con, query)
        # con <<- con
        
        query <<- query
        # browser()
        new <- old
        
        new$modeling_period <- modelingInterval
        new$modeling_hour <- timeAt
        
        old <- old[, c(-1, -2, -8)] %>%
          gather(type, Old)
        
        new <- new[, c(-1, -2, -8)] %>%
          gather(type, New)
        
        message <- old %>%
          inner_join(new, by = 'type') %>% 
          unite(message, c('Old', 'New'), sep = ' -> ') %>% 
          unite(message, c('type', 'message'), sep = ' : ') %>% 
          apply(1, function(x) paste('<br>', x, '</br>')) %>% 
          HTML()
        
        print('Update the anomaly rule due to existing rule')
        
        showModal(modalDialog(
          title = "Update the anomaly rule due to existing rule",
          message,
          easyClose = TRUE,
          footer = tagList(
            modalButton("Close"),
            actionButton("confirm", 'Confirm')
          )
        ))
        
      } else {
        
        save_config <- data.frame('agent_id' = AGENT_ID(),
                                  'resource' = input$anomType,
                                  'target' = target,
                                  'metric' = input$selMetric,
                                  'modeling_period' = modelingInterval,
                                  'modeling_hour' = timeAt)
        
        message <- save_config[, -1] %>%
          gather(type, message) %>% 
          unite(message, c('type', 'message'), sep = ' : ') %>% 
          apply(1, function(x) paste('<br>', x, '</br>')) %>% 
          HTML()
        
        dbWriteTable(con, 
                     name = 'monitoring_metric', 
                     value = save_config,
                     row.names = F,
                     append = T)
        
        showModal(modalDialog(
          title = "Success to save model configuration",
          message,
          easyClose = TRUE,
          footer = tagList(
            modalButton("Close"),
            actionButton("confirm_db", 'Confirm')
          )
        ))
        
      }
      
    } else if (input$setType == "Redefine") {
      # browser()
      ss <- match(input$selMetricRule, metricRules$textVector)
      
      id <- metricRules$table$id[ss]
      
      query <- "update monitoring_metric
                set resource = '%s',
                    target = '%s',
                    metric = '%s',
                    modeling_period = '%s',
                    modeling_hour = '%s'
                    where id = '%s'" %>% 
        sprintf(input$anomType,
                target,
                input$selMetric,
                modelingInterval,
                timeAt,
                id)
      
      # dbGetQuery(con, query)
      query <<- query
      # browser()
      message <- metricRules$table[ss, -1] %>% 
        gather(type, Old) %>% 
        inner_join(data.frame('type' = c('resource', 'target', 'metirc',
                                         'modeling_period', 'modeling_hour'),
                              'New' = c(input$anomType, target, input$selMetric,
                                        modelingInterval, timeAt),
                              stringsAsFactors = F), by = 'type') %>% 
        unite(message, c('Old', 'New'), sep = ' -> ') %>% 
        unite(message, c('type', 'message'), sep = ' : ') %>% 
        apply(2, function(x) paste('<br>', x, '</br>')) %>% 
        HTML()
      
      print('Update the anomaly rule')
      
      showModal(modalDialog(
        title = "Update the anomaly rule",
        message,
        easyClose = TRUE,
        footer = tagList(
          modalButton("Close"),
          actionButton("confirm", 'Confirm')
        )
      ))
      
    }
    
    dbCommit(con)
    
    dbDisconnect(con)
    # print('##############')
    # print(dbListConnections(MySQL()) %>% length())
    # print('##############')
    # browser()
    # metricRules <- getMetricRule(AGENT_ID())
    # 
    # output$TotalRules <- renderValueBox({
    #   valueBox(
    #     nrow(metricRules$table),
    #     subtitle = "Number of metrics defined",
    #     icon = icon("list-ul", class = "iconNexBig")
    #   )
    # })
    # 
    # output$datatableAllList <- renderDataTable(metricRules$table[, -1],
    #                                            filter = 'bottom', class = "compact",
    #                                            rownames = F,
    #                                            selection = 'single',
    #                                            options = list(
    #                                              dom = 'rtp',
    #                                              pageLength = 5,
    #                                              columnDefs = list(list(className = 'dt-center', targets = 0:4))))
    # 
    # updateSelectizeInput(session, "selMetricRule", label =  "Selecte a exsisting Rule : ", 
    #                      choices = metricRules$textVector)
    
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
    
    if (input$setType == "Define a new metric") {
      
      metric_list <- switch(input$anomType,
                            'host' = HOST_METRIC_LIST,
                            # 'task' = TASK_METRIC_LIST,
                            'docker' = DOCKER_METRIC_LIST)
      
      updateSelectizeInput(session, 'selMetric', choices = metric_list)
      
    }
    
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
      
      # updateSelectizeInput(session, 'selHost')
      # 
      # updateSelectizeInput(session, 'selMetric')
      
    } else if (input$setType == "Redefine") {
      
      # if(is.null(metricRules)) metricRules <<- getMetricRule(AGENT_ID())
      
      shinyjs::show("selMetricRule")
      shinyjs::show("actionViewMonitoring")
      shinyjs::show("actionViewAllList")
      shinyjs::show("tempPlot")
      
      shinyjs::show("datatableAllList")
      shinyjs::hide("actionDelete")
      
      shinyjs::hide("step1_comment")        
      
    } else if (input$setType == "Delete") {
      
      # if(is.null(metricRules)) metricRules <<- getMetricRule(AGENT_ID())
      
      shinyjs::show("selMetricRule")
      shinyjs::show("actionViewMonitoring")
      shinyjs::show("actionViewAllList")
      shinyjs::show("tempPlot")
      
      shinyjs::show("datatableAllList")
      shinyjs::show("actionDelete")
      
      shinyjs::hide("step1_comment")     
    }
    
  })
  
  observeEvent(input$actionDelete, {
    
    if (!is.null(input$datatableAllList_rows_selected)) {
      
      ss = input$datatableAllList_rows_selected
      
      id <- metricRules$table$id[ss]
      
      query <<- "update monitoring_metric
                 set is_delete = 1
                 where id = '%s'" %>% 
        sprintf(id)
      
      # browser()
      message <- metricRules$table[ss, -1] %>% 
        gather(type, value) %>% 
        unite(message, c('type', 'value'), sep = ' : ') %>% 
        apply(2, function(x) paste('<br>', x, '</br>')) %>% 
        HTML()
      
      showModal(modalDialog(
        title = "Delete the anomaly rule",
        message,
        easyClose = TRUE,
        footer = tagList(
          modalButton("Close"),
          actionButton("confirm", 'Confirm')
        )
      ))
      
      # uid = "admin"
      # password = "password"
      # dbname = "defaultdb"
      # host = "mysql.marathon.l4lb.thisdcos.directory"
      # port = 3306
      # 
      # con <- dbConnect(MySQL(), 
      #                  user = uid, 
      #                  password = password,
      #                  dbname = dbname,
      #                  host = host, 
      #                  port = port)
      # 
      # dbGetQuery(con, query)
      # 
      # dbCommit(con)
      # 
      # dbDisconnect(con)
      # 
      # metricRules <<- getMetricRule(AGENT_ID())
      # 
      # output$datatableAllList <- renderDataTable(metricRules$table[, -1],
      #                                            filter = 'bottom', class = "compact",
      #                                            rownames = F,
      #                                            selection = 'single',
      #                                            options = list(
      #                                              dom = 'rtp',
      #                                              pageLength = 5,
      #                                              columnDefs = list(list(className = 'dt-center', targets = 0:4))))
      # 
      # updateSelectizeInput(session, "selMetricRule", label =  "Selecte a exsisting Rule : ", 
      #                      choices = metricRules$textVector)
      # 
      # output$TotalRules <- renderValueBox({
      #   valueBox(
      #     nrow(metricRules$table),
      #     subtitle = "Number of metrics defined",
      #     icon = icon("list-ul", class = "iconNexBig")
      #   )
      # })
      
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
        output$datatableAllList <- renderDataTable(metricRules$table[, -1],
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
  observeEvent(input$datatableAllList_rows_selected, {
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
                       selected = metricRules$table[ss, 2])
    
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
        
        
        # } else if (input$anomType == "task") {
        #   tagList(
        #     selectizeInput("selTask", "Select Task : ", choices = TASK_TAG_LIST)
        #   )
        
        
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
      # browser()
      if(input$anomType == "service") {
        updateSelectizeInput(session, "selService", label =  "Select Service : ",
                             # choices = metricRules$table[ss, 4],
                             selected = metricRules$table[ss, 5])
        
      } else if(input$anomType == "host") {
        
        updateSelectizeInput(session, "selHost", label =  "Select Host : ",
                             selected = metricRules$table[ss, 3])
        
        updateSelectizeInput(session, "selMetric", label =  "Select a Metric to Monitor :",
                             choices = HOST_METRIC_LIST,
                             selected = metricRules$table[ss, 4])
        
        # } else if(input$anomType == "task") {
        #   
        #   updateSelectizeInput(session, "selTask", label =  "Select Task : ",
        #                        selected = metricRules$table[ss, 3])
        #   
        #   updateSelectizeInput(session, "selMetric", label =  "Select a Metric to Monitor :",
        #                        choices = TASK_METRIC_LIST,
        #                        selected = metricRules$table[ss, 4])
        
      } else if (input$anomType == "docker") {
        updateSelectizeInput(session, "selDocker", label =  "Select Docker : ",
                             # choices = metricRules$table[ss, 2],
                             selected = metricRules$table[ss, 3])
        
        updateSelectizeInput(session, "selMetric", label =  "Select a Metric to Monitor :",
                             choices = DOCKER_METRIC_LIST,
                             selected = metricRules$table[ss, 4])
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
    # jqui_draggable(ui = '.modal-content')
    
  })
  
  # Model UI : Advanced Options
  observeEvent(input$adOptions, {
    showModal(showAdvancedOption())
    # jqui_draggable(selector = '.modal-content')
    
  })
  
}
