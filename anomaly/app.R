#### Anomaly ####

source("../Source/load_package.R", local = T, encoding = "utf-8")
source("../Source/server_func.R", local = T, encoding = "utf-8")


global_series = NULL

global_pData = NULL

numVar = NULL

HOST_TAG_LIST <- NULL

HOST_METRIC_LIST <- NULL

HOST_MOUNT_PATH <- NULL

# TASK_TAG_LIST <- NULL
# 
# TASK_METRIC_LIST <- NULL

DOCKER_TAG_LIST <- NULL

DOCKER_METRIC_LIST <- NULL


ui <- fluidPage(
  
  includeCSS("../www/custom.css"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      width = 3,
      
      wellPanel(
        
        prettyRadioButtons(
          inputId = 'resource',
          label = 'Select Resource',
          choices = list('Host' = 'host',
                         # 'Task' = 'task',
                         'Docker' = 'docker'),
          selected = 'host',
          inline = T
        ),
        
        selectizeInput(
          inputId = 'resource_assist',
          label = 'Select Host',
          choices = ''
        ),
        
        # conditionalPanel(
        #   condition = "input.resource == 'task'",
        #   helpText("Note : if task name is same and host is seperated, Merge = No.\
        #                    if host is seperated for same task, Merge = Yes."),
        #   prettyRadioButtons(
        #     inputId = 'merge',
        #     label = 'Merge or Not',
        #     choices = list('Yes' = 1,
        #                    'No' = 0),
        #     selected = 1,
        #     inline = T
        #   )
        # ),
        # 
        # conditionalPanel(
        #   condition = "input.merge == '0' & input.resource == 'task'",
        #   selectizeInput(
        #     inputId = 'host_for_task',
        #     label = 'Select Host',
        #     choices = ''
        #   )
        #   
        # ),
        
        selectizeInput(
          inputId = 'single_metric',
          label = 'Select Metric',
          choices = ''
        ),
        
        uiOutput('mount')
        
      ),
      
      wellPanel(
        
        actionButton("execute",
                     "  Execute",
                     icon = icon("sign-out"),
                     width = "100%",
                     Height = 40)
        
      ),
      
      br(),
      
      h4(class = 'h4_alter', "Advanced Option"),
      
      br(),
      
      wellPanel(
        
        prettyRadioButtons(
          "unit",
          'Select Time Unit',
          choices = list('Days' = 0,
                         'Hours' = 1),
          selected = 0,
          inline = T),
        
        sliderInput("period",
                    "Select Data Period (Days)",
                    min = 3, max = 240, value = 3),
        
        selectInput(
          "renewal",
          'Select time to renew graph',
          choices = c('off', '5s', '10s', '30s', '1m', '5m', '10m', '30m', '1h'),
          selected = '10s'),
        
        prettyRadioButtons(
          "groupby",
          'Select Group By',
          choices = c('1m', '5m', '10m', '1h'),
          selected = '1h',
          inline = T),
        
        sliderInput(
          "anomaly_CI",
          "Select Confidence Interval",
          min = 0.9, max = 1, value = 0.95),
        
        helpText("It takes much time to make forecasting model if there is no model for this metric!"),
        
        style = "padding: 15px 20px 0px 20px;"
        
      )
      
    ),
    
    mainPanel(
      
      width = 9,
      
      tags$body(class = 'body_alter'),
      
      fluidRow(
        
        column(
          
          width = 8,
          
          fluidRow(
            
            class = 'graph_panel_ano',
            
            br(),
            
            h4(class = 'h4_alter', "Anomaly Detection Chart"),
            
            hr(),
            
            plotOutput("monitoring", height = 500) %>% 
              withSpinner()
            
          ),
          
          fluidRow(
            
            class = 'notice_ano',
            
            verbatimTextOutput("notice"),
            tags$style("#notice{color: green;
                                font-size: 20px;
                                font-style: italic;}")
            
          ),
          
          fluidRow(
            
            class = 'graph_panel_ano',
            
            br(),
            
            h4(class = 'h4_alter', "Whole data plot for Modeling Data"),
            
            hr(),
            
            imageOutput("modeling_img", height = '70%', width = '100%') %>% 
              withSpinner()
            
          )
          
        ),
        
        column(
          
          width = 4,
          
          fluidRow(
            
            class = 'graph_panel',
            
            br(),
            
            h4(class = 'h4_alter', "Anomaly Detection Chart"),
            
            hr(),
            
            dataTableOutput('anomaly_table') %>% 
              withSpinner()
          )
          
        )
        
      )
      
    ) # mainPanel
    
  ) # sidebarLayout
  
)


server <- function(input, output, session) {
  
  AGENT_ID <- reactive({
    
    url_search <- session$clientData$url_search
    
    agent <- str_extract(url_search, 'agent_id=\\d+') %>%
      strsplit('=') %>%
      unlist()
    
    agent[2]
    
  })
  
  
  observeEvent(AGENT_ID(), {
    
    label_ <- switch(input$resource,
                     'host' = 'Select Host Name',
                     # 'task' = 'Select Task Name',
                     'docker' = 'Select Container Name')
    
    HOST_TAG_LIST <<- load_tag_list('host', AGENT_ID())
    
    # TASK_TAG_LIST <<- load_tag_list('task', AGENT_ID())
    
    DOCKER_TAG_LIST <<- load_tag_list('docker', AGENT_ID())
    
    HOST_METRIC_LIST <<- load_metric_list('host')
    
    # TASK_METRIC_LIST <<- load_metric_list('task')
    
    DOCKER_METRIC_LIST <<- load_metric_list('docker')
    
    if (sum(unlist(HOST_TAG_LIST) == 'null') == 2) {
      
      HOST_MOUNT_PATH <<- c('null')
      
    } else {
      
      HOST_MOUNT_PATH <<- load_host_disk_mount_path(input$resource_assist,
                                                    AGENT_ID())
      
    }
    
    resource_assist <- switch(input$resource,
                              'host' = HOST_TAG_LIST,
                              # 'task' = TASK_TAG_LIST,
                              'docker' = DOCKER_TAG_LIST)
    
    metrics <- switch(input$resource,
                      'host' = HOST_METRIC_LIST,
                      # 'task' = TASK_METRIC_LIST,
                      'docker' = DOCKER_METRIC_LIST)
    
    updateSelectizeInput(
      session = session,
      inputId = 'resource_assist',
      label = label_,
      choices = resource_assist)
    
    updateSelectizeInput(
      session = session,
      inputId = 'single_metric',
      choices = metrics
    )
    
    # if (input$resource != 'task') {
    #   
    #   updateSelectizeInput(
    #     session = session,
    #     inputId = 'merge',
    #     selected = '1'
    #   )
    #   
    #   updateSelectizeInput(
    #     session = session,
    #     inputId = 'host_for_task',
    #     selected = ''
    #   )
    #   
    # }
    
  })
  
  
  observeEvent(input$resource, {
    
    label_ <- switch(input$resource,
                     'host' = 'Select Host Name',
                     # 'task' = 'Select Task Name',
                     'docker' = 'Select Container Name')
    
    resource_assist <- switch(input$resource,
                              'host' = HOST_TAG_LIST,
                              # 'task' = TASK_TAG_LIST,
                              'docker' = DOCKER_TAG_LIST)
    
    metrics <- switch(input$resource,
                      'host' = HOST_METRIC_LIST,
                      # 'task' = TASK_METRIC_LIST,
                      'docker' = DOCKER_METRIC_LIST)
    
    updateSelectizeInput(
      session = session,
      inputId = 'resource_assist',
      label = label_,
      choices = resource_assist)
    
    updateSelectizeInput(
      session = session,
      inputId = 'single_metric',
      choices = metrics
    )
    
    # if (input$resource != 'task') {
    #   
    #   updateSelectizeInput(
    #     session = session,
    #     inputId = 'merge',
    #     selected = '1'
    #   )
    #   
    #   updateSelectizeInput(
    #     session = session,
    #     inputId = 'host_for_task',
    #     selected = ''
    #   )
    #   
    # }
    
  })
  
  
  observeEvent(input$single_metric, {
    
    if ((input$resource == 'host') &
        (input$single_metric %in% HOST_METRIC_LIST$host_disk)) {
      
      output$mount <- renderUI({
        
        selectizeInput(
          'mount_path',
          'Select Mount Path',
          choices = HOST_MOUNT_PATH
        )
      })
      
    } else {
      
      output$mount <- renderUI({
        
        conditionalPanel(
          condition = 'false',
          selectizeInput(
            'mount_path',
            'Select Mount Path',
            choices = 'null'
          )
        )
      })
      
    }
  })
  
  
  observeEvent(c(input$resource_assist, input$merge), {
    
    if (input$resource == 'host' &
        input$resource_assist != '' &
        !str_detect(input$resource_assist, 'Choose')) {
      
      HOST_MOUNT_PATH <<- load_host_disk_mount_path(input$resource_assist,
                                                    AGENT_ID())
      updateSelectInput(
        session = session,
        inputId = 'mount_path',
        choices = HOST_MOUNT_PATH
      )
      
    }
    
    # if (input$merge == "0" & input$resource == 'task') {
    #   
    #   choices_ <- load_host_list_for_task(input$resource_assist)
    #   
    #   updateSelectizeInput(
    #     session = session,
    #     inputId = 'host_for_task',
    #     choices = choices_
    #   )
    #   
    # } else {
    #   
    #   updateSelectizeInput(
    #     session = session,
    #     inputId = 'host_for_task',
    #     choices = ''
    #   )
    #   
    # }
    
  })

  
  observeEvent(input$unit, {
    
    if (input$unit == '0') {
      
      updateSliderInput(
        session = session,
        inputId = 'period',
        label = 'Select Data Period (Days)',
        value = 3, min = 1, max = 60
      )
      
      updatePrettyRadioButtons(
        session = session,
        inputId = 'groupby',
        selected = '1h'
      )
      
    } else {
      
      updateSliderInput(
        session = session,
        inputId = 'period',
        label = 'Select Data Period (Hours)',
        value = 6, min = 1, max = 60
      )
      
      updatePrettyRadioButtons(
        session = session,
        inputId = 'groupby',
        selected = '10m'
      )
      
    }
    
  })
  
  
  observe({
    
    if (input$single_metric != "") {
      
      resource <- input$resource
      
      host <- input$resource_assist
      
      metric <- input$single_metric
      
      period <- input$period
      
      unit <- input$unit
      
      groupby <- input$groupby
      
      mount <- input$mount_path
      
      renewal <- input$renewal
      
      renewal_time <- renew(renewal)
      
      if (renewal_time > 0)
        invalidateLater(renewal_time * 1000)
      
      # developed_time <- Sys.time() %>%
      #   strptime(format = '%Y-%m-%d %H-%M') %>% 
      #   as.character()
      # 
      # developed_time <- gsub(':', '-', developed_time)
      # 
      # dir.name <-  paste("../Model",
      #                    paste0('agent_id_', AGENT_ID()),
      #                    resource, host,
      #                    paste0('unit_', unit),
      #                    metric, developed_time, sep = "/")
      dir.name <- paste("../Model",
                        paste0('agent_id_', AGENT_ID()),
                        resource, host,
                        paste0('unit_', unit),
                        metric, sep = '/')
        
      modelFile.name <- paste(dir.name, "fcst.rdata", sep = "/")
      
      figFile.name <- paste(dir.name, "anomaly.png", sep = "/")
      
      if (!file.exists(modelFile.name)) {   # 모델이 없는 경우.... -----------------------
        
        output$monitoring <- renderPlot({
          
          series <- load_single_metric(resource, host, metric, period, groupby,
                                       unit, AGENT_ID(), mount) %>% 
            as.data.table()
          
          ggplot(series, aes(ds, y)) + geom_point() + geom_line() +
            ylab(metric) + xlab("Time")
          
        })
        
        output$modeling_img <- renderImage({
          
          figFile.name <- '../Image/no-image.png'
          
          return(list(
            src = figFile.name,
            filetype = "image/png",
            width = "100%"))
          
        }, deleteFile = FALSE)
        
        output$anomaly_table <- renderDataTable({})
        
        output$notice <- renderText("There is no Forecasting Model. Build a Model First.")
        
      } else {
        # 모델이 있는 경우....
        # 이미 만들어진 모형이 있으면  
        output$monitoring <- renderPlot({
          
          # anomaly 차트용 데이터 
          series <- load_single_metric(resource, host, metric, period, groupby,
                                       unit, AGENT_ID(), mount) %>% 
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
          
          global_pData <<- pData
          
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
          
        })
        
        output$modeling_img <- renderImage({
          
          dir.name <-  paste("../Model",
                             paste0('agent_id_', AGENT_ID()),
                             resource, host,
                             paste0('unit_', unit),
                             metric, sep = "/")
          
          figFile.name <- paste(dir.name, "anomaly.png", sep = "/")
          
          return(list(
            src = figFile.name,
            filetype = "image/png",
            width = "100%",
            alt = "This is a forecasting model data"
          ))

        }, deleteFile = FALSE)


        # 테이블 렌더링
        output$anomaly_table <- renderDataTable({

          tdf <- as.data.frame(setorder(global_pData, -ds))[1:15, 1:4]
          
          names(tdf) <- c("Time", input$single_metric, "Lower_Limit", "Upper_Limit")
          
          tdf
          
        },
        
        options = list(scrollX  = TRUE,
                       paging = F,
                       pageLength = 10,
                       searching = F))
        
        output$notice <- renderText('Success to model building')
        
      }
    }
  })

  
  observeEvent(input$execute, {
    
    resource <- input$resource
    
    host <- input$resource_assist
    
    metric <- input$single_metric
    
    period <- input$period
    
    groupby <- input$groupby
    
    unit <- input$unit
    
    mount <- input$mount_path
    
    # developed_at <- Sys.time() %>%
    #   strptime(format = '%Y-%m-%d %H:%M')
    # 
    # if (mount == 'null') {
    #   
    #   dir.name <-  paste("../Model",
    #                      paste0('agent_id_', AGENT_ID()),
    #                      resource, host,
    #                      paste0('unit_', unit),
    #                      metric, gsub(':', '-', developed_at), sep = "/")
    #   
    # } else {
    #   
    #   dir.name <-  paste("../Model",
    #                      paste0('agent_id_', AGENT_ID()),
    #                      resource, host,
    #                      paste0('unit_', unit),
    #                      metric,
    #                      paste0('mount_', mount),
    #                      gsub(':', '-', developed_at), sep = "/")
    # }
    if (mount == 'null') {
      
      dir.name <-  paste("../Model",
                         paste0('agent_id_', AGENT_ID()),
                         resource, host,
                         paste0('unit_', unit),
                         metric, sep = "/")
      
    } else {
      
      dir.name <-  paste("../Model",
                         paste0('agent_id_', AGENT_ID()),
                         resource, host,
                         paste0('unit_', unit),
                         metric,
                         paste0('mount_', mount), sep = "/")
    }
    
    modelFile.name <- paste(dir.name, "fcst.rdata", sep = "/")
    
    figFile.name <- paste(dir.name, "anomaly.png", sep = "/")
    
    if (metric != "") {
      
      mseries <- load_single_metric(resource, host, metric, period, groupby,
                                    unit, AGENT_ID(), mount) %>% 
        as.data.table()
      
      
      developed_during <- system.time({
        
        fcastModel <- prophet(mseries,
                              changepoint.prior.scale = 0.01,
                              uncertainty.samples = 100,
                              interval.width = input$anomaly_CI)
        
      })[3]
      
      # 폴더 없으면 생성
      if (!dir.exists(dir.name)) dir.create(dir.name, recursive = T)
      
      # 모형 결과 저장
      save(fcastModel, file = modelFile.name)
      
      # 그림 결과 저장
      future <- data.frame(ds = seq(min(mseries$ds),
                                    max(mseries$ds),
                                    by = posixt_helper_func(str_sub(groupby, -1))))
      # browser()
      fcst <- predict(fcastModel, future)
      
      pData <- merge(mseries, fcst, by = "ds", all.x = T)[, .(ds, y, yhat_lower, yhat_upper)]
      
      pData[, anomaly := y]
      
      pData[, anomaly := ifelse(y > yhat_upper | y < yhat_lower, y, NA)]
      
      anom_count <- pData[, sum(anomaly, na.rm = T)]
      
      gm <- ggplot(pData, aes(ds, y)) + geom_point() +
        geom_ribbon(aes(ymin = yhat_lower, ymax = yhat_upper), alpha = 0.2) +
        xlim(c(min(pData$ds),max(pData$ds))) +
        ylab(metric) + xlab("Time")
      
      pdf(NULL)
      
      ggsave("anomaly.png", plot = gm, path = dir.name, width = 12, height = 8)
      
      updateSelectInput(session,
                        "single_metric",
                        choices = numVar)
      
      # save_model_info(AGENT_ID(), resource, host, unit,
      #                 metric, mount, developed_at, developed_during)
      
    }
    
  })
  
}


shinyApp(ui, server)

