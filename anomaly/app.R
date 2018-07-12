#### Anomaly ####

source("../Source/load_package.R", local = T, encoding = "utf-8")
source("../Source/server_func.R", local = T, encoding = "utf-8")


CLIENT = "nexcloud"

global_series = NULL

global_pData = NULL

numVar = NULL


ui <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      
      width = 3,
      
      wellPanel(
        
        prettyRadioButtons(
          inputId = 'resource',
          label = 'Select Resource',
          choices = list('Host' = 'host',
                         'Task' = 'task',
                         'Docker' = 'docker'),
          selected = 'host',
          inline = T
        ),
        
        selectizeInput(
          inputId = 'resource_assist',
          label = 'Select Host',
          choices = ''
        ),
        
        conditionalPanel(
          condition = "input.resource == 'task'",
          helpText("Fill here!"),
          prettyRadioButtons(
            inputId = 'merge',
            label = 'Merge or Not',
            choices = list('Yes' = 1,
                           'No' = 0),
            selected = 1,
            inline = T
          )
        ),
        
        conditionalPanel(
          condition = "input.merge == '0' & input.resource == 'task'",
          selectizeInput(
            inputId = 'host_for_task',
            label = 'Select Host',
            choices = ''
          )
        ),
        
        selectizeInput(
          inputId = 'single_metric',
          label = 'Select Metric',
          choices = ''
        )
        
      ),
      
      wellPanel(
        
        actionButton("execute",
                     "  Execute",
                     icon = icon("sign-out"),
                     width = "100%",
                     Height = 40)
        
      ),
      
      hr(),
      
      h4("Model Development Options"),
      
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

      # wellPanel(
      #   
      #   sliderInput("period",
      #               "Data Period (Hours) :",
      #               min = 1, max = 120, value = 6),
      #   
      #   sliderInput("groupby",
      #               "Select Group By (sec)",
      #               min = 1, max = 120, value = 10),
      #   
      #   sliderInput("anomaly_CI",
      #               "Confidence Interval : ",
      #               min = 0.9, max = 1, value = 0.99),
      #   
      #   helpText("It takes much time to make forecasting model if there is no model for this metric!"),
      #   
      #   style = "padding: 15px 20px 0px 20px;"
      #   
      # )
      
    ),
    
    mainPanel(
      
      width = 9,
      
      fluidRow(
        
        column(
          
          width = 8,
          
          br(),
          
          h4("Anomaly Detection Chart"),
          
          br(),
          
          plotOutput("monitoring", height = 500),
          
          br(),
          
          verbatimTextOutput("notice"),
          
          br(),
          
          h4("Whole data plot for Modeling Data"),
          
          br(),
          
          imageOutput("modeling_img", height = 500)
          
        ),
        
        column(
          
          width = 4,
          
          br(),
          
          h4("Anomaly Detection Chart"),
          
          br(),
          
          dataTableOutput('anomaly_table')                                          )
        
      )
      
    ) # mainPanel
    
  ) # sidebarLayout
  
)


server <- function(input, output, session) {
  
  observeEvent(input$unit, {
    
    if (input$unit == '0') {
      
      updateSliderInput(
        session = session,
        inputId = 'period',
        label = 'Select Data Period (Days)',
        min = 3, max = 30, value = 6
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
        label = 'Select Data Period (Days)',
        min = 6, max = 240, value = 9
      )
      
      updatePrettyRadioButtons(
        session = session,
        inputId = 'groupby',
        selected = '1m'
      )
      
    }
    
  })
  
  observeEvent(input$resource, {
    
    label_ <- switch(input$resource,
                     'host' = 'Select Host IP',
                     'task' = 'Select Task Name',
                     'docker' = 'Select Container Name')
    
    choices_ <- load_tag_list(input$resource)
    
    updateSelectizeInput(
      session = session,
      inputId = 'resource_assist',
      label = label_,
      choices = choices_)
    
    metrics_ <- load_metric_list(input$resource)
    
    updateSelectizeInput(
      session = session,
      inputId = 'single_metric',
      choices = metrics_
    )
    
  })
  
  observe({
  # observeEvent(input$single_metric, {
    
    if (input$single_metric != "") {
      
      resource <- input$resource
      
      host <- input$resource_assist
      
      metric <- input$single_metric
      
      period <- input$period
      
      unit <- input$unit
      
      groupby <- input$groupby
      
      node_ip <- input$host_for_task
      
      renewal <- input$renewal
      
      renewal_time <- renew(renewal)
      
      dir.name <-  paste("../Model", CLIENT, resource, metric, sep = "/")
      
      modelFile.name <- paste(dir.name, "fcst.rdata", sep = "/")
      
      figFile.name <- paste(dir.name, "anomaly.png", sep = "/")
      
      if (!file.exists(modelFile.name)) {   # 모델이 없는 경우.... -----------------------
        
        output$monitoring <- renderPlot({
          
          # host <- input$resource_assist
          # 
          # groupby <- input$groupby
          # 
          # period <- input$period
          invalidateLater(renewal_time * 1000)
          # anomaly 차트용 데이터 
          series <- load_single_metric(resource, host, metric, period, groupby,
                                       unit, node_ip,
                                       limit = 100) %>% 
            as.data.table()
          # invalidateLater(groupby * 1000)
          
          # # anomaly 차트용 데이터 
          # global_series <<- load_single_metric(table, host, metric, period, groupby,
          #                                      limit = 100, type = 'anomaly')
          
          ggplot(series, aes(ds, y)) + geom_point() + geom_line() +
            ylab(metric) + xlab("Time")
          
        })
        
        "
        # Error in shiny        

        output$modeling_img <- NULL
        
        output$anomaly_table  <- NULL
        "
        
        output$notice <- renderText("There is no Forecasting Model!! Make a Model First!!")
        
      } else {
        # 모델이 있는 경우....
        # 이미 만들어진 모형이 있으면  
        
        output$monitoring <- renderPlot({
          
          invalidateLater(renewal_time * 1000)
          
          # anomaly 차트용 데이터 
          series <- load_single_metric(resource, host, metric, period, groupby,
                                       unit, node_ip,
                                       limit = 100) %>% 
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
        
        # 모형 개발 그림 렌더링
        output$modeling_img <- renderImage({
          # if (is.null(input$picture))
          #     return(NULL)
          
          table <- input$resource
          
          metric <- input$single_metric
          
          dir.name <-  paste("../Model", CLIENT, table, metric, sep = "/")
          
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
          
          names(tdf) <- c("Time", input$combo_Anomaly_Metric, "Lower_Limit", "Upper_Limit")
          
          tdf
          
        },
        
        options = list(scrollX  = TRUE,
                       paging = F,
                       pageLength = 10,
                       searching = F))
        "
        Error in shiny

        output$notice <- NULL
        "
        
      }
    }
  })
  

  # observeEvent(input$execute, {
  # 
  #   resource <- input$resource
  # 
  #   metric <- input$single_metric
  # 
  #   period <- input$period
  # 
  #   groupby <- input$groupby
  # 
  #   tb_ <- load_single_metric(resource, host, metric, period, groupby)
  #   
  #   anomaly_result <- anomalization(tb_)
  #   
  #   anomalized_plot <- plot_anomalies(anomaly_result, T)
  #   
  #   output$anomalized_plot <- renderPlotly({
  #     
  #     ggplotly(anomalized_plot)
  #     
  #   })
  #   
  #   output$decomposed_plot <- renderPlot({
  #     
  #     plot_anomaly_decomposition(anomaly_result)
  #     
  #   })
  #   
  # 
  # })
  
  observeEvent(input$execute, {
    
    table <- input$resource
    
    host <- input$resource_assist
    
    metric <- input$single_metric
    
    period <- input$period
    
    groupby <- input$groupby
    
    unit <- input$unit
    
    node_ip <- input$host_for_task
    
    # 모델 이름 결정
    dir.name <-  paste("../Model", CLIENT, table, metric, sep = "/")
    
    modelFile.name <- paste(dir.name, "fcst.rdata", sep = "/")
    
    figFile.name <- paste(dir.name, "anomaly.png", sep = "/")
    
    if (metric != "") {
      
      # 모형 새로 개발
      # mseries <- ifx_getGroupBy(DB_HOST, DB_PORT, DB,
      #                           table,
      #                           metric,
      #                           paste0(input$slide_Anomaly_Period, "h"),
      #                           group_by = paste0(input$slide_Anomaly_Time, "s"),
      #                           addWhere = "", #input$itext_Anomaly_Where)
      #                           limit = 100000)
      # mseries <- as.data.table(mseries)
      # names(mseries) <- c("ds", "y")
      # browser()
      mseries <- load_single_metric(table, host, metric, period, groupby,
                                    unit, node_ip, limit = 100) %>% 
        as.data.table()
      
      fcastModel <- prophet(mseries,
                            changepoint.prior.scale = 0.01,
                            uncertainty.samples = 100,
                            interval.width = input$anomaly_CI)
      
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
      
      ggsave("anomaly.png", path = dir.name, width = 12, height = 8)
      
      updateSelectInput(session,
                        "single_metric",
                        choices = numVar)
      
      # updateSelectInput(session,
      #                   "single_metric",
      #                   choices = numVar)
      
    }
    
    
  })
  
}


shinyApp(ui, server)

