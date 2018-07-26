#### FORECAST ####

rm(list = ls())
source("../Source/load_package.R", local = T, encoding = "utf-8")
source("../Source/server_func.R", local = T, encoding = "utf-8")


AGENT_ID <- NULL

HOST_TAG_LIST <- NULL

HOST_METRIC_LIST <- NULL

TASK_TAG_LIST <- NULL

TASK_METRIC_LIST <- NULL

DOCKER_TAG_LIST <- NULL

DOCKER_METRIC_LIST <- NULL


# setup logging
setLogFile("loggit.json")
loggit("INFO", "app has started", app = "start")


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
          helpText("Note : if task name is same and host is seperated, Merge = No.\
                           if host is seperated for same task, Merge = Yes."),
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
        ),
        
        conditionalPanel(
          condition = "input.resource == 'host'",
          selectizeInput(
            inputId = 'mount_path',
            label = 'Select Mount Path',
            choices = ''
          )
        )
        
      ),
      
      wellPanel(
        
        actionButton("execute",
                     "  Execute",
                     icon = icon("sign-out"),
                     width = "100%",
                     Height = 40)
        
      ),
      
      br(),
      
      h4(class = 'h4_alter', 'Advanced Options'),
      
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
                    value = 3, min = 1, max = 60),
        
        sliderInput("predicted_period",
                    "Predicted Period (Days)",
                    value = 1, min = 1, max = 30),
        
        prettyRadioButtons(
          "groupby",
          'Select Group By',
          choices = c('1m', '5m', '10m', '1h'),
          selected = '1h',
          inline = T),
        
        style = "padding: 15px 20px 0px 20px;"
        
      )
      
    ),
    
    mainPanel(
      
      width = 9,
      
      tags$body(class = 'body_alter',
                
                fluidRow(
                  
                  column(width = 6,
                         
                         fluidRow(
                           
                           class = "graph_panel",  
                           
                           br(),
                           
                           h4(class = 'h4_alter', "Time Series Plot"),
                           
                           hr(),
                           
                           dygraphOutput(
                             'trend_plot',
                             width = "100%",
                             height = "300px")
                         )
                         
                  ),
                  
                  column(width = 6, 
                         
                         fluidRow(
                           
                           class = "graph_panel",  
                           
                           br(),
                           
                           h4(class = 'h4_alter', "Forecasting Plot"),
                           
                           hr(),
                           
                           dygraphOutput(
                             'predicted_plot',
                             width = "100%",
                             height = "300px")
                           
                         )
                         
                  )
                  
                )
      ),
      
      fluidRow(
        
        column(
          
          width = 6,
          
          fluidRow(
            
            class = 'graph_panel',
            
            br(),
            
            h4(class = 'h4_alter', "Forecasting Component Plot"),
            
            hr(),
            
            plotOutput('component_plot', height = "350px")
            
          )
          
        ),
        
        column(
          
          width = 6,
          
          fluidRow(
            
            class = 'graph_panel',
            
            br(),
            
            h4(class = 'h4_alter', "Forecasting Statistics"),
            
            hr()
            
          )
        )
      )
      
    ) # mainPanel
    
  ) # sidebarLayout
  
)


server <- function(input, output, session) {
  
  session$onSessionEnded(function() {
    
    loggit("INFO", "app has stopped", app = "stop")
    
  })
  
  observeEvent(session$clientData$url_search, {
    
    url_search <- session$clientData$url_search
    
    agent <- str_extract(url_search, 'agent_id=\\d+') %>%
      strsplit('=') %>%
      unlist()

    AGENT_ID <<- agent[2]
    # AGENT_ID <<- 27
    
  })
  
  
  observeEvent(input$resource, {
    
    label_ <- switch(input$resource,
                     'host' = 'Select Host Name',
                     'task' = 'Select Task Name',
                     'docker' = 'Select Container Name')
    
    if (is.null(HOST_TAG_LIST)) {
      
      HOST_TAG_LIST <<- load_tag_list('host', AGENT_ID)
      
      TASK_TAG_LIST <<- load_tag_list('task', AGENT_ID)
      
      DOCKER_TAG_LIST <<- load_tag_list('docker', AGENT_ID)
      
      HOST_METRIC_LIST <<- load_metric_list('host')
      
      TASK_METRIC_LIST <<- load_metric_list('task')
      
      DOCKER_METRIC_LIST <<- load_metric_list('docker')
      
    }
    
    resource_assist <- switch(input$resource,
                              'host' = HOST_TAG_LIST,
                              'task' = TASK_TAG_LIST,
                              'docker' = DOCKER_TAG_LIST)
    
    metrics <- switch(input$resource,
                      'host' = HOST_METRIC_LIST,
                      'task' = TASK_METRIC_LIST,
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
    
    if (input$resource != 'task') {
      
      updateSelectizeInput(
        session = session,
        inputId = 'merge',
        selected = '1'
      )
      
      updateSelectizeInput(
        session = session,
        inputId = 'host_for_task',
        selected = ''
      )
      
      # if (input$resource == 'host') {
      #   
      #   updateSelectInput(
      #     session = session,
      #     inputId = 'mount_path',
      #     choices = HOST_MOUNT_PATH
      #   )
      #   
      # } else {
      #   
      #   updateSelectInput(
      #     session = session,
      #     inputId = 'mount_path',
      #     selected = ''
      #   )
      #   
      # }
      
    }
    
  })
  
  
  observeEvent(input$resource_assist, {
    
    if (input$resource == 'host') {
      # print('mount path!')
      # print(input$resource_assist)
      HOST_MOUNT_PATH <- load_host_disk_mount_path(input$resource_assist,
                                                   AGENT_ID)
      
      updateSelectizeInput(
        session = session,
        inputId = 'mount_path',
        choices = HOST_MOUNT_PATH
      )
      
    }
  })
  
  
  observeEvent(c(input$resource_assist, input$merge), {
    
    if (input$merge == "0" & input$resource == 'task') {
      
      choices_ <- load_host_list_for_task(input$resource_assist)
      
      updateSelectizeInput(
        session = session,
        inputId = 'host_for_task',
        choices = choices_
      )
      
    } else {
      
      updateSelectizeInput(
        session = session,
        inputId = 'host_for_task',
        choices = ''
      )
      
    }
    
  })
  
  
  observeEvent(input$unit, {
    
    if (input$unit == '0') {
      
      updateSliderInput(
        session = session,
        inputId = 'period',
        label = 'Select Data Period (Days)',
        value = 3, min = 1, max = 60
      )
      
      updateSliderInput(
        session = session,
        inputId = 'predicted_period',
        label = 'Select Predicted Period (Days)',
        value = 1, min = 1, max = 30
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
      
      updateSliderInput(
        session = session,
        inputId = 'predicted_period',
        label = 'Select Predicted Period (Hours)',
        value = 2, min = 1, max = 30
      )
      
      updatePrettyRadioButtons(
        session = session,
        inputId = 'groupby',
        selected = '10m'
      )
      
    }
  })
  
  
  observe({
    
    if (input$single_metric == "") return()
    
    resource <- input$resource
    
    host <- input$resource_assist
    
    metric <- input$single_metric
    
    period <- input$period
    
    unit <- input$unit
    
    groupby <- input$groupby
    
    node_ip <- input$host_for_task
    
    mount <- input$mount_path
    
    output$predicted_plot <- renderDygraph({})
    
    output$component_plot <- renderPlot({})
    
    output$trend_plot <- renderDygraph({
    
      series <- load_single_metric(resource, host, metric, period, groupby,
                                   unit, node_ip, AGENT_ID, mount)
      
      ts <- xts(series$y,
          order.by = series$ds,
          tzone = Sys.getenv("TZ"))
      
      dygraph(ts) %>%
        dyRangeSelector(height = 30)
      
    })
    
  })
  
  
  observeEvent(input$execute, {
    
    resource <- input$resource
    
    host <- input$resource_assist
    
    metric <- input$single_metric
    
    period <- input$period
    
    unit <- input$unit
    
    pred_period <- input$predicted_period
    
    groupby <- input$groupby
    
    node_ip <- input$host_for_task
    
    mount <- input$mount_path
    
    render_result <- render_forecast(resource, host, metric, period, groupby,
                                     pred_period, unit, node_ip,
                                     AGENT_ID, mount)
    
    forecast_result <- render_result$forecast_result
    
    output$predicted_plot <- render_result$rendered
    
    output$component_plot <- render_forecast_component(forecast_result)
    
  })
  
}


shinyApp(ui, server)

