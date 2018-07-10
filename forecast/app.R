#### FORECAST ####

source("../Source/load_package.R", local = T, encoding = "utf-8")
source("../Source/server_func.R", local = T, encoding = "utf-8")


CLUSTER_METRICS <- load_metric_list('cluster')

HOST_METRICS <- load_metric_list('host')

TASK_METRICS <- load_metric_list('task')

forecast_result <- NULL


ui <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      
      width = 3,
      
      wellPanel(
        
        prettyRadioButtons(
          inputId = 'resource',
          label = 'Select Resource',
          choices = list('Cluster' = 'cluster',
                         'Host' = 'host',
                         'Task' = 'task'),
          selected = 'cluster',
          inline = T
          
        ),
        
        pickerInput(
          inputId = 'resource_assist',
          label = 'Select Master',
          choices = c('192.168.0.161'),
          options = list(`style` = "btn-info")
        ),
        
        pickerInput(
          inputId = 'single_metric',
          label = 'Select Metric',
          choices = '',
          options = list(`style` = "btn-info")
        )
        
      ),
      
      wellPanel(
        
        actionButton("execute",
                     "  Execute",
                     icon = icon("sign-out"),
                     width = "100%",
                     Height = 40)
        
      ),

      wellPanel(
        
        sliderInput("period",
                    "Data Period (days) :",
                    min = 1, max = 100, value = 5),
        
        radioButtons("groupby",
                     "Select Group By",
                     choices = c('5m', "10m", "30m", "1h"),
                     selected = "1h",
                     inline = T),
        
        style = "padding: 15px 20px 0px 20px;"
        
      )
      
    ),
    
    mainPanel(
      width = 9,
      
      fluidRow(
        
        column(width = 6, 
               br(),
               h4("Time Series Plot"),
               br(),
               dygraphOutput(
                 'trend_plot',
                 width = "100%",
                 height = "350px")
         ),
        
        column(width = 6, 
               br(),
               h4("Forecasting Plot"),
               br(),
               dygraphOutput(
                 'predicted_plot',
                 width = "100%",
                 height = "350px")
        )
        
      ),
      
      fluidRow(
        column(width = 6, 
               br(),
               h4("Forecasting Component Plot"),
               br(),
               plotOutput('component_plot', height = "350px")
        ),
        
        column(width = 6,
               br(),
               h4("Forecasting Statistics"),
               br()
        )
      )
      
    ) # mainPanel
    
  ) # sidebarLayout
  
)


server <- function(input, output, session) {
  
  observeEvent(input$resource, {
    
    label_ <- switch(input$resource,
                     'cluster' = 'Select Master',
                     'host' = 'Select Host IP',
                     'task' = 'Select Task Name')
    
    choices_ <- load_tag_list(input$resource)
    
    updatePickerInput(
      session = session,
      inputId = 'resource_assist',
      label = label_,
      choices = choices_)
    
    metrics_ <- load_metric_list(input$resource)
    
    updatePickerInput(
      session = session,
      inputId = 'single_metric',
      choices = metrics_
    )
    
  })
  
  observe({
    
    resource <- input$resource
    
    host <- input$resource_assist
    
    metric <- input$single_metric
    
    period <- input$period
    
    groupby <- input$groupby
    
    "
    Error in shiny-server

    output$predicted_plot <- NULL
    output$component_plot <- NULL
    "
    
    output$trend_plot <- renderDygraph({
    
      series <- load_single_metric(resource, host, metric, period, groupby)
      
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
    
    groupby <- input$groupby
    
    render_result <- render_forecast(resource, host, metric, period, groupby)
    
    forecast_result <<- render_result$forecast_result
    
    output$predicted_plot <- render_result$rendered
    output$component_plot <- render_forecast_component(forecast_result)
    
  })
  
}


shinyApp(ui, server)

