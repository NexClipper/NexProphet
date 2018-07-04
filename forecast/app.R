#### FORECAST ####
# .libPaths('C:/Users/yunseop/Documents/R/win-library/3.5')
# rm(list=ls())
source("Source/BIZSCAPE_FUNCTIONS.R", local = T, encoding = "utf-8")
source("Source/package_manage.R", local = T, encoding = "utf-8")
source("Source/server_func.R", local = T, encoding = "utf-8")
source("Source/ui_func.R", local = T, encoding = "utf-8")
# source("Source/global_variable.R", local = T, encoding = "utf-8")


CLUSTER_METRICS <- metrics_list('cluster')

HOST_METRICS <- metrics_list('host')

TASK_METRICS <- metrics_list('task')

forecast_result <- NULL

ui <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      
      width = 4,
      
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
          choices = 'cpu_used_percent',
          options = list(`style` = "btn-info")
        )
        
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
        
      ),
      
      wellPanel(
        
        actionButton("execute",
                     "  Execute",
                     icon = icon("sign-out"),
                     width = "100%",
                     Height = 40)
        
      )
      
    ),
    
    mainPanel(
      
      dygraphOutput(
        'predicted_plot',
        width = "100%",
        height = "500px"),
      
      plotOutput('component_plot')

    ) # mainPanel
    
  ), # sidebarLayout
  
  theme = "./css/bootstrap-theme.css"
  
)


server <- function(input, output, session) {
  
  observeEvent(input$resource, {
    
    label_ <- switch(input$resource,
                     'cluster' = 'Select Master',
                     'host' = 'Select Host IP',
                     'task' = 'Select Task Name')
    
    choices_ <- load_metric_value(input$resource)
    
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
  
  output$predicted_plot <- renderDygraph({
    
    resource <- input$resource
    
    metric <- input$single_metric
    
    period <- input$period
    
    groupby <- input$groupby
    
    tb_ <- load_single_metric(resource, metric, period, groupby)
    
    forecast_result <<- forecasting(tb_, groupby, 48)
    
    fcst <- forecast_result$forecast
    
    draw_forecast_dygraph(tb_, fcst, max(tb_$ds))
    
  })
  
  output$component_plot <- renderPlot({
    
    model <- forecast_result$model
    
    fcst <- forecast_result$forecast
    
    prophet_plot_components(model, fcst)
    
  })
  
}


shinyApp(ui, server)

# shiny::runExample("01_hello")
