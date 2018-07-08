#### Anomaly ####
rm(list=ls())
source("../Source/package_manage.R", local = T, encoding = "utf-8")
source("../Source/server_func.R", local = T, encoding = "utf-8")
# source("../Source/ui_func.R", local = T, encoding = "utf-8")


CLUSTER_METRICS <- load_metric_list('cluster')

HOST_METRICS <- load_metric_list('host')

TASK_METRICS <- load_metric_list('task')

anomaly_result <- NULL

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
          choices = '',
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
      
      plotlyOutput('anomalized_plot', height = '500px'),
      
      plotOutput('decomposed_plot', height = '800px')

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
  

  # output$anomalized_plot <- renderPlotly({
  # 
  #   resource <- input$resource
  # 
  #   metric <- input$single_metric
  # 
  #   period <- input$period
  # 
  #   groupby <- input$groupby
  #   cat(resource, metric, period, groupby, '\n')
  #   tb_ <- load_single_metric(resource, metric, period, groupby)
  # 
  #   anomaly_result <<- anomalization(tb_)
  # 
  #   anomalized_plot <- plot_anomalies(anomaly_result, T)
  # 
  #   ggplotly(anomalized_plot)
  # 
  # })

  # output$decomposed_plot <- renderPlotly({
    # print(class(anomaly_result))
  #   plot_anomaly_decomposition(anomaly_result) %>% ggplotly()
  #   
  # })

  observeEvent(input$execute, {

    resource <- input$resource

    metric <- input$single_metric

    period <- input$period

    groupby <- input$groupby

    tb_ <- load_single_metric(resource, metric, period, groupby)
    
    anomaly_result <- anomalization(tb_)
    
    anomalized_plot <- plot_anomalies(anomaly_result, T)
    
    output$anomalized_plot <- renderPlotly({
      
      ggplotly(anomalized_plot)
      
    })
    
    output$decomposed_plot <- renderPlot({
      
      plot_anomaly_decomposition(anomaly_result)
      
    })
    

  })
  
}


shinyApp(ui, server)

