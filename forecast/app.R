#### FORECAST ####

source("../Source/load_package.R", local = T, encoding = "utf-8")
source("../Source/server_func.R", local = T, encoding = "utf-8")


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
      
      h4('Advanced Options'),
      
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
                    "Select Data Period (Hours)",
                    min = 6, max = 240, value = 6),
        
        sliderInput("predicted_period",
                    "Predicted Period (Hours)",
                    min = 3, max = 72, value = 3),
        
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
        
        column(
          width = 6, 
          br(),
          h4("Forecasting Component Plot"),
          br(),
          plotOutput('component_plot', height = "350px")
        ),
        
        column(
          width = 6,
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
    }
    
  })
  
  
  observeEvent(c(input$resource_assist, input$merge), {
    
    if (input$merge == "0") {
      
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
    
    if (input$unit == 0) {
      
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
    query <- parseQueryString(session$clientData$url_search)
    q <- paste(names(query), query, sep = "=", collapse = ", ")
    cat('\n\n\n', session$clientData$url_search, '##########', '\n\n\n')
    resource <- input$resource
    
    host <- input$resource_assist
    
    metric <- input$single_metric
    
    period <- input$period
    
    unit <- input$unit
    
    groupby <- input$groupby
    
    node_ip <- input$host_for_task
    
    output$predicted_plot <- renderDygraph({})
    
    output$component_plot <- renderPlot({})
    
    output$trend_plot <- renderDygraph({
    
      series <- load_single_metric(resource, host, metric, period, groupby,
                                   unit, node_ip)
      
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
    
    render_result <- render_forecast(resource, host, metric, period, groupby,
                                     pred_period, unit, node_ip)
    
    forecast_result <- render_result$forecast_result
    
    output$predicted_plot <- render_result$rendered
    
    output$component_plot <- render_forecast_component(forecast_result)
    
  })
  
}


shinyApp(ui, server)

