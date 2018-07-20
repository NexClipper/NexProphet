#### Metric Association ####

source("../Source/load_package.R", local = T, encoding = "utf-8")
source("../Source/server_func.R", local = T, encoding = "utf-8")


HOST_LIST <- load_tag_list('host')
HOST_METRICS <- load_metric_list('host')

HOST_METRICS <- HOST_METRICS[!str_detect(HOST_METRICS, '_per$')]

TASK_LIST <- load_tag_list('task')
TASK_METRICS <- load_metric_list('task')

DOCKER_LIST <- load_tag_list('docker')
DOCKER_METRICS <- load_metric_list('docker')


# total data
DATA_CORR <- NULL


ui <- fluidPage(
  
  includeCSS('../www/custom.css'),
  
  sidebarLayout(
    
    sidebarPanel(
      
      width = 3,
      
      helpText("Note: You can select multiple metrics you want to see.
                Also, multiple hosts(tasks) can be selected."),
      
      wellPanel(
        
        selectizeInput("host_list",
                       "Select Host to inspect :", 
                       choices = HOST_LIST,
                       selected = "",
                       multiple = T ),

        br(),

        actionButton("host_all",
                     "Select All",
                     Height = 40),
        
        selectizeInput("host_metrics",
                       "Select Host Metric to inspect :", 
                       choices = HOST_METRICS,
                       selected = "",
                       multiple = T )
        
      ),
      
      wellPanel(
        
        selectizeInput("task_list",
                       "Select Task to inspect :", 
                       choices = TASK_LIST,
                       selected = "",
                       multiple = T ),
        
        br(),
        
        actionButton("task_all",
                     "Select All",
                     Height = 40),
        
        selectizeInput("task_metrics",
                       "Select Task Metric to inspect :", 
                       choices = TASK_METRICS,
                       selected = "",
                       multiple = T )
        
      ),
      
      wellPanel(
        
        selectizeInput("docker_list",
                       "Select Docker to inspect :", 
                       choices = DOCKER_LIST,
                       selected = "",
                       multiple = T ),
        
        br(),
        
        actionButton("docker_all",
                     "Select All",
                     Height = 40),
        
        selectizeInput("docker_metrics",
                       "Select Docker Metric to inspect :", 
                       choices = DOCKER_METRICS,
                       selected = "", multiple = T )
        
        
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
      
      tags$body(class = 'body_alter'),
      
      width = 9, 
      
      column(
        width = 8,
        fluidRow(
          class = 'graph_panel',
          
          br(),
          h4(class = 'h4_alter', "Metric Association Plot"),
          hr(),
          d3heatmapOutput('correlation_plot', height = '500px')
        )
        # width = 8,
        # 
        # br(),
        # h4("Metric Association Plot"),
        # hr(),
        # plotlyOutput('correlation_plot', height = "800px")
        
      ),
      
      column(
        width = 4,
        
        fluidRow(
          class = 'graph_panel',
          
          br(),
          h4(class = 'h4_alter', "Find the most related Metrics"),
          hr(),
          selectizeInput("combo_DT_Metric",
                         "Select Metric to inspect :", 
                         choices = c(""), selected = ""),
          dataTableOutput('correlation_table')
        )
        # width = 4,
        # br(),
        # h4("Find the most related Metrics"),
        # br(),
        # selectizeInput("combo_DT_Metric",
        #                "Select Metric to inspect :", 
        #                choices = c(""), selected = ""),
        # dataTableOutput('correlation_table')
        
      )

    ) # mainPanel     
    
  ) # sidebarLayout
  
)


server <- function(input, output, session) {
  
  observeEvent(input$execute, {
    
    period <- input$period
    
    groupby <- input$groupby

    host_list <- list('host' = input$host_list,
                      'task' = input$task_list,
                      'docker' = input$docker_list)
    
    metric_list <- list('host' = input$host_metrics,
                        'task' = input$task_metrics,
                        'docker' = input$docker_metrics)
    
    multiple_metrics <- load_multiple_metric(period = period,
                                             groupby = groupby,
                                             host_list = host_list,
                                             metric_list = metric_list) %>%
      select(-time) %>% 
      select_if(~ sd(., na.rm = T) != 0) %>% 
      as.matrix() %>% 
      standardization()
    
    DATA_CORR <<- multiple_metrics %>% cor()
    
    # all.na.idx <- sapply(multiple_metrics, function(x) all(is.na(x)))
    
    # multiple_metrics %>% 
    #   as.matrix() %>%
    #   standardization()
    
    # browser()
    
    output$correlation_plot <- renderD3heatmap({
      
      d3heatmap(DATA_CORR, colors = "Blues", scale = "none",
                dendrogram = "both", k_row = 3, height = '600px',
                xaxis_font_size = '8px', yaxis_font_size = '8px')
      
    })
    
    
    updateSelectInput(session,
                      "combo_DT_Metric",
                      choices = rownames(DATA_CORR),
                      selected = NULL)
   
  })
  
  
  observeEvent(input$docker_all, {
    
    if (is.null(input$docker_metrics)) {
      
      updateSelectizeInput(session,
                           inputId = 'docker_metrics',
                           selected = DOCKER_METRICS)
      
    } else {
      
      updateSelectizeInput(session,
                           inputId = 'docker_metrics',
                           selected = '')
      
    }
    
  })
  
  
  observeEvent(input$host_all, {
    
    if (is.null(input$host_metrics)) {
      
      updateSelectizeInput(session,
                           inputId = 'host_metrics',
                           selected = HOST_METRICS)
      
    } else {
      
      updateSelectizeInput(session,
                           inputId = 'host_metrics',
                           selected = '')
      
    }
    
  })
  
  
  observeEvent(input$task_all, {
    
    if (is.null(input$task_metrics)) {
      
      updateSelectizeInput(session,
                           inputId = 'task_metrics',
                           selected = TASK_METRICS)
      
    } else {
      
      updateSelectizeInput(session,
                           inputId = 'task_metrics',
                           selected = '')
      
    }
    
  })
  
  
  observeEvent(input$combo_DT_Metric, {
    
    output$correlation_table <- renderDataTable({
      
      if (input$combo_DT_Metric != "") {
        dt <- data.table(Metrics = rownames(DATA_CORR),
                         Correlation = DATA_CORR[, input$combo_DT_Metric],
                         absCorr = abs(DATA_CORR[, input$combo_DT_Metric]))
        
        setorder(dt, -absCorr)
        
        output$correlation_table <- renderDataTable(dt[, 1:2, with = F],
                                                    options = list(scrollX  = TRUE,
                                                                   pageLength = 13))
      }
      
    })
    
  })
  
}


shinyApp(ui, server)

