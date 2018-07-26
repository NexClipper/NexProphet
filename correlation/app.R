#### Metric Correlation ####

rm(list = ls())
source("../Source/load_package.R", local = T, encoding = "utf-8")
source("../Source/server_func.R", local = T, encoding = "utf-8")


HOST_TAG_LIST <- NULL

HOST_METRIC_LIST <- NULL

TASK_TAG_LIST <- NULL

TASK_METRIC_LIST <- NULL

DOCKER_TAG_LIST <- NULL

DOCKER_METRIC_LIST <- NULL

AGENT_ID <- NULL

# total data
data_corr <- NULL


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
                       choices = '',
                       selected = "",
                       multiple = T ),

        br(),

        actionButton("host_all",
                     "Select All",
                     Height = 40),
        
        selectizeInput("host_metrics",
                       "Select Host Metric to inspect :", 
                       choices = "",
                       selected = "",
                       multiple = T )
        
      ),
      
      wellPanel(
        
        selectizeInput("task_list",
                       "Select Task to inspect :", 
                       choices = "",
                       selected = "",
                       multiple = T ),
        
        br(),
        
        actionButton("task_all",
                     "Select All",
                     Height = 40),
        
        selectizeInput("task_metrics",
                       "Select Task Metric to inspect :", 
                       choices = "",
                       selected = "",
                       multiple = T )
        
      ),
      
      wellPanel(
        
        selectizeInput("docker_list",
                       "Select Docker to inspect :", 
                       choices = "",
                       selected = "",
                       multiple = T ),
        
        br(),
        
        actionButton("docker_all",
                     "Select All",
                     Height = 40),
        
        selectizeInput("docker_metrics",
                       "Select Docker Metric to inspect :", 
                       choices = "",
                       selected = "",
                       multiple = T )
        
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
        
      )

    ) # mainPanel     
    
  ) # sidebarLayout
  
)


server <- function(input, output, session) {
  
  observeEvent(session$clientData$url_search, {
    
    agent <- session$clientData$url_search %>%
      str_extract('agent_id=\\d+') %>%
      strsplit('=') %>%
      unlist()
    
    AGENT_ID <<- agent[2]
    # AGENT_ID <<-  27
    
  })
  
  
  observeEvent(AGENT_ID, {
    
    # if (!is.null(AGENT_ID)) {
      
    HOST_TAG_LIST <<- load_tag_list('host', AGENT_ID)
    
    TASK_TAG_LIST <<- load_tag_list('task', AGENT_ID)
    
    DOCKER_TAG_LIST <<- load_tag_list('docker', AGENT_ID)
    
    HOST_METRIC_LIST <<- load_metric_list('host')
    
    TASK_METRIC_LIST <<- load_metric_list('task')
    
    DOCKER_METRIC_LIST <<- load_metric_list('docker')
    
    updateSelectizeInput(
      session = session,
      inputId = 'host_list',
      choices = HOST_TAG_LIST
    )
    
    updateSelectizeInput(
      session = session,
      inputId = 'task_list',
      choices = TASK_TAG_LIST
    )
    
    updateSelectizeInput(
      session = session,
      inputId = 'docker_list',
      choices = DOCKER_TAG_LIST
    )
    
    updateSelectizeInput(
      session = session,
      inputId = 'host_metrics',
      choices = HOST_METRIC_LIST
    )
    
    updateSelectizeInput(
      session = session,
      inputId = 'task_metrics',
      choices = TASK_METRIC_LIST
    )
    
    updateSelectizeInput(
      session = session,
      inputId = 'docker_metrics',
      choices = DOCKER_METRIC_LIST
    )
      
    # }
    
  })
  
  
  observeEvent(input$execute, {
    
    # url_search <- session$clientData$url_search
    # 
    # agent <- str_extract(url_search, 'agent_id=\\d+') %>%
    #   strsplit('=') %>%
    #   unlist()
    # 
    # agent_id <- agent[2]
    # agent_id <- 27
    
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
                                             metric_list = metric_list,
                                             AGENT_ID) %>%
      select(-time) %>% 
      select_if(~sum(!is.na(.)) > 0) %>% 
      select_if(~ sd(., na.rm = T) != 0) %>% 
      as.matrix() %>% 
      standardization()
    
    data_corr <<- multiple_metrics %>%
      cor(use = 'pairwise.complete.obs')
    
    # all.na.idx <- sapply(multiple_metrics, function(x) all(is.na(x)))
    
    # multiple_metrics %>% 
    #   as.matrix() %>%
    #   standardization()
    
    # browser()
    
    output$correlation_plot <- renderD3heatmap({
      
      d3heatmap(data_corr, colors = "Blues", scale = "none",
                dendrogram = "both", k_row = 3, height = '600px',
                xaxis_font_size = '8px', yaxis_font_size = '8px')
      
    })
    
    
    updateSelectInput(session,
                      "combo_DT_Metric",
                      choices = rownames(data_corr),
                      selected = NULL)
   
  })
  
  
  observeEvent(input$docker_all, {
    
    if (is.null(input$docker_metrics)) {
      
      updateSelectizeInput(session,
                           inputId = 'docker_metrics',
                           selected = DOCKER_METRIC_LIST)
      
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
                           selected = HOST_METRIC_LIST)
      
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
                           selected = TASK_METRIC_LIST)
      
    } else {
      
      updateSelectizeInput(session,
                           inputId = 'task_metrics',
                           selected = '')
      
    }
    
  })
  
  
  observeEvent(input$combo_DT_Metric, {
    
    output$correlation_table <- renderDataTable({
      
      if (input$combo_DT_Metric != "") {
        dt <- data.table(Metrics = rownames(data_corr),
                         Correlation = data_corr[, input$combo_DT_Metric],
                         absCorr = abs(data_corr[, input$combo_DT_Metric]))
        
        setorder(dt, -absCorr)
        
        output$correlation_table <- renderDataTable(dt[, 1:2, with = F],
                                                    options = list(scrollX  = TRUE,
                                                                   pageLength = 13))
      }
      
    })
    
  })
  
}


shinyApp(ui, server)

