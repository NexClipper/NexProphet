#### Metric Correlation ####

source("../Source/load_package.R", local = T, encoding = "utf-8")
source("../Source/server_func.R", local = T, encoding = "utf-8")


HOST_TAG_LIST <- NULL

HOST_METRIC_LIST <- NULL

# TASK_TAG_LIST <- NULL
# 
# TASK_METRIC_LIST <- NULL

DOCKER_TAG_LIST <- NULL

DOCKER_METRIC_LIST <- NULL


ui <- fluidPage(
  
  includeCSS('../www/custom.css'),
  
  sidebarLayout(
    
    sidebarPanel(
      
      width = 3,
      
      helpText("Note: You can select multiple metrics you want to see.
                Also, multiple hosts(tasks) can be selected."),
      
      wellPanel(
        
        actionButton("host_list_all",
                     "Select All",
                     Height = 40),
        
        selectizeInput("host_list",
                       "Select Host to inspect :", 
                       choices = '',
                       selected = "",
                       multiple = T ),

        br(),

        actionButton("host_metric_all",
                     "Select All",
                     Height = 40),
        
        selectizeInput("host_metrics",
                       "Select Host Metric to inspect :", 
                       choices = "",
                       selected = "",
                       multiple = T )
        
      ),
      
      # wellPanel(
      #   
      #   actionButton("task_list_all",
      #                "Select All",
      #                Height = 40),
      #   
      #   selectizeInput("task_list",
      #                  "Select Task to inspect :", 
      #                  choices = "",
      #                  selected = "",
      #                  multiple = T ),
      #   
      #   br(),
      #   
      #   actionButton("task_metric_all",
      #                "Select All",
      #                Height = 40),
      #   
      #   selectizeInput("task_metrics",
      #                  "Select Task Metric to inspect :", 
      #                  choices = "",
      #                  selected = "",
      #                  multiple = T )
      #   
      # ),
      
      wellPanel(
        
        actionButton("docker_list_all",
                     "Select All",
                     Height = 40),
        
        selectizeInput("docker_list",
                       "Select Docker to inspect :", 
                       choices = "",
                       selected = "",
                       multiple = T ),
        
        br(),
        
        actionButton("docker_metric_all",
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
        
        width = 12,
        
        fluidRow(
          
          class = 'graph_panel',
          
          br(),
          
          h4(class = 'h4_alter', "Metric Association Plot"),
          
          hr(),
          
          # uiOutput('plot1')
          d3heatmapOutput('correlation_plot', height = '800px') %>% 
            withSpinner()
        )
        
      ),
      
      column(
        
        width = 8,
        
        fluidRow(
          
          class = 'graph_panel',
          
          br(),
          
          h4(class = 'h4_alter', "Show metrics similar to metric you select"),
          
          hr(),
          
          selectizeInput("combo_metric",
                         "Select Metric :", 
                         choices = '', selected = ""),
          
          plotOutput('similar_plot', height = '700px') %>% 
            withSpinner()
          
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
          
          dataTableOutput('correlation_table') %>% 
            withSpinner()
          
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
  
  
  output$correlation_plot <- renderD3heatmap({})
  
  
  observeEvent(AGENT_ID(), {
    
    HOST_TAG_LIST <<- load_tag_list('host', AGENT_ID())
    
    # TASK_TAG_LIST <<- load_tag_list('task', AGENT_ID())
    
    DOCKER_TAG_LIST <<- load_tag_list('docker', AGENT_ID())
    
    HOST_METRIC_LIST <<- load_metric_list('host')
    
    # TASK_METRIC_LIST <<- load_metric_list('task')
    
    DOCKER_METRIC_LIST <<- load_metric_list('docker')
    
    updateSelectizeInput(
      session = session,
      inputId = 'host_list',
      choices = HOST_TAG_LIST
    )
    
    # updateSelectizeInput(
    #   session = session,
    #   inputId = 'task_list',
    #   choices = TASK_TAG_LIST
    # )
    
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
    
    # updateSelectizeInput(
    #   session = session,
    #   inputId = 'task_metrics',
    #   choices = TASK_METRIC_LIST
    # )
    
    updateSelectizeInput(
      session = session,
      inputId = 'docker_metrics',
      choices = DOCKER_METRIC_LIST
    )
      
  })
  
  
  observeEvent(input$execute, {
    
    output$correlation_plot <- renderD3heatmap({
      
      d3heatmap(data_corr(), colors = "Blues", scale = "none",
                dendrogram = "both", k_row = 3, height = '600px',
                xaxis_font_size = '8px', yaxis_font_size = '8px')
      
      
    })
    
    updateSelectInput(session,
                      "combo_DT_Metric",
                      choices = rownames(data_corr()))
    
    updateSelectInput(session,
                      "combo_metric",
                      choices = rownames(data_corr()))
   
  })
  
  
  multiple_metrics <- eventReactive(input$execute, {
    
    period <- input$period
    
    groupby <- input$groupby
    
    host_list <- list('host' = input$host_list,
                      # 'task' = input$task_list,
                      'docker' = input$docker_list)
    
    metric_list <- list('host' = input$host_metrics,
                        # 'task' = input$task_metrics,
                        'docker' = input$docker_metrics)
    # browser()
    load_multiple_metric(period = period,
                         groupby = groupby,
                         host_list = host_list,
                         metric_list = metric_list,
                         AGENT_ID()) %>%  
      select_if(~ sum(is.na(.)) < 30) %>% 
      subset(complete.cases(.)) %>% 
      select_if(~ sd(., na.rm = T) != 0)
    
  })
  
  
  data_corr <- reactive({
    
    multiple_metrics() %>% 
      select(-time) %>%
      as.matrix() %>%
      standardization() %>% 
      cor(use = 'pairwise.complete.obs')
    
  })
  
  
  observeEvent(input$combo_DT_Metric, {
    
    if (input$combo_DT_Metric != "") {
      # browser()
      dt <- data.table(Metrics = rownames(data_corr()),
                       Correlation = data_corr()[, input$combo_DT_Metric],
                       absCorr = abs(data_corr()[, input$combo_DT_Metric]))
      
      setorder(dt, -absCorr)
      
      output$correlation_table <- renderDataTable(dt[, 1:2, with = F],
                                                  options = list(scrollX  = TRUE,
                                                                 pageLength = 10))
    }
      
  })
  
  
  observeEvent(input$combo_metric, {
    
    output$similar_plot <- renderPlot({
      
      if (input$combo_metric != "") {
        
        dt <- data.table(Metrics = rownames(data_corr()),
                         Correlation = round(data_corr()[, input$combo_metric], 4),
                         absCorr = abs(data_corr()[, input$combo_metric]))
        
        setorder(dt, -absCorr)
        # browser()
        
        multiple_metrics() %>%
          select(c(time, dt$Metrics[1:min(10, nrow(dt))])) %>%
          na.omit() %>% 
          gather(key, value, -1) %>% 
          as.data.frame() %>% 
          horizon.panel.ggplot(dt$Correlation[1:min(10, nrow(dt))])
        
      }
      
    })
    
  })
  
  
  observeEvent(input$host_list_all, {
    
    if (is.null(input$host_list)) {
      
      updateSelectizeInput(session,
                           inputId = 'host_list',
                           selected = unlist(HOST_TAG_LIST))
      
    } else {
      
      updateSelectizeInput(session,
                           inputId = 'host_list',
                           selected = '')
      
    }
    
  })
  
  
  # observeEvent(input$task_list_all, {
  #   
  #   if (is.null(input$task_list)) {
  #     
  #     updateSelectizeInput(session,
  #                          inputId = 'task_list',
  #                          selected = unlist(TASK_TAG_LIST))
  #     
  #   } else {
  #     
  #     updateSelectizeInput(session,
  #                          inputId = 'task_list',
  #                          selected = '')
  #     
  #   }
  #   
  # })
  
  
  observeEvent(input$docker_list_all, {
    
    if (is.null(input$docker_list)) {
      
      updateSelectizeInput(session,
                           inputId = 'docker_list',
                           selected = unlist(DOCKER_TAG_LIST))
      
    } else {
      
      updateSelectizeInput(session,
                           inputId = 'docker_list',
                           selected = '')
      
    }
    
  })
  
  
  observeEvent(input$host_metric_all, {
    
    if (is.null(input$host_metrics)) {
      
      updateSelectizeInput(session,
                           inputId = 'host_metrics',
                           selected = unlist(HOST_METRIC_LIST))
      
    } else {
      
      updateSelectizeInput(session,
                           inputId = 'host_metrics',
                           selected = '')
      
    }
    
  })
  
  
  # observeEvent(input$task_metric_all, {
  #   
  #   if (is.null(input$task_metrics)) {
  #     
  #     updateSelectizeInput(session,
  #                          inputId = 'task_metrics',
  #                          selected = unlist(TASK_METRIC_LIST))
  #     
  #   } else {
  #     
  #     updateSelectizeInput(session,
  #                          inputId = 'task_metrics',
  #                          selected = '')
  #     
  #   }
  #   
  # })
  
  
  observeEvent(input$docker_metric_all, {
    
    if (is.null(input$docker_metrics)) {
      
      updateSelectizeInput(session,
                           inputId = 'docker_metrics',
                           selected = unlist(DOCKER_METRIC_LIST))
      
    } else {
      
      updateSelectizeInput(session,
                           inputId = 'docker_metrics',
                           selected = '')
      
    }
    
  })
  
}


shinyApp(ui, server)

