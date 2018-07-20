#### Metric Granger Casuality ####

source("../Source/load_package.R", local = T, encoding = "utf-8")
source("../Source/server_func.R", local = T, encoding = "utf-8")


HOST_LIST <- load_tag_list('host')
HOST_METRICS <- load_metric_list('host')

HOST_METRICS <- HOST_METRICS[!str_detect(HOST_METRICS, '_per$')]

TASK_LIST <- load_tag_list('task')
TASK_METRICS <- load_metric_list('task')

DOCKER_LIST <- load_tag_list('docker')
DOCKER_METRICS <- load_metric_list('docker')


node_df <- NULL
edge_df <- NULL
edge_df_filtered <- NULL


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
        
        sliderInput("max_lag",
                    "Select Max Lag :",
                    min = 1, max = 100, value = 5),
        
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
          
          h4(class = 'h4_alter', "Time Lag Analysis"),
          
          hr(),
          
          visNetworkOutput('network_chart', height = '800px', width = '500px')
          
        ),
        
        fluidRow(

          class = 'graph_panel',

          br(),

          h4(class = 'h4_alter', "Network Chart Control Panel"),

          hr(),
          
          helpText("Do filtering edges using threshold; edges of intension which is less than threshold are filtered."),
          
          conditionalPanel(
            condition = 'input.control_intension != -1',
            sliderInput('control_intension',
                        label = '',
                        min = -1, max = -1, value = -1)
          )
          
        )
        
      ),
      
      column(
        
        width = 4,
        
        fluidRow(
          
          class = 'graph_panel_dt',
          
          br(),
          
          h4(class = 'h4_alter', "Find the most related Metrics"),
          
          hr(),
          
          selectizeInput("combo_DT_Metric",
                         "Select Metric to inspect :", 
                         choices = "", selected = "", width = '100%'),
          
          dataTableOutput('casuality_table')
          
        )
        
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
      select(-time) 
    # browser()
    node_edge_df <- get_node_edge_df(multiple_metrics, input$max_lag)
    
    node_df <- node_edge_df$node
    
    group_idx <- get_node_group_idx(node_df,
                                    host_list$host,
                                    host_list$task,
                                    host_list$docker)
    
    node_df$group[group_idx$host] <- 'host'
    
    node_df$group[group_idx$task] <- 'task'
    
    node_df$group[group_idx$docker] <- 'docker'
    
    node_df <<- node_df
    
    edge_df <<- node_edge_df$edge
    # browser()
    # edge_df_filtered <- edge_df %>% filter(intension > 3)
    # 
    # edge_df_filtered$title <- paste0("<p>Intension: ",
    #                                  edge_df_filtered$intension,
    #                                  "</p>")
    # 
    # edge_df_filtered <<- edge_df_filtered
    # 
    # output$network_chart <- renderVisNetwork({
    #   
    #   net <- visNetwork(node_df, edge_df_filtered) %>% 
    #     visEdges(arrows = 'to') %>% 
    #     visOptions(highlightNearest = T,
    #                selectedBy = 'group',
    #                nodesIdSelection = list(enabled = T,
    #                                        style = ".dropdown {width: auto;}")
    #     ) %>%
    #     visGroups(groupname = 'host', color = 'red') %>% 
    #     visGroups(groupname = 'task', color = 'cyan') %>% 
    #     visGroups(groupname = 'docker', color = 'green') %>% 
    #     visPhysics(repulsion = list('nodeDistance' = 200))
    #   
    #   if (length(unique(node_df$group)) > 1) {
    #     
    #     net <- net %>% visLegend()
    #     
    #   }
    #   
    #   net
    #   
    # })
    # 
    # combo_list <- inner_join(edge_df_filtered, node_df, by = c('to' = 'id')) %>%
    #   select(label, group)
    # 
    # choices_ <- split(combo_list$label, combo_list$group)
    # 
    # updateSelectInput(session,
    #                   "combo_DT_Metric",
    #                   choices = choices_,
    #                   selected = NULL)
    # browser()
    updateSliderInput(session,
                      'control_intension',
                      'Select Threshold',
                      min = floor(min(edge_df$intension)),
                      max = floor(max(edge_df$intension)),
                      value = floor(median(edge_df$intension)))
    
  })
  
  
  observeEvent(input$control_intension, {
    
    if (input$control_intension > -1) {
      
      threshold <- input$control_intension
      
      edge_df_filtered <- edge_df %>% filter(intension >= threshold)
      
      edge_df_filtered$title <- paste0("<p>Intension: ",
                                       edge_df_filtered$intension,
                                       "</p>")
      
      edge_df_filtered <<- edge_df_filtered
      
      output$network_chart <- renderVisNetwork({
        
        net <- visNetwork(node_df, edge_df_filtered) %>% 
          visEdges(arrows = 'to') %>% 
          visOptions(highlightNearest = T,
                     selectedBy = 'group',
                     nodesIdSelection = list(enabled = T,
                                             style = ".dropdown {width: auto;}")
          ) %>%
          visGroups(groupname = 'host', color = 'red') %>% 
          visGroups(groupname = 'task', color = 'cyan') %>% 
          visGroups(groupname = 'docker', color = 'green') %>% 
          visPhysics(repulsion = list('nodeDistance' = 200))
        
        if (length(unique(node_df$group)) > 1) {
          
          net <- net %>% visLegend()
          
        }
        
        net
        
      })
      
      combo_list <- inner_join(edge_df_filtered, node_df, by = c('to' = 'id')) %>%
        select(label, group)
      
      choices_ <- split(combo_list$label, combo_list$group)
      
      updateSelectInput(session,
                        "combo_DT_Metric",
                        choices = choices_,
                        selected = NULL)
      
      output$casuality_table <- renderDataTable({
        
        if (input$combo_DT_Metric != "") {
          
          edge_df_filtered$to_label <- inner_join(edge_df_filtered, node_df, by = c('to' = 'id'))$label
          
          edge_df_filtered$from_label <- inner_join(edge_df_filtered, node_df, by = c('from' = 'id'))$label
          
          df <- edge_df_filtered %>% 
            subset(to_label == input$combo_DT_Metric) #[edge_df_filtered$to_label == input$combo_DT_Metric, ]
          
          dt <- data.table(from = df$from_label,
                           to = df$to_label,
                           intension = df$intension,
                           p_value = df$p_value,
                           lag = df$max_lag)
          
          setorder(dt, -intension)
          
        }
        
      })
      
    }
    
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
    
    output$casuality_table <- renderDataTable({
      
      if (input$combo_DT_Metric != "") {
        
        edge_df_filtered$to_label <- inner_join(edge_df_filtered, node_df, by = c('to' = 'id'))$label
        
        edge_df_filtered$from_label <- inner_join(edge_df_filtered, node_df, by = c('from' = 'id'))$label
        
        df <- edge_df_filtered %>% 
          subset(to_label == input$combo_DT_Metric) #[edge_df_filtered$to_label == input$combo_DT_Metric, ]
        
        dt <- data.table(from = df$from_label,
                         to = df$to_label,
                         intension = df$intension,
                         p_value = df$p_value,
                         lag = df$max_lag)
        
        setorder(dt, -intension)
        
      }
      
    })
    
    
  })
  
  
}


shinyApp(ui, server)

