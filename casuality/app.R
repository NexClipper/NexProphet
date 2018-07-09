#### Metric Granger Casuality ####

source("../Source/load_package.R", local = T, encoding = "utf-8")
source("../Source/server_func.R", local = T, encoding = "utf-8")


CLUSTER_LIST <- load_tag_list('cluster')
CLUSTER_METRICS <- load_metric_list('cluster')

HOST_LIST <- load_tag_list('host')
HOST_METRICS <- load_metric_list('host')

TASK_LIST <- load_tag_list('task')
TASK_METRICS <- load_metric_list('task')


# total data
casuality_df <- NULL


ui <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      
      width = 3,
      
      helpText("Note: You can select multiple metrics you want to see.
               Also, multiple hosts(tasks) can be selected."),
      
      wellPanel(
        
        actionButton("clus_all",
                     "Select All",
                     Height = 40),
        
        selectizeInput("clus_metrics",
                       "Select Cluster Metric to inspect :", 
                       choices = CLUSTER_METRICS, selected = "", multiple = T )
        
      ),
      
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
      width = 9, 
      
      column(
        width = 8,
        
        br(),
        h4("Time Lag Analysis"),
        forceNetworkOutput('network_plot')
        
      ),
      
      column(
        
        width = 4,
        br(),
        h4("Find the most related Metrics"),
        br(),
        selectizeInput("combo_DT_Metric",
                       "Select Metric to inspect :", 
                       choices = c(""), selected = ""),
        dataTableOutput('casuality_table')
        
      )
      
      
    ) # mainPanel     
    
  ) # sidebarLayout
  
)


server <- function(input, output, session) {
  
  observeEvent(input$execute, {
    
    period <- input$period
    
    groupby <- input$groupby
    
    host_list <- list('cluster' = input$clus_list,
                      'host' = input$host_list,
                      'task' = input$task_list)
    
    metric_list <- list('cluster' = input$clus_metrics,
                        'host' = input$host_metrics,
                        'task' = input$task_metrics)
    
    multiple_metrics <- load_multiple_metric(period = period,
                                             groupby = groupby,
                                             host_list = host_list,
                                             metric_list = metric_list) %>%
      select(-time)
    
    all.na.idx <- sapply(multiple_metrics, function(x) all(is.na(x)))
    
    multiple_metrics <- multiple_metrics[, !all.na.idx] %>%
      na.omit()
    
    casuality_df <<- casuality(multiple_metrics, input$max_lag)
    
    node_df <- casuality_df$node
    
    link_df <- casuality_df$link
    
    output$network_plot <- renderForceNetwork({
      
      forceNetwork(Links = link_df,
                   Nodes = node_df,
                   Source = 'source',
                   Target = 'target',
                   NodeID = 'node',
                   Group = 'node',
                   linkDistance = 200,
                   zoom = T,
                   fontSize = 10,
                   opacityNoHover = T,
                   legend = T,
                   arrows = T,
                   opacity = 1,
                   Value = 'intension',
                   Nodesize = 'size')
      
    })
    
    combo_list <- inner_join(link_df, node_df, by = c('target' = 'idx')) %>%
      select(node) %>% unique()
    
    updateSelectInput(session,
                      "combo_DT_Metric",
                      choices = combo_list,
                      selected = NULL)
    
  })
  
  observeEvent(input$clus_all, {
    
    if (is.null(input$clus_metrics)) {
      
      updateSelectizeInput(session,
                           inputId = 'clus_metrics',
                           selected = CLUSTER_METRICS)
      
    } else {
      
      updateSelectizeInput(session,
                           inputId = 'clus_metrics',
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
        
        node_df <- casuality_df$node
        
        link_df <- casuality_df$link
        
        link_df$target <- inner_join(link_df, node_df, by = c('target' = 'idx'))$node
        
        link_df$source <- inner_join(link_df, node_df, by = c('source' = 'idx'))$node
        
        df <- link_df[link_df$target == input$combo_DT_Metric, ]
        
        dt <- data.table(source = df$source,
                         target = df$target,
                         intension = df$intension,
                         p_value = df$p_value,
                         lag = df$max_lag)
        
        setorder(dt, -intension)
        
      }
      
    })
    
    
  })
  
  
}


shinyApp(ui, server)

