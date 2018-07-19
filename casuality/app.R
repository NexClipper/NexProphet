#### Metric Granger Casuality ####

source("../Source/load_package.R", local = T, encoding = "utf-8")
source("../Source/server_func.R", local = T, encoding = "utf-8")


HOST_LIST <- load_tag_list('host')
HOST_METRICS <- load_metric_list('host')

TASK_LIST <- load_tag_list('task')
TASK_METRICS <- load_metric_list('task')

DOCKER_LIST <- load_tag_list('docker')
DOCKER_METRICS <- load_metric_list('docker')


net <- NULL
node_edge_df <- NULL


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
          
          # DiagrammeROutput('network_chart')
          visNetworkOutput('network_chart')
          
        )#,
        
        # fluidRow(
        #   
        #   class = 'graph_panel',
        #   
        #   br(),
        #   
        #   h4(class = 'h4_alter', "Network Chart Control Panel"),
        #   
        #   hr()
        #   
        #   # sankeyNetworkOutput('network_plot', height = "500px")
        # )
        
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
                         choices = "", selected = ""),
          
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
    
    node_edge_df <<- get_node_edge_df(multiple_metrics, input$max_lag)
    
    node_df <- node_edge_df$node
    
    group_idx <- get_node_group_idx(node_df,
                                    host_list$host,
                                    host_list$task,
                                    host_list$docker)
    
    node_df$group[group_idx$host] <- 'host'
    
    node_df$group[group_idx$task] <- 'task'
    
    node_df$group[group_idx$docker] <- 'docker'
    
    edge_df <- node_edge_df$edge
    # browser()
    edge_df_filtered <- edge_df %>% filter(intension > 2)
    
    edge_df_filtered$title <- paste0("<p>Intension: ",
                                     edge_df_filtered$intension,
                                     "</p>")
    
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
      
      if (sum(!is.null(group_idx$host),
              !is.null(group_idx$task),
              !is.null(group_idx$docker)) > 1) {
        
        net <- net %>% visLegend()
        
      }
      
      net
      
    })
    
    
    
    # net <<- get_network_graph(node_edge_df,
    #                           host_list$host,
    #                           host_list$task,
    #                           host_list$docker)
    # DiagrammeR::Di
    # output$network_chart <- renderGrViz({
    #   grViz({
    #   net %>% render_graph(output = 'DOT') %>% 
    #     tempfile(fileext = "~/dot.gv") #%>% 
    #     #DiagrammeR(type = 'grViz')
    #   })
    # })
    # DiagrammeR::gr
    # file <- tempfile(fileext = ".html")
    # 
    # saveWidget(graph, file, selfcontained = TRUE)
    # 
    # paste(readLines(file), collapse = "")
    
    # test <- renderDiagrammeR({
    #   net %>% render_graph(layout = 'kk')
    # })
    # print(str(test))
    # DiagrammeR::dig
    # output$network_chart <- test
  }
    
    # all.na.idx <- sapply(multiple_metrics, function(x) all(is.na(x)))
    # 
    # multiple_metrics <- multiple_metrics[, !all.na.idx] %>%
    #   na.omit() %>% 
    #   as.matrix %>% 
    #   standardization()
    # 
    # casuality_df <<- casuality(multiple_metrics, input$max_lag)
    
    # node_df <- casuality_df$node
    # 
    # link_df <- casuality_df$link
    # 
    # output$network_plot <- renderSankeyNetwork({
    #   
    #   forceNetwork(Links = link_df,
    #                Nodes = node_df,
    #                Source = 'source',
    #                Target = 'target',
    #                NodeID = 'node',
    #                Group = 'node',
    #                linkDistance = 200,
    #                zoom = T,
    #                fontSize = 10,
    #                opacityNoHover = T,
    #                legend = T,
    #                arrows = T,
    #                opacity = 1,
    #                Value = 'intension',
    #                Nodesize = 'size')
    #   
    # })
    # 
    # combo_list <- inner_join(link_df, node_df, by = c('target' = 'idx')) %>%
    #   select(node) %>% unique()
    # 
    # updateSelectInput(session,
    #                   "combo_DT_Metric",
    #                   choices = combo_list,
    #                   selected = NULL)
    # 
  )
  
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

