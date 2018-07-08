#### Metric Association ####
rm(list = ls())
source("../Source/package_manage.R", local = T, encoding = "utf-8")
source("../Source/server_func.R", local = T, encoding = "utf-8")
# source("../Source/ui_func.R", local = T, encoding = "utf-8")


CLUSTER_LIST <- load_tag_list('cluster')
CLUSTER_METRICS <- load_metric_list('cluster')

HOST_LIST <- load_tag_list('host')
HOST_METRICS <- load_metric_list('host')

TASK_LIST <- load_tag_list('task')
TASK_METRICS <- load_metric_list('task')


# 전체 데이터 
DATA_CORR <- NULL


ui <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      
      width = 3,
      
      helpText("Note: You can select multiple metrics you want to see.
                Also, multiple hosts(tasks) can be selected."),
      
      wellPanel(
        
        # dropdownButton(
        #   
        #   label = 'Select Cluster',
        #   circle = F,
        #   width = "500px",
        #   
        #   actionButton("clus_all",
        #                "All",
        #                width = "100%",
        #                Height = 40),
        #   
        #   multiInput(
        #     inputId = 'clus_list',
        #     label = '',
        #     choices = CLUSTER_LIST,
        #     width = '400px')
        #   
        # ),
        actionButton("clus_all",
                     "Select All",
                     Height = 40),
        
        selectizeInput("clus_metrics",
                       "Select Cluster Metric to inspect :", 
                       choices = CLUSTER_METRICS, selected = "", multiple = T )
        
        # dropdownButton(
        #   
        #   label = 'Select Cluster Metrics',
        #   circle = F,
        #   width = "500px",
        #   
        #   actionButton("host_all",
        #                "All",
        #                width = "100%",
        #                Height = 40),
        #   
        #   multiInput(
        #     inputId = 'clus_metrics',
        #     label = '',
        #     choices = CLUSTER_METRICS,
        #     width = '400px')
        #   
        # )
        
      ),
      
      wellPanel(
        
        # dropdownButton(
        #   
        #   label = 'Select Host',
        #   circle = F,
        #   width = "500px",
        #   
        #   actionButton("task_all",
        #                "All",
        #                width = "100%",
        #                Height = 40),
        #   
        #   multiInput(
        #     inputId = 'host_list',
        #     label = '',
        #     choices = HOST_LIST,
        #     width = '400px')
        #   
        # ),
        selectizeInput("host_list",
                       "Select Host to inspect :", 
                       choices = HOST_LIST,
                       selected = "",
                       multiple = T ),

        br(),

        # dropdownButton(
        #   
        #   label = 'Select Host Metrics',
        #   circle = F,
        #   width = "500px",
        #   
        #   multiInput(
        #     inputId = 'host_metrics',
        #     label = '',
        #     choices = HOST_METRICS,
        #     width = '400px')
        #   
        # )
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
        
        # dropdownButton(
        #   
        #   label = 'Select Task',
        #   circle = F,
        #   width = "500px",
        #   
        #   multiInput(
        #     inputId = 'task_list',
        #     label = '',
        #     choices = TASK_LIST,
        #     width = '500px')
        #   
        # ),
        selectizeInput("task_list",
                       "Select Task to inspect :", 
                       choices = TASK_LIST,
                       selected = "",
                       multiple = T ),
        
        br(),
        
        # dropdownButton(
        #   
        #   label = 'Select Task Metrics',
        #   circle = F,
        #   width = "500px",
        #   
        #   multiInput(
        #     inputId = 'task_metrics',
        #     label = '',
        #     choices = TASK_METRICS,
        #     width = '400px')
        #   
        # )
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
      width = 9, 
      
      column(
        width = 8,
        
        br(),
        h4("Metric Association Plot"),
        plotlyOutput('correlation_plot')
        
      ),
      
      column(
        
        width = 4,
        br(),
        h4("Find the most related Metrics"),
        br(),
        selectizeInput("combo_DT_Metric",
                       "Select Metric to inspect :", 
                       choices = c(""), selected = ""),
        dataTableOutput('correlation_table')
        
      )
      
      # plotlyOutput('anomalized_plot', height = '500px'),
      # 
      # plotOutput('decomposed_plot', height = '800px')

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
    

    
    # browser()
    
    multiple_metrics <- load_multiple_metric(period = period,
                                             groupby = groupby,
                                             host_list = host_list,
                                             metric_list = metric_list) %>%
      select(-time) 
    
    all.na.idx <- sapply(multiple_metrics, function(x) all(is.na(x)))
    
    multiple_metrics[, !all.na.idx] %>% 
      as.matrix() %>%
      standardization()
    
    # browser()
    
    corr <- cor(multiple_metrics, use = "pairwise.complete.obs")
    
    na.idx <- sapply(as.data.frame(corr), function(x){
      all(is.na(x))
    })
    corr <- corr[!na.idx, !na.idx]
    # browser()
    corr[is.na(corr)] <- 0
    
    DATA_CORR <<- corr
    
    output$correlation_plot <- renderPlotly({
      
      # browser()
      ggcorrplot(DATA_CORR,
                 hc.order = TRUE,
                 type = "lower",
                 lab = T,
                 outline.color = 'white',
                 lab_size = 2,
                 tl.cex = 8) %>% ggplotly()
      
    })
    
    # browser()
    updateSelectInput(session,
                      "combo_DT_Metric",
                      choices = rownames(DATA_CORR),
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
    output$correlation_table <- renderDataTable({
      
      # browser()
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

