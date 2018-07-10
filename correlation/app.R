#### Metric Association ####

source("../Source/load_package.R", local = T, encoding = "utf-8")
source("../Source/server_func.R", local = T, encoding = "utf-8")


CLUSTER_LIST <- load_tag_list('cluster')
CLUSTER_METRICS <- load_metric_list('cluster')

HOST_LIST <- load_tag_list('host')
HOST_METRICS <- load_metric_list('host')

TASK_LIST <- load_tag_list('task')
TASK_METRICS <- load_metric_list('task')


# total data
DATA_CORR <- NULL


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
        
        style = "padding: 15px 20px 0px 20px;"
        
      )
      
    ),
    
    mainPanel(
      width = 9, 
      
      column(
        width = 8,
        
        br(),
        h4("Metric Association Plot"),
        plotlyOutput('correlation_plot', height = "800px")
        
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
    
    multiple_metrics[, !all.na.idx] %>% 
      as.matrix() %>%
      standardization()
    
    corr <- cor(multiple_metrics, use = "pairwise.complete.obs")
    
    na.idx <- sapply(as.data.frame(corr), function(x){
      
      all(is.na(x))
      
    })
    
    corr <- corr[!na.idx, !na.idx]
    
    corr[is.na(corr)] <- 0
    
    DATA_CORR <<- corr
    
    output$correlation_plot <- renderPlotly({
      
      ggcorrplot(DATA_CORR,
                 hc.order = TRUE,
                 type = "lower",
                 lab = T,
                 outline.color = 'white',
                 lab_size = 2,
                 tl.cex = 8) %>% ggplotly()
      
    })
    
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

