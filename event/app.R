#### Event Association ####

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
      
    ) # mainPanel     
    
  ) # sidebarLayout
  
  )