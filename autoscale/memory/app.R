#### MEMORY AUTOSCALE ####

source("../../Source/load_package.R", local = T, encoding = "utf-8")
source("../../Source/server_func.R", local = T, encoding = "utf-8")

DOCKER_TAG_LIST <- NULL

h2o.init()


ui <- fluidPage(
  
  includeCSS("../../www/custom.css"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      width = 3,
      
      wellPanel(
        
        selectizeInput(
          inputId = 'task_id',
          label = 'Select Docker',
          choices = ''
        )
        
      ),
      
      br(),
      
      h4(class = 'h4_alter', 'Advanced Options'),
      
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
                    "Select Data Period (Days)",
                    value = 7, min = 3, max = 60),
        
        prettyRadioButtons(
          "groupby",
          'Select Group By',
          choices = c('1m', '5m', '15m', '1h'),
          selected = '15m',
          inline = T),
        
        style = "padding: 15px 20px 0px 20px;"
        
      ),
      
      actionButton("execute",
                   "  Execute",
                   icon = icon("sign-out"),
                   width = "100%",
                   Height = 40)
      
    ),
    
    mainPanel(
      
      width = 9,
      
      tags$body(class = 'body_alter',
                
                fluidRow(
                  
                  class = "graph_panel",

                  br(),

                  h4(class = 'h4_alter', "Current Probability"),
                  hr(),
                  plotOutput('plot0', height = '500px'),
                  br(),
                  
                  h4(class = 'h4_alter', "Plot1"),
                  hr(),
                  dygraphOutput('plot1', height = "500px"),
                  br(),
                  
                  h4(class = 'h4_alter', "Plot2"),
                  hr(),
                  dygraphOutput('plot2', height = "200px")
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
  
  observeEvent(AGENT_ID(), {
    
    DOCKER_TAG_LIST <<- load_tag_list('docker', AGENT_ID())
    
    updateSelectizeInput(
      session = session,
      inputId = 'task_id',
      label = 'Docker Name',
      choices = DOCKER_TAG_LIST)
    
  })
  
  memory <- eventReactive(list(AGENT_ID(), input$execute), {
    
    period <- paste0(input$period,
                     ifelse(input$unit == '0', 'd', 'h'))
    
    load_data_for_autoscale_memory(AGENT_ID(), period, input$groupby)
    
  })
  
  observeEvent(input$unit, {
    
    if (input$unit == '0') {
      
      updateSliderInput(
        session = session,
        inputId = 'period',
        label = 'Select Data Period (Days)',
        value = 7, min = 3, max = 60
      )
      
    } else {
      
      updateSliderInput(
        session = session,
        inputId = 'period',
        label = 'Select Data Period (Hours)',
        value = 60, min = 60, max = 1200
      )
      
    }
    
  })
  
  dir.name <- eventReactive(AGENT_ID(), {
    
    paste("../../Model",
          'autoscale',
          'memory',
          paste0('agent_id_', AGENT_ID()),
          sep = '/')
    
  })
  
  modelFile.name <- eventReactive(dir.name(), {
    
    paste(dir.name(), dir(dir.name()), sep = "/")
    
  })
  
  observeEvent(input$task_id, {
    
    if ((input$task_id == 'Choose Container') | (input$task_id == '')) {
      
      output$plot1 <- renderDygraph({})
      
      output$plot2 <- renderDygraph({})
      
    } else {
      
      if (!is_empty(dir.name())) {
        
        h2o.removeAll()
        
        model <- h2o.loadModel(modelFile.name())
        
        H_memory <- as.h2o(memory())
        
        pred <- h2o.predict(model, H_memory) %>% as.data.frame() %>% .[,3]
        
        memory_pred <- memory() %>% copy() %>% 
          .[, c('pred', 'ds') := list(pred, as_datetime(ds))]
        
        memory_task_id <- memory_pred[key == input$task_id, .(ds, y, pred)]
        
        output$plot0 <- renderPlot({
          
          ggplot(memory_pred[ds == max(ds), .(key, pred)], aes(y = pred, x = key)) + 
            geom_bar(stat = 'identity') +
            coord_flip() +
            geom_col(aes(fill = pred)) +
            scale_fill_gradient(low = "darkgreen", 
                                 high = "red") + 
            labs(y = 'Probability', x = 'Container name')
          
        })
        
        output$plot1 <- renderDygraph({
          xts(memory_task_id$y,
              order.by = memory_task_id$ds) %>%
            dygraph(group = "g1")  %>%
            dyRangeSelector()
        })
        
        output$plot2 <- renderDygraph({
          xts(memory_task_id$pred,
              order.by = memory_task_id$ds) %>%
            dygraph(group = "g1") %>%
            dyBarChart() %>%
            dyAxis("y", label = "Probability", valueRange = c(0, 1))
        })
        
      } else {
        
        output$plot1 <- renderDygraph({})
        
        output$plot2 <- renderDygraph({})
        
      }
      
    }
    
  })
  
  observeEvent(input$execute, {
    
    x.names <- setdiff(names(memory()), c("key", "ds", "Y"))
    
    y.name <- "Y"
    
    H_memory <- as.h2o(memory())
    
    gbm <- h2o.gbm(x = x.names, y = y.name, training_frame = H_memory)
    
    h2o.rm('GBM')
    
    hyper_params = list(max_depth = seq(1,13,2))
    
    h2o.grid(
      hyper_params = hyper_params,
      search_criteria = list(strategy = "Cartesian"),
      algorithm = "gbm",
      grid_id = "GBM",
      x = x.names,
      y = y.name,
      training_frame = H_memory,
      ntrees = 1000,
      learn_rate = 0.05,
      learn_rate_annealing = 0.99,
      sample_rate = 0.8,
      col_sample_rate = 0.8,
      seed = 1234,
      stopping_rounds = 5,
      stopping_tolerance = 1e-4,
      stopping_metric = "AUC",
      score_tree_interval = 10
    )
    
    sortedGrid <- h2o.getGrid("GBM", sort_by = "auc", decreasing = TRUE)
    
    gbm <- h2o.getModel(sortedGrid@model_ids[[1]])
    
    # dir.name <- paste("../../Model",
    #                   'autoscale',
    #                   'memory',
    #                   paste0('agent_id_', AGENT_ID()),
    #                   sep = '/')
    
    # model.dir <- tempfile('', dir.name())
    
    # filename <- paste(dir.name, dir(dir.name), sep = '/')
    
    # if (!is_empty(dir(dir.name()))) unlink(modelFile.name(), T)
    
    if (!file.exists(dir.name())) dir.create(dir.name(), recursive = T)    
    
    h2o.saveModel(object = gbm,
                  path = dir.name(),
                  force = T)
    
    sendSweetAlert(
      session = session,
      title = "Finish modeling!",
      type = "success"
    )
    
  })
  
}


shinyApp(ui, server)
