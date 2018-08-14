
# Check library existance
bs.Library <- function(pkg, add = T) {
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  
  if (length(new.pkg))
    
    install.packages(new.pkg, dependencies = T)
  
  if (add == TRUE)
    
    sapply(pkg, require, character.only = TRUE)
  
}


# shiny
bs.Library(c("shiny", "shinyWidgets", 'shinyjs', 'shinycssloaders',
             'shinydashboard', 'shinyjqui'))

# data manipulation
bs.Library(c('tidyverse', "data.table", "influxdbr", 'xts',
             'RMySQL', 'DT'))


# visualization
bs.Library(c("dygraphs", 'd3heatmap'))


# machine learning
bs.Library(c("prophet", 'lmtest'))


# fetch resource list
bs.Library(c('httr', 'jsonlite'))
