# .libPaths('C:/Users/yunseop/Documents/R/win-library/3.5')

# Check library existance
bs.Library <- function(pkg, add = T) {
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  
  if (length(new.pkg))
    
    install.packages(new.pkg, dependencies = TRUE)
  
  if (add == TRUE)
    
    sapply(pkg, require, character.only = TRUE)
  
}


# shiny
bs.Library(c("shiny", "shinyWidgets"))


# data manipulation
bs.Library(c('tidyverse', "data.table", "influxdbr", 'xts'))


# visualization
bs.Library(c("dygraphs", "visNetwork", "networkD3", 'plotly',
             'ggcorrplot'))


# machine learning
bs.Library(c("prophet", "anomalize", 'lmtest'))