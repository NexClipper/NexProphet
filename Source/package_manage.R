# .libPaths('C:/Users/yunseop/Documents/R/win-library/3.5')
# shiny
bs.Library(c("shiny", "shinyWidgets"))

# data manipulation
bs.Library(c('tidyverse', "data.table", "influxdbr", 'xts'))

# visualization
bs.Library(c("dygraphs", "visNetwork", "networkD3", 'plotly',
             'ggcorrplot', 'ggplot2'))

# machine learning
bs.Library(c("prophet", "anomalize", 'lmtest'))
