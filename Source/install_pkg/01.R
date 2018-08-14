bs.Library <- function(pkg, add = T) {
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  
  if (length(new.pkg))
    
    install.packages(new.pkg, dependencies = T)
  
  if (add == TRUE)
    
    sapply(pkg, require, character.only = TRUE)
  
}


# shiny
bs.Library(c('shinydashboard', 'shinyjqui'))


# data manipulation
bs.Library(c('DT', 'RMySQL'))


# fetch resource list
bs.Library(c('httr', 'jsonlite'))
