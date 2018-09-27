
# Check library existance
bs.Library <- function(pkg, add = T) {
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  
  if (length(new.pkg))
    
    install.packages(new.pkg, dependencies = T)
  
  if (add == TRUE)
    
    sapply(pkg, require, character.only = TRUE)
  
}


# data manipulation
bs.Library(c('tidyverse', "data.table", "influxdbr", 'zoo'))


# machine learning
bs.Library(c("prophet"))


# etc
bs.Library(c('devtools'))

