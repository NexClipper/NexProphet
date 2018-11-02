#### LOAD_FUNCTION ####

# Check library existance
bs.Library <- function(pkg, add = T) {
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  
  if (length(new.pkg))
    
    install.packages(new.pkg, dependencies = TRUE)
  
  if (add == TRUE)
    
    sapply(pkg, require, character.only = TRUE)
  
}


#### LOAD_PACKAGES ####

# data manipulation
bs.Library(c('tidyverse', "data.table", "influxdbr", 'xts', 'zoo'))


# machine learning
bs.Library(c("prophet"))
