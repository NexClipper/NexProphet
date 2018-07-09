bs.Library <- function(pkg, add = T) {
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  
  if (length(new.pkg))
    
    install.packages(new.pkg, dependencies = TRUE)
  
  if (add == TRUE)
    
    sapply(pkg, require, character.only = TRUE)
  
}


# Related to Event Association
bs.Library(c("RMySQL", "arules"))