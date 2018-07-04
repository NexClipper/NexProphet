# multiInput, metrics list

metrics_list <- function(measurement) {
  
  connector <- connect()
  
  con <- connector$con
  
  dbname <- connector$dbname
  
  query <- "show field keys on %s from %s"
  
  query <- sprintf(query, dbname, measurement)
  
  res <- influx_query(con,
                      dbname,
                      query,
                      return_xts = F)[[1]] %>%
    as.data.frame() %>%
    select(fieldKey) %>% 
    t() %>% 
    as.vector() %>% 
    setdiff('timestamp')

  return(res)
  
}

