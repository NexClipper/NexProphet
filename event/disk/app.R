source('app_func.R')

load_disk_used_percent() %>%
  lapply(function(dt) handling_disk_data(dt)) %>% 
  lapply(function(dt) diskForecasting(dt)) %>% 
  lapply(function(dt) add_DFT(dt, THRESHOLD)) %>% 
  lapply(function(dt) save_result_mysql(dt))
  # lapply(function(dt) draw_graph(dt))
