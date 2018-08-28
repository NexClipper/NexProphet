source('app_func.R')


load_disk_used_percent() %>%
  lapply(function(dt, cut_) handling_disk_data(dt, cut_), cut_ = CUT) %>% 
  lapply(function(dt) diskForecasting(dt)) %>% 
  lapply(function(dt) add_DFT(dt, THRESHOLD)) %>% 
  lapply(function(dt) draw_graph(dt))
