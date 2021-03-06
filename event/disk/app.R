source('app_func.R')

event_config <- get_event_config()

for (idx in 1:nrow(event_config)) {
  
  timezone <- event_config$timezone[idx]
  
  execution_hour <- event_config$frequency[idx] %>%
    as.integer()
  
  current_hour <- START_TIME %>% 
    as_datetime(tz = timezone) %>% 
    hour()

  if (execution_hour != current_hour) next()
  
  agent_id <- event_config$agent_id[idx]
  
  metric <- event_config$metric[idx]
  
  measurement <- event_config$measurement[idx]
  
  threshold <- event_config$threshold[idx]
  
  warning <- event_config$warning[idx]
  
  critical <- event_config$critical[idx]
  
  period <- event_config$period[idx]

  data_ <- load_disk_used_percent(agent_id, timezone)
  
  if (is.null(data_)) next()
  
  data_ %>% 
    lapply(function(dt) handling_disk_data(dt)) %>%
    lapply(function(dt) diskForecasting(dt, warning, timezone)) %>%
    lapply(function(dt) add_DFT(dt, threshold, critical)) %>%
    lapply(function(dt) save_result_mysql(dt, agent_id, threshold, critical, warning, timezone))
  
}
