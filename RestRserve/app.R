library(RestRserve)
library(tidyr)
library(jsonlite)
library(RMySQL)

#### ENVIRONMENT VARIABLE ####

MYSQL_ENV <- Sys.getenv(c('MYSQL_USER', 'MYSQL_PW', 'MYSQL_DB',
                          'MYSQL_HOST', 'MYSQL_PORT'))

MYSQL_USER <- MYSQL_ENV['MYSQL_USER']

MYSQL_PW <- MYSQL_ENV['MYSQL_PW']

MYSQL_DB <- MYSQL_ENV['MYSQL_DB']

MYSQL_HOST <- MYSQL_ENV['MYSQL_HOST']

MYSQL_PORT <- MYSQL_ENV['MYSQL_PORT'] %>% as.integer()

#----

#### MYSQL ####
write_init_to_mysql <- function(agent_id, key_, start_time) {
  
  con <- dbConnect(MySQL(),
                   user = MYSQL_USER,
                   password = MYSQL_PW,
                   dbname = MYSQL_DB,
                   host = MYSQL_HOST,
                   port = MYSQL_PORT)
  
  info <- data.frame('agent_id' = agent_id,
                     'key_id' = key_,
                     'start_time' = start_time,
                     'status' = 199
  )
  
  dbWriteTable(con, 
               name = 'nexclipper_key', 
               value = info,
               row.names = F,
               append = T)
  
  dbCommit(con)
  
  dbDisconnect(con)
  
}

#### APP ####

FORECAST <- function(request, response) {
  
  agent_id <- request$headers$agent_id
  
  key_ <- request$headers$key
  
  host_ip <- request$query$host_ip
  
  measurement <- request$query$measurement
  
  metric <- request$query$metric
  
  period <- request$query$period
  
  p_period <- request$query$p_period
  
  groupby <- request$query$groupby
  
  start_time <- request$query$start_time
  
  request_body <- request$body %>% rawToChar()
  
  write_init_to_mysql(agent_id, key_, start_time)
  
  cmd <- "Rscript forecast.R --agent_id '%s' --key '%s' --measurement '%s' --host_ip '%s' --metric '%s' --period '%s' --p_period '%s' --groupby '%s' --start_time '%s' --request_body '%s'" %>%
    sprintf(agent_id, key_, measurement, host_ip, metric, period, p_period,
            groupby, start_time, request_body)

  system(cmd, wait = F)
  
  response$body = ''
  
  response$content_type = "text/plain"
  
  response$headers = character(0)
  
  response$status_code = 204L
  
  forward()
  
}


ANOMALY <- function(request, response) {
  
  agent_id <- request$headers$agent_id
  
  key_ <- request$headers$key
  
  host_ip <- request$query$host_ip
  
  measurement <- request$query$measurement
  
  metric <- request$query$metric
  
  period <- request$query$period
  
  groupby <- request$query$groupby
  
  start_time <- request$query$start_time
  
  request_body <- request$body %>% rawToChar()
  
  write_init_to_mysql(agent_id, key_, start_time)
  
  cmd <- "Rscript anomaly.R --agent_id '%s' --key '%s' --measurement '%s' --host_ip '%s' --metric '%s' --period '%s' --groupby '%s' --start_time '%s' --request_body '%s'" %>%
    sprintf(agent_id, key_, measurement, host_ip, metric, period, groupby,
            start_time, request_body)

  system(cmd, wait = F)
  
  response$body = ''
  
  response$content_type = "text/plain"
  
  response$headers = character(0)
  
  response$status_code = 204L
  
  forward()
  
}


CORRELATION <- function(request, response) {
  
  agent_id <- request$headers$agent_id
  
  key_ <- request$headers$key
  
  period <- request$query$period
  
  groupby <- request$query$groupby
  
  start_time <- request$query$start_time
  
  request_body <- request$body %>% rawToChar()
  
  write_init_to_mysql(agent_id, key_, start_time)
  
  cmd <- "Rscript correlation.R --agent_id '%s' --key '%s' --period '%s' --groupby '%s' --start_time '%s' --request_body '%s'" %>%
    sprintf(agent_id, key_, period, groupby, start_time, request_body)
  
  system(cmd, wait = F)
  
  response$body = ''
  
  response$content_type = "text/plain"
  
  response$headers = character(0)
  
  response$status_code = 204L
  
  forward()
  
}


HEALTHCHECK <- function(request, response) {
  
  response$body = data.frame('status' = 200,
                             'data' = 'Running') %>% toJSON()
  
  response$content_type = "text/plain"
  
  response$headers = character(0)
  
  response$status_code = 200L
  
  forward()
  
}


RestRserveApp <- RestRserveApplication$new()

RestRserveApp$add_post(path = "/v0/forecast", FUN = FORECAST)

RestRserveApp$add_post(path = "/v0/anomaly", FUN = ANOMALY)

RestRserveApp$add_post(path = "/v0/correlation", FUN = CORRELATION)

RestRserveApp$add_get(path = "/v0/check", FUN = HEALTHCHECK)

RestRserveApp$add_static(path = '/nexopenapi.yaml', 
                         file_path = 'nexopenapi.yaml', 
                         content_type = "application/yaml")

RestRserveApp$add_swagger_ui(path = "/api/swagger",
                             path_openapi = "/nexopenapi.yaml")

RestRserveApp$run(http_port = "8484")
