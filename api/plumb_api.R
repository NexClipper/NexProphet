##### LOAD PACKAGES ####
source('load_package.R')


#### COMMON FUNCTION ####

posixt_helper_func <- function(x) {
  switch(x,
         's' = 'sec',
         'm' = 'min',
         'h' = 'hour',
         stop('incorrect group time'))
}


connect <- function() {
  con <- influx_connection(host = '192.168.0.162', port = 10091)
  
  dbname <- 'nexclipper'
  
  conn <- list(connector = con, dbname = dbname)
  
  return(conn)
}


load_single_metric <- function(measurement, host, metric, period, groupby,
                               unit, node_ip, limit = -1) {
  # For forecasting, anomaly detection, read only one metric
  # host : host or task name
  
  # measurement <- 'docker'
  # host <- 'redis.f6da7a1a-736e-11e8-bc55-525d563011bd'
  # metric <- 'rx_bytes'
  # period <- 1
  # groupby <- 10
  # limit = -1
  
  con <- connect()
  
  connector <- con$connector
  
  dbname <- con$dbname
  
  if (unit == 0) {
    
    period <- paste0(period, 'd')
    
  } else {
    
    period <- paste0(period, 'h')
    
  }
  
  query <- "select mean(%s) as metric
            from %s
            where time > now() - %s and %s = '%s' %s
            group by time(%s), %s%s
            fill(linear)
            order by time desc
            %s"
  # browser()
  tag <- switch(measurement,
                'host' = 'host_ip',
                'task' = 'task',
                'docker' = 'task_id')
  
  if (measurement == 'docker') measurement <- 'docker_container, docker_network'
  
  if (node_ip != "") {
    
    node_ip <- sprintf("and node_ip = '%s'", node_ip)
    by_node <- ', node_ip'
    
  } else {
    
    by_node <- ''
    
  }
  
  if (limit > 0) {
    
    limit <- paste('limit', limit)
    
  } else {
    
    limit <- ""
    
  }
  
  query <- sprintf(query,
                   metric,
                   measurement,
                   period, tag, host, node_ip,
                   groupby, tag, by_node,
                   limit)
  # print(query)
  print(query)
  raw_data <- influx_query(connector,
                           db = dbname,
                           query = query,
                           simplifyList = T,
                           return_xts = F)[[1]]
  
  if (!('metric' %in% names(raw_data)))
    return(default_time_seqeunce(period, groupby))
  
  unit <- str_extract(groupby, '[:alpha:]') %>% posixt_helper_func()
  
  by <- str_extract(groupby, '\\d+') %>% paste(unit)
  
  ts <- seq.POSIXt(min(raw_data$time),
                   max(raw_data$time),
                   by = by)
  
  df <- tibble(ds = ts)
  
  raw_data <- select(raw_data, c(time, metric))
  
  tb <- full_join(df, raw_data, by = c('ds' = 'time'))
  
  tb$metric <- na.approx(tb$metric)
  
  names(tb) <- c("ds", "y")
  
  return(tb)
  
}


default_time_seqeunce <- function(period, groupby) {
  
  current <- as.POSIXlt(Sys.time())
  
  unit <- str_sub(groupby, -1)
  
  period <- str_extract(period, '\\d+') %>% as.integer()
  
  past <- current
  
  if (unit == 'd') {
    
    past$mday <- past$mday - period
    
  } else if (unit == 'h') {
    
    past$hour <- past$hour - period
    
  } else {
    
    past$sec <- past$sec - period
    
  }
  
  seq <- seq.POSIXt(past,
                    current,
                    by = posixt_helper_func(str_sub(groupby, -1)))
  
  dt <- data.table('ds' = seq,
                   'y' = -1)
  
  return(dt)
  
}



load_metric_list <- function(measurement) {
  
  connector <- connect()
  
  con <- connector$connector
  
  dbname <- connector$dbname
  
  if (measurement == 'docker') {
    
    cond <- ', docker_network'
    
    measurement <- 'docker_container'
    
  } else {cond <- ''}
  
  query <- 'show field keys from %s%s' %>% sprintf(measurement, cond)
  
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


load_tag_list <- function(measurement) {
  
  connector <- connect()
  
  con <- connector$con
  
  dbname <- connector$dbname
  
  tag <- switch(measurement,
                'host' = 'host_ip',
                'task' = 'task',
                'docker' = 'task_id',
                stop('incorrect measurement'))
  
  if (measurement == 'docker') measurement <- 'docker_container'
  
  query <- 'SHOW TAG VALUES from %s WITH KEY = %s' %>% sprintf(measurement, tag)
  
  res <- influx_query(con,
                      dbname,
                      query,
                      return_xts = F)[[1]] %>%
    as.data.frame() %>% 
    select(value) %>% 
    t() %>% 
    as.vector()
  
  return(res)
  
}


load_host_list_for_task <- function(task_name) {
  
  connector <- connect()
  
  con <- connector$con
  
  dbname <- connector$dbname
  
  query <- "select mean(*)
  from task
  where time > now() - 3d and task = '%s'
  group by time(1w), node_ip" %>% sprintf(task_name)
  
  res <- influx_query(con,
                      dbname,
                      query,
                      return_xts = F,
                      simplifyList = T)[[1]] %>% as.data.frame()
  
  if (!('node_ip' %in% names(res))) return('Not Exist!')
  
  return(res$node_ip)
  
}



#### FORECASTING ####

single_metric_data <- NULL

predicted_data <- NULL

metric_fcst_data <- NULL


#' @get /forecast
function(measurement = 'host', host = '192.168.0.163', metric = 'load1',
         period = 7, groupby = '1h', unit = "0", node_ip = "",
         pred_period = 1, changepoint = 0.01, limit = -1, renew = F) {
  
  if (is.null(single_metric_data) | renew)
    single_metric_data <<- load_single_metric(measurement, host, metric, period,
                                              groupby, unit, limit)
  
  if (is.null(predicted_data) | renew)
    predicted_data <<- forecasting(single_metric_data, groupby, pred_period,
                                   unit, changepoint)
  
  if (is.null(metric_fcst_data) | renew)
    metric_fcst_data <<- history_forecast(single_metric_data, predicted_data) %>% 
      as.data.frame()
  
  return(metric_fcst_data)
  
}


history_forecast <- function(tb_, fcst) {
  
  maxDate <- max(tb_$ds)
  
  fcst[fcst$ds < maxDate,]$yhat_lower <- NA
  
  fcst[fcst$ds < maxDate,]$yhat_upper <- NA
  
  fcst[fcst$ds < maxDate,]$yhat <- NA
  
  series0 <- xts(tb_$y,
                 order.by = tb_$ds,
                 tzone = Sys.getenv("TZ"))
  
  series1 <- xts(fcst$yhat,
                 order.by = fcst$ds,
                 tzone = Sys.getenv("TZ"))
  
  series2 <- xts(fcst$yhat_lower,
                 order.by = fcst$ds,
                 tzone = Sys.getenv("TZ"))
  
  series3 <- xts(fcst$yhat_upper,
                 order.by = fcst$ds,
                 tzone = Sys.getenv("TZ"))
  
  series <- cbind(series0, series1, series2, series3)
  
  names(series) <- c("hist", "yhat", "yhat_lower", "yhat_upper")
  
  return(series)
  
}


#' @get /forecast/metric
function(measurement = 'host', host = '192.168.0.163', metric = 'load1',
         period = 7, groupby = '1h',
         unit = '0', node_ip = '', limit = -1, renew = F) {
  
  if (is.null(single_metric_data) | renew)
    single_metric_data <<- load_single_metric(measurement, host, metric, 
                                              period, groupby,
                                              unit, node_ip, limit = -1)
  
  return(single_metric_data)
  
}


forecasting <- function(tb_, groupby, pred_period, unit, 
                        changepoint = 0.01) {
  # pred_period : how long predict
  
  fcst_model <- prophet(tb_,
                        changepoint.prior.scale = changepoint)
  
  future_data <- make_future(fcst_model, groupby, unit, pred_period)
  
  predicted_data <- predict(fcst_model, future_data)
  
  return(predicted_data)
  
}


make_future <- function(model, groupby, unit, pred_period) {
  
  if (str_sub(groupby, -1) == 's') {
    
    freq <- as.integer(str_sub(groupby, end = -2))
    
  } else if (str_sub(groupby, -1) == 'm') {
    
    freq <- as.integer(str_sub(groupby, end = -2)) * 60
    
  } else {
    
    freq <- as.integer(str_sub(groupby, end = -2)) * 60 * 60
    
  }
  
  if (unit == "0") {
    
    pred_period <- (pred_period * 24 * 60 * 60) %/% freq
    
  } else {
    
    pred_period <- (pred_period * 60 * 60) %/% freq
    
  }
  
  future <- make_future_dataframe(model,
                                  periods = pred_period,
                                  freq = freq)
  
  return(future)
  
}

