library(RestRserve)

#### FORECAST ####

forecast_ <- function(agent_id, resource, host,
                      metric, period = 6, predicted_period = 2,
                      groupby = '1h', unit = '0', mount = 'null') {
  
  tb_ <- load_single_metric(resource, host, metric, period, groupby,
                            unit, agent_id, mount)
  
  result <- forecasting(tb_, groupby, predicted_period, unit,
                        changepoint.prior.scale = 0.1)
  
  data_with_plot <- forecast_result(tb_, result$forecast, metric)
  
  decomponent <- prophet_plot_components(result$model,
                                         result$forecast)
  
  return(list('result' = data_with_plot$pred_data,
              'pred_plot' = data_with_plot$plot,
              'decomponent' = decomponent))
  
}


posixt_helper_func <- function(x) {
  switch(x,
         's' = 'sec',
         'm' = 'min',
         'h' = 'hour',
         stop('incorrect group time'))
}


connect <- function() {
  
  con <- influx_connection(host = 'influxdb.marathon.l4lb.thisdcos.directory',
                           port = 8086)
  
  dbname <- 'nexclipper'
  
  conn <- list(connector = con, dbname = dbname)
  
  return(conn)
  
}


# in the case that mount path is not null
load_single_metric_from_mount_path <- function(host, metric, period, groupby,
                                               unit, agent_id, mount) {
  # host='192.168.0.168';metric='cpu_used_percent';period=2;groupby='1h';unit='0';agent_id=27;mount='/'
  con <- connect()
  
  connector <- con$connector
  
  dbname <- con$dbname
  
  if (unit == '0') {
    
    period <- paste0(period, 'd')
    
  } else {
    
    period <- paste0(period, 'h')
    
  }
  
  query <- "select mean(*)
  from host, host_net, host_disk
  where time > now() - %s and host_ip = '%s' and agent_id = '%s'
  group by time(%s), host_ip, agent_id, mount_name
  fill(none)
  order by time asc" %>%
    sprintf(period, host, agent_id,
            groupby)
  
  cat('\n', query, '\n')
  # if (host == '192.168.0.160' & metric == 'used_percent')
  #   browser()
  res <- influx_query(connector,
                      db = dbname,
                      query = query,
                      simplifyList = T,
                      return_xts = F)[[1]] %>%
    as.data.frame()
  
  host_split <- split(res[, -2], res[, 2])
  
  host_ <- host_split$host %>%
    select_if(~sum(!is.na(.)) > 0) %>%
    select(c(-1:-5, -mean_timestamp))
  
  names(host_) <- gsub('mean_', '', names(host_))
  
  host_net <- host_split$host_net %>%
    select_if(~sum(!is.na(.)) > 0) %>%
    select(c(-1:-5, -mean_timestamp))
  
  names(host_net) <- gsub('mean_', '', names(host_net))
  
  host_disk <- host_split$host_disk %>%
    select_if(~sum(!is.na(.)) > 0) %>%
    subset(mount_name == mount, select = c(-1:-5, -mean_timestamp))
  
  names(host_disk) <- gsub('mean_', '', names(host_disk))
  
  result_host <- inner_join(host_, host_net, by = 'time') %>%
    inner_join(host_disk, by = 'time')
  # browser()
  result_host <- result_host[, c('time', metric)]
  
  unit <- str_extract(groupby, '[:alpha:]') %>% posixt_helper_func()
  
  by <- str_extract(groupby, '\\d+') %>% paste(unit)
  
  ts <- seq.POSIXt(min(result_host$time),
                   max(result_host$time),
                   by = by)
  
  df <- tibble(ds = ts)
  
  tb <- full_join(df, result_host, by = c('ds' = 'time'))
  
  names(tb) <- c("ds", "y")
  
  tb$y <- na.approx(tb$y)
  
  return(tb)
  
}


load_single_metric <- function(measurement, host, metric, period, groupby,
                               unit, agent_id, mount = 'null') {
  # For forecasting, anomaly detection, read only one metric
  # host : host or task name
  
  # measurement <- 'docker'
  # host <- 'influxdb.6d7f9135-8681-11e8-80dc-664d329f843c'
  # metric <- 'cpu_used_percent'
  # period <- 1
  # groupby <- '1m'
  # unit = '0'
  
  con <- connect()
  
  connector <- con$connector
  
  dbname <- con$dbname
  
  if (str_detect(host, 'Choose') | str_detect(metric, 'Choose'))
    
    return(default_time_seqeunce(period, groupby))
  
  if (measurement == 'host' & mount != 'null')
    
    return(load_single_metric_from_mount_path(host, metric, period, groupby,
                                              unit, agent_id, mount))
  
  'load %s' %>% sprintf(measurement) %>% print()
  
  if (unit == '0') {
    
    period <- paste0(period, 'd')
    
  } else {
    
    period <- paste0(period, 'h')
    
  }
  
  query <- "select mean(%s) as metric
  from %s
  where time > now() - %s and %s = '%s' and agent_id = '%s' %s
  group by time(%s), %s, agent_id %s
  fill(none)
  order by time asc"
  
  tag <- switch(measurement,
                'host' = 'host_ip',
                # 'task' = 'executor_id',
                'docker' = 'task_id')
  
  docker_host_ip <- ''
  
  docker_groupby <- ''
  
  if (measurement == 'docker') {
    
    measurement <- 'docker_container, docker_network'
    
    host <- strsplit(host, '/-/') %>% unlist()
    
    docker_groupby <- ', host_ip'
    
    docker_host_ip <- "and host_ip = '%s'" %>% sprintf(host[2])
    
    host <- host[1]
    
  } else if (measurement == 'host') {
    
    measurement <- 'host, host_disk, host_net'
    
  }
  
  query <- sprintf(query,
                   metric,
                   measurement,
                   period, tag, host, agent_id, docker_host_ip,
                   groupby, tag, docker_groupby)
  
  cat('\n', query, '\n')
  
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


forecasting <- function(tb_, groupby, pred_period, unit,
                        changepoint.prior.scale = 0.01) {
  
  model <- prophet(tb_,
                   changepoint.prior.scale = changepoint.prior.scale)
  
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
  # browser()
  future <- make_future_dataframe(model,
                                  periods = pred_period,
                                  freq = freq)
  
  fcst <- predict(model, future)
  
  res <- list('model' = model,
              'forecast' = fcst)
  
  return(res)
  
}


forecast_result <- function(tb_, fcst, metric) {
  
  maxDate <- tb_$ds %>% max()
  
  pred_data <- fcst %>%
    select(ds, yhat_lower, yhat_upper, yhat) %>%
    full_join(tb_) %>%
    as.data.table(key = 'ds')
  
  pred_data[ds < maxDate, 'yhat_lower'] <- NA
  
  pred_data[ds < maxDate, 'yhat_upper'] <- NA
  
  pred_data[ds < maxDate, 'yhat'] <- NA
  
  p <- ggplot(pred_data, aes(x = ds)) +
    geom_line(aes(y = y, colour = 'Actual'), size = 1, na.rm = T) +
    geom_line(aes(y = yhat, color = 'Predicted'), size = 1, na.rm = T) +
    geom_ribbon(aes(ymin = yhat_lower,
                    ymax = yhat_upper),
                alpha = 0.1, linetype = 2) +
    scale_color_manual(labels = c('Actual', 'Predicted'),
                       values = c("blue", "red")) +
    theme(legend.background = element_rect(fill = "white", size = 2),
          legend.justification = 'right',
          legend.position = 'top',
          legend.direction = 'horizontal',
          legend.title = element_blank(),
          legend.text = element_text(size = 10),
          legend.spacing.x = unit(0.3, 'cm')) +
    labs(x = 'time', y = metric)
  
  return(list('pred_data' = pred_data,
              'plot' = p))
  
}


