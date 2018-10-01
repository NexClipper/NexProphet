library(RestRserve)
library(jsonlite)

#### FORECAST ####
forecast_ <- function(agent_id, measurement, host_ip,
                      metric, period, predicted_period,
                      groupby, unit, ...) {
  
  dt_ <- load_single_metric(agent_id, measurement, host_ip, metric,
                            period, groupby, unit, ...)
  
  result <- forecasting(dt_, groupby, predicted_period, unit,
                        changepoint.prior.scale = 0.1)
  
  result %>% toJSON() %>% as.character() %>% return()
  
}


#### EVENT ALARM ####

select_unit <- function(x) {
  
  switch(x,
         's' = 'sec',
         'm' = 'min',
         'h' = 'hour') %>% return()
  
}


connect <- function() {
  
  # con <- influx_connection(host = 'influxdb.marathon.l4lb.thisdcos.directory',
  #                          port = 8086)
  con <- influx_connection(host = '192.168.0.162',
                           port = 10091)
  
  dbname <- 'nexclipper'
  
  conn <- list(connector = con, dbname = dbname)
  
  return(conn)
  
}


# in the case that mount path is not null
load_single_metric_from_mount_path <- function(host, metric, period, groupby,
                                               unit, agent_id, mount) {
  # host='192.168.0.168';metric='used_percent';period=6;groupby='1h';unit='0';agent_id=27;mount='/'
  con <- connect()
  
  connector <- con$connector
  
  dbname <- con$dbname
  
  if (unit == '0') {
    
    period <- paste0(period, 'd')
    
  } else {
    
    period <- paste0(period, 'h')
    
  }
  
  query <- "select mean(%s) as y
            from host_disk
            where time > now() - %s and
                  host_ip = '%s' and
                  agent_id = '%s' and
                  mount_name = '%s'
            group by time(%s)
            fill(none)" %>%
    sprintf(metric,
            period, host, agent_id, mount,
            groupby)
  
  cat('\n', query, '\n')
  
  res <- influx_query(connector,
                      db = dbname,
                      query = query,
                      simplifyList = T,
                      return_xts = F)[[1]] %>%
    as.data.table() %>% 
    .[, -1:-4] %>% 
    setnames('time', 'ds') %>% 
    setkey(ds)
  
  unit <- str_extract(groupby, '[:alpha:]') %>%
    posixt_helper_func()
  
  by <- str_extract(groupby, '\\d+') %>%
    paste(unit)
  
  ts <- seq.POSIXt(min(res$ds),
                   max(res$ds),
                   by = by)
  
  df <- data.table(ds = ts, key = 'ds')
  
  res <- res[df]
  
  res$y <- na.approx(res$y) %>% na.fill('extend')
  
  return(res)
  
}


load_single_metric <- function(agent_id, measurement, host_ip, metric,
                               period, groupby, unit, ...) {
  
  arg <- list(...)
  
  agent_id <- agent_id %>% as.integer()
  
  period <- period %>% as.integer()
  
  unit <- unit %>% as.character()
  
  switch(measurement,
         'host' = load_host(agent_id, host_ip, metric, period, groupby, unit),
         'host_disk' = load_host_disk(agent_id, host_ip, metric, period, groupby, unit,
                                      arg$mount),
         'host_net' = load_host_net(agent_id, host_ip, metric, period, groupby, unit,
                                    arg$hostIF),
         'host_process' = load_host_process(agent_id, host_ip, metric, period, groupby, unit,
                                            arg$pname),
         'docker_container' = load_docker_container(agent_id, host_ip, metric, period, groupby, unit,
                                                    arg$dname),
         'docker_network' = load_docker_network(agent_id, host_ip, metric, period, groupby, unit,
                                                arg$dname, arg$dockerIF)) %>% 
    return()
  
}

dtt <- load_single_metric(27, 'docker_network', '192.168.0.165', 'rx_bytes',
                          6, '1h', 0, dname = 'mysql.8491cb20-acb7-11e8-ae9f-aae0d7e58657', dockerIF = 'eth0')

load_docker_container <- function(agent_id, host_ip, metric, period, groupby, unit, dname) {
  #agent_id=27;host_ip='192.168.0.165';metric='cpu_used_percent';period=6;groupby='1h';unit='0';dname='/Nexclipper-Agent'
  con <- connect()
  
  connector <- con$connector
  
  dbname <- con$dbname
  
  period <- ifelse(unit == '0',
                   paste0(period, 'd'),
                   paste0(period, 'h'))
  
  query <- "select mean(%s) as y
            from docker_container
            where agent_id = '%s' and
                  time > now() - %s and
                  host_ip = '%s' and 
                  task_id = '%s'
            group by time(%s)" %>% 
    sprintf(metric,
            agent_id,
            period,
            host_ip,
            dname,
            groupby)
  
  cat('\n', query, '\n\n')
  
  res <- influx_query(connector,
                      dbname,
                      query, return_xts = F,
                      simplifyList = T)[[1]] %>% 
    as.data.table()
  
  if (!('time' %in% names(res)))
    
    return(get_alternative_data(period, groupby))
  
  res %>% 
    .[, -1:-4] %>% 
    setnames('time', 'ds') %>% 
    .[, ds := with_tz(ds, 'Asia/Seoul')] %>% 
    setkey(ds) %>% 
    return()
  
}


load_docker_network <- function(agent_id, host_ip, metric, period, groupby, unit, dname, interface) {
  #agent_id=27;host_ip='192.168.0.165';metric='rx_bytes';period=6;groupby='1h';unit='0';dname='nexcloud_nexclipperui.081024c1-c2f2-11e8-8aa1-aae0d7e58657';interface='eth0'
  con <- connect()
  
  connector <- con$connector
  
  dbname <- con$dbname
  
  period <- ifelse(unit == '0',
                   paste0(period, 'd'),
                   paste0(period, 'h'))
  
  query <- "select mean(%s) as y
            from docker_network
            where agent_id = '%s' and
                  time > now() - %s and
                  host_ip = '%s' and 
                  task_id = '%s' and
                  interface = '%s'
            group by time(%s)" %>% 
    sprintf(metric,
            agent_id,
            period,
            host_ip,
            dname,
            interface,
            groupby)
  
  cat('\n', query, '\n\n')
  
  res <- influx_query(connector,
                      dbname,
                      query, return_xts = F,
                      simplifyList = T)[[1]] %>% 
    as.data.table()
  
  if (!('time' %in% names(res)))
    
    return(get_alternative_data(period, groupby))
  
  res %>% 
    .[, -1:-4] %>% 
    setnames('time', 'ds') %>% 
    .[, ds := with_tz(ds, 'Asia/Seoul')] %>% 
    setkey(ds) %>% 
    return()
  
}


load_host <- function(agent_id, host_ip, metric, period, groupby, unit) {
  #agent_id=27;host_ip='192.168.0.165';metric='cpu_used_percent';period=6;groupby='1h';unit='0'
  con <- connect()
  
  connector <- con$connector
  
  dbname <- con$dbname
  
  period <- ifelse(unit == '0',
                   paste0(period, 'd'),
                   paste0(period, 'h'))
  
  query <- "select mean(%s) as y
            from host
            where agent_id = '%s' and
                  time > now() - %s and
                  host_ip = '%s'
            group by time(%s)" %>% 
    sprintf(metric,
            agent_id,
            period,
            host_ip,
            groupby)
  
  cat('\n', query, '\n\n')
  
  res <- influx_query(connector,
                      dbname,
                      query, return_xts = F,
                      simplifyList = T)[[1]] %>% 
    as.data.table()
  
  if (!('time' %in% names(res)))
    
    return(get_alternative_data(period, groupby))
  
  res %>% 
    .[, -1:-4] %>% 
    setnames('time', 'ds') %>% 
    .[, ds := with_tz(ds, 'Asia/Seoul')] %>% 
    setkey(ds) %>% 
    return()
  
}


load_host_disk <- function(agent_id, host_ip, metric, period, groupby, unit, mount) {
  #agent_id=27;host_ip='192.168.0.165';metric='used_percent';period=6;groupby='1h';unit='0';mount='/'
  con <- connect()
  
  connector <- con$connector
  
  dbname <- con$dbname
  
  period <- ifelse(unit == '0',
                   paste0(period, 'd'),
                   paste0(period, 'h'))
  
  query <- "select mean(%s) as y
            from host_disk
            where agent_id = '%s' and
                  time > now() - %s and
                  host_ip = '%s' and
                  mount_name = '%s'
            group by time(%s)" %>% 
    sprintf(metric,
            agent_id,
            period,
            host_ip,
            mount,
            groupby)
  
  cat('\n', query, '\n\n')
  
  res <- influx_query(connector,
                      dbname,
                      query, return_xts = F,
                      simplifyList = T)[[1]] %>% 
    as.data.table()
  
  if (!('time' %in% names(res)))
    
    return(get_alternative_data(period, groupby))
  
  res %>% 
    .[, -1:-4] %>% 
    setnames('time', 'ds') %>% 
    .[, ds := with_tz(ds, 'Asia/Seoul')] %>% 
    setkey(ds) %>% 
    return()
  
}


load_host_net <- function(agent_id, host_ip, metric, period, groupby, unit, interface) {
  #agent_id=27;host_ip='192.168.0.165';metric='rxbyte';period=6;groupby='1h';unit='0';interface='veth99a298c8'
  con <- connect()
  
  connector <- con$connector
  
  dbname <- con$dbname
  
  period <- ifelse(unit == '0',
                   paste0(period, 'd'),
                   paste0(period, 'h'))
  
  query <- "select mean(%s) as y
            from host_net
            where agent_id = '%s' and
                  time > now() - %s and
                  host_ip = '%s' and
                  interface = '%s'
            group by time(%s)" %>% 
    sprintf(metric,
            agent_id,
            period,
            host_ip,
            interface,
            groupby)
  
  cat('\n', query, '\n\n')
  
  res <- influx_query(connector,
                      dbname,
                      query, return_xts = F,
                      simplifyList = T)[[1]] %>% 
    as.data.table()
  
  if (!('time' %in% names(res)))
    
    return(get_alternative_data(period, groupby))
  
  res %>% 
    .[, -1:-4] %>% 
    setnames('time', 'ds') %>% 
    .[, ds := with_tz(ds, 'Asia/Seoul')] %>% 
    setkey(ds) %>% 
    return()
  
}


load_host_process <- function(agent_id, host_ip, metric, period, groupby, unit, pname) {
  #agent_id=27;host_ip='192.168.0.165';metric='cpu_used_percent';period=6;groupby='1h';unit='0';pname='mysqld'
  con <- connect()
  
  connector <- con$connector
  
  dbname <- con$dbname
  
  period <- ifelse(unit == '0',
                   paste0(period, 'd'),
                   paste0(period, 'h'))
  
  pname <- paste0('"name" = ', "'%s'") %>% 
    sprintf(pname)
  
  query <- "select mean(%s) as y
            from host_process
            where agent_id = '%s' and
                  time > now() - %s and
                  host_ip = '%s' and 
                  %s
            group by time(%s)" %>% 
    sprintf(metric,
            agent_id,
            period,
            host_ip,
            pname,
            groupby)
  
  cat('\n', query, '\n\n')
  
  res <- influx_query(connector,
                      dbname,
                      query, return_xts = F,
                      simplifyList = T)[[1]] %>% 
    as.data.table()
  
  if (!('time' %in% names(res)))
    
    return(get_alternative_data(period, groupby))
  
  res %>% 
    .[, -1:-4] %>% 
    setnames('time', 'ds') %>% 
    .[, ds := with_tz(ds, 'Asia/Seoul')] %>% 
    setkey(ds) %>% 
    return()
  
}


load_single_metric <- function(measurement, host, metric, period, groupby,
                               unit, agent_id, mount) {
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
  
  # if (str_detect(host, 'Choose') | str_detect(metric, 'Choose'))
  #   
  #   return(default_time_seqeunce(period, groupby))
  
  if (measurement == 'host' & mount != 'null')
    
    return(load_single_metric_from_mount_path(host, metric, period, groupby,
                                              unit, agent_id, mount))
  
  'load %s' %>% sprintf(measurement) %>% print()
  
  if (unit == '0') {
    
    period <- paste0(period, 'd')
    
  } else {
    
    period <- paste0(period, 'h')
    
  }
  
  tag <- switch(measurement,
                'host' = 'host_ip',
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
  
  query <- "select mean(%s) as y
            from %s
            where time > now() - %s and
                  %s = '%s' and
                  agent_id = '%s' %s
            group by time(%s), %s, agent_id %s
            fill(none)" %>% 
    sprintf(metric,
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


get_alternative_data <- function(groupby) {
  # when no data!
  current <- as.POSIXlt(Sys.time())
  
  by <- paste('-1', select_unit(str_sub(groupby, -1)))
  
  seq <- seq.POSIXt(from = current,
                    length.out = 30,
                    by = by)
  
  dt <- data.table('ds' = seq,
                   'y' = 404, key = 'ds') %>% 
    setorder(ds)
  
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
  
  maxDate <- tb_$ds %>% max()
  
  pred_data <- predict(model, future) %>%
    select(ds, yhat_lower, yhat_upper, yhat) %>%
    full_join(tb_) %>%
    as.data.table(key = 'ds')
  
  pred_data[ds < maxDate, 'yhat_lower'] <- NA
  
  pred_data[ds < maxDate, 'yhat_upper'] <- NA
  
  pred_data[ds < maxDate, 'yhat'] <- NA
  
  return(pred_data)
  
  # res <- list('model' = model,
  #             'forecast' = fcst)
  # 
  # return(res)
  
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
  
  return(pred_data)
  # p <- ggplot(pred_data, aes(x = ds)) +
  #   geom_line(aes(y = y, colour = 'Actual'), size = 1, na.rm = T) +
  #   geom_line(aes(y = yhat, color = 'Predicted'), size = 1, na.rm = T) +
  #   geom_ribbon(aes(ymin = yhat_lower,
  #                   ymax = yhat_upper),
  #               alpha = 0.1, linetype = 2) +
  #   scale_color_manual(labels = c('Actual', 'Predicted'),
  #                      values = c("blue", "red")) +
  #   theme(legend.background = element_rect(fill = "white", size = 2),
  #         legend.justification = 'right',
  #         legend.position = 'top',
  #         legend.direction = 'horizontal',
  #         legend.title = element_blank(),
  #         legend.text = element_text(size = 10),
  #         legend.spacing.x = unit(0.3, 'cm')) +
  #   labs(x = 'time', y = metric)
  
  return(list('pred_data' = pred_data,
              'plot' = p))
  
}


