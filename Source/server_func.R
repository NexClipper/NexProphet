#### ENVIRONMENT VARIABLE ####

INFLUX_ADDRESS <- 'influxdb.marathon.l4lb.thisdcos.directory'

INFLUX_PORT <- 8086

INFLUX_DBNAME <- 'nexclipper'

HOSTAPI_ADDRESS <- 'nexcloudhostapi.marathon.l4lb.thisdcos.directory:10100'

#----

#### COMMON FUNCTION ####

posixt_helper_func <- function(x) {
  switch(x,
         's' = 'sec',
         'm' = 'min',
         'h' = 'hour',
         stop('incorrect group time'))
}


connect <- function() {
  
  con <- influx_connection(host = INFLUX_ADDRESS,
                           port = INFLUX_PORT)
  
  dbname <- INFLUX_DBNAME
  
  conn <- list(connector = con, dbname = dbname)
  
  return(conn)
  
}


standardization <- function(mtx) {
  
  col_min <- as.vector(apply(mtx, 2, function(x) min(x, na.rm = T)))
  col_max <- as.vector(apply(mtx, 2, function(x) max(x, na.rm = T)))
  new_mtx <- t(apply(mtx,
                     1,
                     function(x) (x - col_min) / (col_max - col_min + 1e-6)))
  return(new_mtx)
}


load_single_metric_from_mount_path <- function(host, metric, period, groupby,
                                               unit, agent_id, mount) {
  
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


load_multiple_metric <- function(period, groupby,
                                 host_list, metric_list,
                                 agent_id) {
  
  connector <- connect()
  
  con <- connector$connector
  
  dbname <- connector$dbname
  
  whole_data <- NULL
  
  if (!is.null(metric_list$host)) {
    
    host_metric <- load_metric_from_host(period = period,
                                         groupby = groupby,
                                         host_list = host_list$host,
                                         metric_list = metric_list$host,
                                         agent_id)
    
    whole_data <- host_metric
    
  }
  
  if (!is.null(metric_list$docker)) {
    
    docker_metric <- load_metric_from_docker(period = period,
                                             groupby = groupby,
                                             host_list = host_list$docker,
                                             metric_list = metric_list$docker,
                                             agent_id)
    
    if (is.null(whole_data)) {
      
      whole_data <- docker_metric
      
    } else {
      
      whole_data <- inner_join(whole_data, docker_metric, by = 'time')
      
    }
    
  }
  
  return(whole_data)
  
}


load_metric_from_host <- function(period, groupby,
                                  host_list, metric_list,
                                  agent_id) {
  
  connector <- connect()
  
  con <- connector$connector
  
  dbname <- connector$dbname
  
  period <- paste0(period, 'd')
  
  host_ip <- paste('host_ip', "'%s'", sep = ' = ') %>%
    sprintf(host_list) %>% 
    paste(collapse = ' or ')
  
  #### host ####
  query <- "select mean(*)
            from host
            where time > now() - %s and agent_id = '%s' and (%s)
            group by time(%s), host_ip, agent_id
            fill(none)"
  
  query <- sprintf(query, 
                   period, agent_id, host_ip,
                   groupby)
  cat('\n', query, '\n')
  res_host <- influx_query(con,
                           db = dbname,
                           query = query,
                           simplifyList = T,
                           return_xts = F)[[1]]
  
  names(res_host) <- gsub('mean_', '', names(res_host))
  
  res_host <- res_host %>%
    select(which(names(res_host) %in%
                   c('time', 'host_ip', metric_list)))
  
  #### host disk ####
  query <- "select mean(*)
            from host_disk
            where time > now() - %s and agent_id = '%s' and (%s)
            group by time(%s), host_ip, agent_id
            fill(none)"
  
  query <- sprintf(query, 
                   period, agent_id, host_ip,
                   groupby)
  
  res_host_disk <- influx_query(con,
                                db = dbname,
                                query = query,
                                simplifyList = T,
                                return_xts = F)[[1]]
  
  names(res_host_disk) <- gsub('mean_', '', names(res_host_disk))
  
  res_host_disk <- res_host_disk %>%
    select(which(names(res_host_disk) %in%
                   c('time', 'host_ip', metric_list)))
  
  #### host network ####
  query <- "select mean(*)
            from host_net
            where time > now() - %s and agent_id = '%s' and (%s)
            group by time(%s), host_ip, agent_id
            fill(none)"
  
  query <- sprintf(query, 
                   period, agent_id, host_ip,
                   groupby)
  
  res_host_net <- influx_query(con,
                               db = dbname,
                               query = query,
                               simplifyList = T,
                               return_xts = F)[[1]]
  
  names(res_host_net) <- gsub('mean_', '', names(res_host_net))
  
  res_host_net <- res_host_net %>%
    select(which(names(res_host_net) %in%
                   c('time', 'host_ip', metric_list)))
  
  #### inner join host, host_disk, host_net ####
  res_host <- inner_join(res_host, res_host_disk,
                         by = c('host_ip', 'time')) %>% 
    inner_join(res_host_net,
               by = c('host_ip', 'time'))
  
  #### full join ####
  unit <- str_extract(groupby, '[:alpha:]') %>% posixt_helper_func()

  by <- str_extract(groupby, '\\d+') %>% paste(unit)
  
  ts <- seq.POSIXt(min(res_host$time),
                   max(res_host$time),
                   by = by)
  
  df <- tibble(time = ts)
  
  res_host <- full_join(df, res_host, by = 'time')
  
  res_host <- gather(res_host, var, value, c(-time, -host_ip)) %>% 
    unite(var_new, host_ip, var, sep = ', ') %>% 
    spread(var_new, value)
  
  print('Load mutiple metric for host!')
  
  return(res_host)
  
}


load_metric_from_docker <- function(period, groupby,
                                    host_list, metric_list,
                                    agent_id) {
  
  connector <- connect()
  
  con <- connector$connector
  
  dbname <- connector$dbname
  
  period <- paste0(period, 'd')
  
  host_df <- data.frame('var' = host_list) %>%
    separate(var, c('col1', 'col2'), sep = '/-/')
  
  task_id <- paste('task_id', "'%s'", sep = ' = ') %>%
    sprintf(unique(host_df$col1)) %>% 
    paste(collapse = ' or ')
  
  host_ip <- paste('host_ip', "'%s'", sep = ' = ') %>%
    sprintf(unique(host_df$col2)) %>% 
    paste(collapse = ' or ')
  
  #### Read docker container ####
  query <- "select mean(*)
            from docker_container
            where time > now() - %s and agent_id = '%s' and (%s) and (%s)
            group by time(%s), task_id, host_ip, agent_id
            fill(linear)" %>% 
    sprintf(period, agent_id, task_id, host_ip,
            groupby)
  
  cat('\n', query, '\n')
  
  res_container <- influx_query(con,
                                db = dbname,
                                query = query,
                                simplifyList = T,
                                return_xts = F)[[1]] 
  
  names(res_container) <- gsub('mean_', '', names(res_container))
  
  res_container <- res_container %>% 
    select(which(names(res_container) %in%
                   c('time', 'task_id', 'host_ip', metric_list)))
  
  #### Read docker network ####
  query <- "select mean(*)
            from docker_network
            where time > now() - %s and agent_id = '%s' and (%s) and (%s)
            group by time(%s), task_id, agent_id, host_ip
            fill(linear)" %>% 
    sprintf(period, agent_id, task_id, host_ip,
            groupby)
  
  cat('\n', query, '\n')
  
  res_network <- influx_query(con,
                              db = dbname,
                              query = query,
                              simplifyList = T,
                              return_xts = F)[[1]]
  
  names(res_network) <- gsub('mean_', '', names(res_network))
  
  res_network <- res_network %>% 
    select(which(names(res_network) %in%
                   c('time', 'task_id', 'host_ip', metric_list)))
  
  #### inner join res_container with res_network ####
  if (length(names(res_container)) + length(names(res_network)) == 0)
    
    return(tibble(ds = NA, y = NA))
  if (length(names(res_container)) == 0 &
      length(names(res_network)) != 0) {
    
    res_docker <- res_network
    
  } else if (length(names(res_container)) != 0 &
             length(names(res_network)) == 0) {
    
    res_docker <- res_container
    
  } else {
    
    res_docker <- full_join(res_container,
                            res_network,
                            by = c('time', 'task_id', 'host_ip'))
    
  }
  
  res_docker$task_id <- str_extract(res_docker$task_id, '[[:alpha:]\\d-_]+')
  
  unit <- str_extract(groupby, '[:alpha:]') %>% posixt_helper_func()
  
  by <- str_extract(groupby, '\\d+') %>% paste(unit)
  
  ts <- seq.POSIXt(min(res_docker$time),
                   max(res_docker$time),
                   by = by)
  
  df <- tibble(time = ts)
  
  res_docker <- full_join(df, res_docker, by = 'time')
  
  gather(res_docker, var, value, -1:-3) %>% 
    unite(var_new, task_id, host_ip, var, sep = ', ') %>% 
    group_by(time, var_new) %>% 
    summarise('value' = mean(value)) %>% 
    spread(var_new, value) %>% 
    return()
  
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


load_metric_list <- function(measurement, default_ = T) {
  
  connector <- connect()
  'load %s metric list!' %>%
    sprintf(measurement) %>%
    print()
  con <- connector$connector
  
  dbname <- connector$dbname
  
  if (measurement == 'docker') 
    
    measurement <- 'docker_container, docker_network'
  
  if (measurement == 'host')
    
    measurement <- 'host, host_disk, host_net'
  
  query <- 'show field keys from %s' %>% 
    sprintf(measurement)
  
  res <- influx_query(con,
                      dbname,
                      query,
                      return_xts = F)[[1]] %>%
    as.data.frame() %>%
    select(c(series_names, fieldKey)) %>% 
    subset(fieldKey != 'timestamp' & !str_detect(fieldKey, '_per$'))
  
  if (!default_)
    
    return(split(res$fieldKey,
                 res$series_names))
  
  res <- rbind(c('Choose Metric', 'Choose Metric'), res)
  
  metric <- split(res$fieldKey, res$series_names)
  
  return(metric)
  
}


load_host_disk_mount_path <- function(host_ip, agent_id) {
  
  connector <- connect()
  
  print('disk mount path')
  
  con <- connector$connector
  
  dbname <- connector$dbname
  
  query <- "show tag values
            from host_disk
            with key = mount_name
            where host_ip =~ /%s/ and agent_id =~ /%s/" %>% 
    sprintf(host_ip, agent_id)
  
  mount_path <- influx_query(con,
                             dbname,
                             query,
                             return_xts = F)[[1]] %>%
    as.data.frame() %>%
    select(value) %>% 
    t() %>% 
    as.vector()
  
  return(c('null', mount_path))
  
}


load_tag_list <- function(measurement, agent_id, default_ = T) {
  
  tag_list <- switch(measurement,
                     'host' = load_host_tag_list(agent_id, default_),
                     'docker' = load_docker_tag_list(agent_id, default_))
  
  return(tag_list)
  
}


load_docker_tag_list <- function(agent_id, default_) {
  
  url <- 'http://%s/v1/docker/snapshot' %>% sprintf(HOSTAPI_ADDRESS)
  
  res <- GET(url,
             content_type_json(),
             add_headers('agent_id' = agent_id)) %>%
    content('parsed')
  
  if ((res$status != 200) |
      (res$data == '[]') |
      (res$data == '{}'))
    return(list('null' = 'null',
                'null' = 'null'))
  
  docker <- res$data %>%
    fromJSON(simplifyVector = F, flatten = T) %>%
    unlist()
  
  docker_name_list <- data.frame('name' = as.vector(docker[grep('containers.Names', names(docker))]),
                                 'type' = as.vector(docker[grep('containers.Type', names(docker))]),
                                 'host_ip' = names(docker[grep('containers.Names', names(docker))]),
                                 stringsAsFactors = F) %>% 
    subset(type == 'Docker', select = c(name, host_ip))
  
  docker_name_list$host_ip <- str_extract(docker_name_list$host_ip,
                                          '\\d+.\\d+.\\d+.\\d+')
  
  mesos_name_list <- data.frame('name' = as.vector(docker[grep('containers.Labels.MESOS_TASK_ID', names(docker))]),
                                'host_ip' = names(docker[grep('containers.Labels.MESOS_TASK_ID', names(docker))]),
                                stringsAsFactors = F)
  
  mesos_name_list$host_ip <- str_extract(mesos_name_list$host_ip, '\\d+.\\d+.\\d+.\\d+')
  
  kuber_name_list <- data.frame('name' = as.vector(docker[grep('io.kubernetes.container.name', names(docker))]),
                                'type' = as.vector(docker[grep('io.kubernetes.docker.type', names(docker))]),
                                'host_ip' = names(docker[grep('io.kubernetes.container.name', names(docker))]),
                                stringsAsFactors = F) %>% 
    subset(type == 'container', select = c(-type))
  
  kuber_name_list$host_ip <- str_extract(kuber_name_list$host_ip, '\\d+.\\d+.\\d+.\\d+')
  
  docker_name_list <- rbind(docker_name_list,
                            mesos_name_list,
                            kuber_name_list)
  
  united <- unite(docker_name_list, name, name, host_ip, sep = '/-/')
  
  docker_name_list$name <- united$name
  
  unique_ip <- which(table(docker_name_list$host_ip) == 1) %>% names()
  
  idx <- which(docker_name_list$host_ip %in% unique_ip)
  
  docker_name_list$host_ip[idx] <- docker_name_list$name[idx]
  
  name_split <- split(docker_name_list$name,
                      docker_name_list$host_ip)
  
  if (!default_)
    return(name_split)
  
  names_ <- c('Choose Container', names(name_split))
  
  name_split$`Choose Container` <- 'Choose Container'
  
  name_split[names_] %>% return()
  
}


load_host_tag_list <- function(agent_id, default_) {
  
  url <- 'http://%s/v1/agent/status' %>% 
    sprintf(HOSTAPI_ADDRESS)
  
  res <- GET(url,
             content_type_json(),
             add_headers('agent_id' = agent_id)) %>%
    content('parsed')
  
  if ((res$status != 200) |
      (res$data == '[]') |
      (res$data == '{}'))
    return(list('null' = 'null'))
  
  host <- res$data %>%
    fromJSON(simplifyVector = F, flatten = T) %>%
    unlist()
  
  names_ <- as.vector(host[grep('host_ip', names(host))])
  
  if (!default_)
    return(names_)
  
  c('Choose Host', names_) %>% 
    return()
  
}

#### FORECAST ####

forecasting <- function(tb_, groupby, pred_period, unit, changepoint.prior.scale = 0.01) {
  
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
  
  future <- make_future_dataframe(model,
                                  periods = pred_period,
                                  freq = freq)
  
  fcst <- predict(model, future)
  
  res <- list('model' = model,
              'forecast' = fcst)
  
  return(res)
  
}


draw_forecast_dygraph <- function(tb_, fcst, maxDate) {
  
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
  
  dygraph(series, main = "Forecasting Result") %>%
    dySeries(c( "hist"), label = "Actual") %>%
    dySeries(c( "yhat_lower", "yhat", "yhat_upper"), label = "Predictd") %>%
    dyRangeSelector(height = 30)
  
}


render_forecast <- function(resource, host, metric, period, groupby,
                            pred_period, unit, agent_id, mount) {
  
  tb_ <- load_single_metric(resource, host, metric, period, groupby, unit,
                            agent_id, mount)
  
  forecast_result <- forecasting(tb_, groupby, pred_period, unit)
  
  fcst <- forecast_result$forecast
  
  rendered <- renderDygraph({
    
    draw_forecast_dygraph(tb_,
                          fcst,
                          max(tb_$ds))
    
  })
  
  res <- list('forecast_result' = forecast_result,
              'rendered' = rendered)
  
  return(res)
  
}


render_forecast_component <- function(result) {
  
  model <- result$model
  
  fcst <- result$forecast
  
  rendered <- renderPlot({
    
    prophet_plot_components(model, fcst)
    
  })
  
  return(rendered)
  
}




#### ANOMALY ####

save_model_info <- function(agent_id, resource, host, unit,
                            metric, mount, developed_at,
                            developed_during) {
  
  developed_during <- developed_during %>% as.vector()

  con <- dbConnect(MySQL(), 
                   user = 'admin', 
                   password = 'password',
                   dbname = 'defaultdb',
                   host = 'mysql.marathon.l4lb.thisdcos.directory', 
                   port = 3306)
  
  info <- data.frame('agent_id' = agent_id,
                     'resource' = resource,
                     'target' = host,
                     'unit' = unit,
                     'metric' = metric,
                     'mount' = mount,
                     'developed_at' = developed_at,
                     'developed_during' = developed_during
                     )
  
  dbWriteTable(con, 
               name = 'model_info', 
               value = info,
               row.names = F,
               append = T)
  
  dbCommit(con)
  
  dbDisconnect(con)
  
  print('Success to save model information!')
  
}


renew <- function(renewal) {
  
  unit <- str_extract(renewal, '[:alpha:]+')
  
  time_ <- str_extract(renewal, '\\d+') %>% as.integer()
  
  time_ <- ifelse(is.na(time_), 0, time_)
  
  renew_time <- time_ * switch(unit,
                               's' = 1,
                               'm' = 60,
                               'h' = 60 * 60,
                               'off' = 0)
  
  return(renew_time)
  
}


#### METRIC CORRELATION ####
horizon.panel.ggplot <- function(df, add_text=NULL) {
  
  lv <- levels(df$grouping)
  
  if (is.null(add_text)) {
    
    add_text = ""
    
  } else {
    
    add_text = paste0("(", add_text, ")")  
    
  }
  
  labeli2 <- function(variable, value) {
    
    value <- droplevels(value)
    
    names_li <- as.list(paste(lv, add_text, sep = ' '))
    
    names(names_li) <- lv
    
    return(names_li[value])
    
  }
  
  ggplot(data = df) +
    geom_line(aes(x = time, y = y), size = 0.75, color = "darkgoldenrod") + 
    facet_wrap(. ~ grouping, labeller = labeli2, scales = "free_y", ncol = 1) +    #do new subplot for each group
    theme_bw() +                  #this is optional, but I prefer to default
    theme(legend.position = "none",    #remove legend
          strip.text = element_text(size = 15),
          strip.text.y = element_text(angle = 0),#rotate strip text to horizontal
          strip.background = element_rect(fill = 'grey95'),
          axis.text.y = element_blank(),#remove y axis labels
          axis.ticks.y = element_blank(), #remove tick marks
          axis.title.y = element_blank(),#remove title for the y axis
          axis.title.x = element_blank()
    )
  
}
