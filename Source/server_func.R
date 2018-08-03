
#### COMMON FUNCTION ####

posixt_helper_func <- function(x) {
  switch(x,
         's' = 'sec',
         'm' = 'min',
         'h' = 'hour',
         stop('incorrect group time'))
}


connect <- function() {
  
  db_info <- read_json('../Source/influx_info.json')
  
  con <- influx_connection(host = db_info$host,
                           port = db_info$port)
  
  dbname <- db_info$dbname
  
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


# load_single_metric_from_docker <- function(host, metric, period, groupby,
#                                            unit, agent_id) {
#   # host = 'redis.0529f75a-8697-11e8-80dc-664d329f843c';
#   # metric='cpu_used_percent'; period=2; groupby='1h';unit='0';agent_id=27
#   con <- connect()
#   
#   connector <- con$connector
#   
#   dbname <- con$dbname
#   
#   if (unit == '0') {
#     
#     period <- paste0(period, 'd')
#     
#   } else {
#     
#     period <- paste0(period, 'h')
#     
#   }
#   
#   query <- "select mean(%s) as metric
#             from docker_container
#             where time > now() - %s and agent_id = '%s' and task_id = '%s'
#             group by time(%s), agent_id, task_id" %>% 
#     sprintf(metric,
#             period, agent_id, host,
#             groupby)
#   
#   res_docker <- influx_query(connector,
#                              db = dbname,
#                              query = query,
#                              simplifyList = T,
#                              return_xts = F)[[1]] %>% 
#     as.data.frame()
#   
#   if (!('metric' %in% names(res_docker))) {
#     
#     query <- "select mean(%s) as metric
#               from docker_network
#               where time > now() - %s and agent_id = '%s'
#                     and task_id = '%s'
#               group by time(%s), agent_id, task_id" %>% 
#       sprintf(metric,
#               period, agent_id, host,
#               groupby)
#     
#     res_docker <- influx_query(connector,
#                                db = dbname,
#                                query = query,
#                                simplifyList = T,
#                                return_xts = F)[[1]] %>% 
#       as.data.frame()
#   }
#   
#   res_docker <- res_docker %>% 
#     select(c(time, metric))
#   
#   unit <- str_extract(groupby, '[:alpha:]') %>% posixt_helper_func()
#   
#   by <- str_extract(groupby, '\\d+') %>% paste(unit)
#   
#   ts <- seq.POSIXt(min(res_docker$time),
#                    max(res_docker$time),
#                    by = by)
#   
#   df <- tibble(ds = ts)
#   
#   tb <- full_join(df, res_docker, by = c('ds' = 'time'))
#   
#   tb$metric <- na.approx(tb$metric)
#   
#   names(tb) <- c("ds", "y")
#   
#   return(tb)
#     
# }


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
                'task' = 'executor_id',
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

  # by_node <- ''
  # 
  # if (node_ip != "") {
  # 
  #   node_ip <- sprintf("and node_ip = '%s'", node_ip)
  #   by_node <- ', node_ip'
  # 
  # }
  

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


load_metric_from_host <- function(period, groupby,
                                  host_list, metric_list,
                                  agent_id) {
  
  # metric_list <- c("cpu_idle_per", "cpu_idle_percent", 'used_percent')
  # host_list <- c("192.168.0.160", "192.168.0.161")
  # period = 5
  # agent_id = 27
  # groupby = "1h"
  
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


load_metric_from_task <- function(period, groupby,
                                  host_list, metric_list,
                                  agent_id) {
  
  # metric_list <- c("cpu_used_percent", "mem_used_percent")
  # host_list <- c('kafka', 'agent.nexcloud')
  # period = 5
  # agent_id <- 27
  # groupby = "1h"
  
  connector <- connect()
  
  con <- connector$connector
  
  dbname <- connector$dbname
  
  period <- paste0(period, 'd')
  
  # browser()
  task <- paste('executor_id', "'%s'", sep = ' = ') %>%
    sprintf(host_list) %>% 
    paste(collapse = ' or ')
  
  query <- "select mean(*)
            from %s
            where time > now() - %s and agent_id = '%s' and (%s)
            group by time(%s), executor_id, agent_id
            fill(none)"
  
  query <- sprintf(query, 
                   'task', 
                   period, agent_id, task,
                   groupby)
  cat('\n', query, '\n')
  res_task <- influx_query(con,
                           db = dbname,
                           query = query,
                           simplifyList = T,
                           return_xts = F)[[1]]
  # browser()
  if (!('executor_id' %in% names(res_task)))
    
    return(default_time_seqeunce(period, groupby))
  
  names(res_task) <- gsub('mean_', '', names(res_task))
  
  res_task <- res_task %>%
    select(which(names(res_task) %in%
                   c('time', 'executor_id', metric_list)))
  
  unit <- str_extract(groupby, '[:alpha:]') %>% posixt_helper_func()
  
  by <- str_extract(groupby, '\\d+') %>% paste(unit)
  
  ts <- seq.POSIXt(min(res_task$time),
                   max(res_task$time),
                   by = by)
  
  df <- tibble(time = ts)
  
  res_task <- full_join(df, res_task, by = 'time')
  
  res_task <- gather(res_task, var, value, c(-time, -executor_id)) %>% 
    unite(var_new, executor_id, var, sep = ', ') %>% 
    spread(var_new, value)
  
  print('Load multiple metrics for task')
  
  return(res_task)
  
}


load_metric_from_docker <- function(period, groupby,
                                    host_list, metric_list,
                                    agent_id) {
  
  # metric_list <- c("cpu_used_percent", "mem_used_percent")
  # host_list <- c('influxdb.86876958-898f-11e8-80dc-664d329f843c',
  #                'kafka-manager.db6362d7-8696-11e8-80dc-664d329f843c')
  # period = 6
  # 
  # groupby = "1h"
  # agent_id <- 27
  
  connector <- connect()
  
  con <- connector$connector
  
  dbname <- connector$dbname
  # browser()
  period <- paste0(period, 'd')
  
  host_df <- data.frame('var' = host_list) %>%
    separate(var, c('col1', 'col2'), sep = '/-/')
  
  task_id <- paste('task_id', "'%s'", sep = ' = ') %>%
    sprintf(host_df$col1) %>% 
    paste(collapse = ' or ')
  
  host_ip <- paste('host_ip', "'%s'", sep = ' = ') %>%
    sprintf(host_df$col2) %>% 
    paste(collapse = ' or ')
  # browser()
  #### Read docker container ####
  query <- "select mean(*)
            from docker_container
            where time > now() - %s and agent_id = '%s' and (%s) and (%s)
            group by time(%s), task_id, host_ip, agent_id
            fill(none)"
  
  query <- sprintf(query, 
                   period, agent_id, task_id, host_ip,
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
            fill(none)"
  cat('\n', query, '\n')
  query <- sprintf(query, 
                   period, agent_id, task_id, host_ip,
                   groupby)
  
  res_network <- influx_query(con,
                              db = dbname,
                              query = query,
                              simplifyList = T,
                              return_xts = F)[[1]]
  
  names(res_network) <- gsub('mean_', '', names(res_network))
  
  res_network <- res_network %>% 
    select(which(names(res_network) %in%
                   c('time', 'task_id', 'host_ip', metric_list)))
  
  if (length(names(res_container)) + length(names(res_network)) == 0)
    
    return(tibble(ds = NA, y = NA))
  
  #### inner join res_container with res_network ####
  res_docker <- full_join(res_container,
                          res_network,
                          by = c('time', 'task_id', 'host_ip'))
  
  res_docker$task_id <- str_extract(res_docker$task_id, '[[:alpha:]\\d-_]+')
  
  unit <- str_extract(groupby, '[:alpha:]') %>% posixt_helper_func()
  
  by <- str_extract(groupby, '\\d+') %>% paste(unit)
  
  ts <- seq.POSIXt(min(res_docker$time),
                   max(res_docker$time),
                   by = by)
  
  df <- tibble(time = ts)
  
  res_docker <- full_join(df, res_docker, by = 'time')
  # browser()
  res_docker <- gather(res_docker, var, value, c(-time, -task_id, -host_ip)) %>% 
    unite(var_new, task_id, host_ip, var, sep = ', ') %>% 
    spread(var_new, value)
  # browser()
  print('Load multiple metrics for docker')
  
  return(res_docker)
  
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
    # browser()
    if (is.null(whole_data)) {
      
      whole_data <- docker_metric
      
    } else {
      
      whole_data <- inner_join(whole_data, docker_metric, by = 'time')
      
    }
    
  }
  
  if (!is.null(metric_list$task)) {
    
    task_metric <- load_metric_from_task(period = period,
                                         groupby = groupby,
                                         host_list = host_list$task,
                                         metric_list = metric_list$task,
                                         agent_id)
    
    if (is.null(whole_data)) {
      
      whole_data <- task_metric
      
    } else {
      
      whole_data <- inner_join(whole_data, task_metric, by = 'time')
      
    }
    
  }
  
  return(whole_data)
  
}


# load_multiple_metric(5, '1h',
#                      list('host'=c('192.168.0.162',
#                                    '192.168.0.163')),
#                      list('cluster' = c('cpu_used_percent',
#                                         'mem_used_percent'),
#                           'host' = c('load1')))


# mtx <- load_multiple_metric(period = 5,
#                      groupby = '1h',
#                      host_list = list('cluster'='192.168.0.161',
#                                       'host'=c('192.168.0.162',
#                                                '192.168.0.163'),
#                                       'task'=c('agent.nexcloud')),
#                      metric_list = list('cluster'=c('cpu_used_percent',
#                                                     'mem_used_percent',
#                                                     'disk_used_percent'),
#                                         'host'=c('load1'),
#                                         'task'=c('cpu_used_percent'))
#                      ) %>% as.matrix()


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
  # measurement <- 'host, host_disk, host_net'; measurement <- 'task'; measurement <- 'docker'
  # agent_id <- 27
  connector <- connect()
  print('load metric list!')
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
  
  metric <- split(res$fieldKey, res$series_names)
  
  return(metric)
  
}


load_host_disk_mount_path <- function(host_ip, agent_id) {
  
  # host_ip='172.17.0.1';agent_id='60'
  # host_ip='192.168.0.162';agent_id='27'
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


load_tag_list <- function(measurement, agent_id) {
  # measurement <- 'docker'; agent_id <- 27
  
  tag_list <- switch(measurement,
                     'host' = load_host_tag_list(agent_id),
                     'task' = load_task_tag_list(agent_id),
                     'docker' = load_docker_tag_list(agent_id))
  
  return(tag_list)
  
}


load_task_tag_list <- function(agent_id) {
  
  res <- GET('http://192.168.0.162:10100/nexcloud_mesosapi/v1/dashboard/task',
             content_type_json(),
             add_headers('agent_id' = agent_id)) %>%
    content('parsed')
  
  task <- res$data %>%
    fromJSON(simplifyVector = F, flatten = T) %>%
    unlist()
  
  task_name_list <- data.frame('node_ip' = as.vector(task[grep("tasks.node_ip", names(task))]),
                               'executor_id' = as.vector(task[grep("tasks.executor_id", names(task))]),
                               stringsAsFactors = F)
  
  return(split(task_name_list$executor_id,
               task_name_list$node_ip))
  
}


load_docker_tag_list <- function(agent_id) {
  # agent_id <- 27
  res <- GET('http://192.168.0.162:10100/nexcloud_hostapi/v1/docker/snapshot',
             content_type_json(),
             add_headers('agent_id' = agent_id)) %>%
    content('parsed')
  
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
  
  docker_name_list <- rbind(docker_name_list, mesos_name_list)
  
  united <- unite(docker_name_list, name, name, host_ip, sep = '/-/')
  
  docker_name_list$name <- united$name
  
  # docker_name_list %>% separate(name, c('col1', 'col2'), '/-/')
  
  return(split(docker_name_list$name,
               docker_name_list$host_ip))
  
}


load_host_tag_list <- function(agent_id) {
  # agent_id <- 27
  res <- GET('http://192.168.0.162:10100/nexcloud_hostapi/v1/agent/status',
             content_type_json(),
             add_headers('agent_id' = agent_id)) %>%
    content('parsed')
  
  host <- res$data %>%
    fromJSON(simplifyVector = F, flatten = T) %>%
    unlist()
  
  host_name_list <- data.frame('name' = as.vector(host[grep('host_name', names(host))]),
                               'host_ip' = as.vector(host[grep('host_ip', names(host))]),
                               stringsAsFactors = F)
  
  return(split(host_name_list$host_ip, host_name_list$name))
  
}


# load_host_tag_list <- function(agent_id) {
#   # agent_id <- '60'
#   connector <- connect()
#   
#   con <- connector$con
#   
#   dbname <- connector$dbname
#   
#   query <- 'SHOW series from host where agent_id =~ /%s/' %>%
#     sprintf(agent_id)
#   
#   res <- influx_query(con,
#                       dbname,
#                       query,
#                       return_xts = F)[[1]] %>%
#     as.data.frame() 
#   
#   if (!('key' %in% names(res))) return(NULL)
#   
#   res <- res %>% select(key) %>%
#     separate(key, c('col1', 'col2', 'col3', 'col4'), ',') %>% 
#     select(-col1) %>% 
#     separate(col2, c('col5', 'col6'), '=') %>% 
#     separate(col3, c('col7', 'col8'), '=') %>% 
#     separate(col4, c('col9', 'col10'), '=') #%>% 
#   # subset(col6 != 'null' & col8 != 'null' & col10 != 'null')
#   
#   # if (nrow(res) == 0) return(NULL)
#   
#   res <- split(res$col8, res$col10)
#   
#   return(res)
#   
# }


# load_task_tag_list <- function(agent_id) {
#   
#   connector <- connect()
#   
#   con <- connector$con
#   
#   dbname <- connector$dbname
#   
#   query <- 'SHOW series from task where agent_id =~ /%s/' %>%
#     sprintf(agent_id)
#   
#   res <- influx_query(con,
#                       dbname,
#                       query,
#                       return_xts = F)[[1]] %>%
#     as.data.frame()
#   
#   if (!('key' %in% names(res))) return(NULL)
#   
#   res <- res %>% select(key) %>%
#     separate(key, c('col1', 'col2', 'col3',
#                     'col4', 'col5', 'col6'), ',') %>% 
#     select(c(-col1, -col2)) %>% 
#     separate(col3, c('col7', 'col8'), '=') %>% 
#     separate(col4, c('col9', 'col10'), '=') %>% 
#     separate(col5, c('col11', 'col12'), '=') %>%
#     separate(col6, c('col13', 'col14'), '=') #%>% 
#   # subset(col8 != 'null' & col10 != 'null' & col12 != 'nul' & col14 != 'null')
#   
#   # if (nrow(res) == 0) return(NULL)
#   
#   res <- res[!duplicated(res %>% select(col12, col14)), c('col12', 'col14')]
#   
#   res <- split(res$col14, res$col12)
#   
#   return(res)
#   
# }


# load_docker_tag_list <- function(agent_id) {
#   # agent_id <- 60
#   connector <- connect()
#   
#   con <- connector$con
#   
#   dbname <- connector$dbname
#   
#   query <- 'SHOW series from docker_container where agent_id =~ /%s/' %>%
#     sprintf(agent_id)
#   
#   res <- influx_query(con,
#                       dbname,
#                       query,
#                       return_xts = F)[[1]] %>%
#     as.data.frame()
#   
#   if (!('key' %in% names(res))) return(NULL)
#   
#   res <- res %>% select(key) %>% separate(key, c('col1', 'col2', 'col3', 'col4', 
#                                                  'col5', 'col6', 'col7'), ',') %>% 
#     select(c(-col1, -col2)) %>% 
#     separate(col3, c('col13', 'col14'), '=') %>% 
#     separate(col4, c('col15', 'col16'), '=') %>% 
#     separate(col5, c('col17', 'col18'), '=') %>% 
#     separate(col6, c('col19', 'col20'), '=') %>% 
#     separate(col7, c('col21', 'col22'), '=') #%>% 
#   # subset(col14 != 'null' & col16 != 'null' & col18 != 'null' &
#   #          col20 != 'null' & col22 != 'null')
#   
#   # if (nrow(res) == 0) return(NULL)
#   
#   res <- res[!duplicated(res %>% select(col18, col20)), c('col18', 'col20')]
#   
#   res <- split(res$col20, res$col18)
#   
#   return(res)
#   
# }


# load_host_list_for_task <- function(task_name) {
#   
#   connector <- connect()
#   
#   con <- connector$con
#   
#   dbname <- connector$dbname
#   
#   query <- "show tag values
#             from task
#             with key = node_ip
#             where task =~ /%s/" %>% 
#     sprintf(task_name)
#   
#   res <- influx_query(con,
#                       dbname,
#                       query,
#                       return_xts = F,
#                       simplifyList = T)[[1]] %>% as.data.frame()
#   
#   # if (!('node_ip' %in% names(res))) return('Not Exist!')
#   
#   return(res$value)
#   
# }


# connect_MySQL <- function() {
#   
#   con <- dbConnect(MySQL(), user = 'admin', password = 'password',
#                    host = '192.168.0.168', port = 18951,
#                    dbname = 'defaultdb')
#   
#   return(con)
#   
# }


# load_event_data <- function() {
#   
#   con <- connect_MySQL()
#   
#   columns <- c('start_time', "target_system", "target_ip", "target", "metric",
#                "condition", "id")
#   
#   query <- 'SELECT * \
#             FROM nexclipper_incident \
#             order by start_time'
#   
#   raw_data <- as.data.frame(dbGetQuery(con, query))
#   
#   raw_data <- raw_data[, columns]
#   
#   #### Task parsing ####
#   parsed <- str_split(raw_data[raw_data$target_system == 'Task', 'id'],
#                       '[.]|(__)|(_\\d+)', simplify = T)
#   
#   raw_data[raw_data$target_system == 'Task', 'id'] <- parsed[, 1]
#   
#   # table(raw_data[raw_data$target_system == 'Task', 'id'])
#   
#   
#   #### Agent parsing ####
#   parsed <- str_split(raw_data[raw_data$target_system == 'Agent', 'id'],
#                       '_', simplify = T)
#   
#   id_ <- apply(parsed, 1, function(x) ifelse(x[2] == "", x[1], x[2]))
#   
#   raw_data[raw_data$target_system == 'Agent', 'id'] <- id_
#   
#   # table(raw_data[raw_data$target_system == 'Agent', 'id'])
#   
#   
#   #### Host parsing ####
#   parsed <- str_split(raw_data[raw_data$target_system == 'Host', 'id'],
#                       '_', simplify = T)
#   
#   id_ <- apply(parsed, 1, function(x) ifelse(x[2] == "", x[1], x[2]))
#   
#   raw_data[raw_data$target_system == 'Host', 'id'] <- id_
#   
#   # table(raw_data[raw_data$target_system == 'Host', 'id'])
#   
#   
#   #### Docker parsing ####
#   idx <- which(!is.na(str_extract(raw_data$id, '[:alpha:]+_[:alpha:]+')))
#   
#   raw_data$id[idx] <- str_extract(raw_data$id, '[:alpha:]+_[:alpha:]+')[idx]
#   
#   idx <- which(!is.na(str_extract(raw_data$id, '[:alpha:]+-[:alpha:]+')))
#   
#   raw_data$id[idx] <- str_extract(raw_data$id, '[:alpha:]+-[:alpha:]+')[idx]
#   
#   raw_data$id[str_detect(raw_data$id, 'influxdb')] <- 'influxdb'
#   
#   # table(raw_data[raw_data$target_system == 'Docker', 'id'])
#   
#   df <- raw_data
#   
#   names(df)[6] <- 'service_id'
#   
#   df <- gather(df, key, value, 2) %>% unite(target_system, key, value, sep = '=')
#   
#   df <- gather(df, key, value, 2) %>% unite(target_ip, key, value, sep = '=')
#   
#   df <- gather(df, key, value, 2) %>% unite(target, key, value, sep = '=')
#   
#   df <- gather(df, key, value, 2) %>% unite(metric, key, value, sep = '=')
#   
#   df <- gather(df, key, value, 2) %>% unite(service_id, key, value, sep = '=')
#   
#   df <- gather(df, key, value, 2) %>% unite(id, key, value, sep = '=')
#   
#   df <- unite(df, item, 2:7, sep = ', ') %>% as.data.table()
#   
#   return(df)
#   
# }


#### FORECAST ####

forecasting <- function(tb_, groupby, pred_period, unit, changepoint.prior.scale = 0.01) {
  # pred_period : how long predict
  
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
  
  db_info <- read_json('../Source/mysql_info.json')
  
  developed_during <- developed_during %>% as.vector()
  # browser()
  con <- dbConnect(MySQL(), 
                   user = db_info$user, 
                   password = db_info$password,
                   dbname = db_info$dbname,
                   host = db_info$host, 
                   port = db_info$port)
  
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


# will be deleted
# anomalization <- function(tb_,
#                           frequency = 'auto',
#                           method = 'stl',
#                           trend = 'auto') {
#   
#   decomposed <- time_decompose(tb_,
#                                y,
#                                method = method,
#                                frequency = frequency,
#                                trend = trend)
#   
#   anomalized <- anomalize(decomposed,
#                           remainder,
#                           method = 'gesd',
#                           alpha = 0.05,
#                           max_anoms = 0.2,
#                           verbose = F)
#   
#   recomposed <- time_recompose(anomalized)
#   
#   return(recomposed)
#   
# }

#### METRIC CORRELATION ####
horizon.panel.ggplot <- function(df,  add_text=NULL) {
  
  #df parameter should be in form of date (x), grouping, and a value (y)
  colnames(df) <- c("date","grouping","y")
  #get some decent colors from RColorBrewer
  #we will use colors on the edges so 2:4 for red and 7:9 for blue
  
  df$grouping <- as.factor(df$grouping)
  
  lv <- levels(df$grouping)
  
  if (is.null(add_text)) {
    
    add_text = ""
    
  } else {
    add_text = paste0("(", add_text, ")")  
  }
  # browser()
  labeli2 <- function(variable, value) {
    
    value <- droplevels(value)
    names_li <- as.list(paste(lv, add_text, sep = '\n'))
    names(names_li) <- lv
    
    return(names_li[value])
    
  }
  
  #use ggplot to produce an area plot
  p <- ggplot(data = df) +
    geom_line(aes(x = date, y = y), size = 0.75, color = "darkgoldenrod") + 
    facet_grid(grouping ~ ., labeller = labeli2, scales="free_y") +    #do new subplot for each group
    theme_bw() +                  #this is optional, but I prefer to default
    theme(legend.position = "none",    #remove legend
          strip.text.y = element_text(angle = 0),#rotate strip text to horizontal
          strip.background = element_rect(fill = 'grey95'),
          axis.text.y = element_blank(),#remove y axis labels
          axis.ticks.y = element_blank(), #remove tick marks
          axis.title.y = element_blank(),#remove title for the y axis
          axis.title.x = element_blank()
    )
  
  return(p)
  
}



#### METRIC CASUALITY ####

get_node_edge_df <- function(mtx, lag) {
  
  mtx <- mtx %>%
    select_if(~sum(!is.na(.)) > 0) %>%
    select_if(~ sd(., na.rm = T) != 0) %>% 
    as.matrix() %>%
    standardization()
  
  if (ncol(mtx) < 2) stop('At least, two metrics is necessary.')
  
  link_df <- data.frame('from' = NA,
                        'to' = NA,
                        'intension' = NA,
                        'p_value' = NA,
                        'max_lag' = NA,
                        stringsAsFactors = F)
  
  idx <- 1
  
  for (i in 1:ncol(mtx)) {
    
    for (j in 1:ncol(mtx)) {
      
      if (i == j) next
      
      else {
        
        x <- -diff(as.matrix(log(mtx[, i] + 1e-6)))
        
        y <- -diff(as.matrix(log(mtx[, j] + 1e-6)))
        
        if (sd(x, na.rm = T) == 0 | sd(y, na.rm = T) == 0) next
        
        p_value <- 1
        
        lag_ <- 1
        
        for (l in 1:lag) {
          
          tryCatch(
            {
              g_test <- grangertest(x, y, l)
            },
            
            error = function(cond) {
              
              cat('\n', i, j, l, '\n')
              
            }
          )
          
          if (g_test$`Pr(>F)`[2] < p_value) {
            
            p_value <- g_test$`Pr(>F)`[2]
            
            lag_ <- l
            
          }
          
        }
        
        link_df[idx, ] <- c(colnames(mtx)[i],
                            colnames(mtx)[j],
                            log10(1 / (p_value + 1e-8)),
                            p_value,
                            lag_)
        
        idx <- idx + 1
        
      }
    }
  }
  # browser()
  node_df <- data.frame('label' = unique(c(link_df$from,
                                           link_df$to)),
                        'group' = NA,
                        indegree = NA,
                        stringsAsFactors = F)
  
  node_df$id <- 1:(nrow(node_df))
  
  link_df$from <- inner_join(link_df, node_df, by = c('from' = 'label'))$id
  
  link_df$to <- inner_join(link_df, node_df, by = c('to' = 'label'))$id
  
  link_df <- as.data.frame(sapply(link_df, as.numeric))
  
  # link_df$value <- link_df$intension * 0.6
  
  return(list('node' = node_df,
              'edge' = link_df))
  
}



get_node_group_idx <- function(node_df,
                               host_list,
                               task_list,
                               docker_list) {
  
  host_idx <- c()
  
  node <- node_df$label
  
  if (!is.null(host_list))
    
    for (host_ip in host_list) {
      
      idx <- str_which(node, host_ip)
      
      host_idx <- c(host_idx, idx)
      
    }
  
  task_idx <- c()
  
  if (!is.null(task_list))
    
    for (task_name in task_list) {
      
      idx <- str_which(node, task_name)
      
      task_idx <- c(task_idx, idx)
      
    }
  
  docker_idx <- c()
  
  if (!is.null(docker_list))
    
    docker_list <- str_extract(docker_list, '[[:alpha:]\\d-_]+')
  
  for (docker_id in docker_list) {
    
    idx <- str_which(node, docker_id)
    
    docker_idx <- c(docker_idx, idx)
    
  }
  
  return(list('host' = host_idx,
              'task' = task_idx,
              'docker' = docker_idx))
  
}


get_degree <- function(node_df, edge_df) {
  
  indegree_idx <- table(edge_df$to) %>% names() %>% as.integer()
  
  indegree <- table(edge_df$to) %>% as.vector()
  
  node_df$indegree[indegree_idx] <- indegree
  
}


# will be deleted
get_network_graph <- function(node_edge_df,
                              input_host_list,
                              input_task_list,
                              input_docker_list) {
  
  node_df <- node_edge_df$node
  
  edge_df <- node_edge_df$edge
  
  idx_list <- get_node_color_idx(node_df,
                                 input_host_list,
                                 input_task_list,
                                 input_docker_list)
  # browser()
  net <- create_graph() %>% 
    add_nodes_from_table(
      table = node_df,
      label_col = node
    ) %>% 
    add_edges_from_table(
      table = edge_df,
      from_col = source,
      to_col = target,
      from_to_map = id_external
    ) %>% 
    select_nodes() %>% 
    set_node_attrs_ws(
      width, 0.5
    ) %>% 
    clear_selection()
  
  if (!is.null(idx_list$host)) {
    
    net <- net %>%
      select_nodes(
        nodes = idx_list$host
      ) %>% 
      set_node_attrs_ws(
        color, 'red'
      ) %>% 
      set_node_attrs_ws(
        penwidth, 3
      ) %>% 
      clear_selection()
    
  }
  
  
  if (!is.null(idx_list$task)) {
    
    net <- net %>% 
      select_nodes(
        nodes = idx_list$task
      ) %>% 
      set_node_attrs_ws(
        color, 'blue'
      ) %>% 
      set_node_attrs_ws(
        penwidth, 3
      ) %>% 
      clear_selection()
    
  }
  
  if (!is.null(idx_list$docker)) {
    
    net <- net %>% 
      select_nodes(
        nodes = idx_list$docker
      ) %>% 
      set_node_attrs_ws(
        color, 'green'
      ) %>% 
      set_node_attrs_ws(
        penwidth, 3
      ) %>% 
      clear_selection()
    
  }
  
  return(net)
  
}


# will be deleted
casuality <- function(mtx, lag) {
  
  link_df <- data.frame('source' = NA,
                        'target' = NA,
                        'intension' = NA,
                        'p_value' = NA,
                        'max_lag' = NA,
                        stringsAsFactors = F)
  
  # browser()
  idx <- 1
  
  for (i in 1:ncol(mtx)) {
    
    for (j in 1:ncol(mtx)) {
      
      if (i == j) {next}
      else {
        
        x <- -diff(as.matrix(log(mtx[, i] + 1e-6)))
        
        y <- -diff(as.matrix(log(mtx[, j] + 1e-6)))
        
        if (sd(x) == 0 | sd(y) == 0) next
        
        p_value <- 1
        
        lag_ <- 1
        
        for (l in 1:lag) {
          g_test <- grangertest(x, y, l)
          
          if (g_test$`Pr(>F)`[2] < p_value) {
            
            p_value <- g_test$`Pr(>F)`[2]
            
            lag_ <- l
            
          }
          
        }
        
        link_df[idx, ] <- c(colnames(mtx)[i],
                            colnames(mtx)[j],
                            log(1 / (p_value + 1e-6)),
                            p_value,
                            lag_)
        
        # result <- select_lags(x, y, lag)
        # 
        # k <- min(result$selection$aic, result$selection$bic)
        # 
        # p <- ifelse(k == 1, 0, result$pvals[k - 1])
        # 
        # intension <- log(1 / (p + 1e-6))
        # link_df[idx, ] <- c(i, j, intension, p, k)
        
        
        idx <- idx + 1
        
      }
    }
  }
  # browser()
  # node_df <- data.frame('node' = colnames(mtx),
  #                       'idx' = 0:(ncol(mtx) - 1),
  #                       'size' = 0,
  #                       stringsAsFactors = F)
  node_df <- data.frame('node' = unique(c(link_df$source,
                                          link_df$target)),
                        'size' = 0,
                        stringsAsFactors = F)
  
  node_df$idx <- 0:(nrow(node_df) - 1)
  
  link_df$source <- inner_join(link_df, node_df, by = c('source' = 'node'))$idx
  
  link_df$target <- inner_join(link_df, node_df, by = c('target' = 'node'))$idx
  
  link_df <- as.data.frame(sapply(link_df, as.numeric))
  
  for (q in node_df$idx) {
    
    node_df$size[q + 1] <- sum(link_df$source == q)
    
  }
  
  link_df$intension <- as.integer(link_df$intension + 1)
  
  res <- list('node' = node_df,
              'link' = link_df)
  
  return(res)
  
}


# will be deleted
select_lags <- function(x, y, max.lag) {
  
  y <- as.numeric(y)
  y.lag <- embed(y, max.lag + 1)[, -1, drop = FALSE]
  x.lag <- embed(x, max.lag + 1)[, -1, drop = FALSE]
  
  t <- tail(seq_along(y), nrow(y.lag))
  # browser()
  ms = lapply(1:max.lag, function(i) lm(y[t] ~ y.lag[, 1:i] +
                                          x.lag[, 1:i]))
  
  pvals <- mapply(function(i) anova(ms[[i]], ms[[i - 1]])[2, "Pr(>F)"], max.lag:2)
  
  ind <- which(pvals < 0.05)[1]
  
  ftest <- ifelse(is.na(ind), 1, max.lag - ind + 1)
  
  aic <- as.numeric(lapply(ms, AIC))
  
  bic <- as.numeric(lapply(ms, BIC))
  
  res <- structure(list(ic = cbind(aic = aic, bic = bic),
                        pvals = pvals,
                        selection = list(aic = which.min(aic),
                                         bic = which.min(bic),
                                         ftest = ftest)))
  
  return(res)
  
}

# max.lag <- 10
# x <- ChickEgg[, 1] %>% log() %>% diff()
# y <- ChickEgg[, 2] %>% log() %>% diff()
# 
# y <- as.numeric(y)
# y.lag <- embed(y, max.lag + 1)[, -1, drop = FALSE]
# x.lag <- embed(x, max.lag + 1)[, -1, drop = FALSE]
# 
# t <- tail(seq_along(y), nrow(y.lag))
# 
# ms = lapply(1:max.lag, function(i) lm(y[t] ~ y.lag[, 1:i] +
#                                         x.lag[, 1:i]))
# 
# pvals <- mapply(function(i) anova(ms[[i]], ms[[i - 1]])[2, "Pr(>F)"], max.lag:2)
# 
# ind <- which(pvals < 0.05)[1]
# 
# ftest <- ifelse(is.na(ind), 1, max.lag - ind + 1)
# 
# aic <- as.numeric(lapply(ms, AIC))
# 
# bic <- as.numeric(lapply(ms, BIC))
# 
# structure(list(ic = cbind(aic = aic, bic = bic),
#                pvals = pvals,
#                selection = list(aic = which.min(aic),
#                                 bic = which.min(bic),
#                                 ftest = ftest)))

# res <- select_lags(chick, egg, 5)
# res



#### will be deleted ####
# extract_field <- function(df) {
#     
#     # select numeric fields
#     num_var = names(df)[sapply(df, class) == "numeric"]
#     
#     # select fields such that stddev is not equal to zero
#     not0var = names(df[,num_var])[sapply(df[,num_var], var) != 0]
#     
#     # delete timestamp
#     not0var = setdiff(not0var, "timestamp")
#     
#     extracted_df <- df %>% select(c(not0var, 'time'))
#     
#     return(extracted_df)
# }

# will be deleted
# load_metric_from_cluster <- function(period, groupby, metric_list) {
#   
#   # metric_list <- list()
#   # metric_list$cluster <- c("cpu_used", "mem_used_percent") 
#   # metric_list$cluster <- NULL
#   # 
#   # period = 5
#   # groupby = "1h"
#   
#   connector <- connect()
#   con <- connector$connector
#   dbname <- connector$dbname
#   
#   period <- paste0(period, 'd')
#   
#   
#   # browser()
#   
#   
#   #### Cluster ####
#   query <- "select mean(*)
#   from %s
#   where time > now() - %s
#   group by time(%s)
#   fill(none)"
#   
#   query <- sprintf(query, 
#                    'cluster', 
#                    period,
#                    groupby)
#   
#   res_cluster <- influx_query(con,
#                               db = dbname,
#                               query = query,
#                               simplifyList = T,
#                               return_xts = F)[[1]]
#   
#   names(res_cluster) <- gsub('mean_', '', names(res_cluster))
#   
#   res_cluster <- res_cluster %>% select(c('time', metric_list))
#   
#   ts <- seq.POSIXt(min(res_cluster$time),
#                    max(res_cluster$time),
#                    by = posixt_helper_func(str_sub(groupby, -1)))
#   
#   df <- tibble(time = ts)
#   
#   res_cluster <- full_join(df, res_cluster)
#   
#   names(res_cluster) <- paste0(names(res_cluster), '_cluster')
#   
#   names(res_cluster) <- gsub('time_cluster', 'time', names(res_cluster))
#   print('Cluster OK!')
#   
#   return(res_cluster)
#   
# }

# will be deleted
# load_single_metric <- function(measurement, host, metric, period, groupby,
#                                unit, node_ip, agent_id) {
#   
#   single_metric <- switch(measurement,
#                           'host' = load_metric_from_host(period, groupby,
#                                                          c(host), c(metric),
#                                                          agent_id,
#                                                          single = T, unit),
#                           
#                           'task' = load_metric_from_task_for_single(period, groupby,
#                                                                     host, metric,
#                                                                     agent_id, unit,
#                                                                     node_ip),
#                           
#                           'docker' = load_metric_from_docker(period, groupby,
#                                                              c(host), c(metric),
#                                                              agent_id,
#                                                              single = T, unit))
#   browser()
#   single_metric$y <- na.approx(single_metric$y)
#   
#   return(single_metric)
#   
# }

# will be deleted
# load_metric_from_task_for_single <- function(period, groupby,
#                                              task, metric,
#                                              agent_id, unit,
#                                              node_ip) {
#   # period <- 3;groupby <- '1h';task <- 'agent.nexcloud'
#   # metric <- 'cpu_used_percent';agent_id <- 27;unit <- '0';
#   # node_ip <- ''
#   
#   con <- connect()
#   
#   connector <- con$connector
#   
#   dbname <- con$dbname
#   
#   if (unit == '0') {
#     
#     period <- paste0(period, 'd')
#     
#   } else {
#     
#     period <- paste0(period, 'h')
#     
#   }
#   
#   query <- "select mean(%s) as metric
#             from task
#             where time > now() - %s and task = '%s' and agent_id = '%s' %s
#             group by time(%s), task, agent_id%s
#             fill(none)
#             order by time desc"
#   
#   by_node <- ''
#   
#   if (node_ip != "") {
#     
#     node_ip <- sprintf("and node_ip = '%s'", node_ip)
#     by_node <- ', node_ip'
#     
#   }
#   # browser()
#   query <- query %>%
#     sprintf(metric,
#             period, task, agent_id, node_ip,
#             groupby, by_node)
#   cat(query)
#   raw_data <- influx_query(connector,
#                            db = dbname,
#                            query = query,
#                            simplifyList = T,
#                            return_xts = F)[[1]]
#   
#   if (!('metric' %in% names(raw_data)))
#     
#     return(default_time_seqeunce(period, groupby))
#   
#   unit <- str_extract(groupby, '[:alpha:]') %>% posixt_helper_func()
#   
#   by <- str_extract(groupby, '\\d+') %>% paste(unit)
#   
#   ts <- seq.POSIXt(min(raw_data$time),
#                    max(raw_data$time),
#                    by = by)
#   
#   df <- tibble(ds = ts)
#   
#   tb <- select(raw_data, c(time, metric)) %>% 
#     full_join(df, by = c('time' = 'ds'))
#   
#   names(tb) <- c("ds", "y")
#   
#   return(tb)
#   
# }
