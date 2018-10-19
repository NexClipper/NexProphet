source('00.R')
source('01.R')

#### DB CONNECTION ####
connect <- function() {
  
  # con <- influx_connection(host = 'influxdb.marathon.l4lb.thisdcos.directory',
  #                          port = 8086)
  con <- influx_connection(host = '192.168.0.162',
                           port = 10091)
  
  dbname <- 'nexclipper'
  
  conn <- list(connector = con, dbname = dbname)
  
  return(conn)
  
}
#----

#### DB WRITE ####
write_result_to_influx <- function(dt_, key_) {
  
  con <- connect()
  
  connector <- con$connector
  
  dbname <- 'nexclipper_ai'
  
  influx_write(dt_, connector, dbname, key_,
               time_col = 'ds')
  
}
#----


#### APP FUNCTIONS ####
get_corr_mtx <- function(agent_id, period, groupby, start_time, key_, ...) {
  
  arg <- list(...)
  
  # groupby_unit <- str_extract(groupby, '[:alpha:]') %>% 
  #   switch('h' = 'hour',
  #          'm' = 'min',
  #          's' = 'sec')
  # 
  # by <- str_extract(groupby, '\\d+') %>% paste(groupby_unit)
  # 
  # length.out <- 24 * 60 * 60 / switch(groupby_unit,
  #                                     'hour' = 60 * 60,
  #                                     'min' = 60,
  #                                     'sec' = 1) %>% 
  #   as.integer() * as.integer(str_extract(period, '\\d+'))
  # 
  # 
  # ts <- seq.POSIXt(from = Sys.time(),
  #                  length.out = length.out,
  #                  by = by)
  # 
  # df <- tibble(time = ts)
  
  whole_data <- NULL
  
  # docker container
  if (!is.null(arg$docker_container)) {
    
    docker_container <- load_docker_container(agent_id, arg$docker_container, period, groupby, start_time)
    
    if (is.null(whole_data))
      
      whole_data <- docker_container
  }
  
  # docker network
  if (!is.null(arg$docker_network)) {
    
    docker_network <- load_docker_network(agent_id, arg$docker_network, period, groupby, start_time)
    
  }
  
  if (is.null(whole_data)) {
    
    whole_data <- docker_network
    
  } else {
    
    whole_data <- docker_network[whole_data]
    
  }
  
  # host
  if (!is.null(arg$host)) {
    
    host <- load_host(agent_id, arg$host, period, groupby, start_time)
    
  }
  
  if (is.null(whole_data)) {
    
    whole_data <- host
    
  } else {
    
    whole_data <- host[whole_data]
    
  }
  
  # host disk
  if (!is.null(arg$host_disk)) {
    
    host_disk <- load_host_disk(agent_id, arg$host_disk, period, groupby, start_time)
    
  }
  
  if (is.null(whole_data)) {
    
    whole_data <- host_disk
    
  } else {
    
    whole_data <- host_disk[whole_data]
    
  }
  
  # host network
  if (!is.null(arg$host_net)) {
    
    host_net <- load_host_net(agent_id, arg$host_net, period, groupby, start_time)
    
  }
  
  if (is.null(whole_data)) {
    
    whole_data <- host_net
    
  } else {
    
    whole_data <- host_net[whole_data]
    
  }
  
  # host process
  if (!is.null(arg$host_process)) {
    
    host_process <- load_host_process(agent_id, arg$host_process, period, groupby, start_time)
    
  }
  
  if (is.null(whole_data)) {
    
    whole_data <- host_process
    
  } else {
    
    whole_data <- host_process[whole_data]
    
  }
  
  dt_processed <- pre_processing(whole_data)
  
  cor_mtx <- cor(dt_processed) %>% 
    as.data.table()
  
  time_idx <- seq.POSIXt(from = Sys.time(),
                        by = '-1 hour',
                        length.out = nrow(cor_mtx)) %>% 
    sort()
  
  cor_mtx[, ds := time_idx]
  
  write_result_to_influx(cor_mtx, key_)
  
}


load_docker_container <- function(agent_id, request, period, groupby, start_time) {
  #agent_id=27;host_ip=c('192.168.0.165', '192.168.0.164');metric=c('cpu_used_percent', 'mem_used_percent');period='6d';groupby='1h';start_time='2018-10-18 14:30:00';dname=c('/Nexclipper-Agent', 'kafka-manager.fbed1a44-d187-11e8-b067-aae0d7e58657')
  #request <- host_ip, metric, dname
  con <- connect()
  
  connector <- con$connector
  
  dbname <- con$dbname
  
  host_ip <- request$host_ip
  
  metric <- request$metric
  
  dname <- request$dname
  
  metric_for_query <- paste('mean(%s) as', metric) %>% 
    sprintf(metric) %>%
    paste(collapse = ', ')
  
  host_ip_for_query <- "host_ip = '%s'" %>% 
    sprintf(host_ip) %>%
    paste(collapse = ' or ')
  
  dname_for_query <- "task_id = '%s'" %>% 
    sprintf(dname) %>%
    paste(collapse = ' or ')
  
  query <- "select %s
            from docker_container
            where agent_id = '%s' and
                  time > '%s' - %s and
                  (%s) and
                  (%s)
            group by time(%s), host_ip, task_id" %>% 
    sprintf(metric_for_query,
            agent_id,
            start_time, period,
            host_ip_for_query,
            dname_for_query,
            groupby)
  
  cat('\n', query, '\n\n')
  
  res <- influx_query(connector,
                      dbname,
                      query, return_xts = F,
                      simplifyList = T)[[1]] %>% 
    as.data.table()
  
  if (!('time' %in% names(res)))
    
    return(NULL)
  
  res %>% 
    .[, -1:-3] %>% 
    setnames('time', 'ds') %>% 
    .[, ds := with_tz(ds, 'Asia/Seoul')] %>% 
    setkey(ds) %>%
    dcast(ds ~ host_ip + task_id,
          value.var = metric,
          sep = '__') %>%
    return()
  
}


load_docker_network <- function(agent_id, request, period, groupby, start_time) {
  #agent_id=27;host_ip=c('192.168.0.165', '192.168.0.166');metric=c('rx_bytes', 'tx_bytes');period='6d';groupby='1h';dname=c('nexcloud_nexclipperui.84abc80e-d127-11e8-b067-aae0d7e58657', 'nexcloud_fullfillment.020b05f9-d122-11e8-b067-aae0d7e58657');interface=c('eth0')
  #request <- list(host_ip, metric, dname, interface)
  con <- connect()
  
  connector <- con$connector
  
  dbname <- con$dbname
  
  host_ip <- request$host_ip
  
  metric <- request$metric
  
  dname <- request$dname
  
  interface <- request$interface
  
  metric_for_query <- paste('mean(%s) as', metric) %>% 
    sprintf(metric) %>%
    paste(collapse = ', ')
  
  host_ip_for_query <- "host_ip = '%s'" %>% 
    sprintf(host_ip) %>%
    paste(collapse = ' or ')
  
  dname_for_query <- "task_id = '%s'" %>% 
    sprintf(dname) %>%
    paste(collapse = ' or ')
  
  interface_for_query <- "interface = '%s'" %>% 
    sprintf(interface) %>%
    paste(collapse = ' or ')
  
  query <- "select %s
            from docker_network
            where agent_id = '%s' and
                  time > '%s' - %s and
                  (%s) and 
                  (%s) and
                  (%s)
            group by time(%s), host_ip, task_id, interface" %>% 
    sprintf(metric_for_query,
            agent_id,
            start_time, period,
            host_ip_for_query,
            dname_for_query,
            interface_for_query,
            groupby)
  
  cat('\n', query, '\n\n')
  
  res <- influx_query(connector,
                      dbname,
                      query, return_xts = F,
                      simplifyList = T)[[1]] %>% 
    as.data.table()
  
  if (!('time' %in% names(res)))
    
    return(NULL)
  
  res %>% 
    .[, -1:-3] %>% 
    setnames('time', 'ds') %>% 
    .[, ds := with_tz(ds, 'Asia/Seoul')] %>% 
    setkey(ds) %>%
    dcast(ds ~ host_ip + task_id + interface,
          value.var = metric,
          sep = '__') %>%
    return()
  
}


load_host <- function(agent_id, request, period, groupby, start_time) {
  #agent_id=27;host_ip=c('192.168.0.165', '192.168.0.166');metric=c('cpu_used_percent', 'mem_used_percent');period='6d';groupby='1h'
  #request <- host_ip, metric
  con <- connect()
  
  connector <- con$connector
  
  dbname <- con$dbname
  
  host_ip <- request$host_ip
  
  metric <- request$metric
  
  metric_for_query <- paste('mean(%s) as', metric) %>% 
    sprintf(metric) %>%
    paste(collapse = ', ')
  
  host_ip_for_query <- "host_ip = '%s'" %>% 
    sprintf(host_ip) %>%
    paste(collapse = ' or ')
  
  query <- "select %s
            from host
            where agent_id = '%s' and
                  time > '%s' - %s and
                  (%s)
            group by time(%s), host_ip" %>% 
    sprintf(metric_for_query,
            agent_id,
            start_time, period,
            host_ip_for_query,
            groupby)
  
  cat('\n', query, '\n\n')
  
  res <- influx_query(connector,
                      dbname,
                      query, return_xts = F,
                      simplifyList = T)[[1]] %>% 
    as.data.table()
  
  if (!('time' %in% names(res)))
    
    return(NULL)
  
  res %>% 
    .[, -1:-3] %>% 
    setnames('time', 'ds') %>% 
    .[, ds := with_tz(ds, 'Asia/Seoul')] %>% 
    setkey(ds) %>%
    dcast(ds ~ host_ip,
          value.var = metric,
          sep = '__') %>%
    return()
  
}


load_host_disk <- function(agent_id, request, period, groupby, start_time) {
  #agent_id=27;host_ip=c('192.168.0.165', '192.168.0.166');metric=c('used_percent');period='6d';groupby='1h';mount=c('/', '/var')
  #request <- host_ip, metric, mount
  con <- connect()
  
  connector <- con$connector
  
  dbname <- con$dbname
  
  host_ip <- request$host_ip
  
  metric <- request$metric
  
  mount <- request$mount
  
  metric_for_query <- paste('mean(%s) as', metric) %>% 
    sprintf(metric) %>%
    paste(collapse = ', ')
  
  host_ip_for_query <- "host_ip = '%s'" %>% 
    sprintf(host_ip) %>%
    paste(collapse = ' or ')
  
  mount_for_query <- "mount_name = '%s'" %>% 
    sprintf(mount) %>%
    paste(collapse = ' or ')
  
  query <- "select %s
            from host_disk
            where agent_id = '%s' and
                  time > '%s' - %s and
                  (%s) and
                  (%s)
            group by time(%s), host_ip, mount_name" %>% 
    sprintf(metric_for_query,
            agent_id,
            start_time, period,
            host_ip_for_query,
            mount_for_query,
            groupby)
  
  cat('\n', query, '\n\n')
  
  res <- influx_query(connector,
                      dbname,
                      query, return_xts = F,
                      simplifyList = T)[[1]] %>% 
    as.data.table()
  
  if (!('time' %in% names(res)))
    
    return(NULL)
  
  res %>% 
    .[, -1:-3] %>% 
    setnames('time', 'ds') %>% 
    .[, ds := with_tz(ds, 'Asia/Seoul')] %>% 
    setkey(ds) %>%
    dcast(ds ~ host_ip + mount_name,
          value.var = metric,
          sep = '__') %>%
    return()
  
}


load_host_net <- function(agent_id, request, period, groupby, start_time) {
  #agent_id=27;host_ip=c('192.168.0.165', '192.168.0.166');metric=c('rxbyte', 'txbyte');period='6d';groupby='1h';interface=c('eth0', 'docker0')
  #request <- host_ip, metric, interface
  con <- connect()
  
  connector <- con$connector
  
  dbname <- con$dbname
  
  host_ip <- request$host_ip
  
  metric <- request$metric
  
  interface <- request$interface
  
  metric_for_query <- paste('mean(%s) as', metric) %>% 
    sprintf(metric) %>%
    paste(collapse = ', ')
  
  host_ip_for_query <- "host_ip = '%s'" %>% 
    sprintf(host_ip) %>%
    paste(collapse = ' or ')
  
  interface_for_query <- "interface = '%s'" %>% 
    sprintf(interface) %>%
    paste(collapse = ' or ')
  
  query <- "select %s
            from host_net
            where agent_id = '%s' and
                  time > '%s' - %s and
                  (%s) and
                  (%s)
            group by time(%s), host_ip, interface" %>% 
    sprintf(metric_for_query,
            agent_id,
            start_time, period,
            host_ip_for_query,
            interface_for_query,
            groupby)
  
  cat('\n', query, '\n\n')
  
  res <- influx_query(connector,
                      dbname,
                      query, return_xts = F,
                      simplifyList = T)[[1]] %>% 
    as.data.table()
  
  if (!('time' %in% names(res)))
    
    return(NULL)
  
  res %>% 
    .[, -1:-3] %>% 
    setnames('time', 'ds') %>% 
    .[, ds := with_tz(ds, 'Asia/Seoul')] %>% 
    setkey(ds) %>%
    dcast(ds ~ host_ip + interface,
          value.var = metric,
          sep = '__') %>% 
    return()
  
}


load_host_process <- function(agent_id, request, period, groupby, start_time) {
  #agent_id=27;host_ip=c('192.168.0.165', '192.168.0.166');metric=c('cpu_used_percent', 'mem_used_percent');period='6d';groupby='1h';pname=c('mysqld', 'dockerd')
  #request <- host_ip, metric, pname
  con <- connect()
  
  connector <- con$connector
  
  dbname <- con$dbname
  
  host_ip <- request$host_ip
  
  metric <- request$metric
  
  pname <- request$pname
  
  metric_for_query <- paste('mean(%s) as', metric) %>% 
    sprintf(metric) %>%
    paste(collapse = ', ')
  
  host_ip_for_query <- "host_ip = '%s'" %>% 
    sprintf(host_ip) %>%
    paste(collapse = ' or ')
  
  pname_for_query <- paste('"name"', "'%s'", sep = ' = ') %>% 
    sprintf(pname) %>% 
    paste(collapse = ' or ')
  
  query <- "select %s
            from host_process
            where agent_id = '%s' and
                  time > '%s' - %s and
                  (%s) and 
                  (%s)
            group by time(%s), host_ip, \"name\"" %>% 
    sprintf(metric_for_query,
            agent_id,
            start_time, period,
            host_ip_for_query,
            pname_for_query,
            groupby)
  
  cat('\n', query, '\n\n')
  
  res <- influx_query(connector,
                      dbname,
                      query, return_xts = F,
                      simplifyList = T)[[1]] %>% 
    as.data.table()
  
  if (!('time' %in% names(res)))
    
    return(NULL)
  
  res %>% 
    .[, -1:-3] %>% 
    setnames('time', 'ds') %>% 
    .[, ds := with_tz(ds, 'Asia/Seoul')] %>% 
    setkey(ds) %>%
    dcast(ds ~ host_ip + name,
          value.var = metric,
          sep = '__') %>%
    return()
  
}

pre_processing <- function(dt_, threshold = 0.3) {
  
  dt_[, .SD, .SDcols = dt_[, lapply(.SD, function(x) sum(is.na(x)) <= as.integer(length(x) * threshold))] %>% 
        unlist() %>% 
        which()] %>% 
    .[, lapply(.SD, function(x) na.approx(x, na.rm = F) %>% 
                 na.fill('extend')), .SDcols = -1] %>% 
    .[, lapply(.SD, function(x) (x - min(x)) / (max(x) - min(x) + 1e-6))] %>% 
    return()
  
}
#----
dt <- load_multiple_metric(27, '7d', '1h', '2018-10-18 17:00:00', 
                           'docker_container' = list('host_ip' = c('192.168.0.165', '192.168.0.166'),
                                                     'metric' = c('cpu_used_percent', 'mem_used_percent'),
                                                     dname = c('/Nexclipper-Agent',
                                                             'kafka-manager.fbed1a44-d187-11e8-b067-aae0d7e58657')),
                           'docker_network' = list('host_ip' = c('192.168.0.165', '192.168.0.166'),
                                                   'metric' = c('rx_bytes', 'tx_bytes'),
                                                   'dname' = c('nexcloud_nexclipperui.84abc80e-d127-11e8-b067-aae0d7e58657',
                                                               'nexcloud_fullfillment.020b05f9-d122-11e8-b067-aae0d7e58657'),
                                                   'interface' = c('eth0')),
                           'host' = list('host_ip' = c('192.168.0.165', '192.168.0.166'),
                                         'metric' = c('cpu_used_percent', 'mem_used_percent')),
                           'host_disk' = list('host_ip' = c('192.168.0.165', '192.168.0.166'),
                                              'metric' = c('used_percent'),
                                              'mount' = c('/', '/var')),
                           'host_net' = list('host_ip' = c('192.168.0.165', '192.168.0.166'),
                                             'metric' = c('txbyte', 'rxbyte'),
                                             'interface' = c('eth0', 'docker0')),
                           'host_process' = list('host_ip' = c('192.168.0.165', '192.168.0.166'),
                                                 'metric' = c('cpu_used_percent', 'mem_used_percent'),
                                                 'pname' = c('mysqld', 'dockerd')))



#### ARGUMENT PARSING ####
option_list <- list(
  make_option(c("-id", "--agent_id"), action = "store", type = 'character'),
  make_option(c("-m", "--measurement"), action = "store", type = 'character'),
  make_option(c("-ip", "--host_ip"), action = "store", type = 'character'),
  make_option(c("-mtc", "--metric"), action = "store", type = 'character'),
  make_option(c("-p", "--period"), action = "store", type = 'character'),
  make_option(c("-pp", "--p_period"), action = "store", type = 'character'),
  make_option(c("-g", "--groupby"), action = "store", type = 'character'),
  make_option(c("-t", "--start_time"), action = "store", type = 'character'),
  make_option(c("-k", "--key"), action = "store", type = 'character'),
  make_option(c("-mnt", "--mount"), action = "store", type = 'character'),
  make_option(c("-hIF", "--hostIF"), action = "store", type = 'character'),
  make_option(c("-pn", "--pname"), action = "store", type = 'character'),
  make_option(c("-dn", "--dname"), action = "store", type = 'character'),
  make_option(c("-dIF", "--dockerIF"), action = "store", type = 'character')
)

opt = parse_args(OptionParser(option_list = option_list))

print('#####################')
opt %>% unlist() %>% print()
print('#####################')