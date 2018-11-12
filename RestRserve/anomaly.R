source('00.R')
source('01.R')
source('02.R')
source('app_func.R')

#### ENVIRONMENT VARIABLE ####
ENV <- Sys.getenv(c('INFLUX_HOST', 'INFLUX_PORT', 'INFLUX_DB',
                           'MYSQL_USER', 'MYSQL_PW', 'MYSQL_DB',
                           'MYSQL_HOST', 'MYSQL_PORT'))

INFLUX_HOST <- ENV['INFLUX_HOST']

INFLUX_PORT <- ENV['INFLUX_PORT'] %>% as.integer()

INFLUX_DB <- ENV['INFLUX_DB']

MYSQL_USER <- ENV['MYSQL_USER']

MYSQL_PW <- ENV['MYSQL_PW']

MYSQL_DB <- ENV['MYSQL_DB']

MYSQL_HOST <- ENV['MYSQL_HOST']

MYSQL_PORT <- ENV['MYSQL_PORT'] %>% as.integer()
#----

#### DB CONNECTION ####
CONN <- influx_connection(host = INFLUX_HOST,
                          port = INFLUX_PORT)

#----

#### DB WRITE ####
write_result_to_influx <- function(dt_) {
  
  influx_write(dt_, CONN, 'nexclipper_ai', 'anomaly',
               time_col = 'ds', tag_cols = c('key', 'agent_id', 'anomaly'))
  
}
#----

#### APP FUNCTIONS ####
load_single_metric <- function(agent_id, measurement, host_ip, metric,
                               period, groupby, start_time, request_body) {
  
  arg <- request_body %>% fromJSON(simplifyDataFrame = F)
  
  agent_id <- agent_id %>% as.integer()
  
  dt_ <- switch(measurement,
                'host' = load_host(agent_id, host_ip, metric, period, groupby, start_time),
                'host_disk' = load_host_disk(agent_id, host_ip, metric, period, groupby, start_time,
                                             arg$mount),
                'host_net' = load_host_net(agent_id, host_ip, metric, period, groupby, start_time,
                                           arg$hostIF),
                'node' = load_node(agent_id, host_ip, metric, period, groupby, start_time),
                'task' = load_task(agent_id, host_ip, metric, period, groupby, start_time,
                                   arg$E_ID),
                'network' = load_network(agent_id, host_ip, metric, period, groupby, start_time,
                                         arg$IF),
                'k8s_pod' = load_k8s_pod(agent_id, host_ip, metric, period, groupby, start_time,
                                         arg$namespace, arg$pod),
                'k8s_node' = load_k8s_node(agent_id, host_ip, metric, period, groupby, start_time),
                # 'host_process' = load_host_process(agent_id, host_ip, metric, period, groupby, start_time,
                #                                    arg$pname),
                'docker_container' = load_docker_container(agent_id, host_ip, metric, period, groupby, start_time,
                                                           arg$dname),
                'docker_network' = load_docker_network(agent_id, host_ip, metric, period, groupby, start_time,
                                                       arg$dname, arg$dockerIF))
  
  if (is.null(dt_)) {
    
    return(NULL)
    
  } else {
    
    dt_[, y := na.approx(y)] %>% return()
    
  }
  
}


load_docker_container <- function(agent_id, host_ip, metric, period, groupby, start_time, dname) {
  #agent_id=27;host_ip='192.168.0.165';metric='cpu_used_percent';period='6d';groupby='1h';start_time='2018-10-04 10:31:05';dname='/Nexclipper-Agent'
  query <- "select mean(%s) as y
            from docker_container
            where agent_id = '%s' and
                  time > '%s' - %s and
                  host_ip = '%s' and 
                  task_id = '%s'
            group by time(%s)" %>% 
    sprintf(metric,
            agent_id,
            start_time, period,
            host_ip,
            dname,
            groupby)
  
  cat('\n', query, '\n\n')
  
  res <- influx_query(CONN,
                      INFLUX_DB,
                      query,
                      return_xts = F,
                      simplifyList = T)[[1]] %>% 
    as.data.table()
  
  if (!('time' %in% names(res)))
    
    return(NULL)
  
  res %>% 
    .[, -1:-4] %>% 
    setnames('time', 'ds') %>% 
    setkey(ds) %>% return()
  
}


load_docker_network <- function(agent_id, host_ip, metric, period, groupby, start_time, dname, interface) {
  #agent_id=27;host_ip='192.168.0.165';metric='rx_bytes';period=6;groupby='1h';unit='0';dname='nexcloud_nexclipperui.081024c1-c2f2-11e8-8aa1-aae0d7e58657';interface='eth0'
  query <- "select mean(%s) as y
            from docker_network
            where agent_id = '%s' and
                  time > '%s' - %s and
                  host_ip = '%s' and 
                  task_id = '%s' and
                  interface = '%s'
            group by time(%s)" %>% 
    sprintf(metric,
            agent_id,
            start_time, period,
            host_ip,
            dname,
            interface,
            groupby)
  
  cat('\n', query, '\n\n')
  
  res <- influx_query(CONN,
                      INFLUX_DB,
                      query,
                      return_xts = F,
                      simplifyList = T)[[1]] %>% 
    as.data.table()
  
  if (!('time' %in% names(res)))
    
    return(NULL)
  
  res %>% 
    .[, -1:-4] %>% 
    setnames('time', 'ds') %>% 
    setkey(ds) %>% return()
  
}


load_host <- function(agent_id, host_ip, metric, period, groupby, start_time) {
  #agent_id=27;host_ip='192.168.0.165';metric='cpu_used_percent';period=6;groupby='1h';unit='0'
  query <- "select mean(%s) as y
            from host
            where agent_id = '%s' and
                  time > '%s' - %s and
                  host_ip = '%s'
            group by time(%s)" %>% 
    sprintf(metric,
            agent_id,
            start_time,period,
            host_ip,
            groupby)
  
  cat('\n', query, '\n\n')
  
  res <- influx_query(CONN,
                      INFLUX_DB,
                      query,
                      return_xts = F,
                      simplifyList = T)[[1]] %>% 
    as.data.table()
  
  if (!('time' %in% names(res)))
    
    return(NULL)
  
  res %>% 
    .[, -1:-4] %>% 
    setnames('time', 'ds') %>% 
    setkey(ds) %>% return()
  
}


load_host_disk <- function(agent_id, host_ip, metric, period, groupby, start_time, mount) {
  #agent_id=27;host_ip='192.168.0.165';metric='used_percent';period=6;groupby='1h';unit='0';mount='/'
  query <- "select mean(%s) as y
            from host_disk
            where agent_id = '%s' and
                  time > '%s' - %s and
                  host_ip = '%s' and
                  mount_name = '%s'
            group by time(%s)" %>% 
    sprintf(metric,
            agent_id,
            start_time, period,
            host_ip,
            mount,
            groupby)
  
  cat('\n', query, '\n\n')
  
  res <- influx_query(CONN,
                      INFLUX_DB,
                      query,
                      return_xts = F,
                      simplifyList = T)[[1]] %>% 
    as.data.table()
  
  if (!('time' %in% names(res)))
    
    return(NULL)
  
  res %>% 
    .[, -1:-4] %>% 
    setnames('time', 'ds') %>% 
    setkey(ds) %>% return()
  
}


load_host_net <- function(agent_id, host_ip, metric, period, groupby, start_time, interface) {
  #agent_id=27;host_ip='192.168.0.165';metric='rxbyte';period=6;groupby='1h';unit='0';interface='veth99a298c8'
  query <- "select mean(%s) as y
            from host_net
            where agent_id = '%s' and
                  time > '%s' - %s and
                  host_ip = '%s' and
                  interface = '%s'
            group by time(%s)" %>% 
    sprintf(metric,
            agent_id,
            start_time, period,
            host_ip,
            interface,
            groupby)
  
  cat('\n', query, '\n\n')
  
  res <- influx_query(CONN,
                      INFLUX_DB,
                      query,
                      return_xts = F,
                      simplifyList = T)[[1]] %>% 
    as.data.table()
  
  if (!('time' %in% names(res)))
    
    return(NULL)
  
  res %>% 
    .[, -1:-4] %>% 
    setnames('time', 'ds') %>% 
    setkey(ds) %>% return()
  
}


load_node <- function(agent_id, host_ip, metric, period, groupby, start_time) {
  #agent_id=27;host_ip='192.168.0.165';metric='cpu_used_percent';period='7d';groupby='1h'
  query <- "select mean(%s) as y
  from node
  where agent_id = '%s' and
  time > '%s' - %s and
  node_ip = '%s'
  group by time(%s)" %>% 
    sprintf(metric,
            agent_id,
            start_time, period,
            host_ip,
            groupby)
  
  cat('\n', query, '\n\n')
  
  res <- influx_query(CONN,
                      INFLUX_DB,
                      query,
                      return_xts = F,
                      simplifyList = T)[[1]] %>% 
    as.data.table()
  
  if (!('time' %in% names(res)))
    
    return(NULL)
  
  res %>% 
    .[, -1:-4] %>% 
    setnames('time', 'ds') %>% 
    setkey(ds) %>% return()
  
}


load_task <- function(agent_id, host_ip, metric, period, groupby, start_time, executor_id) {
  #agent_id=27;host_ip='192.168.0.168';executor_id='influxdb.28755d4d-e17a-11e8-ae5d-8ac1dc5733cc';metric='cpu_used_percent';period='7d';groupby='1h'
  query <- "select mean(%s) as y
            from task
            where agent_id = '%s' and
                  time > '%s' - %s and
                  node_ip = '%s' and
                  executor_id = '%s'
            group by time(%s)" %>% 
    sprintf(metric,
            agent_id,
            start_time, period,
            host_ip,
            executor_id,
            groupby)
  
  cat('\n', query, '\n\n')
  
  res <- influx_query(CONN,
                      INFLUX_DB,
                      query,
                      return_xts = F,
                      simplifyList = T)[[1]] %>% 
    as.data.table()
  
  if (!('time' %in% names(res)))
    
    return(NULL)
  
  res %>% 
    .[, -1:-4] %>% 
    setnames('time', 'ds') %>% 
    setkey(ds) %>% return()
  
}


load_network <- function(agent_id, host_ip, metric, period, groupby, start_time, interface) {
  #agent_id=27;host_ip='192.168.0.165';metric='in_bytes';period='7d';groupby='1h';interface='null'
  query <- "select mean(%s) as y
            from network
            where agent_id = '%s' and
                  time > '%s' - %s and
                  node_ip = '%s' and
                  interface = '%s'
            group by time(%s)" %>% 
    sprintf(metric,
            agent_id,
            start_time, period,
            host_ip,
            interface,
            groupby)
  
  cat('\n', query, '\n\n')
  
  res <- influx_query(CONN,
                      INFLUX_DB,
                      query,
                      return_xts = F,
                      simplifyList = T)[[1]] %>% 
    as.data.table()
  
  if (!('time' %in% names(res)))
    
    return(NULL)
  
  res %>% 
    .[, -1:-4] %>% 
    setnames('time', 'ds') %>% 
    setkey(ds) %>% return()
  
}


load_k8s_pod <- function(agent_id, node_ip, metric, period, groupby, start_time, namespace, pod) {
  #agent_id=5;node_ip='192.168.1.4';metric='cpu_used_percent';period='7d';groupby='1h';pod='kube-controller-manager-kubemaster';namespace='kube-system';
  query <- "select mean(%s) as y
            from k8s_pod
            where agent_id = '%s' and
                  time > '%s' - %s and
                  node_ip = '%s' and
                  namespace = '%s' and
                  pod = '%s'
            group by time(%s)" %>% 
    sprintf(metric,
            agent_id,
            start_time, period,
            node_ip,
            namespace,
            pod,
            groupby)
  
  cat('\n', query, '\n\n')
  
  res <- influx_query(CONN,
                      INFLUX_DB,
                      query,
                      return_xts = F,
                      simplifyList = T)[[1]] %>% 
    as.data.table()
  
  if (!('time' %in% names(res)))
    
    return(NULL)
  
  res %>% 
    .[, -1:-4] %>% 
    setnames('time', 'ds') %>% 
    setkey(ds) %>% return()
  
}


load_k8s_node <- function(agent_id, node_ip, metric, period, groupby, start_time) {
  #agent_id=5;node_ip='192.168.1.4';metric='cpu_used_percent';period='7d';groupby='1h';pod='kube-controller-manager-kubemaster';namespace='kube-system';
  query <- "select mean(%s) as y
            from k8s_node
            where agent_id = '%s' and
                  time > '%s' - %s and
                  node_ip = '%s'
            group by time(%s)" %>% 
    sprintf(metric,
            agent_id,
            start_time, period,
            node_ip,
            groupby)
  
  cat('\n', query, '\n\n')
  
  res <- influx_query(CONN,
                      INFLUX_DB,
                      query, return_xts = F,
                      simplifyList = T)[[1]] %>% 
    as.data.table()
  
  if (!('time' %in% names(res)))
    
    return(NULL)
  
  res %>% 
    .[, -1:-4] %>% 
    setnames('time', 'ds') %>% 
    setkey(ds) %>% return()
  
}
# load_host_process <- function(agent_id, host_ip, metric, period, groupby, start_time, pname) {
#   #agent_id=27;host_ip='192.168.0.165';metric='cpu_used_percent';period='6d';groupby='1h';pname='mysqld'
#   con <- connect()
#   
#   connector <- con$connector
#   
#   dbname <- con$dbname
#   
#   pname <- paste0('"name" = ', "'%s'") %>% 
#     sprintf(pname)
#   
#   query <- "select mean(%s) as y
#             from host_process
#             where agent_id = '%s' and
#                   time > '%s' - %s and
#                   host_ip = '%s' and 
#                   %s
#             group by time(%s)" %>% 
#     sprintf(metric,
#             agent_id,
#             start_time, period,
#             host_ip,
#             pname,
#             groupby)
#   
#   cat('\n', query, '\n\n')
#   
#   res <- influx_query(connector,
#                       dbname,
#                       query, return_xts = F,
#                       simplifyList = T)[[1]] %>% 
#     as.data.table()
#   
#   if (!('time' %in% names(res)))
#     
#     return(NULL)
#   
#   res %>% 
#     .[, -1:-4] %>% 
#     setnames('time', 'ds') %>% 
#     .[, ds := with_tz(ds, 'Asia/Seoul')] %>% 
#     setkey(ds) %>% return()
#   
# }
load_model <- function(tb_,
                       agent_id, measurement, host_ip,
                       metric, remake, request_body,
                       changepoint.prior.scale) {
  
  arg <- request_body %>% fromJSON(simplifyDataFrame = F)
  
  agent_id <- agent_id %>% as.integer()
  
  con <- dbConnect(MySQL(),
                   user = MYSQL_USER,
                   password = MYSQL_PW,
                   dbname = MYSQL_DB,
                   host = MYSQL_HOST,
                   port = MYSQL_PORT)
  
  if (as.logical(remake) == TRUE) {
    
    query <- "select id, filename
              from nexclipper_anomaly_model
              where agent_id = '%s' and
                    measurement = '%s' and
                    host_ip = '%s' and
                    metric = '%s' and
                    mount = '%s' and
                    hostIF = '%s' and
                    dname = '%s' and
                    dockerIF = '%s' and
                    E_ID = '%s' and
                    interface = '%s'" %>% 
      sprintf(agent_id, measurement, host_ip, metric, arg$mount, arg$hostIF,
              arg$dname, arg$dockerIF, arg$E_ID, arg$IF)
    
    res <- dbGetQuery(con, query)
    
    new_filename <- tempfile('', '')
    
    new_dir.name <-  paste0("model", new_filename, '.rdata')
    
    model <- prophet(tb_,
                     changepoint.prior.scale = changepoint.prior.scale)
    
    save(model, file = new_dir.name)
    
    if (nrow(res) > 0) {
      
      id <- res$id %>% tail(1)
      
      filename <- res$filename %>% tail(1)
      
      dir.name <-  paste0("model", filename, '.rdata')
      
      if (file.exists(dir.name)) file.remove(dir.name)
      
      query <- "update nexclipper_anomaly_model
                set filename = '%s'
                where id = '%s'" %>% 
        sprintf(new_filename, id)
      
      cat('\n', query, '\n\n')
      
      dbGetQuery(con, query)
      
      print('Update filename!')
      
    } else {
      
      info <- data.frame('agent_id' = agent_id,
                         'measurement' = measurement,
                         'host_ip' = host_ip,
                         'metric' = metric,
                         'mount' = arg$mount,
                         'hostIF' = arg$hostIF,
                         'dname' = arg$dname,
                         'dockerIF' = arg$dockerIF,
                         'E_ID' = arg$E_ID,
                         'interface' = arg$IF,
                         'filename' = filename,
                         stringsAsFactors = F)
      
      print(info)
      
      dbWriteTable(con, 
                   name = 'nexclipper_anomaly_model', 
                   value = info,
                   row.names = F,
                   append = T)
      
      dbCommit(con)
      
    }
    
  } else {
    
    query <- "select filename
              from nexclipper_anomaly_model
              where agent_id = '%s' and
                    measurement = '%s' and
                    host_ip = '%s' and
                    metric = '%s' and
                    mount = '%s' and
                    hostIF = '%s' and
                    dname = '%s' and
                    dockerIF = '%s' and
                    E_ID = '%s' and
                    interface = '%s'" %>% 
      sprintf(agent_id, measurement, host_ip, metric, arg$mount, arg$hostIF,
              arg$dname, arg$dockerIF, arg$E_ID, arg$IF)
    
    res <- dbGetQuery(con, query)
    
    filename <- res$filename %>% tail(1)
    
    dir.name <- paste0('./model', filename, '.rdata')
    
    if (nrow(res) == 0 | !file.exists(dir.name)) {
      
      filename <- tempfile('', '')
      
      dir.name <-  paste0("model", filename, '.rdata')
      
      model <- prophet(tb_,
                       changepoint.prior.scale = changepoint.prior.scale)
      
      save(model, file = dir.name)
      
      info <- data.frame('agent_id' = agent_id,
                         'measurement' = measurement,
                         'host_ip' = host_ip,
                         'metric' = metric,
                         'mount' = arg$mount,
                         'hostIF' = arg$hostIF,
                         'dname' = arg$dname,
                         'dockerIF' = arg$dockerIF,
                         'E_ID' = arg$E_ID,
                         'interface' = arg$IF,
                         'filename' = filename,
                         stringsAsFactors = F)
      
      print(info)
      
      dbWriteTable(con, 
                   name = 'nexclipper_anomaly_model', 
                   value = info,
                   row.names = F,
                   append = T)
      
      dbCommit(con)
      
    } else {
      
      filename <- res$filename %>% tail(1)
      
      dir.name <- paste0('./model', filename, '.rdata')
      
      load(dir.name)
      
      print('Re-Use model!')
      
    }
    
  }
  
  dbDisconnect(con)
  
  return(model)
  
}

anomalyDetection <- function(tb_, groupby, 
                             agent_id, measurement, host_ip,
                             metric, remake, request_body,
                             changepoint.prior.scale = 0.01) {
  
  model <- load_model(tb_,
                      agent_id, measurement, host_ip,
                      metric, remake, request_body,
                      changepoint.prior.scale)
  
  unit_ <- str_extract(groupby, '[:alpha:]')
  
  groupby_ <- str_extract(groupby, '\\d+') %>% as.integer()
  
  freq <- switch(unit_,
                 's' = groupby_,
                 'm' = groupby_ * 60,
                 'h' = groupby_ * 60 * 60)
  
  future <- data.frame(ds = seq(min(tb_$ds),
                                max(tb_$ds),
                                by = freq))
  
  ano_result <- predict(model, future) %>% 
    select(ds, yhat_lower, yhat_upper) %>% 
    as.data.table() %>%
    setkey(ds) %>% 
    .[tb_] %>% 
    .[, anomaly := ifelse(y < yhat_lower | y > yhat_upper, 1, 0)] %>% 
    .[, c('yhat_lower', 'yhat_upper') := NULL]
  
  if (nrow(ano_result) > 1000)
    
    ano_result[1:1000] %>% return()
  
  return(ano_result)
  
}
#----

#### ARGUMENT PARSING ####
option_list <- list(
  make_option(c("-id", "--agent_id"), action = "store", type = 'character'),
  make_option(c("-m", "--measurement"), action = "store", type = 'character'),
  make_option(c("-ip", "--host_ip"), action = "store", type = 'character'),
  make_option(c("-mtc", "--metric"), action = "store", type = 'character'),
  make_option(c("-p", "--period"), action = "store", type = 'character'),
  make_option(c("-g", "--groupby"), action = "store", type = 'character'),
  make_option(c("-t", "--start_time"), action = "store", type = 'character'),
  make_option(c("-k", "--key"), action = "store", type = 'character'),
  make_option(c("-r", "--remake"), action = "store", type = 'character'),
  make_option(c("-req", "--request_body"), action = "store", type = 'character')
)

opt = parse_args(OptionParser(option_list = option_list))

print('########ANOMALY########')
opt[-10] %>% unlist() %>% print()
opt[[10]] %>% fromJSON(simplifyDataFrame = F) %>% unlist() %>% print()
print('########################')
#----

#### EXECUTION ####
detection_ <- function(agent_id, measurement, host_ip,
                       metric, period, groupby,
                       start_time, key, remake, request_body) {
  #agent_id=27;measurement='host';host_ip='192.168.0.169';metric='cpu_used_percent';period='7d';predicted_period='2d';groupby='1h';start_time='2018-10-05 16:04:27';key='618827342';request_body="\"{'mount':'null'}\""
  res <- load_single_metric(agent_id, measurement, host_ip, metric,
                            period, groupby, start_time, request_body)
  
  if (is.null(res)) {
    
    update_key_id_to_mysql(agent_id, key, 404, 'not found')
    
    return()
    
  }
  
  result <- anomalyDetection(res, groupby, 
                             agent_id, measurement, host_ip,
                             metric, remake, request_body)
  
  result[, (c('key', 'agent_id')) := list(key, agent_id)]
  
  write_result_to_influx(result)
  
  update_key_id_to_mysql(agent_id, key, 200, 'Success')
  
}

detection_(opt$agent_id, opt$measurement, opt$host_ip,
           opt$metric, opt$period, opt$groupby,
           opt$start_time, opt$key, opt$remake, opt$request_body)
#----


