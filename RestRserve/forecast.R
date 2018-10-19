source('00.R')
source('01.R')
source('02.R')


#### DB CONNECTION ####
connect <- function() {
  
  con <- influx_connection(host = 'influxdb.marathon.l4lb.thisdcos.directory',
                           port = 8086)
  
  dbname <- 'nexclipper'
  
  conn <- list(connector = con, dbname = dbname)
  
  return(conn)
  
}
#----

#### DB WRITE ####
write_result_to_influx <- function(dt_) {
  
  con <- connect()
  
  connector <- con$connector
  
  influx_write(dt_, connector, 'nexclipper_ai', 'forecast',
               time_col = 'ds', tag_cols = c('key'))
  
}
#----

#### APP FUNCTIONS ####
load_single_metric <- function(agent_id, measurement, host_ip, metric,
                               period, groupby, start_time, ...) {
  
  arg <- list(...)
  
  agent_id <- agent_id %>% as.integer()
  
  # unit <- unit %>% as.character()
  
  switch(measurement,
         'host' = load_host(agent_id, host_ip, metric, period, groupby, start_time),
         'host_disk' = load_host_disk(agent_id, host_ip, metric, period, groupby, start_time,
                                      arg$mount),
         'host_net' = load_host_net(agent_id, host_ip, metric, period, groupby, start_time,
                                    arg$hostIF),
         'host_process' = load_host_process(agent_id, host_ip, metric, period, groupby, start_time,
                                            arg$pname),
         'docker_container' = load_docker_container(agent_id, host_ip, metric, period, groupby, start_time,
                                                    arg$dname),
         'docker_network' = load_docker_network(agent_id, host_ip, metric, period, groupby, start_time,
                                                arg$dname, arg$dockerIF)) %>% 
    .[!is.na(y)] %>% return()
  
}


load_docker_container <- function(agent_id, host_ip, metric, period, groupby, start_time, dname) {
  #agent_id=27;host_ip='192.168.0.165';metric='cpu_used_percent';period='6d';groupby='1h';start_time='2018-10-04 10:31:05';dname='/Nexclipper-Agent'
  con <- connect()
  
  connector <- con$connector
  
  dbname <- con$dbname
  
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
  
  res <- influx_query(connector,
                      dbname,
                      query, return_xts = F,
                      simplifyList = T)[[1]] %>% 
    as.data.table()
  
  if (!('time' %in% names(res)))
    
    return(NULL)
  
  res %>% 
    .[, -1:-4] %>% 
    setnames('time', 'ds') %>% 
    .[, ds := with_tz(ds, 'Asia/Seoul')] %>% 
    setkey(ds) %>% return()
  
}


load_docker_network <- function(agent_id, host_ip, metric, period, groupby, start_time, dname, interface) {
  #agent_id=27;host_ip='192.168.0.165';metric='rx_bytes';period=6;groupby='1h';unit='0';dname='nexcloud_nexclipperui.081024c1-c2f2-11e8-8aa1-aae0d7e58657';interface='eth0'
  con <- connect()
  
  connector <- con$connector
  
  dbname <- con$dbname
  
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
  
  res <- influx_query(connector,
                      dbname,
                      query, return_xts = F,
                      simplifyList = T)[[1]] %>% 
    as.data.table()
  
  if (!('time' %in% names(res)))
    
    return(NULL)
  
  res %>% 
    .[, -1:-4] %>% 
    setnames('time', 'ds') %>% 
    .[, ds := with_tz(ds, 'Asia/Seoul')] %>% 
    setkey(ds) %>% return()
  
}


load_host <- function(agent_id, host_ip, metric, period, groupby, start_time) {
  #agent_id=27;host_ip='192.168.0.165';metric='cpu_used_percent';period=6;groupby='1h';unit='0'
  con <- connect()
  
  connector <- con$connector
  
  dbname <- con$dbname
  
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
  
  res <- influx_query(connector,
                      dbname,
                      query, return_xts = F,
                      simplifyList = T)[[1]] %>% 
    as.data.table()
  
  if (!('time' %in% names(res)))
    
    return(NULL)
  
  res %>% 
    .[, -1:-4] %>% 
    setnames('time', 'ds') %>% 
    .[, ds := with_tz(ds, 'Asia/Seoul')] %>% 
    setkey(ds) %>% return()
  
}


load_host_disk <- function(agent_id, host_ip, metric, period, groupby, start_time, mount) {
  #agent_id=27;host_ip='192.168.0.165';metric='used_percent';period=6;groupby='1h';unit='0';mount='/'
  con <- connect()
  
  connector <- con$connector
  
  dbname <- con$dbname
  
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
  
  res <- influx_query(connector,
                      dbname,
                      query, return_xts = F,
                      simplifyList = T)[[1]] %>% 
    as.data.table()
  
  if (!('time' %in% names(res)))
    
    return(NULL)
  
  res %>% 
    .[, -1:-4] %>% 
    setnames('time', 'ds') %>% 
    .[, ds := with_tz(ds, 'Asia/Seoul')] %>% 
    setkey(ds) %>% return()
  
}


load_host_net <- function(agent_id, host_ip, metric, period, groupby, start_time, interface) {
  #agent_id=27;host_ip='192.168.0.165';metric='rxbyte';period=6;groupby='1h';unit='0';interface='veth99a298c8'
  con <- connect()
  
  connector <- con$connector
  
  dbname <- con$dbname
  
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
  
  res <- influx_query(connector,
                      dbname,
                      query, return_xts = F,
                      simplifyList = T)[[1]] %>% 
    as.data.table()
  
  if (!('time' %in% names(res)))
    
    return(NULL)
  
  res %>% 
    .[, -1:-4] %>% 
    setnames('time', 'ds') %>% 
    .[, ds := with_tz(ds, 'Asia/Seoul')] %>% 
    setkey(ds) %>% return()
  
}


load_host_process <- function(agent_id, host_ip, metric, period, groupby, start_time, pname) {
  #agent_id=27;host_ip='192.168.0.165';metric='cpu_used_percent';period='6d';groupby='1h';pname='mysqld'
  con <- connect()
  
  connector <- con$connector
  
  dbname <- con$dbname
  
  pname <- paste0('"name" = ', "'%s'") %>% 
    sprintf(pname)
  
  query <- "select mean(%s) as y
            from host_process
            where agent_id = '%s' and
                  time > '%s' - %s and
                  host_ip = '%s' and 
                  %s
            group by time(%s)" %>% 
    sprintf(metric,
            agent_id,
            start_time, period,
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
    
    return(NULL)
  
  res %>% 
    .[, -1:-4] %>% 
    setnames('time', 'ds') %>% 
    .[, ds := with_tz(ds, 'Asia/Seoul')] %>% 
    setkey(ds) %>% return()
  
}


forecasting <- function(tb_, groupby, predicted_period,
                        changepoint.prior.scale = 0.01) {
  
  model <- prophet(tb_,
                   changepoint.prior.scale = changepoint.prior.scale)
  
  unit_ <- str_extract(groupby, '[:alpha:]')
  
  groupby_ <- str_extract(groupby, '\\d+') %>% as.integer()
  
  freq <- switch(unit_,
                 's' = groupby_,
                 'm' = groupby_ * 60,
                 'h' = groupby_ * 60 * 60)
  
  unit_ <- str_extract(predicted_period, '[:alpha:]')
  
  predicted_period_ <- str_extract(predicted_period, '\\d+') %>% 
    as.integer()
  
  pred_period <- switch(unit_,
                        'd' = (predicted_period_ * 24 * 60 * 60) %/% freq,
                        'h' = (predicted_period_ * 60 * 60) %/% freq)
  
  future <- make_future_dataframe(model,
                                  periods = pred_period,
                                  freq = freq,
                                  include_history = F)
  
  pred_result <- predict(model, future) %>%
    select(ds, yhat_lower, yhat_upper, yhat) %>% 
    as.data.table()
  
  if (nrow(pred_result) > 1000)
    
    pred_result[1:1000] %>% return()
  
  return(pred_result)
  
}
#----

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
#----

#### write mysql ####
update_key_id_to_mysql <- function(agent_id, key_,
                                   status, message) {
  
  # con <- dbConnect(MySQL(), 
  #                  user = 'admin', 
  #                  password = 'password',
  #                  dbname = 'defaultdb',
  #                  host = 'mysql.marathon.l4lb.thisdcos.directory', 
  #                  port = 3306)
  con <- dbConnect(MySQL(), 
                   user = 'admin', 
                   password = 'password',
                   dbname = 'defaultdb',
                   host = '192.168.0.165', 
                   port = 25322)
  
  query <- "update nexclipper_key
            set end_time = '%s',
                status = %s, 
                message = '%s'
            where agent_id = '%s' and key_id = '%s'" %>% 
    sprintf(Sys.time(), status, message,
            agent_id, key_)
  
  cat('\n', query, '\n')
  
  dbGetQuery(con, query)
  
  dbCommit(con)
  
  dbDisconnect(con)
  
}
#----

#### EXECUTION ####
forecast_ <- function(agent_id, measurement, host_ip,
                      metric, period, predicted_period,
                      groupby, start_time, key, ...) {
  #agent_id=27;measurement='host';host_ip='192.168.0.165';metric='cpu_used_percent';period='7d';predicted_period='2d';groupby='1h';start_time='2018-10-05 16:04:27';key='forecast_618827342'
  #agent_id=27;measurement='host_disk';host_ip='192.168.0.165';metric='used_percent';period='7d';predicted_period='2d';groupby='1h';start_time='2018-10-05 16:04:27';mount='/';key='forecast_618827342'
  #agent_id=27;measurement='host_disk';host_ip='192.168.0.169';metric='used_percent';period='7d';predicted_period='2d';groupby='1h';start_time='2018-10-05 16:04:27';mount='/';key='forecast_618827342'
  res <- load_single_metric(agent_id, measurement, host_ip, metric,
                            period, groupby, start_time, ...)
  
  result <- forecasting(res, groupby, predicted_period,
                        changepoint.prior.scale = 0.1)
  
  result[, key := key]
  
  write_result_to_influx(result)
  
  update_key_id_to_mysql(agent_id, key, 200, 'Success')
  
}

forecast_(opt$agent_id, opt$measurement, opt$host_ip,
          opt$metric, opt$period, opt$p_period,
          opt$groupby, opt$start_time, opt$key,
          mount = opt$mount, hostIF = opt$hostIF, pname = opt$pname,
          dname = opt$dname, dockerIF = opt$dockerIF)
#----


