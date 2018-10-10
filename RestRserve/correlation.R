source('00.R')
source('01.R')

#### DB CONNECTION ####
connect <- function() {
  
  con <- influx_connection(host = 'influxdb.marathon.l4lb.thisdcos.directory',
                           port = 8086)
  # con <- influx_connection(host = '192.168.0.162',
  #                          port = 10091)
  
  dbname <- 'nexclipper_ai'
  
  conn <- list(connector = con, dbname = dbname)
  
  return(conn)
  
}
#----

#### DB WRITE ####
write_result_to_influx <- function(dt_, key_) {
  
  con <- connect()
  
  connector <- con$connector
  
  dbname <- con$dbname
  
  influx_write(dt_, connector, dbname, key_,
               time_col = 'ds', tag_cols = c('key'))
  
}
#----


#### APP FUNCTIONS ####
load_multiple_metric <- function() {
  
  
}


load_docker_container <- function(agent_id, host_ip, metric, period, groupby, start_time, dname) {
  #agent_id=27;host_ip='192.168.0.165';metric='cpu_used_percent';period='6d';groupby='1h';start_time='2018-10-04 10:31:05';dname='/Nexclipper-Agent'
  con <- connect()
  
  connector <- con$connector
  
  dbname <- con$dbname
  
  metric <- 
  
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
            stat_time, period,
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
            start_time, period,
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
  #agent_id=27;host_ip='192.168.0.165';metric='cpu_used_percent';period=6;groupby='1h';unit='0';pname='mysqld'
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
#----