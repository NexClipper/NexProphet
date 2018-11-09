source('00.R')
source('01.R')
source('02.R')
source('app_func.R')

#### ENVIRONMENT VARIABLE ####
INFLUX_ENV <- Sys.getenv(c('INFLUX_HOST', 'INFLUX_PORT', 'INFLUX_DB'))

INFLUX_HOST <- INFLUX_ENV['INFLUX_HOST']

INFLUX_PORT <- INFLUX_ENV['INFLUX_PORT'] %>% as.integer()

INFLUX_DB <- INFLUX_ENV['INFLUX_DB']
#----

#### DB CONNECTION ####

CONN <- influx_connection(host = INFLUX_HOST,
                          port = INFLUX_PORT)

#----

#### DB WRITE ####
write_result_to_influx <- function(dt_, key_) {
  
  influx_write(dt_, CONN, 'nexclipper_ai', key_,
               time_col = 'ds')
  
}
#----

#### APP FUNCTIONS ####
get_corr_mtx <- function(agent_id, period, groupby, start_time, key_,
                         request_body) {
  
  arg <- request_body %>% fromJSON(simplifyDataFrame = F)
  
  whole_data <- NULL
  
  # docker container
  if (!is.null(arg$docker_container)) {
    
    docker_container <- load_docker_containers(agent_id, arg$docker_container, period, groupby, start_time)
    
    if (!is.null(docker_container))
      
      whole_data <- docker_container
  }
  
  # docker network
  if (!is.null(arg$docker_network)) {
    
    docker_network <- load_docker_networks(agent_id, arg$docker_network, period, groupby, start_time)
    
    if (!is.null(docker_network)) {
      
      if (!is.null(whole_data)) {
        
        whole_data <- docker_network[whole_data]
        
      } else {
        
        whole_data <- docker_network
        
      }
    }
    
  }
  
  # host
  if (!is.null(arg$host)) {
    
    host <- load_hosts(agent_id, arg$host, period, groupby, start_time)
    
    if (!is.null(host)) {
      
      if (!is.null(whole_data)) {
        
        whole_data <- host[whole_data]
        
      } else {
        
        whole_data <- host
        
      }
    }
    
  }
  
  # host disk
  if (!is.null(arg$host_disk)) {
    
    host_disk <- load_host_disks(agent_id, arg$host_disk, period, groupby, start_time)
    
    if (!is.null(host_disk)) {
      
      if (!is.null(whole_data)) {
        
        whole_data <- host_disk[whole_data]
        
      } else {
        
        whole_data <- host_disk
        
      }
    }
    
  }
  
  # host network
  if (!is.null(arg$host_net)) {
    
    host_net <- load_host_nets(agent_id, arg$host_net, period, groupby, start_time)
    
    if (!is.null(host_net)) {
      
      if (!is.null(whole_data)) {
        
        whole_data <- host_net[whole_data]
        
      } else {
        
        whole_data <- host_net
        
      }
    }
    
  }
  
  # node
  if (!is.null(arg$node)) {
    
    node <- load_nodes(agent_id, arg$node, period, groupby, start_time)
    
    if (!is.null(node)) {
      
      if (!is.null(whole_data)) {
        
        whole_data <- node[whole_data]
        
      } else {
        
        whole_data <- node
        
      }
    }
    
  }
    
  # task
  if (!is.null(arg$task)) {
    
    task <- load_tasks(agent_id, arg$task, period, groupby, start_time)
    
    if (!is.null(task)) {
      
      if (!is.null(whole_data)) {
        
        whole_data <- task[whole_data]
        
      } else {
        
        whole_data <- task
        
      }
    }
    
  }
    
  # network
  if (!is.null(arg$network)) {
    
    network <- load_networks(agent_id, arg$network, period, groupby, start_time)
    
    if (!is.null(network)) {
      
      if (!is.null(whole_data)) {
        
        whole_data <- network[whole_data]
        
      } else {
        
        whole_data <- network
        
      }
    }
    
  }
  
  # k8s_pod
  if (!is.null(arg$k8s_pod)) {
    
    k8s_pod <- load_k8s_pods(agent_id, arg$k8s_pod, period, groupby, start_time)
    
    if (!is.null(k8s_pod)) {
      
      if (!is.null(whole_data)) {
        
        whole_data <- k8s_pod[whole_data]
        
      } else {
        
        whole_data <- k8s_pod
        
      }
    }
    
  }
  
  # k8s_node
  if (!is.null(arg$k8s_node)) {
    
    k8s_node <- load_k8s_nodes(agent_id, arg$k8s_node, period, groupby, start_time)
    
    if (!is.null(k8s_node)) {
      
      if (!is.null(whole_data)) {
        
        whole_data <- k8s_node[whole_data]
        
      } else {
        
        whole_data <- k8s_node
        
      }
    }
    
  }
    
  
  # host process
  # holding,,,
  # if (!is.null(arg$host_process)) {
  #   
  #   host_process <- load_host_processes(agent_id, arg$host_process, period, groupby, start_time)
  #   
  #   if (is.null(whole_data)) {
  #     
  #     whole_data <- host_process
  #     
  #   } else {
  #     
  #     whole_data <- host_process[whole_data]
  #     
  #   }
  #   
  # }
  if (!is.null(whole_data) & (ncol(whole_data) != 1)) {
    
      dt_processed <- pre_processing(whole_data)
      
    } else {
      
      update_key_id_to_mysql(agent_id, key_, 404, 'not found'); return()
      
    }
  
  cor_mtx <- cor(dt_processed) %>% 
    as.data.table()
  
  time_idx <- seq.POSIXt(from = Sys.time(),
                         by = '-1 hour',
                         length.out = nrow(cor_mtx)) %>% 
    sort()
  
  cor_mtx[, ds := time_idx]
  
  write_result_to_influx(cor_mtx, key_)
  
  # comparing before pre-processing with after
  before_cols <- colnames(whole_data) %>% setdiff('ds')
    
  after_cols <- colnames(dt_processed)
  
  diff_cols <- before_cols %>% setdiff(after_cols)
  
  if (length(diff_cols) == 0) {
    
    update_key_id_to_mysql(agent_id, key_, 200, 'Success')
    
  } else {
    
    message <- paste0('{', paste(diff_cols, collapse = ', '), '}') %>% 
      paste('are excluded.')
    
    update_key_id_to_mysql(agent_id, key_, 206, message)
    
  }
  
}


load_docker_containers <- function(agent_id, request, period, groupby, start_time) {
  #agent_id=27;host_ip=c('192.168.0.165', '192.168.0.164');metric=c('cpu_used_percent', 'mem_used_percent');period='6d';groupby='1h';start_time='2018-10-18 14:30:00';dname=c('/Nexclipper-Agent', 'kafka-manager.fbed1a44-d187-11e8-b067-aae0d7e58657')
  #request <- host_ip, metric, dname
  host_ip <- request$host_ip
  
  metric <- request$metric
  
  dname <- request$dname
  
  metric_for_query <- paste('mean(%s) as', metric) %>% 
    sprintf(metric) %>%
    paste(collapse = ', ')
  
  host_ip_for_query <- paste(host_ip, collapse = '|')
  
  dname_for_query <- dname %>%
    str_replace('/', '') %>% 
    paste(collapse = '|')
  
  query <- "select %s
            from docker_container
            where agent_id = '%s' and
                  time > '%s' - %s and
                  host_ip =~ /%s/ and
                  task_id =~ /%s/
            group by time(%s), host_ip, task_id" %>% 
    sprintf(metric_for_query,
            agent_id,
            start_time, period,
            host_ip_for_query,
            dname_for_query,
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
    .[, -1:-3] %>% 
    setnames('time', 'ds') %>% 
    setkey(ds) %>% 
    melt(id.vars = 1:3,
         measure.vars = 4:ncol(.),
         variable.name = 'metric',
         value.name = 'y') %>% 
    .[, key := paste(host_ip, metric, task_id, sep = '__')] %>%
    .[, c('ds', 'key', 'y')] %>% 
    dcast(ds ~ key,
          value.var = 'y') %>%
    return()
  
}


load_docker_networks <- function(agent_id, request, period, groupby, start_time) {
  #agent_id=27;period='6d';groupby='1h';start_time='2018-10-30 10:00:00'
  #request <- list('host_ip' = c('192.168.0.168'), 'metric' = c('rx_bytes', 'tx_bytes'), dname = c('influxdb.131a06b9-afee-11e8-ae9f-aae0d7e58657'), interface = c('eth0'))
  host_ip <- request$host_ip
  
  metric <- request$metric
  
  dname <- request$dname
  
  interface <- request$interface
  
  metric_for_query <- paste('mean(%s) as', metric) %>% 
    sprintf(metric) %>%
    paste(collapse = ', ')
  
  host_ip_for_query <- paste(host_ip, collapse = '|')
  
  dname_for_query <- dname %>%
    str_replace('/', '') %>% 
    paste(collapse = '|')
  
  interface_for_query <- paste(interface, collapse = '|')
  
  query <- "select %s
            from docker_network
            where agent_id = '%s' and
                  time > '%s' - %s and
                  host_ip =~ /%s/ and 
                  task_id =~ /%s/ and
                  interface =~ /%s/
            group by time(%s), host_ip, task_id, interface" %>% 
    sprintf(metric_for_query,
            agent_id,
            start_time, period,
            host_ip_for_query,
            dname_for_query,
            interface_for_query,
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
    .[, -1:-3] %>%
    setnames('time', 'ds') %>%
    setkey(ds) %>% 
    melt(id.vars = 1:4,
         measure.vars = 5:ncol(.),
         variable.name = 'metric',
         value.name = 'y') %>% 
    .[, key := paste(host_ip, metric, task_id, interface, sep = '__')] %>%
    .[, c('ds', 'key', 'y')] %>% 
    dcast(ds ~ key,
          value.var = 'y') %>%
    return()
  
}


load_hosts <- function(agent_id, request, period, groupby, start_time) {
  #agent_id=27;host_ip=c('192.168.0.165', '192.168.0.166');metric=c('cpu_used_percent', 'mem_used_percent');period='6d';groupby='1h'
  #agent_id=27;host_ip=c('192.168.0.165', '192.168.0.166');metric=c('cpu_used_percent');period='6d';groupby='1h'
  #request <- host_ip, metric
  host_ip <- request$host_ip
  
  metric <- request$metric
  
  metric_for_query <- paste('mean(%s) as', metric) %>% 
    sprintf(metric) %>%
    paste(collapse = ', ')
  
  host_ip_for_query <- paste(host_ip, collapse = '|')
  
  query <- "select %s
            from host
            where agent_id = '%s' and
                  time > '%s' - %s and
                  host_ip =~ /%s/
            group by time(%s), host_ip" %>% 
    sprintf(metric_for_query,
            agent_id,
            start_time, period,
            host_ip_for_query,
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
    .[, -1:-3] %>% 
    setnames('time', 'ds') %>% 
    setkey(ds) %>% 
    melt(id.vars = 1:2,
         measure.vars = 3:ncol(.),
         variable.name = 'metric',
         value.name = 'y') %>% 
    .[, key := paste(host_ip, metric, sep = '__')] %>%
    .[, c('ds', 'key', 'y')] %>%
    dcast(ds ~ key,
          value.var = 'y') %>%
    return()
  
}


load_host_disks <- function(agent_id, request, period, groupby, start_time) {
  #agent_id=27;host_ip=c('192.168.0.165', '192.168.0.166');metric=c('used_percent');period='7d';groupby='1h';mount=c('/', '/var')
  #request <- host_ip, metric, mount
  host_ip <- request$host_ip
  
  metric <- request$metric
  
  mount <- request$mount
  
  metric_for_query <- paste('mean(%s) as', metric) %>% 
    sprintf(metric) %>%
    paste(collapse = ', ')
  
  host_ip_for_query <- paste(host_ip, collapse = '|')
  
  mount_for_query <- "mount_name = '%s'" %>% 
    sprintf(mount) %>% 
    paste(collapse = ' or ')
  
  query <- "select %s
            from host_disk
            where agent_id = '%s' and
                  time > '%s' - %s and
                  host_ip =~ /%s/ and
                  (%s)
            group by time(%s), host_ip, mount_name" %>% 
    sprintf(metric_for_query,
            agent_id,
            start_time, period,
            host_ip_for_query,
            mount_for_query,
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
    .[, -1:-3] %>% 
    setnames('time', 'ds') %>% 
    setkey(ds) %>%
    melt(id.vars = 1:3,
         measure.vars = 4:ncol(.),
         variable.name = 'metric',
         value.name = 'y') %>% 
    .[, key := paste(host_ip, metric, mount_name, sep = '__')] %>%
    .[, c('ds', 'key', 'y')] %>% 
    dcast(ds ~ key,
          value.var = 'y') %>%
    return()
  
}


load_host_nets <- function(agent_id, request, period, groupby, start_time) {
  #agent_id=27;host_ip=c('192.168.0.165', '192.168.0.166');metric=c('rxbyte', 'txbyte');period='6d';groupby='1h';interface=c('eth0', 'docker0')
  #request <- host_ip, metric, interface
  host_ip <- request$host_ip
  
  metric <- request$metric
  
  interface <- request$interface
  
  metric_for_query <- paste('mean(%s) as', metric) %>% 
    sprintf(metric) %>%
    paste(collapse = ', ')
  
  host_ip_for_query <- paste(host_ip, collapse = '|')
  
  # interface_for_query <- "interface = '%s'" %>% 
  #   sprintf(interface) %>%
  #   paste(collapse = ' or ')
  interface_for_query <- paste(interface, collapse = '|')
  
  query <- "select %s
            from host_net
            where agent_id = '%s' and
                  time > '%s' - %s and
                  host_ip =~ /%s/ and
                  interface =~ /%s/
            group by time(%s), host_ip, interface" %>% 
    sprintf(metric_for_query,
            agent_id,
            start_time, period,
            host_ip_for_query,
            interface_for_query,
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
    .[, -1:-3] %>% 
    setnames('time', 'ds') %>% 
    setkey(ds) %>%
    melt(id.vars = 1:3,
         measure.vars = 4:ncol(.),
         variable.name = 'metric',
         value.name = 'y') %>% 
    .[, key := paste(host_ip, metric, interface, sep = '__')] %>%
    .[, c('ds', 'key', 'y')] %>% 
    dcast(ds ~ key,
          value.var = 'y') %>%
    return()
  
}


load_nodes <- function(agent_id, request, period, groupby, start_time) {
  #agent_id=27;node_ip=c('192.168.0.165', '192.168.0.166');metric=c('cpu_used_percent', 'mem_used_percent');period='7d';groupby='1h'
  #request <- node_ip, metric
  node_ip <- request$node_ip
  
  metric <- request$metric
  
  metric_for_query <- paste('mean(%s) as', metric) %>% 
    sprintf(metric) %>%
    paste(collapse = ', ')
  
  node_ip_for_query <- paste(node_ip, collapse = '|')
  
  query <- "select %s
            from node
            where agent_id = '%s' and
                  time > '%s' - %s and
                  node_ip =~ /%s/
            group by time(%s), node_ip" %>% 
    sprintf(metric_for_query,
            agent_id,
            start_time, period,
            node_ip_for_query,
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
    .[, -1:-3] %>% 
    setnames('time', 'ds') %>% 
    setkey(ds) %>% 
    melt(id.vars = 1:2,
         measure.vars = 3:ncol(.),
         variable.name = 'metric',
         value.name = 'y') %>% 
    .[, key := paste(node_ip, metric, sep = '__')] %>%
    .[, c('ds', 'key', 'y')] %>% 
    dcast(ds ~ key,
          value.var = 'y') %>%
    return()
  
}


load_tasks <- function(agent_id, request, period, groupby, start_time) {
  #agent_id=27;node_ip=c('192.168.0.165', '192.168.0.168');metric=c('cpu_used_percent', 'mem_used_percent');period='6d';groupby='1h';E_ID=c('kafka__9ac45849-069f-4653-a05e-993f211e83ee', 'nexcloud_hostapi.50287b23-e17a-11e8-ae5d-8ac1dc5733cc')
  #request <- node_ip, metric, executor_id
  node_ip <- request$node_ip
  
  metric <- request$metric
  
  E_ID <- request$E_ID
  
  metric_for_query <- paste('mean(%s) as', metric) %>% 
    sprintf(metric) %>%
    paste(collapse = ', ')
  
  node_ip_for_query <- paste(node_ip, collapse = '|')
  
  E_ID_for_query <- paste(E_ID, collapse = '|')
  
  query <- "select %s
            from task
            where agent_id = '%s' and
                  time > '%s' - %s and
                  executor_id =~ /%s/ and
                  node_ip =~ /%s/
            group by time(%s), node_ip, executor_id" %>% 
    sprintf(metric_for_query,
            agent_id,
            start_time, period,
            E_ID_for_query,
            node_ip_for_query,
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
    .[, -1:-3] %>% 
    setnames('time', 'ds') %>% 
    setkey(ds) %>% 
    melt(id.vars = 1:3,
         measure.vars = 4:ncol(.),
         variable.name = 'metric',
         value.name = 'y') %>% 
    .[, key := paste(node_ip, metric, executor_id, sep = '__')] %>%
    .[, c('ds', 'key', 'y')] %>% 
    dcast(ds ~ key,
          value.var = 'y') %>%
    return()
  
}


load_networks <- function(agent_id, request, period, groupby, start_time) {
  #agent_id=27;host_ip=c('192.168.0.165', '192.168.0.166');metric=c('in_bytes', 'out_bytes');period='7d';groupby='1h';interface=c('null')
  #request <- host_ip, metric, interface
  node_ip <- request$node_ip
  
  metric <- request$metric
  
  interface <- request$IF
  
  metric_for_query <- paste('mean(%s) as', metric) %>% 
    sprintf(metric) %>%
    paste(collapse = ', ')
  
  node_ip_for_query <- paste(node_ip, collapse = '|')
  
  interface_for_query <- paste(interface, collapse = '|')
  
  query <- "select %s
            from network
            where agent_id = '%s' and
                  time > '%s' - %s and
                  node_ip =~ /%s/ and
                  interface =~ /%s/
            group by time(%s), node_ip, interface" %>% 
    sprintf(metric_for_query,
            agent_id,
            start_time, period,
            node_ip_for_query,
            interface_for_query,
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
    .[, -1:-3] %>% 
    setnames('time', 'ds') %>% 
    setkey(ds) %>%
    melt(id.vars = 1:3,
         measure.vars = 4:ncol(.),
         variable.name = 'metric',
         value.name = 'y') %>% 
    .[, key := paste(node_ip, metric, interface, sep = '__')] %>%
    .[, c('ds', 'key', 'y')] %>% 
    dcast(ds ~ key,
          value.var = 'y') %>%
    return()
  
}


load_k8s_pods <- function(agent_id, request, period, groupby, start_time) {
  #agent_id=5;node_ip=c('192.168.1.5', '192.168.1.6');metric=c('cpu_used_percent', 'mem_used_percent');period='7d';groupby='1h';namespace=c('default', 'kube-system');pod=c('datadog-agent-wxxfs', 'weave-net-zzk8g')
  #request <- host_ip, metric, interface
  node_ip <- request$node_ip
  
  metric <- request$metric
  
  namespace <- request$namespace
  
  pod <- request$pod
  
  metric_for_query <- paste('mean(%s) as', metric) %>% 
    sprintf(metric) %>%
    paste(collapse = ', ')
  
  node_ip_for_query <- paste(node_ip, collapse = '|')
  
  namespace_for_query <- paste(namespace, collapse = '|')
  
  pod_for_query <- paste(pod, collapse = '|')
  
  query <- "select %s
            from k8s_pod
            where agent_id = '%s' and
                  time > '%s' - %s and
                  node_ip =~ /%s/ and
                  namespace =~ /%s/ and
                  pod =~ /%s/
            group by time(%s), node_ip, namespace, pod" %>% 
    sprintf(metric_for_query,
            agent_id,
            start_time, period,
            node_ip_for_query,
            namespace_for_query,
            pod_for_query,
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
    .[, -1:-3] %>% 
    setnames('time', 'ds') %>% 
    setkey(ds) %>% 
    melt(id.vars = 1:4,
         measure.vars = 5:ncol(.),
         variable.name = 'metric',
         value.name = 'y') %>% 
    .[, key := paste(node_ip, metric, namespace, pod, sep = '__')] %>%
    .[, c('ds', 'key', 'y')] %>% 
    dcast(ds ~ key,
          value.var = 'y') %>%
    return()
  
}


load_k8s_nodes <- function(agent_id, request, period, groupby, start_time) {
  #agent_id=5;node_ip=c('192.168.1.5', '192.168.1.6');metric=c('cpu_used_percent', 'mem_used_percent');period='7d';groupby='1h';
  #request <- host_ip, metric, interface
  node_ip <- request$node_ip
  
  metric <- request$metric
  
  metric_for_query <- paste('mean(%s) as', metric) %>% 
    sprintf(metric) %>%
    paste(collapse = ', ')
  
  node_ip_for_query <- paste(node_ip, collapse = '|')
  
  query <- "select %s
            from k8s_node
            where agent_id = '%s' and
                  time > '%s' - %s and
                  node_ip =~ /%s/
            group by time(%s), node_ip" %>% 
    sprintf(metric_for_query,
            agent_id,
            start_time, period,
            node_ip_for_query,
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
    .[, -1:-3] %>% 
    setnames('time', 'ds') %>% 
    setkey(ds) %>%
    melt(id.vars = 1:2,
         measure.vars = 3:ncol(.),
         variable.name = 'metric',
         value.name = 'y') %>% 
    .[, key := paste(node_ip, metric, sep = '__')] %>%
    .[, c('ds', 'key', 'y')] %>% 
    dcast(ds ~ key,
          value.var = 'y') %>%
    return()
  
}




# load_host_processes <- function(agent_id, request, period, groupby, start_time) {
#   #agent_id=27;host_ip=c('192.168.0.165', '192.168.0.166');metric=c('cpu_used_percent', 'mem_used_percent');period='6d';groupby='1h';pname=c('mysqld', 'dockerd')
#   #request <- host_ip, metric, pname
#   con <- connect()
#   
#   connector <- con$connector
#   
#   dbname <- con$dbname
#   
#   host_ip <- request$host_ip
#   
#   metric <- request$metric
#   
#   pname <- request$pname
#   
#   metric_for_query <- paste('mean(%s) as', metric) %>% 
#     sprintf(metric) %>%
#     paste(collapse = ', ')
#   
#   host_ip_for_query <- "host_ip = '%s'" %>% 
#     sprintf(host_ip) %>%
#     paste(collapse = ' or ')
#   
#   pname_for_query <- paste('"name"', "'%s'", sep = ' = ') %>% 
#     sprintf(pname) %>% 
#     paste(collapse = ' or ')
#   
#   query <- "select %s
#             from host_process
#             where agent_id = '%s' and
#                   time > '%s' - %s and
#                   (%s) and 
#                   (%s)
#             group by time(%s), host_ip, \"name\"" %>% 
#     sprintf(metric_for_query,
#             agent_id,
#             start_time, period,
#             host_ip_for_query,
#             pname_for_query,
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
#     .[, -1:-3] %>% 
#     setnames('time', 'ds') %>% 
#     .[, ds := with_tz(ds, 'Asia/Seoul')] %>% 
#     setkey(ds) %>%
#     dcast(ds ~ host_ip + name,
#           value.var = metric,
#           sep = '__') %>%
#     return()
#   
# }


pre_processing <- function(dt_, threshold = 0.33) {
  
  dt_ %>% copy() %>% 
    .[, .SD, .SDcols = dt_[, lapply(.SD, function(x) sum(is.na(x)) <= as.integer(length(x) * threshold))] %>% 
        unlist() %>% 
        which()] %>% 
    .[, lapply(.SD, function(x) na.approx(x, na.rm = F) %>% 
                 na.fill('extend')), .SDcols = -1] %>% 
    .[, lapply(.SD, function(x) (x - min(x)) / (max(x) - min(x) + 1e-6))] %>% 
    return()
  
}
#----
# request_body <- list('docker_network' = list('metric' = c('rx_bytes',
#                                                           'tx_bytes'),
#                                              'host_ip' = c('192.168.0.168'),
#                                              'interface' = c('eth0'),
#                                              'dname' = c("influxdb.131a06b9-afee-11e8-ae9f-aae0d7e58657")),
#                      'docker_container' = list('metric' = c('cpu_used_percent',
#                                                             'mem_used_percent'),
#                                                'host_ip' = c('192.168.0.165',
#                                                              '192.168.0.166'),
#                                                'dname' = c('/Nexclipper-Agent')),
#                      'host_net' = list('metric' = c('rxbyte',
#                                                     'txbyte'),
#                                        'host_ip' = c('192.168.0.165',
#                                                      '192.168.0.166'),
#                                        'interface' = c('eth0',
#                                                        'docker0')),
#                      'host_disk' = list('metric' = c('used_percent'),
#                                         'host_ip' = c('192.168.0.165',
#                                                       '192.168.0.166'),
#                                         'mount' = c('/', '/var')),
#                      'host' = list('metric' = c('cpu_used_percent',
#                                                 'mem_used_percent'),
#                                    'host_ip' = c('192.168.0.165',
#                                                  '192.168.0.166')))
# 
# dt <- get_corr_mtx(27, '10d', '1h', '2018-10-31 02:00:00', '123132', request_body = request_body)



#### ARGUMENT PARSING ####
option_list <- list(
  make_option(c("-id", "--agent_id"), action = "store", type = 'character'),
  make_option(c("-p", "--period"), action = "store", type = 'character'),
  make_option(c("-g", "--groupby"), action = "store", type = 'character'),
  make_option(c("-t", "--start_time"), action = "store", type = 'character'),
  make_option(c("-k", "--key"), action = "store", type = 'character'),
  make_option(c("-req", "--request_body"), action = "store", type = 'character')
)

opt = parse_args(OptionParser(option_list = option_list))

print('########METRIC ASSOCIATION########')
opt[-6] %>% unlist() %>% print()
opt[[6]] %>% fromJSON(simplifyDataFrame = F) %>% unlist() %>% print()
print('##################################')
#-----


#### EXECUTION ####
get_corr_mtx(opt$agent_id, opt$period, opt$groupby,
             opt$start_time, opt$key, opt$request_body)
#----