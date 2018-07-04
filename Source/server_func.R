
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

extract_field <- function(df) {
    
    # select numeric fields
    num_var = names(df)[sapply(df, class) == "numeric"]
    
    # select fields such that stddev is not equal to zero
    not0var = names(df[,num_var])[sapply(df[,num_var], var) != 0]
    
    # delete timestamp
    not0var = setdiff(not0var, "timestamp")
    
    extracted_df <- df %>% select(c(not0var, 'time'))
    
    return(extracted_df)
}

standardization <- function(mtx) {
    col_min <- as.vector(apply(mtx, 2, min))
    col_max <- as.vector(apply(mtx, 2, max))
    new_mtx <- t(apply(mtx,
                       1,
                       function(x) (x - col_min) / (col_max - col_min + 1e-6)))
    return(new_mtx)
}

multiple_join <- function(period = 5, group_by = '1h',
                          worker_list = c(), metric_list=c()) {
    
    connector <- connect()
    con <- connector$connector
    dbname <- connector$dbname
    
    period <- paste0(period, 'd')
    
    metric_list <- c(metric_list, 'time')
    
    # for master
    query <- "select mean(*)
              from cluster
              where time > now() - %s
              group by time(%s)
              fill(none)"
    
    query <- sprintf(query, period, group_by)
    
    res_master <- influx_query(con,
                               db = dbname,
                               query = query,
                               simplifyList = T,
                               return_xts = F)[[1]]
    
    df <- as.data.frame(res_master)
    
    names(df) <- gsub('mean_', '', names(df))
    
    res <- extract_field(df)
    
    res <- res[, names(res) %in% metric_list]
    
    names(res) <- paste0(names(res), '_master')
    
    names(res) <- gsub('time_master', 'time', names(res))
    
    # for worker
    if (length(worker_list) > 0) {
        
        query <- "select mean(*)
                  from host
                  where host_ip = '%s' and time > now() - %s
                  group by time(%s)
                  fill(none)"
        
        for (ip in worker_list) {
            
            query <- sprintf(query, ip, period, group_by)
            
            res_worker <- influx_query(con,
                                       db = dbname,
                                       query = query,
                                       simplifyList = T,
                                       return_xts = F)[[1]]
            
            df <- as.data.frame(res_worker)
            
            names(df) <- gsub('mean_', '', names(df))
            
            df <- extract_field(df)
            
            df <- df[, names(df) %in% metric_list]
            
            worker_name <- switch(ip,
                                  '192.168.0.162' = '_pub',
                                  '192.168.0.163' = '_pri_1',
                                  '192.168.0.164' = '_pri_2',
                                  '192.168.0.165' = '_pri_3',
                                  '192.168.0.166' = '_pri_4',
                                  '192.168.0.167' = '_pri_5',
                                  '192.168.0.168' = '_pri_6')
            
            names(df) <- paste0(names(df), worker_name)
            
            time_name <- paste0('time', worker_name)
            
            names(df) <- gsub(time_name, 'time', names(df))
            
            res <- inner_join(res, df,
                              by = 'time')
        }
    }
    
    res <- subset(res, select = -time)
    
    return(res)
    
}

correlation_D3 <- function(mtx, lag = 5) {
    
    node_df <- data.frame('node' = colnames(mtx),
                          'idx' = 0:(ncol(mtx) - 1),
                          'size' = 0,
                          stringsAsFactors = F)
    
    link_df <- data.frame('source' = NA, 'target' = NA, 'edge' = NA)
    
    idx <- 1
    
    for (i in 1:nrow(node_df))
    {
        for (j in 1:nrow(node_df))
        {
            if (i == j) {next}
            else
            {
                x <- -diff(as.matrix(log(mtx[, node_df$node[i]] + 1e-6)))
                y <- -diff(as.matrix(log(mtx[, node_df$node[j]] + 1e-6)))
                
                for (k in 1:lag)
                {
                    
                    g_test <- grangertest(x, y, k)
                    
                    if (g_test$`Pr(>F)`[2] <= 0.05)
                    {
                        link_df[idx, ] <- c(i - 1, j - 1, lag + 1 - k)
                        idx <- idx + 1
                        break
                    }
                }
            }
        }
    }
    
    for (i in node_df$idx) {
        
        node_df$size[i + 1] <- sum(link_df$source == i)
        
    }
    
    net <- forceNetwork(Links = link_df,
                        Nodes = node_df,
                        Source = 'source',
                        Target = 'target',
                        NodeID = 'node',
                        Group = 'node',
                        linkDistance = 200,
                        zoom = T,
                        fontSize = 10,
                        opacityNoHover = T,
                        legend = T,
                        arrows = T,
                        opacity = 1,
                        Value = 'edge',
                        Nodesize = 'size')
    
    return(net)
    
}

load_single_metric <- function(measurement, metric, period, group_by) {
  # For forecasting, anomaly detection, read only one metric
  
  con <- connect()
  
  connector <- con$connector
  
  dbname <- con$dbname
  
  period <- paste0(period, 'd')
  
  query = "select mean(${metric}) as metric \
           from ${measurement} \
           where time > now() - ${period} \
           group by time(${group_by}) \
           fill(none)"
  
  query <- str_interp(query)
  
  raw_data <- influx_query(connector,
                           db = dbname,
                           query = query,
                           simplifyList = T,
                           return_xts = F)[[1]]
  
  tb <- subset(raw_data, select = c(time, metric))
  
  ts <- seq.POSIXt(min(tb$time),
                   max(tb$time),
                   by = posixt_helper_func(str_sub(group_by, -1)))
  
  df <- tibble(time = ts)
  
  tb <- full_join(df, tb)
  
  tb$metric <- na.approx(tb$metric)
  
  names(tb) <- c("ds", "y")
  
  return(tb)
  
}

forecasting <- function(tb_, group_by, pred_period, changepoint.prior.scale = 0.01) {
  # pred_period : how long predict
  
  model <- prophet(tb_,
                   changepoint.prior.scale = changepoint.prior.scale)
  
  if (str_sub(group_by, -1) == 's') {
    
    freq <- as.integer(str_sub(group_by, end = -2))
    
  } else if (str_sub(group_by, -1) == 'm') {
    
    freq <- as.integer(str_sub(group_by, end = -2)) * 60
    
  } else {
    
    freq <- as.integer(str_sub(group_by, end = -2)) * 60 * 60
    
  }
  
  future <- make_future_dataframe(model,
                                  periods = pred_period,
                                  freq = freq)
  
  fcst <- predict(model, future)
  
  res <- list('model' = model,
              'forecast' = fcst)
  
  return(res)
  
}

load_metric_list <- function(measurement) {
  
  connector <- connect()
  
  con <- connector$connector
  
  dbname <- connector$dbname
  
  query <- 'show field keys from %s' %>% sprintf(measurement)
  
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


load_metric_value <- function(measurement) {
  
  connector <- connect()
  
  con <- connector$con
  
  dbname <- connector$dbname
  
  tag <- switch(
    measurement,
    'cluster' = 'master',
    'host' = 'host_ip',
    'task' = 'task',
    stop('incorrect measurement'))
  
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


draw_forecast_dygraph <- function(tb_, fcst, maxDate) {
  
  fcst[fcst$ds <= maxDate,]$yhat_lower <- NA
  
  fcst[fcst$ds <= maxDate,]$yhat_upper <- NA
  
  fcst[fcst$ds <= maxDate,]$yhat <- NA
  
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
    dyRangeSelector(height = 50)
  
}

