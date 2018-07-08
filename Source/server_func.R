
#### COMMON FUNCTION ####

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

load_metric_from_host <- function(period, groupby, host_list, metric_list) {
  
  # metric_list <- c("cpu_idle_per", "cpu_idle_percent")
  # host_list <- c("192.168.0.160", "192.168.0.161")
  # period = 5
  
  # groupby = "1h"
  
  connector <- connect()
  con <- connector$connector
  dbname <- connector$dbname
  
  period <- paste0(period, 'd')
  
  
  #### host ####
  query <- "select mean(*)
  from %s
  where time > now() - %s
  group by time(%s), host_ip
  fill(none)"
  
  query <- sprintf(query, 
                   'host', 
                   period,
                   groupby)
  
  res_host <- influx_query(con,
                           db = dbname,
                           query = query,
                           simplifyList = T,
                           return_xts = F)[[1]]
  
  names(res_host) <- gsub('mean_', '', names(res_host))
  
  res_host <- res_host %>% filter(host_ip %in% host_list) %>% 
    select(c('time', 'host_ip', metric_list)) 
  
  ts <- seq.POSIXt(min(res_host$time),
                   max(res_host$time),
                   by = posixt_helper_func(str_sub(groupby, -1)))
  
  df <- tibble(time = ts)
  
  res_host <- full_join(df, res_host)
  
  res_host <- gather(res_host, var, value, -(1:2)) %>% 
    unite(var_new, host_ip, var) %>% 
    spread(var_new, value)
  
  
  print('Host OK!')
  
  return(res_host)
  
}

load_metric_from_cluster <- function(period, groupby, metric_list) {
  
  # metric_list <- list()
  # metric_list$cluster <- c("cpu_used", "mem_used_percent") 
  # metric_list$cluster <- NULL
  # 
  # period = 5
  # groupby = "1h"
  
  connector <- connect()
  con <- connector$connector
  dbname <- connector$dbname
  
  period <- paste0(period, 'd')
  
  
  # browser()
  
  
  #### Cluster ####
  query <- "select mean(*)
  from %s
  where time > now() - %s
  group by time(%s)
  fill(none)"
  
  query <- sprintf(query, 
                   'cluster', 
                   period,
                   groupby)
  
  res_cluster <- influx_query(con,
                              db = dbname,
                              query = query,
                              simplifyList = T,
                              return_xts = F)[[1]]
  
  names(res_cluster) <- gsub('mean_', '', names(res_cluster))
  
  res_cluster <- res_cluster %>% select(c('time', metric_list))
  
  ts <- seq.POSIXt(min(res_cluster$time),
                   max(res_cluster$time),
                   by = posixt_helper_func(str_sub(groupby, -1)))
  
  df <- tibble(time = ts)
  
  res_cluster <- full_join(df, res_cluster)
  
  names(res_cluster) <- paste0(names(res_cluster), '_cluster')
  
  names(res_cluster) <- gsub('time_cluster', 'time', names(res_cluster))
  print('Cluster OK!')
  
  return(res_cluster)
  
}


load_metric_from_host <- function(period, groupby, host_list, metric_list) {
  
  metric_list <- c("cpu_idle_per", "cpu_idle_percent")
  host_list <- c("192.168.0.160", "192.168.0.161")
  period = 5
  
  groupby = "1h"
  
  connector <- connect()
  con <- connector$connector
  dbname <- connector$dbname
  
  period <- paste0(period, 'd')
  
  
  #### host ####
  query <- "select mean(*)
            from %s
            where time > now() - %s
            group by time(%s), host_ip
            fill(none)"
  
  query <- sprintf(query, 
                   'host', 
                   period,
                   groupby)
  
  res_host <- influx_query(con,
                           db = dbname,
                           query = query,
                           simplifyList = T,
                           return_xts = F)[[1]]
  
  names(res_host) <- gsub('mean_', '', names(res_host))
  
  res_host <- res_host %>% filter(host_ip %in% host_list) %>% 
    select(c('time', 'host_ip', metric_list)) 
  
  
  # browser()
  ts <- seq.POSIXt(min(res_host$time),
                   max(res_host$time),
                   by = posixt_helper_func(str_sub(groupby, -1)))
  
  df <- tibble(time = ts)
  
  res_host <- full_join(df, res_host)
  
  res_host <- gather(res_host, var, value, -(1:2)) %>% 
    unite(var_new, host_ip, var) %>% 
    spread(var_new, value)
  
  
  print('Host OK!')
  
  return(res_host)
  
}


load_metric_from_task <- function(period, groupby, host_list, metric_list) {
  
  # metric_list <- c("cpu_used_percent", "mem_used_percent")
  # host_list <- c('kafka', 'agent.nexcloud')
  # period = 5
  # 
  # groupby = "1h"
  
  connector <- connect()
  con <- connector$connector
  dbname <- connector$dbname
  
  period <- paste0(period, 'd')
  
  
  #### host ####
  query <- "select mean(*)
            from %s
            where time > now() - %s
            group by time(%s), task, node_ip
            fill(none)"
  
  query <- sprintf(query, 
                   'task', 
                   period,
                   groupby)
  
  res_task <- influx_query(con,
                           db = dbname,
                           query = query,
                           simplifyList = T,
                           return_xts = F)[[1]]
  
  names(res_task) <- gsub('mean_', '', names(res_task))
  
  res_task <- res_task %>%
    filter(task %in% host_list) %>% 
    select(c('time', 'task', 'node_ip', metric_list)) 
  
  
  # browser()
  ts <- seq.POSIXt(min(res_task$time),
                   max(res_task$time),
                   by = posixt_helper_func(str_sub(groupby, -1)))
  
  df <- tibble(time = ts)
  
  res_task <- full_join(df, res_task)
  
  res_task <- gather(res_task, var, value, -(1:3)) %>% 
    unite(var_new, task, node_ip, var) %>% 
    spread(var_new, value)
  
  print('Task OK!')
  
  return(res_task)
  
}


load_multiple_metric <- function(period, groupby,
                                 host_list, metric_list) {
  
  connector <- connect()
  
  con <- connector$connector
  
  dbname <- connector$dbname
  
  
  cluster_metric <- load_metric_from_cluster(period = period,
                                             groupby = groupby,
                                             metric_list = metric_list$cluster)

  
  whole_data <- cluster_metric
  
  if (!is.null(metric_list$host)) {
    
    host_metric <- load_metric_from_host(period = period,
                                         groupby = groupby,
                                         host_list = host_list$host,
                                         metric_list = metric_list$host)
    
    whole_data <- inner_join(whole_data, host_metric, by = 'time')
    
  }
  
  if (!is.null(metric_list$task)) {
    
    task_metric <- load_metric_from_task(period = period,
                                         groupby = groupby,
                                         host_list=host_list$task,
                                         metric_list = metric_list$task)
    
    whole_data <- inner_join(whole_data, task_metric, by = 'time')
    
  }
  # if(!is.null(task_metric)) whole_data <- merge(whole_data, task_metric)
  
  # task_metric <- load_metric_from_task()
  
  
  # #### Task ####
  # for (task_name in host_list$task) {
  #   print(task_name)
  #   query <- sprintf(query,
  #                    'task',
  #                    period, 'task', task_name,
  #                    groupby)
  #   
  #   res_task <- influx_query(con,
  #                            db = dbname,
  #                            query = query,
  #                            simplifyList = T,
  #                            return_xts = F)[[1]]
  #   
  #   names(res_task) <- gsub('mean_', '', names(res_task))
  #   
  #   res_task <- res_task %>%
  #     select(c('time', metric_list$task))
  #   
  #   names(res_task) <- paste(names(res_task), task_name, sep = '_')
  #   
  #   time_name <- paste('time', task_name, sep = '_')
  #   
  #   names(res_task) <- gsub(time_name, 'time', names(res_task))
  #   
  #   res_cluster <- left_join(res_cluster, res_task,
  #                            by = 'time')
  #   
  # }
  # 
  # res_cluster <- res_cluster %>% 
  #   subset(select = -time)
  # 
  # res_cluster <- res_cluster[complete.cases(res_cluster),]
  # 
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


load_tag_list <- function(measurement) {
  
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



#### FORECAST ####

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
    dyRangeSelector(height = 30)
  
}


render_forecast <- function(resource, metric, period, groupby) {
  
  tb_ <- load_single_metric(resource, metric, period, groupby)
  
  forecast_result <- forecasting(tb_, groupby, 48)
  
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

anomalization <- function(tb_,
                          frequency = 'auto',
                          method = 'stl',
                          trend = 'auto') {
  
  decomposed <- time_decompose(tb_,
                               y,
                               method = method,
                               frequency = frequency,
                               trend = trend)
  
  anomalized <- anomalize(decomposed,
                          remainder,
                          method = 'gesd',
                          alpha = 0.05,
                          max_anoms = 0.2,
                          verbose = F)
  
  recomposed <- time_recompose(anomalized)
  
  return(recomposed)
  
}




#### METRIC CASUALITY ####

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
