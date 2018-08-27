# .libPaths('C:/Users/yunseop/Documents/R/win-library/3.5')


#### library ####
library(prophet)
library(tidyverse)
library(xts)
library(influxdbr)
library(zoo)
library(dygraphs)
library(data.table)
library(htmltools)


#### read data ####
con <- influx_connection(host = '13.77.154.37',
                         port = 10091)

dbname <- 'nexclipper'

# query <- "select mean(*)
#           from host_disk
#           where time >= '2018-08-01T00:00:00Z' and
#                 time < '2018-08-21T00:00:00Z' and
#                 agent_id = '5' and
#                 mount_name = '/'
#           group by time(1h), agent_id, mount_name, host_name
#           fill(linear)"

query <- "select mean(*)
          from host_disk
          where time > now() - 22d and
                time < now() -1d and
                agent_id = '5' and
                mount_name = '/'
          group by time(1h), agent_id, mount_name, host_name
          fill(linear)"

res <- influx_query(con,
                    dbname,
                    query,
                    timestamp_format = 'h',
                    return_xts = F)[[1]] %>% 
  select(time, host_name, mean_used_percent) %>% 
  spread(host_name, mean_used_percent, -1)

res <- influx_query(con,
                    dbname,
                    query,
                    timestamp_format = 'h',
                    return_xts = F)[[1]] %>% 
  as.data.table()

res[, .(ds = time, host_name, y = mean_used_percent)] %>% 
  dcast(ds ~ host_name, value.var = c('y'))

load_disk_used_percent <- function(agent_id = 5, mount_name = NULL,
                                   groupby = '1h') {
  # agent_id=5;mount_name='/';groupby='1h'
  con <- influx_connection(host = '13.77.154.37',
                           port = 10091)
  
  dbname <- 'nexclipper'
  
  if (is.null(mount_name)) {
    
    mount_name <- ''
    
  } else {
    
    mount_name <- "and mount_name = '%s'" %>% sprintf(mount_name)
    
  }
  
  query <- "select mean(*)
            from host_disk
            where time > now() - 22d and
                  time < now() -1d and
                  agent_id = '%s' %s
            group by time(%s), agent_id, mount_name, host_name
            fill(linear)" %>% 
    sprintf(agent_id, mount_name,
            groupby)
  
  cat('\n', query, '\n')
  
  res <- influx_query(con,
                      dbname,
                      query,
                      timestamp_format = 'h',
                      return_xts = F)[[1]] %>% 
    # select(time, host_name, mean_used_percent) %>% 
    # rename(ds = time, y = mean_used_percent) %>% 
    # spread(host_name, y, -1)
    as.data.table()
  
  res <- res[, .(ds = time, host_name, y = mean_used_percent)] %>% 
    setkey(ds) %>% 
    dcast(ds ~ host_name, value.var = c('y'))
    
  
  ts <- seq.POSIXt(min(res$ds),
                   max(res$ds),
                   by = '1 hour')
  
  # df <- tibble(ds = ts)
  df <- data.table(ds = ts, key = 'ds')
  
  # disk <- full_join(df, res)
  disk <- df[res]
  
  disk[disk < 0] <- NA
  
  # disk %>% 
  #   select(-ds) %>% 
  #   sapply(function(x) na.fill(na.approx(x, na.rm = F), 'extend')) %>%
  #   as.data.frame() %>% 
  #   cbind(ds = df$ds) %>% 
  #   select(ds, everything()) %>%
  #   gather(host_name, y, -1) %>% 
  #   as.data.table() %>% 
  #   setkey(ds) %>% 
  #   return()
  disk[, lapply(.SD,
                function(x) na.fill(na.approx(x, na.rm = F),
                                    'extend')),
       .SDcols = 2:length(disk)] %>% 
    cbind(df[, 'ds']) %>% 
    setcolorder(c(ncol(.), 1:(ncol(.) - 1))) %>% 
    melt(id.vars = 1,
         measure.vars = 2:ncol(.),
         variable.name = 'host_name',
         value.name = 'y') %>%
    setkey(ds) %>%
    return()
  
}

raw_data <- load_disk_used_percent(mount_name = '/')

#### descriptive statistics ####
# raw_data %>% 
#   select(-ds) %>% 
#   group_by(host_name) %>% 
#   summarise('mean_' = mean(y),
#             'sd_' = sd(y),
#             'min_' = min(y),
#             'max_' = max(y)) %>%
#   View('summary for original disk data')
raw_data[, .(mean_ = mean(y),
             sd_ = sd(y),
             min_ = min(y),
             max_ = max(y),
             median_ = median(y)),
         by = host_name] %>%
  View('summary for original disk data')
#----

#### split into train, test ####
cut_date <- '2018-08-16' %>% 
  as.POSIXct(tz = 'GMT')

train <- raw_data[ds < cut_date] %>% 
  split(by = 'host_name')

test <- raw_data[ds >= cut_date] %>% 
  split(by = 'host_name')

#----

# train_master <- split(train[, c('ds', 'y')], train$host_name)
# test_master <- split(test[, c('ds', 'y')], test$host_name)$master

handling_disk_data <- function(data_, cut = 0.9) {
  
  data__ <- data_[, .(ds, y)] %>% copy()
  
  trainM <- data__[, ':='(CR = y / shift(y, n = 1, type = "lag"),
                          Diff = y - shift(y, n = 1, type = "lag"))]
  
  trainM[, cutYN := ifelse(CR < cut, 1, 0)]
  
  trainM$cutYN[1] = 0
  
  setorder(trainM, -ds)
  
  yRecenct <- trainM$y[1]
  
  trainM <- trainM[cutYN != 1]
  
  trainM[, cumdiff := shift(cumsum(Diff), n = 1, type = 'lag')]
  
  trainM$cumdiff[1] = 0
  
  trainM[, ':='(New = yRecenct - cumdiff,
                New_ds = seq.POSIXt(from = max(ds),
                                    length.out = .N,
                                    by = '-1 hour'))] %>% 
    select(New_ds, New) %>% 
    rename(ds = New_ds, y = New) %>%
    setorder(ds) %>%
    return()
  
}

# handled_train_master <- handling_disk_data(train_master)
# handled_test_master <- handling_disk_data(test_master)
cut_ <- 0.93

handled_train <- train %>% copy() %>% 
  lapply(function(dt) handling_disk_data(dt, cut_))

# handled_test <- test %>% 
#   lapply(function(dt) handling_disk_data(dt))

handled_raw_data <- raw_data %>% 
  split(by = 'host_name') %>% 
  lapply(function(dt) handling_disk_data(dt))

# compare original with pre-processed data
compare_A_B <- function(A, B, idx = 1) {
  
  before <- xts(A[[idx]]$y,
                order.by = A[[idx]]$ds)
  
  names(before) <- 'before'
  
  after <- xts(B[[idx]]$y,
               order.by = B[[idx]]$ds)
  
  names(after) <- 'after'
  
  main <- "%s : Before & After" %>% 
    sprintf(switch(idx,
                   'master',
                   'private1',
                   'private2',
                   'private3',
                   'private4',
                   'private5',
                   'public'))
  
  cbind(before, after) %>% 
    dygraph(main = main) %>%
    dyRangeSelector(height = 30)# %>% 
    # dyOptions(useDataTimezone = T)
  
}

compare_A_B(train, handled_train, 3)

raw_data %>% 
  split(by = 'host_name') %>% 
  compare_A_B(handled_raw_data, 2)

diskForecasting <- function(train_, idx = 1, pred_period = 30,
                            changepoint.range = 0.7,
                            changepoint.prior.scale = 0.2) {
  
  model <- prophet(train_[[idx]][, .(ds, y)],
                   changepoint.range = changepoint.range,
                   changepoint.prior.scale = changepoint.prior.scale)
  
  future <- make_future_dataframe(model,
                                  periods = 24 * pred_period,
                                  freq = 3600)
  
  predict(model, future) %>%
    as.data.table(key = 'ds') %>%
    return()
  
}

# N_pred <- diskForecasting(train, idx)
# 
# A_pred <- diskForecasting(handled_train, idx)
# # train[[idx]][handled_train[[idx]]]
# N_eval <- evalPred(N_pred, test, idx)[, .(ds, B_cumRMSE = cumRMSE)]
# 
# A_eval <- evalPred(A_pred, test, idx)[, .(ds, A_cumRMSE = cumRMSE)]
# 
# evaluation <- N_eval[A_eval]
# 
# draw_forecast_disk(N_pred, A_pred, actual, evaluation, cut_date, idx)

Draw_forecast_disk <- function(N_dt, A_dt, actual, idx) {
  
  N_pred <- diskForecasting(N_dt, idx)
  
  A_pred <- diskForecasting(A_dt, idx)
  # train[[idx]][handled_train[[idx]]]
  N_eval <- evalPred(N_pred, test, idx)[, .(ds, B_cumRMSE = cumRMSE)]
  
  A_eval <- evalPred(A_pred, test, idx)[, .(ds, A_cumRMSE = cumRMSE)]
  
  evaluation <- N_eval[A_eval]
  
  draw_forecast_disk(N_pred, A_pred, actual, evaluation, cut_date, idx)
  
}


cut_ <- 0.95

actual <- raw_data %>% split(by = 'host_name')

handled_train <- train %>% copy() %>% 
  lapply(function(dt) handling_disk_data(dt, cut_))

compare_A_B(train, handled_train, 7)

idx <- 6

Draw_forecast_disk(train, handled_train, actual, idx)

draw_forecast_disk <- function(before, after, actual,
                               evaluation, cut_date, idx = 1) {
  
  before_ <- before %>% copy() %>%
    .[, .(ds,
          B_yhat = yhat,
          B_yhat_lower = yhat_lower,
          B_yhat_upper = yhat_upper)] %>% 
    setkey(ds)
  
  after_ <- after %>% copy() %>%
    .[, .(ds,
          A_yhat = yhat,
          A_yhat_lower = yhat_lower,
          A_yhat_upper = yhat_upper)] %>% 
    setkey(ds)
  
  actual_ <- actual[[idx]][, -2, with = F] %>% copy() %>% 
    .[, .(ds, actual_value = y)]
  
  main <- "forecast %s disk used percent" %>% 
    sprintf(switch(idx,
                   'master',
                   'private1',
                   'private2',
                   'private3',
                   'private4',
                   'private5',
                   'public'))
  
  # maxDate <- actual_$ds %>% max()
  maxDate <- cut_date
  
  before_[ds < maxDate, c('B_yhat', 'B_yhat_lower', 'B_yhat_upper')] <- NA
  
  after_[ds < maxDate, c('A_yhat', 'A_yhat_lower', 'A_yhat_upper')] <- NA
  
  dt_ <- evaluation[actual_][before_][after_]
  
  delete = c('B_yhat_lower', 'B_yhat_upper', 'A_yhat_lower', 'A_yhat_upper')
  
  dt_[, (delete) := NULL]
  
  series <- xts(dt_[, -1, with = F],
                order.by = dt_$ds,
                tzone = Sys.getenv("TZ"))
  
  dygraph(series, main = main) %>%
    dySeries("actual_value", label = "Original", color = 'purple', strokeWidth = 2) %>%
    # dySeries(c("B_yhat_lower", "B_yhat", "B_yhat_upper"),
    #          label = "Before_Predicted") %>%
    # dySeries(c("A_yhat_lower", "A_yhat", "A_yhat_upper"),
    #          label = "After_Predictd") %>%
    # dyAxis("y", valueRange = c(0, 120)) %>% 
    dySeries('B_yhat', 'Not applied Predicted', strokeWidth = 2, color = 'green') %>% 
    dySeries('A_yhat', 'Applied Predicted', strokeWidth = 2, color = 'blue') %>% 
    dySeries('B_cumRMSE', 'Not Applied RMSE', color = 'green', strokePattern = 'dashed', axis = 'y2') %>% 
    dySeries('A_cumRMSE', 'Applied RMSE', color = 'blue', strokePattern = 'dashed', axis = 'y2') %>% 
    dyRangeSelector(height = 30)
  # 
  # if (is.null(test_)) {
  #   
  #   list('graph' = d,
  #        'model' = model,
  #        'prediction' = fcst_) %>% return()
  #   
  # } else {
  #   
  #   evaluation <- evalPred(fcst_, test_, idx)
  #   
  #   series_ <- series %>%
  #     as.data.table() %>% 
  #     setnames('index', 'ds') %>%
  #     setkey(ds)
  #   
  #   series_ <- test_[[idx]][, .(ds, test = y)] %>% 
  #     .[series_]
  #   
  #   series_ <- xts(series_[, .(hist, yhat, yhat_lower, yhat_upper, test)],
  #                  order.by = series_$ds,
  #                  tzone = Sys.getenv("TZ"))
  #   
  #   dd <- list(
  #     dygraph(series_, main = main) %>%
  #       dySeries(c("hist"), label = "Train") %>%
  #       dySeries(name = c("yhat_lower", "yhat", "yhat_upper"),
  #                label = "Predictd") %>%
  #       dySeries('test', label = 'Test') %>% 
  #       dyRangeSelector(height = 30),
  #     dygraph(evaluation, main = 'Evaluation : RMSE') %>% 
  #       dyAxis("y", label = "RMSE", independentTicks = TRUE) %>%
  #       # dyAxis("y2", label = "R-squared", independentTicks = TRUE) %>%
  #       dySeries('cumRMSE', axis = 'y') %>% 
  #       # dySeries('cumR2', axis = 'y2') %>% 
  #       dyRangeSelector(height = 30) ) %>% 
  #     tagList() %>% 
  #     browsable()
  #   
  #   list('graph' = dd,
  #        'model' = model,
  #        'prediction' = fcst_,
  #        'evaluation' = evaluation) %>% return()
  # }
}


diskForecasting(handled_raw_data, idx = 2)

raw_data %>% 
  split(by = 'host_name') %>% 
  diskForecasting(idx = 2)


evalPred <- function(fcst_, test_, idx, R_sq = F) {
  
  fcst_ <- fcst_ %>%
    select(ds, yhat) %>%
    as.data.table(key = 'ds')
  
  RMSE_DT(test_[[idx]], fcst_) %>% return()
  
  # test_[[idx]][, .(ds, y)] %>% 
  #   .[fcst_, nomatch = 0] %>% 
  #   .[, .(index_ = 1:.N,
  #         ds,
  #         cumTot = cumsum((y - mean(y))^2),
  #         cumRes = cumsum((y - yhat)^2))] %>% 
  #   .[, .(ds,
  #         cumRMSE = cumRes / index_,
  #         cumR2 = 1 - (cumRes / cumTot))] %>% 
  #   return()

}

RMSE_DT <- function(actual, predicted) {
  
  
  actual[, .(ds, y)] %>% 
    .[predicted, nomatch = 0] %>% 
    .[, .(index_ = 1:.N,
          ds,
          cumTot = cumsum((y - mean(y))^2),
          cumRes = cumsum((y - yhat)^2))] %>% 
    .[, .(ds,
          cumRMSE = cumRes / index_,
          cumR2 = 1 - (cumRes / cumTot))] %>% 
    .[, cumR2 := NULL] %>% 
    return()
    
}

#### pre-processing ####
change <- disk[-1, -1] / disk[-nrow(disk), -1]

rownames(change) <- NULL

ind <- change %>% 
  apply(2, function(x) which(x < 0.9) + 1)

disk_cp <- disk

disk_cp <- ind %>% 
  seq_along() %>% 
  lapply(function(i) {
    
    a <- c(1, diff(ind[[i]]) != 1) %>%
      cumsum() %>%
      as.integer()
    
    dx <- disk[, -1] %>%
      select(i) %>% apply(2, diff)
    
    partition <- split(ind[[i]], a)
    
    partition %>% 
      seq_along() %>% 
      sort(T) %>% 
      lapply(function(y, j) {
        
        if (length(partition[[j]]) == 0) return(disk_cp[, i + 1])
        
        partition[[j]] %>% 
          sort(T) %>% 
          lapply(function(k) {
            
            disk_cp[1:k - 1, i + 1] <<- disk_cp[1:k - 1, i + 1] + dx[k - 1]
            
          })
        
      }, y = partition)
    
    disk_cp[, i + 1]
    
  })

names(disk_cp) <- names(disk)[-1]

disk_compare <- disk_cp %>%
  as.data.frame() %>% 
  cbind(ds = disk$time) %>% 
  select(ds, everything()) %>% 
  as.tibble()



disk_cp <- disk_compare

for (i in 1:length(ind)) {
  
  disk_cp[ind[[i]], i + 1] <- NA
  
}

tb <- disk_cp %>% 
  gather(key, y, -1) %>% 
  na.omit() %>% 
  split(.$key) %>% 
  lapply(function(x, t) {
    
    x <- select(x, -key)
    
    x$ds <- seq.POSIXt(from = t,
                         length.out = nrow(x),
                         by = '-1 hour') %>%
      sort.POSIXlt()
    x}, t = max(disk$time))


#### forecast ####
forecast_disk <- function(tb_, idx, changepoint.range = 0.7,
                          changepoint.prior.scale = 0.2) {
  
  model <- prophet(tb_[[idx]],
                   changepoint.range = changepoint.range,
                   changepoint.prior.scale = changepoint.prior.scale)
  
  future <- make_future_dataframe(model,
                                  periods = 24 * 14,
                                  freq = 3600)
  
  fcst <- predict(model, future)
  
  main <- "forecast %s disk used percent" %>% 
    sprintf(switch(idx,
                   'master',
                   'private1',
                   'private2',
                   'private3',
                   'private4',
                   'private5',
                   'public'))
  
  maxDate <- max(tb_[[idx]]$ds)
  
  fcst[fcst$ds < maxDate,]$yhat_lower <- NA
  
  fcst[fcst$ds < maxDate,]$yhat_upper <- NA
  
  fcst[fcst$ds < maxDate,]$yhat <- NA
  
  series0 <- xts(tb_[[idx]]$y,
                 order.by = tb_[[idx]]$ds,
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
  
  dygraph(series, main = main) %>%
    dySeries(c( "hist"), label = "Actual") %>%
    dySeries(c( "yhat_lower", "yhat", "yhat_upper"), label = "Predictd") %>%
    dyRangeSelector(height = 30)
  
}

idx <- 1

forecast_disk(tb, idx)


