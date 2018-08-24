#### library ####
bs.Library <- function(pkg, add = T) {
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  
  if (length(new.pkg))
    
    install.packages(new.pkg, dependencies = T)
  
  if (add == TRUE)
    
    sapply(pkg, require, character.only = TRUE)
  
}

bs.Library(c('prophet', 'tidyverse', 'xts', 'influxdbr', 'zoo',
             'data.table', 'jsonlite', 'RMySQL'))

get_agent_id <- function(id=ID,
                         user = MYSQL_USER,
                         password = MYSQL_PASSWORD,
                         dbname = MYSQL_DBNAME,
                         host = MYSQL_HOST,
                         port = MYSQL_PORT) {
  return(5)
  con <- dbConnect(MySQL(), 
                   user = user, 
                   password = password,
                   dbname = dbname,
                   host = host, 
                   port = port)
  
  query <- "select agent_id
            from nexclipper_user
            where user_id = '%s'" %>% 
    sprintf(id)
  
  cat('\n', query, '\n')
  
  res <- dbGetQuery(con, query)
  
  return(res$agent_id)
  
}



load_disk_used_percent <- function(agent_id = AGENT_ID,
                                   mount_name = MOUNT_NAME,
                                   con = CONN,
                                   dbname = INFLUX_DBNAME) {
  
  if (is.null(mount_name)) {
    
    mount_name <- ''
    
  } else {
    
    mount_name <- "and mount_name = '%s'" %>% sprintf(mount_name)
    
  }
  
  query <- "select mean(*)
            from host_disk
            where time > now() - 21d and
                  agent_id = '%s' %s
            group by time(1h), agent_id, mount_name, host_name
            fill(linear)" %>% 
    sprintf(agent_id, mount_name)
  
  cat('\n', query, '\n')
  
  res <- influx_query(con,
                      dbname,
                      query,
                      timestamp_format = 'h',
                      return_xts = F)[[1]] %>% 
    as.data.table()
  
  res <- res[, .(ds = time, host_name, y = mean_used_percent)] %>% 
    setkey(ds) %>% 
    dcast(ds ~ host_name, value.var = c('y'))
  
  ts <- seq.POSIXt(min(res$ds),
                   max(res$ds),
                   by = '1 hour')
  
  df <- data.table(ds = ts, key = 'ds')
  
  disk <- df[res]
  
  disk[disk < 0] <- NA
  
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
    split(by = 'host_name') %>% 
    return()
  
}

handling_disk_data <- function(data_, cut_) {
  
  data__ <- data_[, .(ds, y)]
  
  trainM <- data__[, ':='(CR = y / shift(y, n = 1, type = "lag"),
                          Diff = y - shift(y, n = 1, type = "lag"))]
  
  trainM[, cutYN := ifelse(CR < cut_, 1, 0)]
  
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

diskForecasting <- function(train_,
                            pred_period = 30,
                            changepoint.range = 0.7,
                            changepoint.prior.scale = 0.2) {
  
  model <- prophet(train_,
                   changepoint.range = changepoint.range,
                   changepoint.prior.scale = changepoint.prior.scale)
  
  future <- make_future_dataframe(model,
                                  periods = 24 * pred_period,
                                  freq = 3600)
  
  predict(model, future) %>%
    as.data.table(key = 'ds') %>%
    .[, .(ds, yhat)] %>% 
    return()
  
}

add_DFT <- function(dt, THRESHOLD) {
  
  dt[, DFT := 0]
  
  idx_ <- which(dt$yhat > THRESHOLD)
  
  if (length(idx_) != 0)
    
    dt[min(idx_), 'DFT'] <- 1
  
  return(dt)
  
}

