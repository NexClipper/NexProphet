
#### library ####
bs.Library <- function(pkg, add = T) {
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  
  if (length(new.pkg))
    
    install.packages(new.pkg, dependencies = T)
  
  if (add == TRUE)
    
    sapply(pkg, require, character.only = TRUE)
  
}

bs.Library(c('prophet', 'tidyverse', 'xts', 'influxdbr', 'zoo',
             'data.table', 'jsonlite', 'RMySQL', 'slackr', 'scales',
             'lubridate'))


#### CONSTANT ####
envir_list <- Sys.getenv(c('ID', 'THRESHOLD', 'CRITICAL', 'WARNING',
                           'PERIOD'))

ID <- envir_list['ID']

THRESHOLD <- envir_list['THRESHOLD'] %>% as.integer()

CRITICAL <- envir_list['CRITICAL'] %>% as.integer()

WARNING <- envir_list['WARNING'] %>% as.integer()

PERIOD <- envir_list['PERIOD'] %>% as.integer()

internal <- read_json('internal.conf')

INFLUX_HOST <- internal$influx_host

INFLUX_PORT <- internal$influx_port

INFLUX_DBNAME <- internal$influx_dbname

MYSQL_USER <- internal$mysql_user

MYSQL_PASSWORD <- internal$mysql_password

MYSQL_DBNAME <- internal$mysql_dbname

MYSQL_HOST <- internal$mysql_host

MYSQL_PORT <- internal$mysql_port

CUT <- internal$cut

CONN <- influx_connection(host = INFLUX_HOST,
                          port = INFLUX_PORT)

#----


#### functions ####
get_agent_id <- function(id = ID,
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
  
  dbDisconnect(con)
  
  return(res$agent_id)
  
}


AGENT_ID <- get_agent_id()


load_disk_used_percent <- function(agent_id = AGENT_ID,
                                   period = PERIOD,
                                   con = CONN,
                                   dbname = INFLUX_DBNAME) {
  
  query <- "select mean(used_percent) as y
            from host_disk
            where time > now() - %sd and
                  agent_id = '%s'
            group by time(1h), mount_name, host_ip
            fill(none)" %>% 
    sprintf(period, agent_id)
  
  cat('\n', query, '\n')
  
  influx_query(con,
               dbname,
               query,
               return_xts = F)[[1]] %>% 
    select(-1:-3) %>%
    group_by(time, host_ip, mount_name) %>%
    summarise('y' = mean(y, na.rm = T)) %>%
    as.data.table() %>% 
    setnames('time', 'ds') %>% 
    setkey('ds', 'host_ip', 'mount_name') %>% 
    merge(CJ(ds = seq.POSIXt(min(.$ds),
                             max(.$ds),
                             by = '1 hour'),
             host_ip = unique(.$host_ip),
             mount_name = unique(.$mount_name)),
          by = c('ds', 'host_ip', 'mount_name'),
          all = T) %>% 
    dcast(ds ~ host_ip + mount_name, value.var = c('y'), sep = '__') %>% 
    .[, .SD, .SDcols = .[, lapply(.SD, function(x) sum(is.na(x)) <= as.integer(length(x) * 0.10))] %>% 
        unlist() %>% 
        which()] %>% 
    .[, lapply(.SD, function(x) na.approx(x) %>% na.fill('extend'))] %>% 
    .[, ds := as_datetime(ds, origin = '1970-01-01', tz = 'Asia/Seoul')] %>% 
    setkey(ds) %>%
    melt(id.vars = 1,
         measure.vars = 2:ncol(.),
         variable.name = 'key',
         value.name = 'y') %>% 
    split(by = 'key') %>%
    return()
  
  # res %>% setkey('host_ip', 'mount_name') %>% 
  #   .[, list('NA' = sum(is.na(y)), 'length' = length(y)),
  #     by = c('host_ip', 'mount_name')] %>% View()
  # 
  # # res <- res[, .(ds = time + 9 * 60 * 60, host_name, y)] %>% 
  # #   setkey(ds) %>% 
  # #   dcast(ds ~ host_name, value.var = c('y'))
  # 
  # # ts <- seq.POSIXt(min(res$ds),
  # #                  max(res$ds),
  # #                  by = '1 hour')
  # # 
  # # df <- data.table(ds = ts, key = 'ds')
  # # 
  # # disk <- df[res]
  # # 
  # # disk[disk < 0] <- NA
  # 
  # disk[, devmaster := NULL]
  # 
  # disk[, lapply(.SD,
  #               function(x) na.fill(na.approx(x, na.rm = F),
  #                                   'extend')),
  #      .SDcols = 2:length(disk)] %>% 
  #   cbind(df[, 'ds']) %>% 
  #   setcolorder(c(ncol(.), 1:(ncol(.) - 1))) %>% 
  #   melt(id.vars = 1,
  #        measure.vars = 2:ncol(.),
  #        variable.name = 'host_name',
  #        value.name = 'y') %>%
  #   setkey(ds) %>%
  #   split(by = 'host_name') %>% 
  #   return()
  
}


handling_disk_data <- function(data_, cut_ = CUT) {
  
  data__ <- data_ %>% copy()
  
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
    .[, .(ds = New_ds, y = New)] %>% 
    setkey(ds) %>%
    .[data_[, .(ds, key, origin_y = y)]] %>% 
    .[!is.na(y)] %>% 
    setkey(ds) %>% 
    return()
  
}


diskForecasting <- function(train_,
                            pred_period = WARNING,
                            changepoint.range = 0.7,
                            changepoint.prior.scale = 0.2) {
  
  model <- prophet(train_,
                   changepoint.range = changepoint.range,
                   changepoint.prior.scale = changepoint.prior.scale)
  
  future <- make_future_dataframe(model,
                                  periods = 24 * pred_period,
                                  freq = 3600)
  
  train_[predict(model, future) %>%
    as.data.table(key = 'ds') %>%
    .[, .(ds, yhat)]] %>% 
    .[, yhat := ifelse(is.na(y), yhat, NA)] %>% 
    .[, ds := with_tz(ds, 'Asia/Seoul')] %>%
    return()
  
}


add_DFT <- function(dt,
                    threshold = THRESHOLD,
                    critical = CRITICAL) {
  
  dt[, ':='(DFT = -1, severity = 'null')]
  
  DFT_times <- dt[yhat > threshold, ds]
  
  current_time <- dt[!is.na(origin_y), ds] %>% max()
  
  if (length(DFT_times) != 0) {
    
    DFT_time <- min(DFT_times)
    
    dt[ds == DFT_time]$DFT <- dt[ds == DFT_time]$yhat
    
    if (DFT_time <= current_time + critical * 60 * 60) {
      
      dt[ds == DFT_time]$severity <- 'Critical'
      
    } else {
      
      dt[ds == DFT_time]$severity <- 'Warning'
      
    }
    
    dt <- dt[ds <= DFT_time,
             .(ds, origin_y, key, yhat,
               DFT = ifelse(DFT == -1, NA, DFT),
               severity = ifelse(severity == 'null', NA, severity))]
    
  } else {
    
    dt[, c('DFT', 'y', 'severity') := NULL]
    
  }
  
  return(dt)
  
}


save_result_mysql <- function(dt_,
                              agent_id = AGENT_ID,
                              user = MYSQL_USER,
                              password = MYSQL_PASSWORD,
                              dbname = MYSQL_DBNAME,
                              host = MYSQL_HOST,
                              port = MYSQL_PORT,
                              threshold = THRESHOLD,
                              critical = CRITICAL,
                              warning = WARNING) {
  
  if (!'DFT' %in% names(dt_))
    
    return()
  
  con <- dbConnect(MySQL(), 
                   user = user, 
                   password = password,
                   dbname = dbname,
                   host = host, 
                   port = port)
  
  # DFT <- NA
  # 
  # predicted <- NA
  # 
  # if (length(which(dt_$yhat > threshold)) != 0) {
  #   
  #   DFT <- dt_$ds[min(which(dt_$yhat > threshold))]
  #   
  #   predicted <- dt_$yhat[min(which(dt_$yhat > threshold))]
  #   
  # }
  # 
  # alertYN <- NA
  # 
  # if (!is.na(DFT))
  #   
  #   if (difftime(DFT, current_time, units = 'hours') <= alert) {
  #     
  #     alertYN <- T
  #     
  #   } else {alertYN <- F}
  
  current_time <- dt_[!is.na(origin_y), ds] %>% max()
  
  predicted_time <- dt_[!is.na(DFT), ds]
  
  severity <- dt_[ds == predicted_time, severity]
  
  key_ <- dt_[1, key] %>%
    as.character() %>%
    strsplit('__') %>% 
    unlist()
  
  target_ip <- key_[1]
  
  mount_name <- key_[2]
  
  cond_ <- switch(severity,
                  'Critical' = critical,
                  'Warning' = warning)
  
  condition <- sprintf('>%s and <%sd',
                       threshold, cond_)
  
  contents <- "[%s] The disk usage of mount path : '%s' for Host will exceed threshold" %>% 
    sprintf(target_ip, mount_name)
  
  info <- data.frame('agent_id' = agent_id,
                     'severity' = severity,
                     'target_system' = 'Host',
                     'target_ip' = target_ip,
                     'target' = 'Disk',
                     'metric' = 'used_percent',
                     'condition' = condition,
                     'id' = target_ip,
                     'start_time' = predicted_time,
                     'contents' = contents,
                     stringsAsFactors = F)
  
  dbWriteTable(con, 
               name = 'nexclipper_incident_ai', 
               value = info,
               row.names = F,
               append = T)
  
  dbCommit(con)
  
  dbDisconnect(con)
  
  cat('\nSuccess to save predicted disk usage!\n\n')
  
}


draw_graph <- function(dt) {
  
  if (!('DFT' %in% names(dt)))
    return(dt)
  
  host_name <- dt$host_name[1] %>%
    as.character() %>%
    toupper()
  
  trunc_time <- dt$ds[which(!is.na(dt$origin_y)) %>% max() - 24 * 14]
  
  current_time <- dt$ds[which(!is.na(dt$origin_y)) %>% max()] %>% 
    as.character()
  
  dt <- dt[ds > trunc_time, -3]
  
  dt_ <- dt %>% 
    melt(id.vars = 1,
         measure.vars = 2:3,
         variable.name = 'key',
         value.name = 'value')
  
  ggplot(dt_, aes(y = value, x = ds)) +
    geom_line(aes(color = key), size = 1, na.rm = T) +
    geom_point(data = dt[, .(ds, DFT)],
               aes(x = ds, y = DFT),
               color = 'red',
               size = 3, na.rm = T) +
    geom_text(data = dt,
              aes(x = ds, y = DFT,
                  label = ifelse(!is.na(DFT),
                                 as.character(paste(format(ds, '%m/%d %H:%M'),
                                                    round(DFT, 2),
                                                    sep = '\n')),
                                 '')),
              na.rm = T) +
    scale_x_datetime(breaks = date_breaks('1 day'),
                     labels = date_format('%m\n%d')) +
    scale_y_continuous(breaks = round(seq(min(dt_$value,
                                              na.rm = T),
                                          110, by = 10),
                                      -1)) +
    scale_color_manual(labels = c('Actual', 'Predicted'), values = c("blue", "red")) +
    theme(legend.background = element_rect(fill = "grey90", size = 2),
          legend.justification = c(0,1), legend.position = c(0,1),
          legend.direction = 'horizontal',
          legend.title = element_blank(),
          legend.text = element_text(size = 20),
          legend.spacing.x = unit(0.7, 'cm'),
          plot.title = element_text(size = 20,
                                    face = "bold",
                                    color = "darkgreen",
                                    hjust = 0.5)) +
    labs(title = host_name,
         x = 'Time', y = 'Disk used(%)',
         subtitle = current_time)
  
  # send_slack()
  
}


send_slack <- function() {
  
  slackr_setup(channel = Sys.getenv('SLACK_CHANNEL'),
               api_token = Sys.getenv("SLACK_API_TOKEN"),
               username = Sys.getenv('SLACK_USERNAME'))
  
  ggslackr(height = 6,
           width = 11)
  
  print('Success to send ggplot')
  
}

