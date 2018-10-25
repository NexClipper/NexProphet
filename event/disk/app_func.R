
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

internal <- read_json('internal.conf')

INFLUX_HOST <- internal$influx_host

INFLUX_PORT <- internal$influx_port %>% as.integer()

INFLUX_DBNAME <- internal$influx_dbname

MYSQL_USER <- internal$mysql_user

MYSQL_PASSWORD <- internal$mysql_password

MYSQL_DBNAME <- internal$mysql_dbname

MYSQL_HOST <- internal$mysql_host

MYSQL_PORT <- internal$mysql_port %>% as.integer()

CUT <- internal$cut %>% as.numeric()

CONN <- influx_connection(host = INFLUX_HOST,
                          port = INFLUX_PORT)

START_TIME <- Sys.time()

#----

#### AGENT_ID LIST ####
get_event_config <- function(user = MYSQL_USER,
                             password = MYSQL_PASSWORD,
                             dbname = MYSQL_DBNAME,
                             host = MYSQL_HOST,
                             port = MYSQL_PORT) {
  
  con <- dbConnect(MySQL(), 
                   user = user, 
                   password = password,
                   dbname = dbname,
                   host = host, 
                   port = port)
  
  query <- "select * from nexclipper_event_config"
  
  res <- dbGetQuery(con, query)
  
  dbDisconnect(con)
  
  return(res)
  
}

#### functions ####

load_disk_used_percent <- function(agent_id,
                                   metric, measurement,
                                   period, timezone,
                                   con = CONN,
                                   dbname = INFLUX_DBNAME) {
  
  query <- "select mean(%s) as y
            from %s
            where time > now() - %sd and
                  agent_id = '%s'
            group by time(1h), mount_name, host_ip, host_name
            fill(none)" %>% 
    sprintf(metric,
            measurement, 
            period,
            agent_id)
  
  cat('\n', query, '\n')
  
  res <- influx_query(con,
               dbname,
               query,
               return_xts = F)[[1]]
  
  if (!('time' %in% names(res))) return(NULL)
  
  res %>% 
    select(-1:-3) %>%
    group_by(time, host_ip, host_name, mount_name) %>%
    summarise('y' = mean(y, na.rm = T)) %>%
    as.data.table() %>% 
    setnames('time', 'ds') %>% 
    setkey('ds', 'host_ip', 'host_name', 'mount_name') %>% 
    merge(CJ(ds = seq.POSIXt(min(.$ds),
                             max(.$ds),
                             by = '1 hour'),
             host_ip = unique(.$host_ip),
             host_name = unique(.$host_name),
             mount_name = unique(.$mount_name)),
          by = c('ds', 'host_ip', 'host_name', 'mount_name'),
          all = T) %>% 
    dcast(ds ~ host_ip + host_name + mount_name, value.var = c('y'), sep = '__') %>% 
    .[, .SD, .SDcols = .[, lapply(.SD, function(x) sum(is.na(x)) <= as.integer(length(x) * 0.10))] %>% 
        unlist() %>% 
        which()] %>% 
    .[, lapply(.SD, function(x) na.approx(x) %>% na.fill('extend'))] %>% 
    .[, ds := as_datetime(ds, origin = '1970-01-01', tz = timezone)] %>% 
    setkey(ds) %>%
    melt(id.vars = 1,
         measure.vars = 2:ncol(.),
         variable.name = 'key',
         value.name = 'y') %>% 
    split(by = 'key') %>%
    return()
  
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
                            pred_period, timezone,
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
    .[, ds := with_tz(ds, timezone)] %>%
    return()
  
}


add_DFT <- function(dt,
                    threshold,
                    critical) {
  
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
                              agent_id, threshold,
                              critical, warning,
                              start_time = START_TIME,
                              user = MYSQL_USER,
                              password = MYSQL_PASSWORD,
                              dbname = MYSQL_DBNAME,
                              host = MYSQL_HOST,
                              port = MYSQL_PORT) {
  
  if (!'DFT' %in% names(dt_)) return()
  
  con <- dbConnect(MySQL(), 
                   user = user, 
                   password = password,
                   dbname = dbname,
                   host = host, 
                   port = port)
  
  predicted_time <- dt_[!is.na(DFT), ds]
  
  severity <- dt_[ds == predicted_time, severity]
  
  key_ <- dt_[1, key] %>%
    as.character() %>%
    strsplit('__') %>% 
    unlist()
  
  target_ip <- key_[1]
  
  host_name <- key_[2]
  
  mount_name <- key_[3]
  
  cond_ <- switch(severity,
                  'Critical' = critical,
                  'Warning' = warning)
  
  condition <- sprintf('>%s and <%sd',
                       threshold, cond_)
  
  id <- '%s_%s' %>% 
    sprintf(target_ip, mount_name)
  
  contents <- "[%s][Path : '%s'] The disk usage for Host will exceed %s at %s" %>% 
    sprintf(target_ip, mount_name, threshold, predicted_time)
  
  info <- data.frame('agent_id' = agent_id,
                     'severity' = severity,
                     'target_system' = 'Host',
                     'target_ip' = target_ip,
                     'host_name' = host_name,
                     'target' = 'Disk',
                     'metric' = 'used_percent',
                     'condition' = condition,
                     'id' = id,
                     'start_time' = start_time,
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


save_plot <- function(dt, target_ip, mount_name,
                      start_time = START_TIME) {
  # dt <- m$`192.168.0.161__/`
  # target_ip <- '192.168.0.161'; mount_name='/'
  # host_name <- dt$host_name[1] %>%
  #   as.character() %>%
  #   toupper()
  
  start_time <- start_time %>% as.character()
  
  trunc_time <- dt[!is.na(origin_y), ds] %>% max() - 14 * 60 * 60
  
  dt <- dt[ds > trunc_time, .SD, .SDcols = -'key']
  
  title <- "Host : %s, Mount path : '%s'" %>% 
    sprintf(target_ip, mount_name)
  
  dt_ <- dt %>% 
    melt(id.vars = 1,
         measure.vars = c('origin_y', 'yhat'),
         variable.name = 'key',
         value.name = 'value')
  
  p <- ggplot(dt_, aes(y = value, x = ds)) +
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
    theme(legend.justification = 'right',
          legend.position = 'top',
          legend.direction = 'horizontal',
          legend.margin = margin(-30, 0, 0, 0),
          legend.title = element_blank(),
          legend.text = element_text(size = 20),
          legend.spacing.x = unit(0.7, 'cm'),
          plot.title = element_text(size = 20,
                                    face = "bold",
                                    color = "darkgreen",
                                    hjust = 0.5)) +
    labs(title = title,
         x = 'Time', y = 'Disk used(%)',
         subtitle = start_time)
  
  filepath <- tempfile('', fileext = '.png')
  
  ggsave(filepath, p,
         device = 'png',
         width = 43,
         height = 20,
         units = 'cm')
  
  return(filepath)
  
}


send_slack <- function() {
  
  slackr_setup(channel = Sys.getenv('SLACK_CHANNEL'),
               api_token = Sys.getenv("SLACK_API_TOKEN"),
               username = Sys.getenv('SLACK_USERNAME'))
  
  ggslackr(height = 6,
           width = 11)
  
  print('Success to send ggplot')
  
}

