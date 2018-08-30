cat('\n', '#############',
    format(Sys.time(), format = '%Y-%m-%d %H:00:00'),
    '#############', '\n')

#### library ####
bs.Library <- function(pkg, add = T) {
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  
  if (length(new.pkg))
    
    install.packages(new.pkg, dependencies = T)
  
  if (add == TRUE)
    
    sapply(pkg, require, character.only = TRUE)
  
}

bs.Library(c('prophet', 'tidyverse', 'xts', 'influxdbr', 'zoo',
             'data.table', 'jsonlite', 'RMySQL', 'slackr', 'scales'))


#### CONSTANT ####
envir_list <- Sys.getenv(c('ID', 'MOUNT_NAME', 'THRESHOLD', 'ALERT'))

ID <- envir_list['ID']

MOUNT_NAME <- envir_list['MOUNT_NAME']

THRESHOLD <- envir_list['THRESHOLD'] %>% as.integer()

ALERT <- envir_list['ALERT'] %>% as.integer()

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
  # return(5)
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


create_mysql_table <- function(user = MYSQL_USER,
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
  
  ListTables <- dbListTables(con)
  
  if (!('monitoring_disk' %in% ListTables)) {
    
    dbGetQuery(con,
               "CREATE TABLE `monitoring_disk` (
               `id` int(11) NOT NULL,
               `agent_id` int(11) NOT NULL,
               `resource` varchar(10) CHARACTER SET utf8 NOT NULL,
               `mount` varchar(1000) CHARACTER SET utf8 NOT NULL,
               `current_time` datetime NOT NULL,
               `DFT` datetime,
               `predicted` float,
               `alertYN` tinyint(1)
    ) ENGINE=InnoDB DEFAULT CHARSET=latin1;")
    
    dbGetQuery(con,
               "ALTER TABLE `monitoring_disk` ADD PRIMARY KEY (`id`);")
    
    dbGetQuery(con,
               "ALTER TABLE `monitoring_disk` MODIFY `id` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=1;")
    
    print('Create monitoring_disk table')
    
    dbCommit(con)
    
  }
  
  dbDisconnect(con)
  
}


AGENT_ID <- get_agent_id()
print('######## Get agent_id ########')

create_mysql_table()


load_disk_used_percent <- function(agent_id = AGENT_ID,
                                   mount_name = MOUNT_NAME,
                                   con = CONN,
                                   dbname = INFLUX_DBNAME) {
  
  if (is.null(mount_name)) {
    
    mount_name <- ''
    
  } else {
    
    mount_name <- "and mount_name = '%s'" %>% sprintf(mount_name)
    
  }
  
  query <- "select mean(used_percent) as y
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
                      return_xts = F)[[1]] %>% 
    as.data.table()
  
  res <- res[, .(ds = time + 9 * 60 * 60, host_name, y)] %>% 
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


handling_disk_data <- function(data_, cut_ = CUT) {
  
  data__ <- data_[, .(ds, host_name, y)]
  
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
    .[, .(ds = New_ds, host_name, y = New)] %>% 
    setkey(ds) %>%
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
  
  dt_ <- train_[predict(model, future) %>%
    as.data.table(key = 'ds') %>%
    .[, .(ds, yhat)]] %>% 
    .[, yhat := ifelse(is.na(y), yhat, NA)]
  
  save_result_mysql(dt_)
  
  return(dt_)
  
}


save_result_mysql <- function(dt_,
                              agent_id = AGENT_ID,
                              user = MYSQL_USER,
                              password = MYSQL_PASSWORD,
                              dbname = MYSQL_DBNAME,
                              host = MYSQL_HOST,
                              port = MYSQL_PORT,
                              mount_name = MOUNT_NAME,
                              threshold = THRESHOLD,
                              alert = ALERT) {
  
  con <- dbConnect(MySQL(), 
                   user = user, 
                   password = password,
                   dbname = dbname,
                   host = host, 
                   port = port)
  
  current_time <- dt_$ds[max(which(!is.na(dt_$y)))]
  
  DFT <- NA
  
  predicted <- NA
  
  if (length(which(dt_$yhat > threshold)) != 0) {
    
    DFT <- dt_$ds[min(which(dt_$yhat > threshold))]
    
    predicted <- dt_$yhat[min(which(dt_$yhat > threshold))]
    
  }
  
  alertYN <- NA
  
  if (!is.na(DFT))
    
    if (difftime(DFT, current_time, units = 'hours') <= alert) {
      
      alertYN <- T
      
    } else {alertYN <- F}
  
  info <- data.frame('agent_id' = agent_id,
                     'resource' = dt_$host_name[1],
                     'mount' = mount_name,
                     'current_time' = current_time,
                     'DFT' = DFT,
                     'predicted' = predicted,
                     'alertYN' = alertYN,
                     stringsAsFactors = F)
  
  dbWriteTable(con, 
               name = 'monitoring_disk', 
               value = info,
               row.names = F,
               append = T)
  
  dbCommit(con)
  
  dbDisconnect(con)
  
  print('Success to save predicted disk usage!')
  
}


add_DFT <- function(dt, threshold = THRESHOLD) {
  
  dt[, DFT := -1]
  
  idx_ <- which(dt$yhat > threshold)
  
  if (length(idx_) != 0) {
    
    dt[min(idx_), 'DFT'] <- dt$yhat[min(idx_)]
    
    dt <- dt[1:min(idx_), .(ds, y, host_name, yhat, DFT = ifelse(DFT == -1, NA, DFT))]
    
  } else {
    
    dt[, DFT := NULL]
    
  }
  
  return(dt)
  
}


draw_graph <- function(dt) {
  
  if (!('DFT' %in% names(dt)))
    return(dt)
  
  host_name <- dt$host_name[1] %>%
    as.character() %>%
    toupper()
  
  trunc_time <- dt$ds[which(!is.na(dt$y)) %>% max() - 24 * 14]
  
  current_time <- dt$ds[which(!is.na(dt$y)) %>% max()] %>% 
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
                     labels = date_format('%m/%d')) +
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
  
  send_slack()
  
}


send_slack <- function() {
  
  slackr_setup(channel = Sys.getenv('SLACK_CHANNEL'),
               api_token = Sys.getenv("SLACK_API_TOKEN"),
               username = Sys.getenv('SLACK_USERNAME'))
  
  ggslackr(height = 6,
           width = 10.4)
  
  print('Success to send ggplot')
  
}

