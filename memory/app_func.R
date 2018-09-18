
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
THRESHOLD <- 99

CUT <- 0.95

INFLUX_HOST <- '13.77.154.37'

INFLUX_PORT <- 10091

INFLUX_DBNAME <- 'nexclipper'


"
  #### available target ####
  /instana-agent
  /Nexclipper-Agent
  influxdb
  kafka-manager
  marathon-lb
  mysql
  mysql-admin
  nexcloud_collector
  nexcloud_fullfillment
  nexcloud_hostapi
  nexcloud_mesosapi
  nexcloud_nexclipperui
  nexcloud_nexgate
  nexcloud_nexoauth2
  nexcloud_nexrouter
  nexcloud_searchapi
  nexcloud_servicegate
  redis
  "

#----


#### functions ####

load_mem_used_percent <- function(agent_id) {
  
  con <- influx_connection(host = INFLUX_HOST,
                           port = INFLUX_PORT)
  
  dbname = INFLUX_DBNAME
  
  query <- "select mean(mem_used_percent) as y
            from docker_container
            where time > now() - 30d and
                  agent_id = '%s'
            group by time(1h), task_id
            fill(none)" %>% 
    sprintf(agent_id)
  
  cat('\n', query, '\n')
  
  influx_query(con,
               dbname,
               query,
               return_xts = F)[[1]] %>% 
    separate(task_id, c('task_id', 'na', 'na1'), sep = '[.]') %>%
    select(time, task_id, y) %>%
    group_by(time, task_id) %>%
    summarise('y' = mean(y, na.rm = T)) %>%
    as.data.table(key = c('time', 'task_id')) %>% 
    .[, .(ds = time, task_id, y)] %>% 
    merge(CJ(ds = seq.POSIXt(min(.$ds),
                             max(.$ds),
                             by = '1 hour'),
             task_id = unique(.$task_id)),
          by = c('ds', 'task_id'),
          all = T) %>% 
    dcast(ds ~ task_id, value.var = c('y')) %>% 
    .[, .SD, .SDcols = .[, lapply(.SD, function(x) sum(is.na(x)) <= as.integer(length(x) * 0.05))] %>% 
        unlist() %>% 
        which()] %>% 
    .[, lapply(.SD, function(x) na.approx(x) %>% na.fill('extend'))] %>% 
    .[, ds := as_datetime(ds, origin = '1970-01-01', tz = 'Asia/Seoul')] %>% 
    # .[, ds := as.POSIXct(ds, origin = '1970-01-01', tz = 'Asia/Seoul')] %>% 
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
    .[data_[, .(ds, origin_y = y)]] %>% 
    .[!is.na(y)] %>% 
    setkey(ds) %>% 
    return()
  
}


memForecasting <- function(train_,
                           pred_period = 7,
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
                  .[, .(ds = with_tz(ds, 'Asia/Seoul'), yhat)]] %>% 
    .[, yhat := ifelse(is.na(y), yhat, NA)]
  
  return(dt_)
  
}


add_MFT <- function(dt, threshold = THRESHOLD) {
  
  dt_ <- dt %>% copy() %>% 
    .[, MFT := -1]
  
  idx_ <- which(dt_$yhat > threshold)
  
  if (length(idx_) != 0) {
    
    dt_[min(idx_), 'DFT'] <- dt_$yhat[min(idx_)]
    
    dt_ <- dt_[1:min(idx_), .(ds, y, origin_y, yhat, MFT = ifelse(DFT == -1, NA, MFT))]
    
  } else {
    
    dt_[, c('MFT', 'y') := NULL]
    
  }
  
  return(dt_)
  
}


draw_graph <- function(dt, con_name) {
  
  if (!('MFT' %in% names(dt)))
    print('Nothing!')
    return()
  
  dt_ <- dt %>% copy()
  
  trunc_time <- dt_$ds[which(!is.na(dt$origin_y)) %>% max() - 24 * 14]
  
  current_time <- dt_$ds[which(!is.na(dt$origin_y)) %>% max()] %>% 
    as.character()
  
  dt_ <- dt_[ds > trunc_time] %>% 
    setnames(c('origin_y', 'yhat'), c('actual', 'predicted'))
  
  dt__ <- dt_[, c(-2, -5)] %>%
    melt(id.vars = 1,
         measure.vars = 2:3,
         variable.name = 'key',
         value.name = 'value')
  
  ggplot(dt__, aes(x = ds, y = value)) +
    geom_line(aes(color = key), size = 1, na.rm = T) +
    geom_point(data = dt_[, .(ds, DFT)],
               aes(x = ds, y = DFT),
               color = 'red',
               size = 3, na.rm = T) +
    geom_text(data = dt_,
              aes(x = ds, y = DFT,
                  label = ifelse(!is.na(DFT),
                                 as.character(paste(format(ds, '%m/%d %H:%M'),
                                                    round(DFT, 2),
                                                    sep = '\n')),
                                 '')),
              na.rm = T) +
    scale_x_datetime(breaks = date_breaks('1 day'),
                     labels = date_format('%m\n%d')) +
    scale_y_continuous(breaks = round(seq(min(dt_$actual,
                                              na.rm = T),
                                          110, by = 10),
                                      -1)) +
    scale_color_manual(labels = c('Actual', 'Predicted'),
                       values = c("blue", "red")) +
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
    labs(title = con_name,
         x = 'Time',
         y = 'memory used(%)',
         subtitle = current_time)
  
  # send_slack()
  
}


predict_memory <- function(agent_id, con_name) {
  
  mem <- load_mem_used_percent(agent_id = agent_id)
  
  handled_mem <- handling_disk_data(mem)
  
  mem_pred <- memForecasting(handled_mem)
  
  mem_pred_MFT <- add_MFT(mem_pred)
  
  draw_graph(mem_pred_MFT, con_name)
  
}


mem <- load_mem_used_percent(5)
# mem %>% names()
con_name <- 'nexcloud_fullfillment'

mem_container <- mem[, .SD, .SDcols = c('ds', con_name)] %>% 
  setnames(names(.), c('ds', 'y'))

handled_mem <- handling_disk_data(mem_container)

mem_pred <- memForecasting(handled_mem)

MFT_mem <- add_MFT(mem_pred)

draw_graph(MFT_mem, con_name)


# send_slack <- function() {
#   
#   slackr_setup(channel = Sys.getenv('SLACK_CHANNEL'),
#                api_token = Sys.getenv("SLACK_API_TOKEN"),
#                username = Sys.getenv('SLACK_USERNAME'))
#   
#   ggslackr(height = 6,
#            width = 11)
#   
#   print('Success to send ggplot')
#   
# }

