# Influx Info
library(influxdbr)
library(stringr)
library(data.table)
library(dplyr)
library(prophet)
con_inf <- influx_connection(host = '192.168.0.162', port = 10091)
dbname_inf <- 'nexclipper'


model_data_length = "21d"     # including training and validation period


HOST_QUERY = "SELECT mean(${metric})
              FROM host
              WHERE time > now() - ${model_data_length} and host_name = '${host}'
              GROUP BY time(5m)
              FILL(none)"

HOST_QUERY_MON = "SELECT mean(${metric})
                  FROM host
                  WHERE time > now() - 2m and host_name = '${host}'
                  GROUP BY time(${monitoring_period})
                  FILL(none)"


TASK_QUERY = "SELECT mean(${metric})
              FROM task
              WHERE time > now() - ${model_data_length} and task = '${task}'
              GROUP BY time(5m)
              FILL(none)"

TASK_QUERY_MON = "SELECT mean(${metric})
              FROM task
              WHERE time > now()- 2m and task = '${task}'
              GROUP BY time(${monitoring_period})
              FILL(none)"


# MySQL DB Create  ---------------------------------------------------
library(RMySQL)

uid = "admin"
password = "password"
dbname = "defaultdb"
host = "192.168.0.166"
port = 27604

con <- dbConnect(MySQL(), 
                 user = uid, 
                 password = password,
                 dbname = dbname,
                 host = host, 
                 port = port)

dbListTables(con)
# dbListFields(con, 'nexclipper_host')


# DB Table Create  -----------------------------------------------------------------------------------------------
   # 우리가 모니터링 해야할 지표들을 사전에 등록/관리한다. 
# 
# # We can create tables in the database using R dataframes.
# dbSendQuery(con, "DROP TABLE temp_monitoring_metric")
# dbSendQuery(con, "
#             CREATE TABLE temp_monitoring_metric (
#             id int AUTO_INCREMENT PRIMARY KEY,
#             agent_id int,
#             type VARCHAR(50),
#             host VARCHAR(50),
#             task VARCHAR(50),
#             docker VARCHAR(50),
#             metric VARCHAR(50),
#             modeling_period VARCHAR(50),
#             modeling_hour int
#             );")
# 
# # Sample data
# monitoring_metric <- data.frame(
#   agent_id = c(27, 27),
#   type = c("host", "task"),                         # host, task, docker
#   host = c("private1", NA),
#   task = c(NA, "kafka"),
#   docker = c(NA, NA),
#   metric = c("mem_used_percent", "cpu_used_percent"),
#   modeling_period = c("every day", "every week"),
#   modeling_hour = c(3, 3)
# )
# monitoring_metric
# 
# dbWriteTable(con,
#              name = 'temp_monitoring_metric',
#              value = monitoring_metric,
#              row.names = F,
#              append = T)


# 수정, 삭제, 추가 기능이 있어야 한다



# Auto Modeling  ---------------------------------------------------
# 정해진 시간에 지정된 모니터링 지표에 대해 새롭게 모델 개발을 한다. 
# 매시간 자동으로 아래 코드가 돌아가면서 Check하도록 (스케줄링 작업)


# # 모형 개발 history DB 생성
# dbSendQuery(con, "DROP TABLE temp_model_info")
# dbSendQuery(con, "
# CREATE TABLE temp_model_info (
#             model_id INT AUTO_INCREMENT PRIMARY KEY,
#             agent_id VARCHAR(50),
#             type VARCHAR(50),
#             host VARCHAR(50),
#             task VARCHAR(50),
#             docker VARCHAR(50),
#             metric VARCHAR(50),
#             developed_at timestamp,
#             developed_during time,
#             test_r_square double
# );")



now <- Sys.time()
now

# 룰셋을 읽어온다. 
rs = dbSendQuery(con, "select * from temp_monitoring_metric")
anomalySet = fetch(rs, n=-1)
anomalySet

nMetric <- nrow(anomalySet)



#  now가 정기개발 시간보다 크고, 정기개발시간 이후 생성된 모델이 없으면 알고리즘이 돌아간다. 

for(i in 1:nMetric) {
  
  if(T) { # 재개발 시간이 되었다면, 모델링 작업 시작 
    
    agent_id = anomalySet[i, "agent_id"]
    type =  anomalySet[i, "type"]
    host = ifelse(is.na(anomalySet[i, "host"]), "", anomalySet[i, "host"])
    task = ifelse(is.na(anomalySet[i, "task"]), "", anomalySet[i, "task"])
    docker = ifelse(is.na(anomalySet[i, "docker"]), "", anomalySet[i, "docker"])
    metric = anomalySet[i, "metric"]
    
    model_path <- paste("./anomalyModels", agent, host, docker, task, metric, sep ="/")
    
    if(!dir.exists(model_path)) {       # 특정 agent의 모니터링해야할 지표 모델 폴더가 없으면 생성한다. 
      
      dir.create(model_path, recursive = T)
    
    } 
    
    
    
    # 모형 개발 (시간측정 필요) 
    developed_at <- Sys.time()
    developed_during <- system.time({
      # 모형 개발 코드 
      
      # InfluxDB에서 모형 개발을 위한 데이터 읽어오기 
      if(type == "host") {
        
        data_query <- str_interp(HOST_QUERY)
        
      } else if (type == "task") {
        
        data_query <- str_interp(TASK_QUERY)
        
      } else if (type == "docker") {
        
        
        
      }
      temp_data <- influx_query(con_inf,
                                db = dbname_inf,
                                query =  data_query,
                                simplifyList = T,
                                return_xts = F)[[1]]
      
      
      # data 변환 및 모델링 수행 
      temp_data %<>% as.data.table() %>%  .[, c("time", "mean")] 
      names(temp_data) <- c("ds", "y")
      
      fcastModel <- prophet(temp_data,
                            changepoint.prior.scale = 0.01,
                            uncertainty.samples = 100,
                            interval.width = 0.95)
      
      # 모형 결과 저장 (개발 데이터, 모델)
      developed_at_name <- gsub(" ", "_", developed_at) %>%  gsub(":", "-", .)
      modelFile.name = paste0(model_path, "/", developed_at_name, ".RData")
      save(temp_data, fcastModel, file = modelFile.name)
      
      
      # r_square 계산
      test_r_square = 0.9
      

      
    })[3]  # system.time 끝 

    
    # 모형 개발 History를 DB에 저장
    model_info <- data.frame(
      agent_id = agent_id,
      type = type,                         # host, task, docker
      host = host,
      task = task,
      docker = docker,
      metric = metric,
      developed_at = developed_at,
      developed_during = developed_during,
      test_r_square = test_r_square
    )
    model_info
    
    print(i)
    
    dbWriteTable(con, 
                 name = 'temp_model_info', 
                 value = model_info,
                 row.names = F,
                 append = T)
    
  }
  
}



# 실시간 모니터링 ---------------------------------------------------------------

# We can create tables in the database using R dataframes.
# dbSendQuery(con, "DROP TABLE temp_monitoring_results")
# dbSendQuery(con, "
#             CREATE TABLE temp_monitoring_results (
#             agent_id int,
#             type VARCHAR(50),
#             host VARCHAR(50),
#             task VARCHAR(50),
#             docker VARCHAR(50),
#             metric VARCHAR(50),
#             time VARCHAR(50),
#             value double,
#             ucl double,
#             lcl double,
#             anomalyYN bool
#             );")


# 모든 지표에 대해 매 5초 마다 모니터링을 수행함  ---------------------------------------------------------------


# 우선 현재시간에 대해 모니터링 결과 출력하는 코드
monitoring_period <- "5s"
current_time <- Sys.time() 


for(i in 1:nMetric) {
  
  
  agent_id = anomalySet[i, "agent_id"]
  type =  anomalySet[i, "type"]
  host = ifelse(is.na(anomalySet[i, "host"]), "", anomalySet[i, "host"])
  task = ifelse(is.na(anomalySet[i, "task"]), "", anomalySet[i, "task"])
  docker = ifelse(is.na(anomalySet[i, "docker"]), "", anomalySet[i, "docker"])
  metric = anomalySet[i, "metric"]
  
  model_path <- paste("./anomalyModels", agent, host, docker, task, metric, sep ="/")
  
  
  # data 읽어오기 (한포인트)
  # InfluxDB에서 모형 개발을 위한 데이터 읽어오기 
  if(type == "host") {
    
    data_query <- str_interp(HOST_QUERY_MON)
    
  } else if (type == "task") {
    
    data_query <- str_interp(TASK_QUERY_MON)
    
  } else if (type == "docker") {
    
    
  }
  
  # 여유있게 2분 정도 데이터를 가져와서, 마지막 결과를 이용하자. 
  cur_time = Sys.time()
  temp_data <- influx_query(con_inf,
                            db = dbname_inf,
                            query =  data_query,
                            simplifyList = T,
                            return_xts = F)[[1]] %>%  as.data.table()  %>%  .[, c("time", "mean")] 
  names(temp_data) <- c("ds", "y")
  setorder(temp_data, -ds)
  
  print(i)
  print(temp_data)
  
  
  # 가장 최근 모델을 가져와서 수행함 (개발 시간이 가장 큰애) 
  lst_models <- list.files(model_path, pattern = ".RData")
  recent_model_name <- lst_models[order(lst_models, decreasing = T)[1]]
  
  load(paste0(model_path, "/", recent_model_name))
  
  # 이상치 탐지 수행
  future <- data.frame(ds = max(temp_data$ds))
  fcst <- predict(fcastModel, future)

  yval <- temp_data$y[1]
  ucl <- fcst$yhat_upper
  lcl <- fcst$yhat_lower
  
  anomalyYN <- ifelse (yval > ucl | yval < lcl, T, F)

  # 모형에서 UCL LCL 생성, 요건 1시간에 한번씩 ???, 다 정상으로 tag 해 두기...
  # 모형 개발 History를 DB에 저장
  anomaly_result <- data.frame(
    agent_id = agent_id,
    type = type,                         # host, task, docker
    host = host,
    task = task,
    docker = docker,
    metric = metric,
    # time = cur_time,
    value = yval,
    ucl = ucl,
    lcl = lcl,
    anomalyYN = anomalyYN
  )
  anomaly_result

  dbWriteTable(con, 
               name = 'temp_monitoring_results', 
               value = anomaly_result,
               row.names = F,
               append = T)
  
  
  # 5초마다 check해 실제값이 범위를 벗어나면 anomaly로 tagging, db 결과를 수정 
  

}






