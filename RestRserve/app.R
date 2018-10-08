library('RestRserve')
library(R.utils)
library(tidyverse)
library(jsonlite)

#### APP ####

FORECAST <- function(request, response) {
  
  #' ---
  #' description: Prediction for host or docker metric and Visualization
  #' parameters:
  #'   - name: "agent_id"
  #'     description: "agent_id"
  #'     in: query
  #'     schema:
  #'       type: integer
  #'     required: true
  #'     
  #'   - name: "key"
  #'     description: "key name to save to the influxdb"
  #'     in: query
  #'     schema:
  #'       type: integer
  #'     example: 817340112
  #'     required: true
  #'     
  #'   - name: "measurement"
  #'     description: "one of host, host_disk, host_net, host_process, docker_container, docker_network"
  #'     in: query
  #'     schema:
  #'       type: string
  #'     example: host
  #'     required: true
  #'     
  #'   - name: "host_ip"
  #'     description: "insert host ip as measurement is related to host or docker"
  #'     in: query
  #'     schema:
  #'       type: string
  #'     example: 192.168.0.165
  #'     required: true
  #'     
  #'   - name: "metric"
  #'     description: "select metric to predict"
  #'     in: query
  #'     schema:
  #'       type: string
  #'     example: cpu_used_percent
  #'     required: true
  #'     
  #'   - name: "period"
  #'     description: "select period to train model, default : 6 days"
  #'     in: query
  #'     schema:
  #'       type: string
  #'     example: 6d
  #'     required: true
  #'     
  #'   - name: "p_period"
  #'     description: "select period to predict, default : 2 days"
  #'     in: query
  #'     schema:
  #'       type: string
  #'     example: 2d
  #'     required: true
  #'     
  #'   - name: "groupby"
  #'     description: "select time to group by, default : 1 hour"
  #'     in: query
  #'     schema:
  #'       type: string
  #'     example: 1h
  #'     required: true
  #'     
  #'   - name: "start_time"
  #'     description: "select start time"
  #'     in: query
  #'     schema:
  #'       type: string
  #'     required: true
  #'     
  #'   - name: "mount"
  #'     description: "available when metric related to disk. select mount path. default : null. if you want mount path : / or /var, insert total or var. "
  #'     in: query
  #'     schema:
  #'       type: string
  #'     example: /
  #'     required: false
  #'     
  #'   - name: "hostIF"
  #'     description: "interface of host, which is related to host network metric. available when measurement is host_net."
  #'     in: query
  #'     schema:
  #'       type: string
  #'     example: docker0
  #'     required: false
  #'     
  #'   - name: "pname"
  #'     description: "name of host process. available when measurement is host_process."
  #'     in: query
  #'     schema:
  #'       type: string
  #'     example: dockerd
  #'     required: false
  #'     
  #'   - name: "dname"
  #'     description: "name of docker container. available when measurement is host_net."
  #'     in: query
  #'     schema:
  #'       type: string
  #'     example: /Nexclipper-Agent
  #'     required: false
  #'     
  #'   - name: "dockerIF"
  #'     description: "interface of host, which is related to host network metric. available when measurement is host_net."
  #'     in: query
  #'     schema:
  #'       type: string
  #'     example: eth0
  #'     required: false
  #'     
  #' responses:
  #'   200:
  #'     description: API response
  #'     content:
  #'       text/plain:
  #'         schema:
  #'           type: json
  #'           example : {"status" : "200"}
  #' ---
  
  agent_id <- request$query$agent_id
  
  key_ <- request$query$key
  
  measurement <- request$query$measurement
  
  host_ip <- request$query$host_ip
  
  metric <- request$query$metric
  
  period <- request$query$period
  
  p_period <- request$query$p_period
  
  groupby <- request$query$groupby
  
  start_time <- request$query$start_time
  
  mount <- request$query$mount
  
  hostIF <- request$query$hostIF
  
  pname <- request$query$pname
  
  dname <- request$query$dname
  
  dockerIF <- request$query$dockerIF
  
  cmd <- "Rscript forecast.R --agent_id '%s' --key '%s' --measurement '%s' --host_ip '%s' --metric '%s' --period '%s' --p_period '%s' --groupby '%s' --start_time '%s' --mount '%s' --hostIF '%s' --pname '%s' --dname '%s' --dockerIF '%s'" %>% 
    sprintf(agent_id, key_, measurement, host_ip, metric, period, p_period,
            groupby, start_time, mount, hostIF, pname, dname, dockerIF)
  
  system(cmd, wait = F)
  
  body <- list('status' = 200) %>% 
    toJSON() %>% 
    as.character()
  
  response$body = body
  
  response$content_type = "text/plain"
  
  response$headers = character(0)
  
  response$status_code = 200L
  
  forward()
  
}

RestRserveApp <- RestRserveApplication$new()

RestRserveApp$add_post(path = "/forecast", FUN = FORECAST)

RestRserveApp$add_openapi(path = "/openapi.yaml",
                          file_path = "openapi.yaml")

RestRserveApp$add_swagger_ui(path = "/swagger", 
                             path_openapi = "/openapi.yaml", 
                             path_swagger_assets = "/__swagger__")

RestRserveApp$run(http_port = "8484")

# configuration = c("http.port" = "8484",
#                   "encoding" = "utf8",
#                   "port" = "6311")
# 
# dir = tempdir()
# 
# app_path <- getwd() %>% paste0('/app.R')
# 
# restrserve_deploy(file = app_path,
#                   dir = dir,
#                   configuration = configuration)
# 
# restrserve_start(dir)