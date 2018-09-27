source('app_func.R')
source('00.R')


FORECAST <- function(request, response) {
  
  #' ---
  #' description: Prediction for host or docker metric and Visualization
  #' parameters:
  #'   - name: "agent_id"
  #'     description: "agent_id"
  #'     in: query
  #'     schema:
  #'       type: integer
  #'     example: 5
  #'     required: true
  #'     
  #'   - name: "resource"
  #'     description: "select host or docker"
  #'     in: query
  #'     schema:
  #'       type: string
  #'     example: host
  #'     required: true
  #'     
  #'   - name: "host"
  #'     description: "insert host ip or task id of docker container"
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
  #'     description: "select period to train model, default : 6"
  #'     in: query
  #'     schema:
  #'       type: int
  #'     example: 6
  #'     required: false
  #'     
  #'   - name: "predicted_period"
  #'     description: "select period to predict, default : 2"
  #'     in: query
  #'     schema:
  #'       type: int
  #'     example: 2
  #'     required: false
  #'     
  #'   - name: "groupby"
  #'     description: "select time to group by, default : 1h"
  #'     in: query
  #'     schema:
  #'       type: string
  #'     example: '1h'
  #'     required: false
  #'     
  #'   - name: "unit"
  #'     description: "unit time; 0: hours, 1: seconds, default : 0"
  #'     in: query
  #'     schema:
  #'       type: string
  #'     example: '0'
  #'     required: false
  #'     
  #'   - name: "mount"
  #'     description: "available when metric related to disk. select mount path. default : 'null'"
  #'     in: query
  #'     schema:
  #'       type: string
  #'     example: 'null'
  #'     required: false
  #'     
  #' responses:
  #'   200:
  #'     description: API response
  #'     content:
  #'       text/plain:
  #'         schema:
  #'           type: string
  #'           example: 5
  #' ---
  
  agent_id <- request$query$agent_id
  
  resource <- requset$query$resource
  
  host <- request$query$host
  
  metric <- request$query$metric
  
  period <- request$query$period
  
  period <- ifelse(is.null(period), 6, period)
  
  predicted_period <- request$query$predicted_period
  
  predicted_period <- ifelse(is.null(predicted_period), 2, predicted_period)
  
  groupby <- request$query$groupby
  
  groupby <- ifelse(is.null(groupby), '1h', groupby)
  
  unit <- request$query$unit
  
  unit <- ifelse(is.null(unit), '0', unit)
  
  mount <- request$query$mount
  
  mount <- ifelse(is.null(mount), 'null', mount)
  
  response$body = forecast_(agent_id, resource, host,
                            metric, period, predicted_period,
                            groupby, unit, mount)
  
  # response$content_type = "text/plain"
  
  response$headers = character(0)
  
  response$status_code = 200L
  
  forward()
  
}

app = RestRserve::RestRserveApplication$new()

app$add_get(path = "/forecast", FUN = FORECAST)

app$add_openapi(path = "/openapi.yaml", file_path = "openapi.yaml")

app$add_swagger_ui(path = "/swagger", 
                   path_openapi = "/openapi.yaml", 
                   path_swagger_assets = "/__swagger__")

# app$run(http_port = "8484")

configuration = c("http.port" = "8484",
                  "encoding" = "utf8",
                  "port" = "6311")

dir = tempdir()

app_path = system.file("app.R", package = "RestRserve")

RestRserve::restrserve_deploy(file = app_path,
                              dir = dir,
                              configuration = configuration)

restrserve_start(dir)