source('app_func.R')


FORECAST <- function(request, response) {
  
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

app$run(http_port = "8484")
