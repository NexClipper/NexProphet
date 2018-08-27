source('app_func.R')

#### CONSTANT ####
envir_list <- Sys.getenv(c('ID', 'MOUNT_NAME', 'THRESHOLD'))

ID <- envir_list['ID']

MOUNT_NAME <- envir_list['MOUNT_NAME']

THRESHOLD <- envir_list['THRESHOLD']

if (THRESHOLD == '') { THRESHOLD <- 99 } else {THRESHOLD <- as.integer(THRESHOLD)}

internal <- read_json('../internal.conf')

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

AGENT_ID <- get_agent_id()
#----

load_disk_used_percent() %>%
  lapply(function(dt, cut_) handling_disk_data(dt, cut_), cut_ = CUT) %>% 
  lapply(function(dt) diskForecasting(dt)) %>% 
  lapply(function(dt) add_DFT(dt, THRESHOLD)) %>% 
  lapply(function(dt) draw_graph(dt))

pred_data$master %>% View()
