source('app_func.R')

#### CONSTANT ####
user_conf <- read_json('../user.conf')

internal <- read_json('../internal.conf')

ID <- user_conf$id

MOUNT_NAME <- user_conf$mount_name

THRESHOLD <- user_conf$threshold

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
pred_data <- load_disk_used_percent() %>%
  lapply(function(dt, cut_) handling_disk_data(dt, cut_), cut_ = CUT) %>% 
  lapply(function(dt) diskForecasting(dt)) %>% 
  lapply(function(dt) add_DFT(dt, THRESHOLD))
pred_data$master %>% View()
