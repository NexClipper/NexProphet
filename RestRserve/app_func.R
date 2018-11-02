#### MYSQL ENVIRONMENT ####
MYSQL_ENV <- Sys.getenv(c('MYSQL_USER', 'MYSQL_PW', 'MYSQL_DB',
                          'MYSQL_HOST', 'MYSQL_PORT'))

MYSQL_USER <- MYSQL_ENV['MYSQL_USER']

MYSQL_PW <- MYSQL_ENV['MYSQL_PW']

MYSQL_DB <- MYSQL_ENV['MYSQL_DB']

MYSQL_HOST <- MYSQL_ENV['MYSQL_HOST']

MYSQL_PORT <- MYSQL_ENV['MYSQL_PORT'] %>% as.integer()
#----

#### write mysql ####
update_key_id_to_mysql <- function(agent_id, key_,
                                   status, message) {
  
  con <- dbConnect(MySQL(),
                   user = MYSQL_USER,
                   password = MYSQL_PW,
                   dbname = MYSQL_DB,
                   host = MYSQL_HOST,
                   port = MYSQL_PORT)
  
  end_time <- Sys.time()
  
  query <- "update nexclipper_key
            set end_time = '%s',
                status = %s, 
                message = '%s'
            where agent_id = '%s' and
            key_id = '%s'" %>% 
    sprintf(end_time, status, message,
            agent_id, key_)
  
  cat('\n', query, '\n')
  
  dbGetQuery(con, query)
  
  dbCommit(con)
  
  dbDisconnect(con)
  
  cat('\n', 'write key id to mysql', '\n')
  
}
#----
