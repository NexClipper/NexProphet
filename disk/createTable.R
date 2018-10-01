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

create_mysql_table()