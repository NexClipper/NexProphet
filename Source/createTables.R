# dbListConnections(MySQL())

uid = "admin"
password = "password"
dbname = "defaultdb"
host = "mysql.marathon.l4lb.thisdcos.directory"
port = 3306

con <- dbConnect(MySQL(), 
                 user = uid, 
                 password = password,
                 dbname = dbname,
                 host = host, 
                 port = port)

ListTables <- dbListTables(con)

if (!('model_info' %in% ListTables)) {
  
  dbGetQuery(con,
             "CREATE TABLE `model_info` (
             `id` int(11) NOT NULL,
             `agent_id` int(11) NOT NULL,
             `resource` varchar(10) CHARACTER SET utf8 NOT NULL,
             `target` varchar(100) CHARACTER SET utf8 NOT NULL,
             `unit` varchar(1) CHARACTER SET utf8 NOT NULL,
             `metric` varchar(200) CHARACTER SET utf8 NOT NULL,
             `mount` varchar(1000) CHARACTER SET utf8 NOT NULL,
             `developed_at` datetime NOT NULL,
             `developed_during` float NOT NULL
  ) ENGINE=InnoDB DEFAULT CHARSET=latin1;")
  
  dbGetQuery(con,
             "ALTER TABLE `model_info` ADD PRIMARY KEY (`id`);")
  
  dbGetQuery(con,
             "ALTER TABLE `model_info` MODIFY `id` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=1;")
  
  print('Create model_info table')
  
}


if (!('monitoring_metric' %in% ListTables)) {
  
  dbGetQuery(con,
             "CREATE TABLE `monitoring_metric` (
             `id` int(11) NOT NULL,
             `agent_id` int(11) NOT NULL,
             `resource` varchar(100) CHARACTER SET utf8 NOT NULL,
             `target` varchar(100) CHARACTER SET utf8 NOT NULL,
             `metric` varchar(100) CHARACTER SET utf8 NOT NULL,
             `modeling_period` varchar(100) CHARACTER SET utf8 NOT NULL,
             `modeling_hour` int(11) NOT NULL,
             `is_delete` tinyint(1) NOT NULL DEFAULT '0'
  ) ENGINE=InnoDB DEFAULT CHARSET=latin1;")
  
  dbGetQuery(con,
             "ALTER TABLE `monitoring_metric` ADD PRIMARY KEY (`id`);")
  
  dbGetQuery(con,
             "ALTER TABLE `monitoring_metric` MODIFY `id` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=1;")
  
  print('Create modeling_metric table')
  
}


if (!('monitoring_results' %in% ListTables)) {
  
  dbGetQuery(con,
             "CREATE TABLE `monitoring_results` (
  `id` int(11) NOT NULL,
  `agent_id` int(11) NOT NULL,
  `resource` varchar(100) CHARACTER SET utf8 NOT NULL,
  `target_name` varchar(100) CHARACTER SET utf8 NOT NULL,
  `target` varchar(100) CHARACTER SET utf8 NOT NULL,
  `metric` varchar(100) CHARACTER SET utf8 NOT NULL,
  `time` datetime NOT NULL,
  `value` float NOT NULL,
  `ub` float NOT NULL,
  `lb` float NOT NULL,
  `anomaly` tinyint(1) NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=latin1;")
  
  dbGetQuery(con,
             "ALTER TABLE `monitoring_results` ADD PRIMARY KEY (`id`);")
  
  dbGetQuery(con,
             "ALTER TABLE `monitoring_results` MODIFY `id` int(11) NOT NULL AUTO_INCREMENT, AUTO_INCREMENT=1;")
  
  print('Create monitoring_results table')
  
}

dbDisconnect(con)
