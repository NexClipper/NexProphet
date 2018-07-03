
posixt_helper_func <- function(x) {
    switch(x,
           's' = 'sec',
           'm' = 'min',
           'h' = 'hour',
           stop('incorrect group time'))
}

connect <- function() {
    con <- influx_connection(host = '192.168.0.162', port = 10091)
    dbname <- 'nexclipper'
    conn <- list(connector = con, dbname = dbname)
    return(conn)
}

extract_field <- function(df) {
    
    # select numeric fields
    num_var = names(df)[sapply(df, class) == "numeric"]
    
    # select fields such that stddev is not equal to zero
    not0var = names(df[,num_var])[sapply(df[,num_var], var) != 0]
    
    # delete timestamp
    not0var = setdiff(not0var, "timestamp")
    
    extracted_df <- df %>% select(c(not0var, 'time'))
    
    return(extracted_df)
}

standardization <- function(mtx) {
    col_min <- as.vector(apply(mtx, 2, min))
    col_max <- as.vector(apply(mtx, 2, max))
    new_mtx <- t(apply(mtx,
                       1,
                       function(x) (x - col_min) / (col_max - col_min + 1e-6)))
    return(new_mtx)
}

multiple_join <- function(period = 5, group_by = '1h',
                          worker_list = c(), metric_list=c()) {
    
    connector <- connect()
    con <- connector$connector
    dbname <- connector$dbname
    
    period <- paste0(period, 'd')
    
    metric_list <- c(metric_list, 'time')
    
    # for master
    query <- "select mean(*)
              from cluster
              where time > now() - %s
              group by time(%s)
              fill(none)"
    
    query <- sprintf(query, period, group_by)
    
    res_master <- influx_query(con,
                               db = dbname,
                               query = query,
                               simplifyList = T,
                               return_xts = F)[[1]]
    
    df <- as.data.frame(res_master)
    
    names(df) <- gsub('mean_', '', names(df))
    
    res <- extract_field(df)
    
    res <- res[, names(res) %in% metric_list]
    
    names(res) <- paste0(names(res), '_master')
    
    names(res) <- gsub('time_master', 'time', names(res))
    
    # for worker
    if (length(worker_list) > 0) {
        
        query <- "select mean(*)
                  from host
                  where host_ip = '%s' and time > now() - %s
                  group by time(%s)
                  fill(none)"
        
        for (ip in worker_list) {
            
            query <- sprintf(query, ip, period, group_by)
            
            res_worker <- influx_query(con,
                                       db = dbname,
                                       query = query,
                                       simplifyList = T,
                                       return_xts = F)[[1]]
            
            df <- as.data.frame(res_worker)
            
            names(df) <- gsub('mean_', '', names(df))
            
            df <- extract_field(df)
            
            df <- df[, names(df) %in% metric_list]
            
            worker_name <- switch(ip,
                                  '192.168.0.162' = '_pub',
                                  '192.168.0.163' = '_pri_1',
                                  '192.168.0.164' = '_pri_2',
                                  '192.168.0.165' = '_pri_3',
                                  '192.168.0.166' = '_pri_4',
                                  '192.168.0.167' = '_pri_5',
                                  '192.168.0.168' = '_pri_6')
            
            names(df) <- paste0(names(df), worker_name)
            
            time_name <- paste0('time', worker_name)
            
            names(df) <- gsub(time_name, 'time', names(df))
            
            res <- inner_join(res, df,
                              by = 'time')
        }
    }
    
    res <- subset(res, select = -time)
    
    return(res)
    
}

correlation_D3 <- function(mtx, lag = 5) {
    
    node_df <- data.frame('node' = colnames(mtx),
                          'idx' = 0:(ncol(mtx) - 1),
                          'size' = 0,
                          stringsAsFactors = F)
    
    link_df <- data.frame('source' = NA, 'target' = NA, 'edge' = NA)
    
    idx <- 1
    
    for (i in 1:nrow(node_df))
    {
        for (j in 1:nrow(node_df))
        {
            if (i == j) {next}
            else
            {
                x <- -diff(as.matrix(log(mtx[, node_df$node[i]] + 1e-6)))
                y <- -diff(as.matrix(log(mtx[, node_df$node[j]] + 1e-6)))
                
                for (k in 1:lag)
                {
                    
                    g_test <- grangertest(x, y, k)
                    
                    if (g_test$`Pr(>F)`[2] <= 0.05)
                    {
                        link_df[idx, ] <- c(i - 1, j - 1, lag + 1 - k)
                        idx <- idx + 1
                        break
                    }
                }
            }
        }
    }
    
    for (i in node_df$idx) {
        
        node_df$size[i + 1] <- sum(link_df$source == i)
        
    }
    
    net <- forceNetwork(Links = link_df,
                        Nodes = node_df,
                        Source = 'source',
                        Target = 'target',
                        NodeID = 'node',
                        Group = 'node',
                        linkDistance = 200,
                        zoom = T,
                        fontSize = 10,
                        opacityNoHover = T,
                        legend = T,
                        arrows = T,
                        opacity = 1,
                        Value = 'edge',
                        Nodesize = 'size')
    
    return(net)
    
}

# df <- multiple_join(5, '1h', c('192.168.0.163'), c('load1', 'load5', 'cpu_used_percent'))

read_metric_list <- function(is_host, period, group_by) {
    
    connector <- connect()
    
    con <- connector$connector
    
    dbname <- connector$dbname
    
    period <- paste0(period, 'd')
    
    # master metric name
    query <- "show field keys on %s from %s"
    
    query <- sprintf(query, dbname, 'cluster')
    
    res_master <- influx_query(con,
                               db = dbname,
                               query = query,
                               simplifyList = T,
                               return_xts = F)[[1]] %>% as.data.frame()
    
    master_metric_name <- as.vector(res_master$fieldKey)
    
    master_metric_name <- c(master_metric_name, 'time')
    
    if (!is_host) {
        
        return(master_metric_name)
        
    } else {
        
        query <- "show field keys on %s from host"
        
        query <- sprintf(query, dbname)
        
        res_host <- influx_query(con,
                                 db = dbname,
                                 query = query,
                                 simplifyList = T,
                                 return_xts = F)[[1]] %>% as.data.frame()
        
        worker_metric_name <- as.vector(res_host$fieldKey)
        
        return(c(master_metric_name, worker_metric_name))
        
    }
    
}


# Influx DB명을 가져온다.
ifx_getDBList <- function(url, port) {
    con <- influx_connection(host = url, port = port)
    show_databases(con = con)
}


# Influx DB에서 Table명을 가져온다.
ifx_getTableList <- function(url, port, dbName) {
    
    con <- influx_connection(host = url, port = port)
    df <- influx_query(con, db = dbName, 
                       query = "show measurements;",
                       return_xts = FALSE)
}    


# Table을 정해진 개수 만큼 뽑아낸다 
ifx_getSameple <- function(url, port, dbName, tblName, limit = 1000) {
    
    
    if (tblName != "sampledata") {
        qry <- "SELECT * 
        FROM __TABLE__ 
        WHERE time > now() - 1d 
        LIMIT __LIMIT__;"
    } else {
        qry <- "SELECT * 
        FROM __TABLE__ 
        WHERE time < now() + 1d
        LIMIT __LIMIT__;"
        
    }
    
    
    qry <- gsub("__TABLE__", tblName, qry)
    qry <- gsub("__LIMIT__", limit, qry)
    
    con <- influx_connection(host =url, port=port)
    df <- influx_query(con, db = dbName, 
                       query = qry,
                       return_xts = FALSE)
    
    df[[1]]
}


ifx_getGroupBy <- function(url, port, dbName, tblName, msName, period, group_by = "time(1h)", addWhere,
                           type = "forecast") {
    
    # url = "192.168.0.162"
    # port = "10091"
    # dbName = "telegraf"
    # tblName = "cpu"
    # msName = "usage_system"
    # group_by = "1h"
    # period = "20"
    # type = "anomaly"
    # addWhere =""
    
    con <- influx_connection(host = url, port = port)
    
    if (addWhere != "") addWhere <- paste0(" and ", addWhere)
    
    df <- influx_select(con = con, 
                        db = dbName, 
                        field_keys = paste0("mean(", msName, ")"), 
                        measurement = tblName,
                        group_by = paste0("time(", group_by, ")"),
                        where = paste0("time > now() - ", period ,"d", addWhere),
                        limit = 1000, 
                        order_desc = TRUE, 
                        return_xts = FALSE)[[1]]
    
    names(df)[ncol(df)] <- msName
    
    if (type == "forecast") {
        
        series <- xts(df[, msName], order.by = df$time, tz = "GMT")
        
    } else if (type == "anomaly") {
        
        tb <- subset(df, select = c("time", msName))
        
        # print(tb)
        
        ts <- seq.POSIXt(min(tb$time),
                         max(tb$time),
                         by = posixt_helper_func(str_sub(group_by, -1)))
        
        d1 <- tibble(time = ts)
        series <- full_join(d1, tb)
        series[,2] <- na.approx(series[,2])
        
    }
    
    return(series)
    
}


ifx_getAnomaly <- function(url, port, dbName, tblName, msName, period, group_by, addWhere,
                           type = "forecast") {
    
    con <- list()
    con$connector <- influx_connection(host = url, port = port)
    con$dbName <- dbName
    
    connector <- con$connector
    dbname <- con$dbName
    
    query = "select mean(${msName}) as metric \
             from ${tblName} \
             where time > now() - ${period}d  
             group by time(${group_by}) \
             fill(none)"
    # and \
    # host = \'${host}\' \
    
    query <- str_interp(query)
    
    raw_data <- influx_query(connector,
                             db = dbName,
                             query = query,
                             simplifyList = T,
                             return_xts = F)[[1]]
    
    tb <- subset(raw_data, select = c(time, metric))
    
    ts <- seq.POSIXt(min(tb$time),
                     max(tb$time),
                     by = posixt_helper_func(str_sub(group_by, -1)))
    
    df <- tibble(time = ts)
    
    tb <- full_join(df, tb)
    
    tb$metric <- na.approx(tb$metric)
    
    return(tb)
}


ifx_getMetrics <- function(url, port, dbName, tblName,
                           limit = 1000, addWhere = "") {
    
    # url = "192.168.0.162"
    # port = "10091"
    # dbName = "nexclipper"
    # tblName = "cluster"
    # # msName = "usage_system"
    # group_by = "1h"
    # period = "5"
    # addWhere =""
    # limit = 1000
    
    con <- influx_connection(host =url, port=port)
    
    if(addWhere != "") addWhere <- paste0(" and ", addWhere)
    
    qry <- "SELECT * 
            FROM ${tblName} 
            WHERE time > now() - 1d ${addWhere}
            LIMIT ${limit};"
    
    qry <- str_interp(qry)
    
    df <- influx_query(con, db = dbName, 
                       query = qry,
                       return_xts = FALSE)[[1]]
    
    # show_measurements(con, "nexclipper")
    
    
    # numeric 변수 골라 내기..
    num_var = names(df)[sapply(df, class) == "numeric"]
    # 분산이 0이 아닌 변수 뽑아내기.
    not0var = names(df[,num_var])[sapply(df[,num_var], var) != 0]
    
    not0var = setdiff(not0var, "timestamp")
    
    library(tidyr, dplyr)
    # df %>% gather(var, value, num_var) 
    
    sel_df <- df %>% select(not0var)
    
}


# Sample 데이터를 Influx DB에 만들기 
ifx_makeSampleTable <- function() {
    
    con <- influx_connection(host = "192.168.0.162", port = "10091")
    dbName <- 'mesos_metric'
    
    library(lubridate)
    
    # 1.1. Import Raw Data ----
    tmp <- fread("Data/skt.csv", stringsAsFactors = T)
    tmp[, Date := strptime(as.character(tmp$일자),
                           format = "%Y%m%d",
                           tz = "GMT")  + hours(tmp$시간대)]
    
    df <- tmp
    df <- df[, .(call  = sum(통화건수)), by = c("Date","업종")]
    names(df) <- c("ds", "type", "y")
    
    levels(df$type) = paste0("metric_", 1:4)
    df$type <- as.character(df$type)
    
    library(tidyr)
    df <- spread(df, type, y)
    
    for (i in names(df))
        df[is.na(get(i)), (i) := 0]
    
    
    # 1.2. Delete Table 
    influx_query(con, db = dbName,
                 query = "DROP MEASUREMENT sampledata")
    
    # 1.3. Create Table 
    influx_write(con = con, db = dbName,
                 x = df,
                 time_col = "ds",
                 measurement = "sampledata")
    
    show_measurements(con = con, db = dbName)
    
}

