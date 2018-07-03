
posixt_helper_func <- function(x) {
    if (x == 's')
    {
        return('sec')
    }
    
    else if (x == 'm')
    {
        return('min')
    }
    
    else if (x == 'h')
    {
        return('hour')
    }
    
    else {stop('incorrect group time')}
}

# Influx DB명을 가져온다.
ifx_getDBList <- function(url, port) {
    con <- influx_connection(host =url, port=port)
    show_databases(con = con)
}

# Influx DB에서 Table명을 가져온다.
ifx_getTableList <- function(url, port, dbName) {
    
    con <- influx_connection(host =url, port=port)
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
    
    con <- influx_connection(host =url, port=port)
    
    if(addWhere != "") addWhere <- paste0(" and ", addWhere)
    
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
        
        series <- xts(df[, msName], order.by = df$time, tz="GMT")
        
    } else if(type == "anomaly") {
        
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


posixt_helper_func <- function(x) {
    if (x == 's') 'sec'
    else if (x == 'm') 'min'
    else if (x == 'h') 'hour'
    else stop('incorrect group time')
}


ifx_getAnomaly <- function(url, port, dbName, tblName, msName, period, group_by, addWhere,
                           type = "forecast")
{

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

ifx_getMetrics <- function(url, port, dbName, tblName, limit = 1000) {
    
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
            WHERE time > now() - 1d 
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
    
    con <- influx_connection(host ="192.168.0.162", port="10091")
    dbName <- 'mesos_metric'
    
    library(lubridate)
    
    # 1.1. Import Raw Data ----
    tmp <- fread("Data/skt.csv", stringsAsFactors = T)
    tmp[, Date := strptime(as.character(tmp$일자), format="%Y%m%d", tz="GMT")  + hours(tmp$시간대)]
    
    df <- tmp
    df <- df[, .(call  = sum(통화건수)), by =c("Date","업종")]
    names(df) <- c("ds", "type", "y")
    
    levels(df$type) = paste0("metric_", 1:4)
    df$type <- as.character(df$type)
    
    library(tidyr)
    df <- spread(df, type, y)
    
    for (i in names(df))
        df[is.na(get(i)), (i):=0]
    
    
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

