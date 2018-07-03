### BIZSCAPE 함수 모음 ### ##########################################################################
#   모든 함수는 data.table과 dplyr 기반으로 구현한다.
#   Git Hub에 올려서 공동작업하는 것을 연구한다. 
#   bs.을 접두사로 붙인다. 
#   .이후의 단어 첫자는 대문자로 통일한다.
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

library(data.table)
library(dplyr)
library(stringr)
library(zoo)


# 01. Install 관련 ---------------------------------------------------------------------------------
bs.Library <- function(pkg, add = T){
  
  # Description: 
  #   패키지 설치유무 확인 후 설치되어 있지 않으면 설치 후 라이브러리 등록
  #
  # Args:
  #   pkg : 설치할 패키지(들), 텍스트 벡ㅌ
  #   add : 라이브러리 등록 여부 (default: TRUE)
  #
  # Output:
  #   패키지 설치
  #
  # Update: 2017.08.03
  
  
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  
  if (add == TRUE)
    sapply(pkg, require, character.only = TRUE)
}

# Example &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#
# bs.Library("stringr")
#
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
























# 02. 파일 입출력 관련 -----------------------------------------------------------------------------

bs.MergeFiles <- function(dir.name, find.pattern, b.sep.var = TRUE ) {
  
  # Description: 
  #   한 폴더에 있는 여러 화일 합치기...
  #
  # Args:
  #   dir.name <- "data"        # 폴더명, directory name의 끝에 /는 없어야 한다.
  #   find.pattern <-"LIMS*"    # 화일을 찾을 패턴
  #   b.sep.var <- True         # 화일하나하나를 구분하는 변수 : 화일명
  #
  # Output:
  #   합쳐진 데이터 테이브
  #
  # Update: 2017.08.03
  


  # 패턴이 포함된 모든 화일 
  file.list <- list.files(dir.name, find.pattern)
  
  # 폴더명과 화일명 결합
  fullfile.names <- paste(dir.name, file.list, sep = "/")
  
  # 모든 화일 읽어 오기
  
  dt <- do.call(rbind,
               lapply(fullfile.names, 
                      function(path){
                        dat <- fread(path)
                        
                        if (b.sep.var) {
                          f.name <- unlist(str_split(path, "/"))
                          dat[ , File := f.name[length(f.name)]]
                        }
                        
                      }))

}

# Example &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#
# dt <- bs.MergeFiles(dir.name = "data", find.pattern = "LIMS*") 
# dt
#
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&





















# 03. EDA 관련 -------------------------------------------------------------------------------------

bs.ColTypeTransform <- function(dat, func = as.numeric, col.ind = NULL) {
  
  # Description: 
  #   Numeric으로 변환 가능한 모든 변수를 변환 
  #
  # Args:
  #   dat <- as.data.table(iris)        # 입력 데이터 테이블
  #   func <- as.numeric                # 변수변환할 함수 (User Defined 함수도 된다)
  #   col.ind <- c(3,4)                 # 변환할 컬럼 번호
  # Output:
  #   변환된 데이터 테이블 
  #
  # Update: 2017.08.03
  
  if (is.null(col.ind)) {
    dat <- dat[, lapply(.SD, func)] 
  } else {
    dat[, col.ind] <- dat[, lapply(.SD, func), .SDcols = col.ind]
  }
  
  dat
}

# Example &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#
# dat <- as.data.table(iris)
# dat <- bs.ColTypeTransform(dat, func = as.numeric)
# str(dat)
# 
# dat <- as.data.table(iris)
# dat <- bs.ColTypeTransform(dat, func = as.character, col.ind = 3:5)
# str(dat)
#
# dat <- as.data.table(iris)
# dat <- bs.ColTypeTransform(dat, func = function(x) 10*x, col.ind = 3:4)
# str(dat)
#
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&




bs.ZeroVar <- function(dat, useNA = 'ifany') {

    # Description: 
  #   상수로 되어 있는 (zero variance) 의미 없는 컬럼 모두 찾기 
  #
  # Args:
  #   dat <- as.data.table(iris)        # 입력 데이터 테이블
  #   useNA                             # c("no", "ifany", "always"), table함수의 입력 
  # Output:
  #   컬럼 Index 
  #
  # Update: 2017.08.03
  
  out <- apply(dat, 2, function(x) {length(table(x, useNA = useNA))})     
  which(out==1)
}

# Example &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
#
# dat <- as.data.table(iris)
# dat[, 3:4] <- 3
# dat
# bs.ZeroVar(dat)
#
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


bs.RemoveConstantColumns <- function(dat) {
  
  # Description: 
  #   상수로 되어 있는 (zero variance) 의미 없는 컬럼 모두 삭ㅈ
  #
  # Args:
  #   dat <- as.data.table(iris)        # 입력 데이터 테이블
  #
  # Output:
  #   변환된 데이터 테이블 
  #
  # Update: 2017.08.03
  

  dat[, -bs.ZeroVar(dat), with = F] 

}

# Example &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# 
# dat <- as.data.table(iris)
# dat[, 3:4] <- 3
# dat
# dat <- bs.RemoveConstantColumns(dat)
# dat
# 
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&




bs.SummaryStat <- function(dat, optFast = T) {
      
      # Description: 
      #     각 변수의 통계치를 테이블 형태로 출ㄹ 
      #
      # Args:
      #     dat <- as.data.table(iris)        # 입력 데이터 테이블
      #     optFast                           # 빠른 실행 옵셔 
      # Output:
      #     결과 테이브
      #
      # Update: 2017.010.109

      library(psych)
      library(Hmisc)
      stat <- psych::describe(dat, fast = optFast)  
      head <- psych::describeData(dat, head = 2, tail = 2)  
      
      out <- cbind(as.data.frame(stat), as.data.frame(head$variables)[, 3:7])
      
      names(out) <- capitalize(names(out))
      out$Vars <- rownames(out)
      
      out
      
}

# Example &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# 
# bs.SummaryStat(iris, optFast = T)
# 
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

# 





# 04. Sample Data 관련 -----------------------------------------------------------------------------


bs.GetLS.List <- function() {
  
  # Description: 
  #   현재 R 메모리(ls()에 올라와 있는 리스트 중에 데이터 프레임(DF)이나, 
  #   데이터 테이블(DT), 매트릭스(MATRIX) 등 분석 가능한 것 리스트를 가져온다 
  #
  # Args:
  #    
  # Output:
  #   Dataset 이름이 들어 있는 벡터
  #
  # Update: 2017.08.04

  a <- 1   
      
  list <- ls(envir=globalenv())

  dfs <- list[
              sapply(mget(list, globalenv()), 
                     function(x) {is.data.frame(x) | is.data.table(x) | is.matrix(x)} )
              ]
  dfs <- as.character(dfs)

  return(dfs) 
}

# Example &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# 
# bs.GetLS.List()
# 
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&


bs.GetRDatasetList <- function(optTable = TRUE) {

  # Description: 
  #   R에 내장되어 있는 Dataset의 리스트를 data.frame 형태로 반환해 주는 함수
  #
  # Args:
  #    
  # Output:
  #   데이터 이름, 설명, Class가 들어 있는 data.table
  #   # 1 	BOD		Biochemical Oxygen Demand 			        data.frame
  #   #	2   CO2		Carbon Dioxide Uptake in Grass Plants		data.frame
  #
  # Update: 2017.08.04  
  
  
  # 모든 데이터 셋과 설명 :  [1] "AirPassengers  Monthly Airline Passenger Numbers 1949-1960" 
  data.chr <- library(help = "datasets")$info[[2]]
  
  n <- length(data.chr)
  
  k <- 0
  data.list <- list()
  df.list <- data.frame()
  
  for(i in 1:n) {
    
    temp1 <- strsplit(data.chr[i], "\\s+")[[1]]
    
    if (temp1[1] != "") {
      
      k <- k + 1
      if (i != n) {
        temp2 <- strsplit(data.chr[i+1], "\\s+")[[1]]
      }
      
      if (temp2[1] != "") {
        
        # 데이터 이름을 지운 설명만 가져오기  
        data.description <- temp1[-1]
        
      } else {
        data.description <- paste0(temp1[-1], " ", temp2[-1])
        
      }
      
      # data.list[ <- list(name = temp1[1], description = paste(data.description, collapse = ' '))
      df.list[k,1] <- temp1[1]
      df.list[k,2] <- paste(data.description, collapse = ' ')
      
      cls <- ""
      try(cls <- class(get(temp1[1])), silent = TRUE)
      
      df.list[k,3] <- cls[length(cls)]
      
    }
    
  }
  
  colnames(df.list) <- c("Names", "Description", "Class")
  
  if (optTable) {
    df.list <- df.list[df.list$Class == "data.frame" | df.list$Class == "matrix"  , ]
  }
  
  return(as.data.table(df.list)) 
}

# Example &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# 
# bs.GetRDatasetList()
# 1 	BOD		Biochemical Oxygen Demand 			        data.frame
# 2	  CO2		Carbon Dioxide Uptake in Grass Plants		data.frame
#
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

















# 05. Visualization (ggplot, plotly, dygraph 중심 --------------------------------------------------


# 대응분석
# MCA 등 : http://www.sthda.com/english/wiki/fviz-mca-quick-multiple-correspondence-analysis-data-visualization-r-software-and-data-mining

  # bs.Library("ca")
  # data("smoke")
  # smoke
  # # This dataset contains frequencies of smoking habits (none, light, medium and
  # # heavy) for staff groups (senior managers, junior managers, senior employees, junior employees
  # # and secretaries)
  # fit <- ca(smoke)
  # print(fit) # basic results 
  # summary(fit) # extended results 
  # plot(fit) # symmetric map
  # plot(fit, mass = TRUE, contrib = "absolute", map =
  #        "rowgreen", arrows = c(FALSE, TRUE)) # asymmetric map


# 06. Time Series 관련 -----------------------------------------------------------------------------

bs.AddDateComponents <- function(data, date_Var = "date") {
      
      # Description: 
      #   날짜가 들어 있는 데이터를 받아서, 년, 월, 일, 요일, 주말 등의 정보를 붙인다.
      #
      # Args:
      #   data          : time series data
      #   date_Var      : 날짜 변수명 (일자별)
      #
      # Output:
      #   추가된 데이터 
      #
      # Update: 2017.10.23
      
      data[, year := as.factor(data.table::year(get(date_Var)))]
      data[, quarter := as.factor(data.table::quarter(get(date_Var)))]
      data[, month := as.factor(data.table::month(get(date_Var)))]
      data[, mday := as.factor(data.table::mday(get(date_Var)))]
      data[, wday := as.factor(data.table::wday(get(date_Var)))]
      data[, yday := as.factor(data.table::yday(get(date_Var)))]
      
      data[, week := as.factor((as.numeric(yday)-1)%/%7 + 1)]
      data[week == 53, week := 52]
      # data[, weeknum := as.factor(data.table::isoweek(get(date_Var)))]
      # data[weeknum == 53, weeknum := 52]
}



bs.AddOutlierComponents <- function(data, holiday_Var = "holiday") {
      
      # Description: 
      #   날짜가 들어 있는 데이터를 받아서, 년, 월, 일, 요일, 주말 등의 정보를 붙인다.
      #
      # Args:
      #   data          : time series data
      #   holiday_Var      : 휴일 & 이상치 변수명 (일자별)
      #
      # Output:
      #   추가된 데이터 
      #
      # Update: 2017.10.23
      
      # Outlier 설정 
      data[, outlier.TF := ifelse(holiday == "", F, T)]
      
}



bs.CMA <- function(val, window, outliers = NULL, mean_opt = "mean") {

  # Description: 
  #   벡터/컬럼을 받아서, CMA 변환된 벡터로 반환해 주는 함ㅅ
  #
  # Args:
  #   val       : time series
  #   window    : window size
  #   outliers  : if outlier = T then not calculate mean  (vector 형태)
  #   mean_opt  : "mean" or "median"
  #
  # Output:
  #   CMA 변환된 결과 Vector
  #
  # Update: 2017.08.04  
  
  # 아웃라이러가 NULL이면 조건을 모두 FALSE로 설정 (모두 계사)
  if (is.null(outliers)) {
    outliers <- rep(FALSE, length(val))
  }
  
  mean.outlier <- function(x, outlier) {
    mean(x[outlier == FALSE], na.rm = TRUE)
  }
    
  if (mean_opt == "mean") {
    rollapply(val*ifelse(outliers==T, NA, 1), window, partial = TRUE, mean, na.rm = TRUE) 
  } else if (mean_opt == "median") {
    rollapply(val*ifelse(outliers==T, NA, 1), window, partial = TRUE, median, na.rm = TRUE) 
  }
  
}

# Example &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# 
# window = 3
# val <- c(3,2,1,2,6,3,4,1,2)
# bs.CMA(val, 3)
# 
# window = 3
# val <- c(3,2,1,2,6,3,4,1,2)
# cond <- c(F,T,F,T,F,F,F,F,F)
# bs.CMA(val, 3, cond)
# 
# dat <- as.data.table(iris)
# dat[ , SL_CMA := bs.CMA(Sepal.Length, 3, mean_opt = "median")]
# dat
#
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



bs.Decompose_DailyTimeSeries <- function(data,  date_Var = "date", y_Var = "y", holiday_Var = "holiday", mean_opt = "median",
                                                long.window = 182 * 2 + 1,
                                                month.window = 112 * 2 + 1,
                                                season.window = 14 * 2 + 1,
                                                week.window = 26 * 2 + 1,
                                                weeknum.window = 28 * 2 + 1,
                                                holi.window = 2 *2 + 1,
                                                verbose = TRUE
                                         ) {
      
      # Description: 
      #   날짜가 들어 있는 데이터를 받아서, 년, 월, 일, 요일, 주말 등의 정보를 붙인다.
      #
      # Args:
      #   data          : time series data : date, y, holiday가 들어 있어야 한다 
      #   long.window = 182 * 2 + 1 # 1년
      #   month.window = 112 * 2 + 1 # 약 7년                                           
      #   season.window = 14 * 2 + 1 # 약 4년 
      #   week.window = 26 * 2 + 1  # 1년 53주
      #   weeknum.window = 28 * 2 + 1  # 7년 
      # 
      #   holi.window = 2 *2 + 1,
      #   mean_opt = "median"  각 효과를 추정하는 통계 방식 
      #
      # Output:
      #   Decompose 결과가 들어 있는 테이블 
      #
      # Update: 2017.10.23      
      
      
      
      # data <- TS_Traffic[category=="판교"]
      # date_Var = "date"
      # y_Var = "traffic"
      # holiday_Var = "holiday"
      # 
      # long.window = 182 * 2 + 1 # 1년 
      # month.window = 112 * 2 + 1 # 약 7년 
      # season.window = 14 * 2 + 1 # 약 4년 
      # week.window = 26 * 2 + 1  # 1년 53주
      # weeknum.window = 28 * 2 + 1  # 7년 
      # 
      # holi.window = 2 *2 + 1   # 5년
      # mean_opt = "median"
      # 
      
      
      # 0. 변수명 재정의, date, y, holiday로 OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO   
      names(data)[names(data) == date_Var] = "date" 
      names(data)[names(data) == y_Var] = "y" 
      names(data)[names(data) == holiday_Var] = "holiday" 
      
      # 1. 날짜 컴포넌트 추가 OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
      data <- bs.AddDateComponents(data)
      data <- bs.AddOutlierComponents(data)
      
      
      # 2. Decompose OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
      
      # (0) 날짜 오름차순으로 정렬
      data[order(date)]
      
      
      # (1) Tr : Trend CMA 구하기 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      data[ , Tr := bs.CMA(y, long.window, outlier.TF, mean_opt = mean_opt)]
      
      # (2) Y / Trend : 트렌드에서 벗어난 정도 
      data[ , Y_Trend := y/Tr]
      
      # (3) M : Month 효과 구하기
      data[ , M := bs.CMA(Y_Trend, season.window, outlier.TF, mean_opt = mean_opt), by = month]
      
      # (4) Season Smoothing : CMA_7(Y_Trend) : M을 부드럽게 스무딩 
      data[ , M.SM := bs.CMA(M, season.window, mean_opt ="mean")]
      
      # (5) TxM  
      data[ , TxM := Tr * M.SM]
      
      # (6) y / TxM  : 트렌드 x 월 에서 벗어난 정도 
      data[ , y_TxM := y / (Tr * M.SM)]
      
      # # (7) WeekNum Effect
      # data[ , W := bs.CMA(y_TxM, weeknum.window, outlier.TF, mean_opt = mean_opt), by = week ]
      # 
      # # (8) Period Effect 
      # data[ , P := bs.CMA(y_TxM, weeknum.window, outlier.TF, mean_opt = mean_opt), by = period ]
      
      # (9) Weekday Effect : 요일 효과 
      data[ , W := bs.CMA(y_TxM, week.window, outlier.TF, mean_opt = mean_opt), by = wday ]
      
      # (10) holi Effect
      data[ , H := bs.CMA(y_TxM, holi.window, mean_opt = mean_opt), by = holiday ]

      # (11) T x M x W
      data[ , TxMxW := Tr * M.SM * W]
      
      # (12) T x M x W X H
      data[ , TxMxWxH := Tr * M.SM * ifelse(outlier.TF == FALSE, W, H)]
      
      # (13) Error
      data[ , Error := y/TxMxWxH - 1]
      
      
      # 3. Result : 효과 정리 OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO
      
      # 월별효과 
      month.effect.all <- data[holiday == "", .(mon.effect = mean(M)), by = .(year, month)]
      setorder(month.effect.all, year, month)
      month.effect <- month.effect.all[, .(mon.effect = mean(mon.effect)), by = .(month)]
      
      # 요일별 효과 
      weekday.effect.all <- data[holiday == "", .(weekday.effect = mean(W)), by = .(year, wday)]
      setorder(weekday.effect.all, year, wday)
      weekday.effect <- weekday.effect.all[, .(weekday.effect = mean(weekday.effect)), by = .(wday)]

      # 휴일 효과 
      holiday.effect.all <- data[holiday != "", .(holiday.effect = mean(H)), by = .(year, holiday)]
      holiday.effect <- holiday.effect.all[, .(holiday.effect = mean(holiday.effect)), by = .(holiday)]
            
      
      result <- list()
      result$data <- data
      result$month.effect.all <- month.effect.all
      result$month.effect <- month.effect
      result$weekday.effect.all <- weekday.effect.all
      result$weekday.effect <- weekday.effect
      result$holiday.effect.all <- holiday.effect.all
      result$holiday.effect <- holiday.effect
      
      result
      
}







# 07. Graphic, Color, Chart 관련 -----------------------------------------------------------------------------

bs.Colors_Theme1 <- function() {
      
      # Color Theme 1
      
      c("bisque","darkorange","cadetblue", "coral")
      
}

bs.Colors_Theme2 <- function() {
      
      # Color Theme 2
      
      c("bisque","darkorange","cadetblue", "coral")
      
}

bs.Colors_Theme3 <- function() {
      
      # Color Theme 3
      
      c("bisque","darkorange","cadetblue", "coral")
      
}


## Add an alpha value to a colour (컬러에 투명도 추가)
bs.AddAlpha <- function(col, alpha=1){
      if(missing(col))
            stop("Please provide a vector of colours.")
      apply(sapply(col, col2rgb)/255, 2, 
            function(x) 
                  rgb(x[1], x[2], x[3], alpha=alpha))  
}

# Example &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# 
# myColours = c(1, "steelblue", "#FFBB00", rgb(0.4, 0.2, 0.3))
# add.alpha(myColours, alpha=0.4)     
#
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&



# 08. Machine Learning 관련 -----------------------------------------------------------------------------

bs.DecisionBoundary <- function(model, data, class = NULL, predict_type = "class",
                         resolution = 100, showgrid = TRUE, ...) {
      library(caret)
      
      if(!is.null(class)) 
            cl <- data[, class] 
      else 
            cl <- 1
      
      data <- data[ , 1:2]
      k <- length(unique(cl))
      
      # plot(data, col = as.integer(cl) + 1L, pch = as.integer(cl) + 1L,  ...)
      plot(data, col = as.integer(cl) + 1L, 
           bg = bs.AddAlpha(as.integer(cl) + 1L, 0.5), 
           pch = 21, 
           cex = 1.5, ...)
      
      
      # make grid
      r <- sapply(data, range, na.rm = TRUE)
      xs <- seq(r[1,1], r[2,1], length.out = resolution)
      ys <- seq(r[1,2], r[2,2], length.out = resolution)
      g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))

      colnames(g) <- colnames(r)
      g <- as.data.frame(g)
      
      ### guess how to get class labels from predict
      ### (unfortunately not very consistent between models)
      p <- predict(model, g, type = predict_type)
      
      if(is.list(p)) 
            p <- p$class
      
      p <- as.factor(p)
      
      if(showgrid) points(g, col = as.integer(p) + 1L, pch = ".")
      
      z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
      
      contour(xs, ys, z, add = TRUE, drawlabels = FALSE, col = colors()[276],
              lwd = 2, levels = (1:(k-1))+.5)
      
      invisible(z)
}

# Example &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
# 
# data(iris)
# # Three classes
# x <- iris[1:150, c("Sepal.Length", "Sepal.Width", "Species")]
# library(caret)
# model <- knn3(Species ~ ., data=x, k = 1)
# bs.DecisionBoundary(model, x, class = "Species", main = "kNN (1)")
#
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

# 10. 크롤링 관련 ----------------------------------------------------------------------------------
