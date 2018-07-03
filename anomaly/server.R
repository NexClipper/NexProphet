
# A. 사전 Setting --------------------------------------------------------------------------------------
# A.1. Source 및 library 설정 ::::::::  -------------------------------------------------

# 개발된 함수를 불러 온다
source("Source/BIZSCAPE_FUNCTIONS.R", local = T, encoding="utf-8")

# 필요한 라이브러리가 설치 안되었으면 설치하고, 라이브러리로 등록한다  
# 샤이니 관련  
bs.Library(c("shiny", "shinydashboard", "shinyAce", "shinyBS", "shinyjs", "shinyFiles", "shinyTree",
             "rhandsontable", "RColorBrewer", "colourpicker", "listviewer"))

# 기타 패키지 
bs.Library(c("dygraphs", "visNetwork"))



# B. Dashboard UI Setting ::::::::  --------------------------------------------------------------------------------------
# B.0. Global Variables ::::::::  --------------------------------------------------------------------------------------


# B.1. Sidebar Menu Setting ::::::::  ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 참고 사이트 : https://rstudio.github.io/shinydashboard/structure.html#header

shinyServer(function(input, output, session){
    
    
})

