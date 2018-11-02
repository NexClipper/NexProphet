library(RestRserve)

configuration = c("http.port" = "8484",
                  "encoding" = "utf8",
                  "port" = "6311")

dir = tempdir()

app_path <- paste0(getwd(), '/app.R')

restrserve_deploy(file = app_path,
                  dir = dir,
                  configuration = configuration)

restrserve_start(dir)