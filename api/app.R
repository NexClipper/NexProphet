#### PLUMBER API RUN ####

library(plumber)

app <- plumb('plumb_api.R')

app$run(host = '192.168.0.50', port = 8282)
