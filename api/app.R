#### PLUMBER API RUN ####

library(plumber)

app <- plumb('plumb_api.R')

app$run(host = 'localhost', port = 8282)
