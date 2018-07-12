#### PLUMBER API RUN ####

library(plumber)

app <- plumb('plumb_api.R')

app$run(port = 8000)
