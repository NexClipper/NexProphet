FROM r-base:latest

RUN apt-get update -y

RUN apt-get install -y libssl-dev

RUN apt-get install -y libxml2-dev

RUN apt-get install -y libmariadbclient-dev

RUN apt-get install -y libv8-3.14-dev

RUN apt-get install -y libcurl4-openssl-dev

RUN Rscript -e "install.packages( c( \
    'Rserve', \
    'R6', \
    'uuid', \
    'base64enc', \
    'yaml', \
    'swagger', 'mime', 'remotes'), \
  repos = c('https://cran.rstudio.com/', 'http://www.rforge.net/'))"

RUN Rscript -e "remotes::install_github('dselivanov/RestRserve')"

WORKDIR /home

COPY . .

RUN Rscript 00.R

CMD ["Rscript", "-e", "source(system.file('app.R', package = 'RestRserve'), keep.source = TRUE)"]
