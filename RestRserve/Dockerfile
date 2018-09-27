FROM r-base:latest

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    libssl-dev &&
    libxml2-dev &&
    libmariadbclient-dev &&
    libv8-3.14-dev

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

COPY RestRserve/ .

RUN Rscript 00.R

CMD ["Rscript", "-e", "source(system.file('app.R', package = 'RestRserve'), keep.source = TRUE)"]