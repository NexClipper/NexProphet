FROM rocker/shiny

RUN apt-get -y update && apt-get -y upgrade

RUN apt-get install -y libssl-dev

RUN apt-get install -y libxml2-dev

RUN apt-get install -y libmariadbclient-dev

RUN apt-get install -y libv8-3.14-dev

RUN rm -rf /srv/shiny-server

RUN mkdir /srv/shiny-server

WORKDIR /srv/shiny-server

RUN chmod o+w /usr/local/lib/R/site-library

RUN chmod o+w /srv/shiny-server

COPY Source/install_pkg/00.R .

RUN Rscript 00.R

COPY Source/install_pkg/01.R .

RUN Rscript 01.R

COPY . .

RUN apt-get update -y
