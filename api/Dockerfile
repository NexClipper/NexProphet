FROM rocker/r-base
MAINTAINER Jeff Allen <docker@trestletech.com>

RUN apt-get update -qq && apt-get install -y \
  git-core \
  libssl-dev \
  libcurl4-gnutls-dev \
  r-base

MKDIR -p /home/plumber

WORKDIR /home/plumber

COPY /api .

RUN Rscript api/load_package.R

RUN R -e 'install.packages(c("devtools"))'

RUN R -e 'devtools::install_github("trestletech/plumber")'

# RUN R -e 'install.packages(c("tidyverse", "data.table", "influxdbr", "xts", "zoo", "prophet"))''

EXPOSE 8000

CMD Rscript api/app.R