FROM rocker/r-base
MAINTAINER Jeff Allen <docker@trestletech.com>

RUN apt-get update -qq && apt-get install -y \
  git-core \
  libssl-dev \
  libcurl4-gnutls-dev \
  r-base \
  libxml2-dev

WORKDIR /home

COPY app.R .

COPY plumb_api.R .

COPY load_package.R .

RUN Rscript load_package.R

RUN R -e 'install.packages(c("devtools"))'

RUN R -e 'devtools::install_github("trestletech/plumber")'

EXPOSE 8000

CMD Rscript app.R
