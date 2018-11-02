FROM r-base:latest

RUN apt-get update -y

RUN apt-get install -y libssl-dev

RUN apt-get install -y libxml2-dev

RUN apt-get install -y libmariadbclient-dev

RUN apt-get install -y libcurl4-openssl-dev

WORKDIR /home

RUN chmod o+w /usr/local/lib/R/site-library

COPY 00.R /home

ARG CACHEBUST=1

RUN Rscript 00.R

COPY 01.R /home

RUN Rscript 01.R

RUN mkdir event-disk

COPY . event-disk/

WORKDIR event-disk

CMD Rscript app.R
