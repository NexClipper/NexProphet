FROM r-base:latest

RUN apt-get install -y libssl-dev

RUN apt-get install -y libxml2-dev

RUN apt-get install -y libmariadbclient-dev

WORKDIR /home

RUN chmod o+w /usr/local/lib/R/site-library

COPY ../Source/install_pkg/disk00.R /home

RUN Rscript disk00.R

COPY ../disk /home/disk

COPY ../internal.conf /home

RUN apt-get update -y

CMD ["Rscript", "/disk/app.R"]
