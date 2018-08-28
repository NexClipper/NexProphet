FROM r-base:latest

RUN apt-get update -y

RUN apt-get install -y libssl-dev

RUN apt-get install -y libxml2-dev

RUN apt-get install -y libmariadbclient-dev

WORKDIR /home

RUN chmod o+w /usr/local/lib/R/site-library

COPY . /home

RUN Rscript disk00.R

RUN apt-get install -y libcurl4-openssl-dev

RUN Rscript disk01.R

RUN apt-get update -y

CMD ["Rscript", "app.R"]
