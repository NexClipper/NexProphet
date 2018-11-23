FROM rocker/shiny

RUN apt-get -y update && \
    apt-get -y upgrade && \
    apt-get install -y libssl-dev \
                       libxml2-dev \
		       libmariadbclient-dev \
		       libv8-3.14-dev \
		       vim \
		       default-jre

RUN rm -rf /srv/shiny-server && \
    mkdir /srv/shiny-server

WORKDIR /srv/shiny-server

RUN chmod o+w /usr/local/lib/R/site-library /srv/shiny-server

COPY Source/install_pkg .

RUN Rscript 00.R && \
    rm -f 00.R

COPY . .
