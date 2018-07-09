FROM rocker/shiny

RUN apt-get -y update && apt-get -y upgrade

RUN apt-get install -y libssl-dev

RUN apt-get install -y libxml2-dev

WORKDIR /home

COPY ./Source/package_manage.R .

RUN chmod 777 package_manage.R

RUN Rscript package_manage.R

