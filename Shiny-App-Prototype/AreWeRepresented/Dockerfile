FROM rocker/shiny

RUN apt-get update && apt-get -y install \
  libgsl-dev \
  libssl-dev \
  libcurl4-openssl-dev \
  libxml2-dev

RUN R -e "install.packages(c('shiny', 'shinythemes', 'scales', 'markdown', 'tidyverse'), dependencies=TRUE, repos='https://cran.rstudio.com/')"

COPY Shiny-App-Prototype/AreWeRepresented/app.R /srv/shiny-server
COPY Shiny-App-Prototype/AreWeRepresented/data /srv/shiny-server
