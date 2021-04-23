FROM rocker/shiny:4.0.3

# librerias extra
RUN apt-get update && apt-get install -y \
    telnet \
    libssl-dev

# control de versionado de R
ENV RENV_VERSION 0.13.2
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_github('rstudio/renv@${RENV_VERSION}')"

COPY renv.lock renv.lock
RUN R -e 'renv::consent(provided = TRUE);renv::restore()'

# configuracion de servidor de shiny
COPY shiny-customized.config /etc/shiny-server/shiny-server.conf

RUN rm -rf /srv/shiny-server/*
COPY app/ /srv/shiny-server/

# funciona pero no lo es lo correcto
# CMD ["R", "-e", "shiny::runApp('/srv/shiny-server', port = 8080, host = '0.0.0.0')"] 