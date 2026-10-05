FROM rocker/shiny:4.5.1
WORKDIR /srv/shiny-server/event_pred
COPY scripts/install_core.R scripts/install_core.R
RUN Rscript scripts/install_core.R
COPY . .
EXPOSE 3838
CMD ["Rscript", "-e", "shiny::runApp('app_core.R', host='0.0.0.0', port=3838)"]
