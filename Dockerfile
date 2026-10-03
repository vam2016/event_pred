FROM rocker/shiny:4.5.1
WORKDIR /srv/shiny-server/event_pred
COPY scripts/install.R scripts/install.R
RUN Rscript scripts/install.R
COPY . .
EXPOSE 3838
CMD ["Rscript", "-e", "shiny::runApp('.', host='0.0.0.0', port=3838)"]
