FROM rocker/shiny:4.5.1
RUN apt-get update && apt-get install -y --no-install-recommends libuv1-dev libcurl4-openssl-dev pkg-config && rm -rf /var/lib/apt/lists/*
WORKDIR /srv/shiny-server/event_pred
COPY scripts/install_core.R scripts/install_core.R
COPY scripts/install.R scripts/install.R
RUN Rscript scripts/install.R
COPY . .
EXPOSE 3838
CMD ["Rscript", "-e", "shiny::runApp('.', host='0.0.0.0', port=3838)"]
