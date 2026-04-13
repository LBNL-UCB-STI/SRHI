FROM rocker/geospatial:4.3.0

# Install system dependencies without proxy
RUN unset HTTP_PROXY HTTPS_PROXY http_proxy https_proxy \
    && apt-get update && apt-get install -y --no-install-recommends \
        libgdal-dev libgeos-dev libproj-dev libudunits2-dev \
        libsqlite3-dev libssl-dev libcurl4-openssl-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

#  Install R packages
RUN R -e "install.packages(c('stringr', 'dplyr', 'vroom', 'httr', 'tidyverse', 'leaflet', 'RColorBrewer', 'tigris', 'sf', 'tibble', 'jsonlite', 'tidyr', 'purrr', 'readr', 'ggplot2'), repos = 'https://cloud.r-project.org', verbose = TRUE)"

WORKDIR /app
COPY src/data/ /app/data/
COPY src/analysis/ /app/analysis/
COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
CMD ["--help"]