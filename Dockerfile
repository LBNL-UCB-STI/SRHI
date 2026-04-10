FROM rocker/r-ver:4.3.0

# Install system dependencies for sf, tigris, etc.
RUN apt-get update && apt-get install -y --no-install-recommends \
    libgdal-dev \
    libgeos-dev \
    libproj-dev \
    libudunits2-dev \
    libsqlite3-dev \
    libssl-dev \
    libcurl4-openssl-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c('stringr', 'dplyr', 'vroom', 'httr', 'tidyverse', 'leaflet', 'RColorBrewer', 'tigris', 'sf', 'tibble', 'jsonlite', 'tidyr', 'purrr', 'readr', 'ggplot2'), repos = 'https://cloud.r-project.org', verbose = TRUE)"

# Create directories for scripts
WORKDIR /app
COPY src/data/ /app/data/
COPY src/analysis/ /app/analysis/

# Copy entrypoint script
COPY entrypoint.sh /usr/local/bin/entrypoint.sh
RUN chmod +x /usr/local/bin/entrypoint.sh

# The data will be mounted at /data (not inside the image)
ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]
CMD ["--help"]
