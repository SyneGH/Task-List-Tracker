# Base image from Rocker
FROM rocker/shiny:4.4.0

# 1. Install System Dependencies
# libpq-dev is for PostgreSQL
# libsodium-dev is for Password Hashing (Sodium)
RUN apt-get update && apt-get install -y \
    libpq-dev \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libsodium-dev \
    zlib1g-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# 2. Install R Packages
# We install these directly to ensure your app has exactly what it needs
RUN R -e "install.packages(c('shiny', 'bslib', 'shinyWidgets', 'ggplot2', 'dplyr', 'htmltools', 'shinyjs', 'sortable', 'DBI', 'RPostgres', 'sodium', 'lubridate'), repos='https://cran.rstudio.com/')"

# 3. Create app directory & Copy files
WORKDIR /srv/shiny-server/
COPY . .

# 4. Expose the port
EXPOSE 3838

# 5. Run the application
# We use the PORT environment variable if provided by Render, otherwise 3838
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host = '0.0.0.0', port = as.numeric(Sys.getenv('PORT', 3838)))"]