# Base image from Rocker (Ubuntu 22.04 based)
FROM rocker/shiny:4.4.0

# 1. Install System Dependencies
# We add 'haveged' here to fix the slow login/entropy issue
RUN apt-get update && apt-get install -y \
    libpq-dev \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libsodium-dev \
    zlib1g-dev \
    haveged \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# 2. Install R Packages (USING BINARIES)
# We add the 'repos' argument to point to Posit Package Manager (Linux/Jammy binaries)
# This prevents compiling from source, solving the OOM crash and speeding up build.
RUN R -e "install.packages(c('shiny', 'bslib', 'shinyWidgets', 'ggplot2', 'dplyr', 'htmltools', 'shinyjs', 'sortable', 'DBI', 'RPostgres', 'pool', 'sodium', 'lubridate'), repos='https://packagemanager.posit.co/cran/__linux__/jammy/latest')"

# 3. Create app directory & Copy files
WORKDIR /srv/shiny-server/
COPY . .

# 4. Expose the port
EXPOSE 3838

# 5. Run the application
# We start 'haveged' first to ensure entropy is available for the DB connection
CMD haveged -w 1024 && R -e "shiny::runApp('/srv/shiny-server/', host = '0.0.0.0', port = as.numeric(Sys.getenv('PORT', 3838)))"