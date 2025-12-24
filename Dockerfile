# Base image from Rocker (the standard for R in Docker)
FROM rocker/shiny:4.4.0

# Install system dependencies
# libmysqlclient-dev is required for the RMySQL/RMariaDB package
# libcurl4-gnutls-dev, libssl-dev, libxml2-dev are standard R package deps
RUN apt-get update && apt-get install -y \
    libmysqlclient-dev \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# Install renv
ENV RENV_VERSION 1.0.7
RUN R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))"
RUN R -e "remotes::install_version('renv', version = '$RENV_VERSION')"

# Create app directory
WORKDIR /srv/shiny-server/

# Copy the lock file first (for caching layers)
COPY renv.lock .

# Restore R packages (this takes time, so we want it cached)
RUN R -e "renv::restore()"

# Copy the rest of the application
COPY . .

# Expose the port Shiny runs on
EXPOSE 3838

# Run the application
CMD ["R", "-e", "shiny::runApp('/srv/shiny-server/', host = '0.0.0.0', port = 3838)"]