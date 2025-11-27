FROM rocker/r-ver:4.5.1

# System dependencies (extended)
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libicu-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    make \
    g++ \
    git \
    && apt-get clean

# Install remotes first (lighter)
RUN R -e "install.packages('remotes', repos='https://cloud.r-project.org')"

# Install devtools **with detailed output**
RUN R -e "remotes::install_cran('devtools', dependencies = TRUE, force = TRUE, upgrade = 'always')"

# Verify install
RUN R -e "if(!'devtools' %in% rownames(installed.packages())) stop('devtools NOT installed'); library(devtools); sessionInfo()"

# install my package
RUN R -e "devtools::install_github('prasadadhav/microbs_lu_r_package', force = TRUE)"