#FROM rocker/r-ver:4.2.1
FROM openanalytics/r-base
RUN apt-get update && apt-get install -y  git-core libcurl4-openssl-dev libgit2-dev libicu-dev libssl-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN mkdir -p /usr/local/lib/R/etc/ /usr/lib/R/etc/
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" | tee /usr/local/lib/R/etc/Rprofile.site | tee /usr/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("ggplot2",upgrade="never", version = "3.3.6")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.7.1")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.23")'
RUN Rscript -e 'remotes::install_version("shinycssloaders",upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("Rlabkey",upgrade="never", version = "2.8.4")'
RUN Rscript -e 'remotes::install_version("colourpicker",upgrade="never", version = "1.1.1")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.3.2")'
RUN Rscript -e 'remotes::install_github("sistm/sistmr@4c4af4b0c5312164cd2debd250321745d4391b0d")'
#RUN mkdir /build_zone
#ADD . /build_zone
#WORKDIR /build_zone
COPY sistmR_*.tar.gz /app.tar.gz
RUN R -e 'remotes::install_local("/app.tar.gz")'
#RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 8080
CMD  ["R", "-e", "options('shiny.port'=8080,shiny.host='0.0.0.0');sistmR::run_app()"]
