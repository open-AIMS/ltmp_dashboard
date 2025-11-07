FROM rocker/geospatial:4.5.1

RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  curl unzip groff less

RUN curl "https://awscli.amazonaws.com/awscli-exe-linux-x86_64.zip" -o "awscliv2.zip" \
  && unzip awscliv2.zip \
  && ./aws/install \
  && rm -rf aws awscliv2.zip \
  && apt-get clean && rm -rf /var/lib/apt/lists/*

RUN apt-get -y --no-install-recommends install git

RUN R -e "options(repos = \
  # list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2022-10-10/')); \
  list(CRAN = 'https://packagemanager.posit.co/cran/2025-08-11/')); \
  install.packages(c('emmeans','broom','knitr','ggspatial', 'broom.mixed','DHARMa','tidybayes','remotes', 'HDInterval')); \
  # install.packages('INLA',repos=c(getOption('repos'),INLA='https://inla.r-inla-download.org/R/stable'), dep=TRUE)"
  remotes::install_version('INLA', version = '25.06.13', repos = c(getOption('repos'), INLA = 'https://inla.r-inla-download.org/R/testing'), dep = TRUE)"


RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2025-08-11/')); \
  install.packages(c('tidyverse', 'rlang', 'crayon', 'cli'))"

RUN R -e "options(repos = \
  # list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2022-10-10/')); \
  list(CRAN = 'https://packagemanager.posit.co/cran/2025-08-11/')); \
  remotes::install_github('open-AIMS/status', ref = 'main', force = TRUE)"

## I am adding the following to address an issue where INLA tries to make use of
## kernel mbind().  This is not permitted in docker.
ENV MKL_ENABLE_INSTRUCTIONS=AVX2
ENV MKL_DEBUG_CPU_TYPE=5
ENV MKL_NUM_THREADS=1
ENV MKL_DYNAMIC=FALSE
ENV MKL_DISABLE_FAST_MM=1
ENV KMP_AFFINITY=disabled
ENV OPENBLAS_NUM_THREADS=1
ENV OMP_NUM_THREADS=1

#RUN git clone https://github.com/AIMS/LTMP_web_reporting.git

RUN mkdir -p /home/analysis /home/Project/R

COPY R/ /home/analysis/R/
COPY data/ /home/analysis/data/
#COPY parameters/ /home/analysis/parameters

#ENTRYPOINT ["Rscript ~/LTMP_web_reporting/scripts/LTMP_PT_cover.R"]

WORKDIR /home/analysis/R

CMD pwd && ls -AlhF ./
