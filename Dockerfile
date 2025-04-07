FROM rocker/geospatial:4.2.1

RUN apt-get update -qq && apt-get -y --no-install-recommends install awscli

RUN apt-get -y --no-install-recommends install git

RUN R -e "options(repos = \
  # list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2022-10-10/')); \
  list(CRAN = 'https://packagemanager.posit.co/cran/2022-10-11/')); \
  install.packages(c('emmeans','broom','knitr','ggspatial', 'broom.mixed','DHARMa','tidybayes','remotes')); \
  # install.packages('INLA',repos=c(getOption('repos'),INLA='https://inla.r-inla-download.org/R/stable'), dep=TRUE)"
  remotes::install_version('INLA', version='22.05.07',repos=c(getOption('repos'),INLA='https://inla.r-inla-download.org/R/stable'), dep=TRUE)"

RUN R -e "options(repos = \
  list(CRAN = 'https://packagemanager.posit.co/cran/2022-10-11/')); \
  install.packages(c('tidyverse', 'rlang', 'crayon', 'cli'))"

RUN R -e "options(repos = \
  # list(CRAN = 'http://mran.revolutionanalytics.com/snapshot/2022-10-10/')); \
  list(CRAN = 'https://packagemanager.posit.co/cran/2022-10-11/')); \
  remotes::install_github('open-AIMS/status', ref = 'main', force = TRUE)"

#RUN git clone https://github.com/AIMS/LTMP_web_reporting.git

RUN mkdir -p /home/analysis /home/Project/R

COPY R/ /home/analysis/R/
COPY data/ /home/analysis/data/
#COPY parameters/ /home/analysis/parameters

#ENTRYPOINT ["Rscript ~/LTMP_web_reporting/scripts/LTMP_PT_cover.R"]

WORKDIR /home/analysis/R

CMD pwd && ls -AlhF ./
