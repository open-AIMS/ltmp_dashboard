LTMP dashboard
==================

This repository represents the R backend to the dashboard.

## Installation instructions

1. This repo should be cloned into a project specific folder.

## Directory structure
```
root
|-Dockerfile
|-README.md
|-R
|  |-00_main.R
|  |-LTMP_config.R

```
## Running scripts

The scripts are designed into a hierarchy in which a parent script can
be used to source a series of child scripts. The majority of the time,
only the parent scripts will be run.

Whilst not absolutely necessary, it is advised that these R scripts
should be run in a container (such as a docker container - the
`Dockerfile` of which is provided in this repo).

The scripts should be run via the `Rscripts` command and with
commandline arguments of the following form (note, I have included
linebreaks in the following to aid readability in this readme - do not
included them when constructing a call to R):

`Rscript 00_main.R <s3 address>/<DATA_METHOD>/<DATE>/process/<DATA_PROGRAM>/<YEAR>/<CRUISE_CODE>/
<DOMAIN_CATEGORY>/<DOMAIN_NAME>/raw/<filename>.csv --method=<DATA_METHOD> --domain=<DOMAIN_NAME> 
--scale=<DOMAIN_CATEGORY> --status=<true|false> --refresh_data=<true|false>`

`
where:

- <s3 address> is the s3 bucket address
- <DATA_METHOD> is one of: 
    - photo-transect
    - manta
    - juvenile
    - fish
- <DATE> is the data extraction date? - ignored in analyses
- <DATA_PROGRAM> is one of:
    - LTMP
    - MMP
    - ALL
- <YEAR> is the reporting year of the data extraction date? - ignored
  in analyses
- <CRUISE_CODE> is the LTMP cruise code - ignored in analyses
- <DOMAIN_CATEGORY> is one of:
    - reef (individual reef)
    - bioregion (individual bioregion)
    - nrm (individual NRM region)
    - Sectors (individual AIMS LTMP sector)
    - region (individual De'ath Zone)
    - gbr (whole GBR)
- <DOMAIN_NAME> is the name of the individual reef, bioregion, nrm,
  sector or region
- <filename> is the base name for all input and output files 
- --status (true or false) specifies whether a TUI status update
  should be provided whilst the script is running 
- --refresh_data (true or false) specifies whether the scripts should
  start by removing any stored intermediate artifacts. Typically, this
  is recommended. It ensures that old runs have do way of
  inadvertently effecting current runs.

`


When the parent script (00_main.R) is run, a complete set of intermediate
(processed) and modelled output datafiles will be created nested under
the `data` folder:

```
root
|-Dockerfile
|-README.md
|-log
| |-all.log
|-data
| |-primary
| | |-<filename>.csv
| | |-<filename>.RData
| |-processed
| | |-<filename>.csv
| | |-<filename>.RData
| | |-<filename>.spatial.RData
| |-modelled
| | |-<filename>.csv
| | |-<filename>.RData
| | |-<filename>.js
| | |-<filename>.json
|-R
|  |-00_main.R

```

Note, that the `data` folder only stores temporary artifacts and that
these are not available to the host.

A log file (`log/all.log`) will also be created providing a
chronological log of the data processing and modelling. The R codes
themselves are protected by try catch routines that prevent log the
status of each set of routines and attempt to prevent R crashes. As a
result, it is important that the logs be examined after each run to
ensure there were no errors (marked by a `FAILURE` keyword in the
log).

It is also possible to directly call one of the child scripts in order
to bypass earlier routines (which are assumed to have already run
successfully). For example, the following commandline template could
be used to perform the modelling phase (assuming that the data
pre-processing phase has already been carried out).

`Rscript LTMP_PT_<DATA_TYPE>_fitModel.R <s3 address>/<DATA_METHOD>/<DATE>/process/<DATA_PROGRAM>/<YEAR>/<CRUISE_CODE>/<DOMAIN_CATEGORY>/<DOMAIN_NAME>/raw/<filename>.csv`

Each script is designed to be able to run as either a parent of child.
Only when running as a parent, will various starting routines (such as
loading of libraries, reading commandline arguments and ensuring the
directory structure is present) take place.

