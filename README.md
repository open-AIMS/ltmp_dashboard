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

The scripts are designed into a hierarchy in which a parent script can be used
to source a series of child scripts.  The majority of the time, only the parent
scripts will be run.

The scripts should be run via the `Rscripts` command and with commandline
arguments of the following form:

`Rscript 00_main.R <s3 address>/<DATA_METHOD>/<DATE>/process/<DATA_PROGRAM>/<YEAR>/<CRUISE_CODE>/<DOMAIN_CATEGORY>/<DOMAIN_NAME>/raw/<filename>.csv`

`
where:

- <DATA_TYPE> is one of: 
    - COVER (for group code cover data)
    - FISH (for fish data), 
    - COMP (for benthic composition data)
    - JUV (for juvenile density data)
- <s3 address> is the s3 bucket address
- <DATA_METHOD> is one of: 
    - photo-transect
    - manta
- <DATE> is the data extraction date? - ignored in analyses
- <DATA_PROGRAM> is one of:
    - LTMP
    - MMP
- <YEAR> is the reporting year of the data extraction date? - ignored in analyses
- <CRUISE_CODE> is the LTMP cruise code - ignored in analyses
- <DOMAIN_CATEGORY> is one of:
    - reef (individual reef)
    - bioregion (individual bioregion)
    - nrm (individual NRM region)
    - sector (individual AIMS LTMP sector)
    - region (individual De'ath Zone)
    - gbr (whole GBR)
- <DOMAIN_NAME> is the name of the individual reef, bioregion, nrm, sector or region
- <filename> is the base name for all input and output files
`

