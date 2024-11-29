##############################################################################################
## The following function is a wrapper for loading data from the aws bucket_exists          ##
##                                                                                          ##
## Arguments:                                                                               ##
##   - file:   a string representation of the name of the file to retrieve from the bucket. ##
##   - level:  a string representation of the name of the processing level                  ##
##             (primary, processed, modelled)                                               ##
## Returns:                                                                                 ##
##   - a dataframe containing the data                                                      ##
##############################################################################################
load_aws <- function(file, level, col_types) {
  zipfile <<- gsub('\\.csv','\\.zip',file)
  ## system(paste0('aws s3 cp "', AWS_PATH, ifelse(level=='primary/', 'raw/', level), file, '" "', DATA_PATH, level, file, '"'))
  system(paste0('aws s3 cp "', AWS_PATH, ifelse(level=='primary/', 'raw/', level), zipfile, '" "', DATA_PATH, level, zipfile, '"'))
  unzip(paste0(DATA_PATH, level, zipfile), exdir=paste0(DATA_PATH,level))
  data <- read_csv(paste0(DATA_PATH, level, file), col_types=col_types, trim_ws=TRUE)
  data
}

load_local <- function(file, level, col_types) {
  zipfile <<- gsub('\\.csv','\\.zip',file)
  ## system(paste0('aws s3 cp "', AWS_PATH, ifelse(level=='primary/', 'raw/', level), file, '" "', DATA_PATH, level, file, '"'))
  system(paste0('cp "', AWS_PATH, ifelse(level=='primary/', 'raw/', level), zipfile, '" "', DATA_PATH, level, zipfile, '"'))
  unzip(paste0(DATA_PATH, level, zipfile), junkpaths = TRUE, overwrite = TRUE, exdir=paste0(DATA_PATH,level))
  data <- read_csv(paste0(DATA_PATH, level, file), col_types=col_types, trim_ws=TRUE)

  data
}
