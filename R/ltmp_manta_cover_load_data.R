source("ltmp_startup_functions.R")
source("ltmp_load_functions.R")
if (ltmp_is_parent()) ltmp_start_matter(args)

status::status_set_stage(stage = 2, title = "Obtain data")

## Get the AWS data
if (status::get_setting(element = "data_from") == "AWS") {
  status::remove_status_item(stage = 2, item = "retrieve_data_local")
  status::remove_status_item(stage = 2, item = "retrieve_data_sql")
  status::status_try_catch(
  {
    data <- load_aws(file=CSV_FILE, level='primary/', col_types='cdccccdddTdcdc')
    save(data,  file=paste0(DATA_PATH, 'primary/', RDATA_FILE))
    cat(paste0('Successfully read in data into :', DATA_PATH, 'primary/', '\n'))
  },
  stage_ = 2,
  order_ = 1,
  name_ = "Retrieve data from AWS",
  item_ = "retrieve_data_aws"
  )
}

## Get data from local folder
if (status::get_setting(element = "data_from") == "local copy") {
  status::remove_status_item(stage = 2, item = "retrieve_data_aws")
  status::remove_status_item(stage = 2, item = "retrieve_data_sql")
  status::status_try_catch(
  {
    data <- load_local(file=CSV_FILE, level='primary/', col_types='cdccccdddTdcdc')
    save(data,  file=paste0(DATA_PATH, 'primary/', RDATA_FILE))
    cat(paste0('Successfully read in data into :', DATA_PATH, 'primary/', '\n'))
  },
  stage_ = 2,
  order_ = 1,
  name_ = "Retrieve data from local",
  item_ = "retrieve_data_local"
  )
}

## Get data from sql
if (status::get_setting(element = "data_from") == "sql") {
  status::remove_status_item(stage = 2, item = "retrieve_data_aws")
  status::remove_status_item(stage = 2, item = "retrieve_data_local")
  status::status_try_catch(
  {
    ## data <- load_local(file=CSV_FILE, level='primary/', col_types='cdccccdddddcdTcdcccc')
    ## save(data,  file=paste0(DATA_PATH, 'primary/', RDATA_FILE))
    ## cat(paste0('Successfully read in data into :', DATA_PATH, 'primary/', '\n'))
  },
  stage_ = 2,
  order_ = 1,
  name_ = "Retrieve data from sql",
  item_ = "retrieve_data_sql"
  )
}
