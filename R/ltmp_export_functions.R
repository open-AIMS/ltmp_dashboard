

ltmp_prepare_export <- function(dat, model_lookup) {
  status::status_try_catch(
  {
  dat |>
    left_join(model_lookup |>
              dplyr::select(VARIABLE, model_type,
                            model_response, .units = ylab),
              by = c("VARIABLE", "model_type", "model_response")
              ) |> 
    dplyr::filter(selected) |> 
    dplyr::select(VARIABLE, model_response, posteriors, .units, splits, model_type) |>
    mutate(export = pmap(.l = list(posteriors, .units, splits, VARIABLE, model_type),
                        .f =  ~ {
                          ## data_group <- ..1
                          posteriors <- ..1
                          .units <- ..2
                          splits <- ..3
                          VARIABLE <- ..4
                          model_type <- ..5
                          export <- ltmp__prepare_export(posteriors, .units, splits, VARIABLE, model_type)            
                          export
                        }
                        )) |>
    ungroup() |> 
    dplyr::select(export) |>
    unnest("export")
  },
  stage_ = 4,
  order_ = 20,
  name_ = "Prepare export",
  item_ = "prepare_export"
  )
  return(dat)
}

ltmp__prepare_export <- function(posteriors, .units, splits, VARIABLE, model_type) {
  export <- posteriors$year_sum |>
    mutate(PURPOSE = "GROUP_LEVEL", fGROUP = NA)
  if (status::get_setting(element = "data_scale") == "reef") {
    export <- export |>
      bind_rows(posteriors$year_group_sum |>
                mutate(PURPOSE = "COMPOSITION"))
  }
  ## if (status::get_setting(element = "data_scale") != "reef") {
  ##   export <- export |> mutate(fGROUP = NA)
  ## }

  ## data_group <- data_group |>
  ##   dplyr::select(REEF_ZONE, DEPTH, SHELF

  export <- export |>
    mutate(splits = splits) |>
    mutate(VARIABLE = VARIABLE) |> 
    separate(splits, into = c("REEF_ZONE", "DEPTH", "SHELF", "JUNK"),
             sep = "_") |> 
    mutate(
      DOMAIN_CATEGORY = status::get_setting(element = "data_scale"),
      DOMAIN_NAME = status::get_setting(element = "domain_name"),
      DATA_PROGRAM = status::get_setting(element = "data_program"), # "ALL", "LTMP" or "MMP"
      DATA_TYPE = model_type,
      DATA_METHOD = status::get_setting(element = "data_method"), #"photo-transect" or "manta"
      UNITS = .units) |>
    dplyr::select(PURPOSE, DOMAIN_CATEGORY, DOMAIN_NAME, DATA_PROGRAM,
                  DATA_METHOD, DATA_TYPE, UNITS, REPORT_YEAR, DATE,
                  REEFPAGE_CATEGORY=fGROUP, REEF_ZONE, DEPTH, SHELF,
                  VARIABLE,
                  mean, median, lower, upper) |> 
    filter(!is.na(median)) |> 
    droplevels()
 return(export) 
}

ltmp_export_data <- function(data_export) {
  status::status_try_catch(
  {
  local_file <- paste0(
    status::get_setting(element = "data_path"),
    "modelled/",
    status::get_setting(element = "csv_file")
    )
  remote_file <- paste0(
    status::get_setting(element = "aws_path"),
    "output/",
    status::get_setting(element = "csv_file")
    )
  data_export |> write_csv(file = local_file)
  if (status::get_setting(element = "data_from") == "AWS") {
    write_aws(
      from = local_file,
      to = remote_file,
      catalog_file = FALSE)
  }
  },
  stage_ = 4,
  order_ = 21,
  name_ = "Export data",
  item_ = "export_data"
  )
  return(NULL)
}

##############################################################################################
## The following function is a wrapper for writing data to the aws bucket                   ##
##                                                                                          ##
## Arguments:                                                                               ##
##   - filenm: a string representation of the file basename.  If this is present, it will   ##
##             override from and to                                                         ##
##   - from: a string representation of the local file path.                                ##
##   - to:   a string representation of the aws file path                                   ##
##   - catalog_file: a boolean indicating whether the content moved to the bucket should    ##
##                   be cataloged (cataloged data are those that are not used by the        ##
##                   dashboard, yet are available for internal users to download.           ##
##                   Examples of cataloged data would be posteriors.                        ##
## Returns:                                                                                 ##
##   - NULL.  This function is run for its side-effect.                                     ##
##############################################################################################
write_aws <- function(from = NULL, to = NULL, filenm = NULL, catalog_file = FALSE) {
  if (status::get_setting(element = "data_from") == "AWS") {
    if (!is.null(filenm)) {
      from <- paste0(
        status::get_setting(element = "data_path"),
        "modelled/",
        filenm
      )
      remote_file <- paste0(
        status::get_setting(element = "aws_path"),
        "output/",
        filenm
      )
    }
    system(
      paste0('aws s3 cp --metadata "catalog-file=',
             catalog_file, '" "',
             from,
             '" "',
             to,
             '"')
    )
  }
}

write_external_log <- function() {
  local_file <- paste0(
    status::get_setting(element = "data_path"),
    "modelled/",
    status::get_setting(element = "csv_file")
    )
  remote_file <- paste0(
    status::get_setting(element = "aws_path"),
    "output/",
    status::get_setting(element = "csv_file")
    )
    system(
      paste0('aws s3 cp --metadata "catalog-file=FALSE',
             '" "',
             local_file,
             '" "',
             remote_file,
             '"')
    )
}
