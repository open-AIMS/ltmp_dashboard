

ltmp_prepare_export <- function(dat, model_lookup) {
  status::status_try_catch(
  {
    dat <-
      dat |>
      ## add units to data
      left_join(model_lookup |>
                dplyr::select(VARIABLE, model_type,
                              model_response, .units = ylab) |>
                distinct(),
                by = c("VARIABLE", "model_type", "model_response")
                ) |> 
      dplyr::filter(selected) |> 
      dplyr::select(VARIABLE, model_response, posteriors, .units, splits,
                    model_type, sub_model) |>
      ## add the exports 
      mutate(
        exports = pmap(
          .l = list(posteriors, .units, splits, VARIABLE,
                    model_type, sub_model),
          .f =  ~ {
            posteriors <- ..1
            .units <- ..2
            splits <- ..3
            VARIABLE <- ..4
            model_type <- ..5
            sub_model <- ..6
            ## year_sum and year_group_sum
            year_sum <- ltmp__prepare_export_year_sum(posteriors,
                                          .units, splits,
                                          VARIABLE, model_type,
                                          sub_model) 
            ## year_posteriors and year_group_posteriors
            year_posteriors <- ltmp__prepare_export_year_posteriors(posteriors,
                                          .units, splits,
                                          VARIABLE, model_type,
                                          sub_model) 
            ## yearcomp_sum
            yearcomp_sum <- ltmp__prepare_export_yearcomp_sum(posteriors, .units, splits,
                                              VARIABLE, model_type, sub_model)
            ## yearcomp_posteriors 
            yearcomp_posteriors <- ltmp__prepare_export_yearcomp_posteriors(posteriors,
                                          .units, splits,
                                          VARIABLE, model_type,
                                          sub_model) 
            return(tibble(year_sum = list(year_sum),
                          year_posteriors = list(year_posteriors),
                          yearcomp_sum = list(yearcomp_sum),
                          yearcomp_posteriors = list(yearcomp_posteriors))
                   )
          }
        )) |>
      ungroup() |> 
      dplyr::select(exports) |>
      unnest(exports) |>
      summarise(across(everything(), ~ list(bind_rows(flatten(.x)))))
    ## unnest("export")
  },
  stage_ = 4,
  order_ = 20,
  name_ = "Prepare export",
  item_ = "prepare_export"
  )
  return(dat)
}

## ltmp__prepare_export <- function(posteriors, .units, splits, VARIABLE, model_type, sub_model) {
##   export_year_sum <- ltmp__prepare_export(posteriors, .units, splits,
##                                           VARIABLE, model_type, sub_model) 

##   ## export <- posteriors$year_sum |>
##   ##   mutate(PURPOSE = "GROUP_LEVEL", fGROUP = NA)
##   ## if (status::get_setting(element = "data_scale") == "reef") {
##   ##   export <- export |>
##   ##     bind_rows(posteriors$year_group_sum |>
##   ##               mutate(PURPOSE = "COMPOSITION"))
##   ## }
##   ## export <- export |>
##   ##   mutate(splits = splits) |>
##   ##   mutate(VARIABLE = VARIABLE) |> 
##   ##   separate(splits, into = c("REEF_ZONE", "DEPTH", "SHELF", "JUNK"),
##   ##            sep = "_") |> 
##   ##   mutate(
##   ##     DOMAIN_CATEGORY = status::get_setting(element = "data_scale"),
##   ##     DOMAIN_NAME = status::get_setting(element = "domain_name"),
##   ##     DATA_PROGRAM = status::get_setting(element = "data_program"), # "ALL", "LTMP" or "MMP"
##   ##     DATA_TYPE = model_type,
##   ##     DATA_METHOD = status::get_setting(element = "data_method"), #"photo-transect" or "manta"
##   ##     UNITS = .units,
##   ##     SUB_MODEL = sub_model) |>
##   ##   dplyr::select(PURPOSE, DOMAIN_CATEGORY, DOMAIN_NAME, DATA_PROGRAM,
##   ##                 DATA_METHOD, DATA_TYPE, UNITS, REPORT_YEAR, DATE,
##   ##                 REEFPAGE_CATEGORY=fGROUP, REEF_ZONE, DEPTH, SHELF,
##   ##                 VARIABLE, SUB_MODEL,
##   ##                 mean, median, lower, upper) |> 
##   ##   filter(!is.na(median)) |> 
##   ##   droplevels()
##  return(export_year_sum = export_year_sum) 
## }

ltmp__prepare_export_year_sum <- function(posteriors, .units, splits, VARIABLE, model_type, sub_model) {
  export <- posteriors$year_sum |>
    mutate(PURPOSE = "GROUP_LEVEL", fGROUP = NA)
  if (status::get_setting(element = "data_scale") == "reef") {
    export <- export |>
      bind_rows(posteriors$year_group_sum |>
                mutate(PURPOSE = "COMPOSITION"))
  }
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
      UNITS = .units,
      SUB_MODEL = sub_model) |>
    dplyr::select(PURPOSE, DOMAIN_CATEGORY, DOMAIN_NAME, DATA_PROGRAM,
                  DATA_METHOD, DATA_TYPE, UNITS, REPORT_YEAR, DATE,
                  REEFPAGE_CATEGORY=fGROUP, REEF_ZONE, DEPTH, SHELF,
                  VARIABLE, SUB_MODEL,
                  mean, median, lower, upper) |> 
    filter(!is.na(median)) |> 
    droplevels()
 return(export) 
}

ltmp__prepare_export_year_posteriors <- function(posteriors, .units, splits, VARIABLE, model_type, sub_model) {
  if (!file.exists(posteriors$year_posteriors)) return(NULL)
  year_posteriors <- readRDS(posteriors$year_posteriors)
  export <- year_posteriors |>
    mutate(PURPOSE = "GROUP_LEVEL", fGROUP = NA)
  if (status::get_setting(element = "data_scale") == "reef") {
    if (file.exists(posteriors$year_group_posteriors)) {
      year_group_posteriors <- readRDS(posteriors$year_group_posteriors)
      export <- export |>
        bind_rows(year_group_posteriors |>
                  mutate(PURPOSE = "COMPOSITION"))
    }
  }
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
      UNITS = .units,
      SUB_MODEL = sub_model) |>
    dplyr::select(PURPOSE, DOMAIN_CATEGORY, DOMAIN_NAME, DATA_PROGRAM,
                  DATA_METHOD, DATA_TYPE, UNITS, REPORT_YEAR, DATE,
                  REEFPAGE_CATEGORY=fGROUP, REEF_ZONE, DEPTH, SHELF,
                  VARIABLE, SUB_MODEL,
                  .draw, value) 
 return(export) 
}

ltmp__prepare_export_yearcomp_sum <- function(posteriors, .units, splits, VARIABLE, model_type, sub_model) {
  export <- posteriors$yearcomp_sum |>
    mutate(PURPOSE = "GROUP_LEVEL", fGROUP = NA)
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
      UNITS = .units,
      SUB_MODEL = sub_model) |>
    dplyr::select(PURPOSE, DOMAIN_CATEGORY, DOMAIN_NAME, DATA_PROGRAM,
                  DATA_METHOD, DATA_TYPE, UNITS,
                  ## REPORT_YEAR, DATE,
                  ## REEFPAGE_CATEGORY=fGROUP,
                  REEF_ZONE, DEPTH, SHELF,
                  VARIABLE, SUB_MODEL,
                  YearComp,
                  mean, median, lower, upper) |> 
    filter(!is.na(median)) |> 
    droplevels()
 return(export) 
}

ltmp__prepare_export_yearcomp_posteriors <- function(posteriors, .units, splits, VARIABLE, model_type, sub_model) {
  if (!file.exists(posteriors$yearcomp_posteriors)) return(NULL)
  yearcomp_posteriors <- readRDS(posteriors$yearcomp_posteriors)
  export <- yearcomp_posteriors |>
    mutate(PURPOSE = "GROUP_LEVEL", fGROUP = NA)
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
      UNITS = .units,
      SUB_MODEL = sub_model) |>
    dplyr::select(PURPOSE, DOMAIN_CATEGORY, DOMAIN_NAME, DATA_PROGRAM,
                  DATA_METHOD, DATA_TYPE, UNITS,
                  ## REPORT_YEAR, DATE,
                  ## REEFPAGE_CATEGORY=fGROUP,
                  REEF_ZONE, DEPTH, SHELF,
                  VARIABLE, SUB_MODEL,
                  YearComp,
                  .draw, value, frac) 
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
    ## year_sum data
    locl_file <- str_replace(local_file, ".csv", "_year.csv")
    remt_file <- str_replace(remote_file, ".csv", "_year.csv")
    data_export$year_sum[[1]] |> write_csv(file = local_file)
    if (status::get_setting(element = "data_from") == "AWS") {
      write_aws(
        from = locl_file,
        to = remt_file,
        catalog_file = FALSE)
    }
    ## year_posteriors data
    locl_file <- str_replace(local_file, ".csv", "_year_posteriors.csv")
    remt_file <- str_replace(remote_file, ".csv", "_year_posteriors.csv")
    data_export$year_posteriors[[1]] |> write_csv(file = local_file)
    if (status::get_setting(element = "data_from") == "AWS") {
      write_aws(
        from = locl_file,
        to = remt_file,
        catalog_file = FALSE)
    }
    ## yearcomp_sum data
    locl_file <- str_replace(local_file, ".csv", "_yearcomp.csv")
    remt_file <- str_replace(remote_file, ".csv", "_yearcomp.csv")
    data_export$yearcomp_sum[[1]] |> write_csv(file = local_file)
    if (status::get_setting(element = "data_from") == "AWS") {
      write_aws(
        from = locl_file,
        to = remt_file,
        catalog_file = FALSE)
    }
    ## yearcomp_posteriors data
    locl_file <- str_replace(local_file, ".csv", "_yearcomp_posteriors.csv")
    remt_file <- str_replace(remote_file, ".csv", "_yearcomp_posteriors.csv")
    data_export$yearcomp_posteriors[[1]] |> write_csv(file = local_file)
    if (status::get_setting(element = "data_from") == "AWS") {
      write_aws(
        from = locl_file,
        to = remt_file,
        catalog_file = FALSE)
    }
  },
  stage_ = 4,
  order_ = 21,
  name_ = "Export data",
  item_ = "export_data"
  )
  return(invisible(NULL))
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
