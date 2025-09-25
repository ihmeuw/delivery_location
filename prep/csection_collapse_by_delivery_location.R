message('Setting up environment...')
library(data.table)
library(dplyr)
library(parallel)
library(haven)

user <- Sys.getenv('USER')
if (Sys.info()['sysname'] == 'Linux') {
  j <- '/snfs1'
  h <- paste0('/ihme/homes/', user)
  k <- '/ihme/cc_resources'
} else {
  j <- 'J:'
  h <- 'H:'
  k <- 'K:'
}

ROOT = file.path(h, 'repos', 'pcp')
settings = yaml.load_file(file.path(ROOT, 'config', 'config.yml'))
SCRATCH_DATA_PATH <- file.path(settings$paths$PCP_SCRATCH, 'collapse_facility_csections', 'data')
EXTRACTS_PATH <- file.path(SCRATCH_DATA_PATH, 'tmp_migrated_extracts')
OUT_PATH <- file.path(SCRATCH_DATA_PATH, 'collapsed')
BY_NID_OUT_PATH <- file.path(OUT_PATH, 'by_nid')
LOGS_PATH <- file.path(settings$paths$PCP_SCRATCH, 'collapse_facility_csections', 'logs')

`%ni%` <- Negate(`%in%`)
if (!dir.exists(BY_NID_OUT_PATH)) dir.create(BY_NID_OUT_PATH, recursive=TRUE)

##------------------------------------------------------------

# Function definitions
## Load files
load.file <- function(file) {
  ext <- strsplit(basename(file), '[.]')[[1]][2] %>% tolower
  if (ext == 'csv') {
    df <- fread(file)
  } else if (ext %in% c('dta', 'tmp')) {
    df <- try(read_dta(file))
    if ('try-error' %in% class(df))
      df <- readstata13(file)
  } else
    stop('File not accepted')
  return(data.table(df))
}

## Prep for indicator generation
collapse.prep <- function(df) {
  if ('child_age_year' %in% names(df))
    df <- df[, age_year := child_age_year]
  if ('child_sex_id' %in% names(df))
    df <- df[, sex_id := child_sex_id]
  return(df)
}

## Check for errors before generating indicators for collapse
collapse.check <- function(df, me) {
  continue <- FALSE
  error <- ''
  cols <- c('delivery_location_mapped', 'caesarean_section')
  check1 <- ifelse(length(intersect(cols, names(df))) > 0, 1, 0)
  ## GENERAL CHECKS
  check2 <- ifelse('age_year' %in% names(df), 1, 0) ## No age year variable
  if (check2 == 0) error <- paste0(error, ';No age_year var')
  if ('age_year' %in% names(df)){
    check3 <- ifelse(nrow(df[age_year >= 0 & age_year <= 5]) > 0, 1, 0)
  }
  else{
    check3 <- 1
  }
  if (check3 == 0){
    error <- paste0(error, ';No under 5')
  }
  checklist <- c(check1, check2, check3)
  # In this case we still want to collapse if we have at least one of 
  # delivery location or c-section, and age_year is present
  continue <- check1 == 1 & check2 == 1
  if (error != '') print(error)
  return(list(continue, error))
}

## Generate indicators for collapse
collapse.gen <- function(df) {
  df <- data.table(df)
  df <- df[, .(nid, ihme_loc_id, year_end, year_start, survey_name, survey_module, delivery_location, caesarean_section)]
  df[, sample_size := .N, by = .(nid, ihme_loc_id, year_end, year_start, survey_name, survey_module)]
  df[, delivs := .N,
     by = .(nid, ihme_loc_id, year_end, year_start, survey_name, survey_module, delivery_location)]
  df[, deliv_prop := delivs/sample_size]
  df[is.na(caesarean_section), caesarean_section := 0]
  df[, csecs := sum(caesarean_section), by = .(nid, ihme_loc_id, year_end, year_start, survey_name, survey_module, delivery_location)]
  df[, csec_prop := csecs/delivs]
  df <- unique(df[, .(nid, ihme_loc_id, year_end, year_start, survey_name, survey_module, delivery_location, sample_size, delivs, deliv_prop, csecs, csec_prop)])
}

##------------------------------------------------------------

surveys <- list.files(path = EXTRACTS_PATH)

no_deliv_location <- c()
no_csection <- c()
main <- data.table()
full <- data.table()
log_dt <- data.table(
  nid = integer(),
  filename = character(),
  errors = character()
)
for (file in surveys) {
  message(file)
  data <- load.file(file.path(EXTRACTS_PATH, file)) %>% collapse.prep()
  errors <- c()
  if ('delivery_location' %ni% names(data)) {
    error <- 'No delivery location'
    message(error)
    no_deliv_location <- append(no_deliv_location, file)
    errors <- append(errors, error)
  }
  if ('caesarean_section' %ni% names(data)) {
    error <- 'No c-section'
    message(error)
    no_csection <- append(no_csection, file)
    errors <- append(errors, error)
  }
  if ('age_year' %ni% names(data)) {
    error <- 'No age_year var'
    message(error)
    errors <- append(errors, error)
  } else {
    if (!(nrow(data[age_year >= 0 & age_year <= 5]) > 0)) {
      error <- 'No under 5'
      message(error)
      errors <- append(errors, error)
    }
  }
  if (length(data[, unique(nid)]) > 1) {
    stop('Came across multiple nids in a single extraction!')
  }
  # Sample size and delivs are births not women for this collapse, so here ideally we 
  # want to only sum where at least one of child age var or birth var are not missing,
  # but will resort to using the delivery_location as a proxy if neither of those
  # are present since this is just a high-level descriptive analysis
  if ('child_birth_year' %in% names(data)) {
    data[, child_row := ifelse(is.na(child_birth_year), 0, 1)]
  } else if ('child_age_year' %in% names(data)) {
    data[, child_row := ifelse(is.na(child_age_year), 0, 1)]
  } else if ('age_year' %in% names(data)) {
    data[, child_row := ifelse(is.na(age_year), 0, 1)]
  } else {
    error <- 'No age or birth vars'
    message(error)
    errors <- append(errors, error)
    if ('delivery_location' %in% names(data)) {
      data[, child_row := ifelse(delivery_location == '' | is.na(delivery_location), 0, 1)]
    } else {
      data[, child_row := 0]
    }
  }
  log_dt <- rbind(log_dt, data.table(nid = data[, unique(nid)], filename = file,
                                     errors = paste(errors, collapse = '; ')))
  check <- collapse.check(data)
  # add empty delivery location and/or c-section to data if they don't exist
  # (Note this occurs after log and check so we still will get the feedback that the
  # cols were missing in the first place)
  if ('delivery_location' %ni% names(data)) {
    data[, delivery_location := NA]
  }
  if ('caesarean_section' %ni% names(data)) {
    data[, caesarean_section := NA]
  }
  
  collapsed <- collapse.gen(data[child_row == 1, ])
  main <- rbind(main, collapsed)
  full <- rbind(full, data[child_row == 1, .(delivery_location, caesarean_section)])
  
  out_path <- file.path(BY_NID_OUT_PATH, paste0(tools::file_path_sans_ext(file), '_collapsed.csv'))
  if (!file.exists(out_path)) {
    fwrite(collapsed, out_path)
  }
}

fwrite(log_dt, file.path(LOGS_PATH, paste0('collapse_facilities_by_csection_R_',
                                           Sys.Date(), '.csv')))

# Collapsed at nid level, then concated
fwrite(main, file.path(OUT_PATH, 'main.csv'))

# All (non-error) survey data concated, then collapsed
full_collapse <- copy(full)
full_collapse[, delivery_location := ifelse(is.na(delivery_location), "NA", delivery_location)]
full_collapse[, sample_size := .N]
full_collapse[, delivs := .N, by = .(delivery_location)]
full_collapse[, deliv_prop := delivs/sample_size]
full_collapse[is.na(caesarean_section), caesarean_section := 0]
full_collapse[, csecs := sum(caesarean_section), by = .(delivery_location)]
full_collapse[, csec_prop := csecs/sample_size]
full_collapse <- unique(full_collapse[, .(delivery_location, sample_size, delivs, deliv_prop, csecs, csec_prop)])
fwrite(full_collapse, file.path(OUT_PATH, 'full_collapse.csv'))
##------------------------------------------------------------
