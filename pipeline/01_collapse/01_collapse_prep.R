# !/usr/bin/env Rscript
## Environment Setup
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
#remotes::install_version("optparse", version = "1.6.6", repos = "http://cran.us.r-project.org" ,lib = "/share/scratch/users/mcconrad/untitled folder/")
library(optparse)

# Helpers
is_valid_delivery_location_mapped <- function(deliv_loc_mapped_string) {
  valid_categories <- c(
    "no_hosp", "pub_hosp", "priv_hosp", "ngo_hosp", "no_prim", "pub_prim", "priv_prim", "ngo_prim", "no_split", "pub_split", "priv_split", "ngo_split", "no_none", "pub_none", "priv_none", "ngo_none", "home", "other", "unknown"
  )
  if (deliv_loc_mapped_string %in% valid_categories) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

is_ifd <- function(deliv_loc_mapped_string) {
  ifd_category_substrings <- c("hosp", "prim", "split", "none")
  non_ifd_category_substrings <- c("home", "other")
  if (any(sapply(ifd_category_substrings, function(x) grepl(x, deliv_loc_mapped_string)))) {
    return(1)
  } else if (any(sapply(non_ifd_category_substrings, function(x) grepl(x, deliv_loc_mapped_string)))) {
    return(0)
  } else {
    # Includes "unknown" category and blank responses
    return(NA)
  }
}

# Set up environment ============================================================================
message("Setting up environment...")
ROOT <- file.path(h,'repos', 'pcp')
library(pacman)
pacman::p_load(data.table, dplyr, parallel, haven, readstata13, yaml)
settings = yaml.load_file(file.path(ROOT, "config.yml"))

## Set up to collapse ===========================================================================
## Other reference
error <- data.frame()
count <- 0

## Define collapse functions ====================================================================
# load files function ---------------------------------------------------------------------------
load.file <- function(file) {
  ext <- strsplit(basename(file), "[.]")[[1]][2] %>% tolower
  if (ext=="csv") {
    df <- fread(file)
  } else if (ext %in% c("dta", "tmp")) {
    df <- try(read_dta(file))
    if ('try-error' %in% class(df)) df <- read.dta13(file)
  } else stop("File not accepted")
  return(data.table(df))
}

# Prep for indicator generation function --------------------------------------------------------
collapse.prep <- function(df) {
  ## If child_age_year and child_age_sex, replace
  ## We want the variables to be named "age_year" and "sex_id"; the "child" is
  ## just a relic of how demographics_child works.
  if ("child_age_year" %in% names(df)) df <- df[, age_year := child_age_year]
  if ("child_sex_id" %in% names(df)) df <- df[, sex_id := child_sex_id]
  return(df)
}

## Check for errors function --------------------------------------------------------------------
collapse.check <- function(df) {
  continue <- FALSE
  error <- ""
  
  # maternal checks
  # Make sure at least one of these indicators is in the data
  source <- c("delivery_location_mapped")
  check1 <- ifelse(length(intersect(source, names(df)))>0, 1, 0) ## If 
  if (check1 == 0) error <- paste0(error, ";No vars available")
  
  # general checks
  check2 <- ifelse("age_year" %in% names(df), 1, 0) ## No age year variable
  if (check2 == 0) error <- paste0(error, ";No age_year var")
  if ("age_year" %in% names(df)) check3 <- ifelse(nrow(df[age_year >= 0 & age_year <= 5])>0, 1, 0) else check3 <- 1
  if (check3 == 0) error <- paste0(error, ";No under 5")
  checklist <- c(check1, check2, check3)
  
  # if there are any errors, do not collapse
  continue <- all(checklist==1)
  if (error != "") print(error)
  return(list(continue, error))
}

## Generate indicators function -----------------------------------------------------------------
collapse.gen <- function(df, file) {
  # IFD
  if ("delivery_location_mapped" %in% names(df)) {
    df <- df[, ifd := lapply(delivery_location_mapped, is_ifd)]
    nrows_invalid_delivery_location_mapped <- df[is.na(ifd) & !(delivery_location_mapped %in% c("", "unknown")), .N]
    if (nrows_invalid_delivery_location_mapped > 0) {
      stop(paste0("Validation Error: ", nrows_invalid_delivery_location_mapped, " rows have invalid delivery_location_mapped!"))
    }
  }
  # Generate boolean indicators for each possible mapped delivery location category, and sectors and levels
  delivery_location_mapped_options <- c(
    'home', 'other', 'unknown',
    'pub_hosp', 'pub_prim', 'pub_split', 'pub_none',
    'priv_hosp', 'priv_prim', 'priv_split', 'priv_none',
    'ngo_hosp', 'ngo_prim', 'ngo_split', 'ngo_none',
    'no_hosp', 'no_prim', 'no_split', 'no_none'
  )
  delivery_location_mapped_categories <- list(
    public = c('pub_hosp', 'pub_prim', 'pub_split', 'pub_none'),
    private = c('priv_hosp', 'priv_prim', 'priv_split', 'priv_none'),
    ngo = c('ngo_hosp', 'ngo_prim', 'ngo_split', 'ngo_none'),
    no_sector = c('no_hosp', 'no_prim', 'no_split', 'no_none'),
    hospital = c('pub_hosp', 'priv_hosp', 'ngo_hosp', 'no_hosp'),
    primary = c('pub_prim', 'priv_prim', 'ngo_prim', 'no_prim'),
    split = c('pub_split', 'priv_split', 'ngo_split', 'no_split'),
    no_level = c('pub_none', 'priv_none', 'ngo_none', 'no_none')
  )
  # create columns for each delivery location option
  for (o in delivery_location_mapped_options) {
    df <- df[grepl(o, delivery_location_mapped), eval(o) := 1]
    df <- df[!grepl(o, delivery_location_mapped), eval(o) := 0]
  }
  # create columns for each delivery location category
  for (c in names(delivery_location_mapped_categories)) {
    df <- df[Reduce(`|`, lapply(delivery_location_mapped_categories[[c]], function(o) eval(parse(text = o)))),
             eval(c) := 1]
    df <- df[!Reduce(`|`, lapply(delivery_location_mapped_categories[[c]], function(o) eval(parse(text = o)))),
             eval(c) := 0]
  }
  ## Other custom processing
  ## Round age_year and restrict ages to < 5
  if ("age_year" %in% names(df)) {
    df <- df[, age_year := floor(age_year)]
  }

  return(df)
} # close collapse.prep

#add a pweight column of 1 for additional mexico ids only
add_pweight <- function(df) {
  df <- df[, pweight := 1]
  return(df)
}
  
## MAIN: Generate indicators ==========================================================================
option_list <- list(
  make_option(c('-i', '--nid'), type = 'character', default = NULL,
              help = 'NID for which collapse should be run.'),
  make_option(c('-m', '--multiple_extract_nid'), action = 'store_true', default = FALSE,
              help = 'Multiple files expected for NID in extracts directory.'),
  make_option(c('-v', '--version'), type = 'character', default = NULL,
              help = 'Label of prep version for which collapse should be run.'),
  make_option(c('-l', '--limited_use'), action = 'store_true', default = FALSE,
              help = 'NID data is LIMITED_USE'),
  make_option(c('-o', '--overwrite'), action = 'store_true', default = FALSE,
              help = 'Overwrite existing file.')
)
opt_parser <- OptionParser(option_list = option_list)
opts <- parse_args(opt_parser)
if (is.null(opts$nid) || is.null(opts$version)) {
  cat('Error: Missing required arguments\n')
  print(help(opt_parser))
  quit(status = 1)
}
nid <- opts$nid
version <- opts$version

# NOTE: Paths are analysis-specific
if (opts$limited_use) {
  extracts_dir <- file.path(settings$dla$extract_l)
  collapse_prep_dir <- file.path(settings$dla$collapse_prep_l, version)
} else {
  extracts_dir <- file.path("/mnt/team/hs/pub/lsae/microdata/extraction_archive/TUR")
 # extracts_dir <- file.path(settings$dla$extract_j)
  #collapse_prep_dir <-file.path("/mnt/team/hs/pub/lsae/microdata/extractions_archive/TUR")
  collapse_prep_dir <- file.path(settings$dla$collapse_prep_j, version)
}
if (!opts$overwrite && file.exists(file.path(collapse_prep_dir, paste0("collapse_prep_", nid, ".csv")))) {
  cat('Error: Collapse prep file already exists and --overwrite set to FALSE\n')
  quit(status = 1)
}
if (!dir.exists(extracts_dir)) {
  cat('Error: Microdata extractions directory not found\n')
  quit(status = 1)
}
if (!dir.exists(collapse_prep_dir)) {
  dir.create(collapse_prep_dir, recursive=TRUE)
}

setwd(extracts_dir)

# Ensure a single input (extraction) file for NID actually exists
files <- list.files(extracts_dir)
#extracted_nids <- strsplit(files, "_|\\.")
extracted_nids <- sapply(strsplit(files, "_|\\."),function(x) x[length(x)-1])
if (!(nid %in% extracted_nids)) {
  cat('Error: File for NID not found in extracts directory, or extract file name does not follow convention\n')
  quit(status = 1)
}
# Does 2nd to last element of file name split by "_" match NID?
nid_fp_matches <- which(extracted_nids == nid)
# Try loading in the data
if (length(nid_fp_matches) > 1) {
  if (!opts$multiple_extract_nid) {
    cat('Error: Multiple files found for NID in extracts directory\n')
    quit(status = 1)
  }
  message(paste0("Multiple files found for NID in extracts directory. Assuming subnats and concatenating.\n"))
  data <- data.table()
  for (i in 1:length(nid_fp_matches)) {
    fp <- file.path(extracts_dir, files[nid_fp_matches[i]])
    message(paste0("Loading input dataset ",fp, "\n"))
    data_i <- load.file(fp)
    data <- rbind(data, data_i, fill=TRUE)
  }
} else {
  fp <- file.path(extracts_dir, files[nid_fp_matches])
  message(paste0("Loading input dataset ",fp, "\n"))
  data <- load.file(fp)
}
message(paste0("Input data ",fp," starts with ",nrow(data)," rows.", "\n"))
message("Prepping for Collapse\n")
data <- data %>% collapse.prep
check <- collapse.check(data)
continue <- check[[1]]
errors <- check[[2]]
if (continue) {
  data <- collapse.gen(data, fp)
  message(paste0("Indicators generated for ",fp,"!\n"))
  message("Writing prepped file to ",collapse_prep_dir, "\n")
  file_out <- paste0("collapse_prep_", nid)
  fwrite(data, file=file.path(collapse_prep_dir, paste0(file_out,".csv")), row.names=F)
} else {
  message(paste0("Error: could not prep ",fp, "\n"))
  out <- data.frame(fp,errors)
  error <- rbind(error,out)
  count <- count+1
}
