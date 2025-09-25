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
ROOT <- file.path(h,'repos', 'pcp')
pacman::p_load(yaml)
settings = yaml.load_file(file.path(ROOT, 'config.yml'))
source(file.path(settings$ubcov_central_root, 'modules/collapse/launch.r'))
#library(optparse, lib.loc = file.path(h, 'rlibs'))
library(optparse)

option_list <- list(

  make_option(c('-v', '--version'), type = 'character', default = NULL,
              help = 'Label of prep version for which collapse should be run.'),
  make_option(c( '-n','--nid'), type = 'character', default = NULL,
              help = 'NID to collapse.'),
  make_option(c('-l', '--limited_use'), action = 'store_true', default = FALSE,
              help = 'NID data is LIMITED_USE'),
  make_option(c('-o', '--overwrite'), action = 'store_true', default = FALSE,
              help = 'Overwrite existing file.')
)
opt_parser <- OptionParser(option_list = option_list)
opts <- parse_args(opt_parser)
if (is.null(opts$version)) {
  cat('Error: Missing required arguments\n')
  print(help(opt_parser))
  quit(status = 1)
}
nid <- opts$nid#will need to separate list into individual NIDS. ... sep(delim= comma)
version<- opts$version


# if (opts$limited_use) {
#   collapse_prep_dir <- file.path(settings$dla$collapse_prep_l, version)
# } else {
#   collapse_prep_dir <- file.path(settings$dla$collapse_prep_j, version)
# }
collapse_prep_dir <- if (opts$limited_use) {
  file.path(settings$dla$collapse_prep_l, version)
} else {
  file.path("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep", version)
}
collapse_prep_dir <- file.path("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse_prep", version)
if (!dir.exists(collapse_prep_dir)) {
  stop("Prep directory does not exist.")
}

collapse_dir <- file.path(settings$dla$collapse, version,nid)
if (!dir.exists(collapse_dir)) {
  dir.create(collapse_dir, recursive = TRUE)
}
# if (!opts$overwrite && file.exists(file.path(collapse_dir, paste0('collapse_maternal_', nid, '.csv')))) {
#   cat('Error: Collapse prep file already exists and --overwrite set to FALSE\n')
#   quit(status = 1)
# }
message("dont check 1")
if (!dir.exists(collapse_prep_dir)) {
  cat('Error: Collapse prep directory not found\n')
  quit(status = 1)
}
message("dont check 2")

if (!dir.exists(collapse_dir)) {
  dir.create(collapse_dir, recursive=TRUE)
}
message("dont check 3")

# prep_file <- file.path(collapse_prep_dir, paste0('collapse_prep_', nid, '.csv'))
# if (!file.exists(prep_file)) {
#   cat('Info: Collapse prep file not found\n')
#   quit(status = 0)
# }
prep_files <- list.files(
  path = collapse_prep_dir,
  pattern = "^collapse_prep_\\d+\\.csv$",
  full.names = TRUE
)

if (length(prep_files) == 0) {
  message("No prep files found to collapse.")
  quit(status = 0)
}

print("reading in config file")
config = read.csv(file.path(ROOT, 'delivery_location_remapping', 'collapse', 'config_new.csv'))
#adjust output root in config for each nid and then write back out, making the folder if it doesn't exist
output_root = settings$dla$collapse
input_root = settings$dla$collapse_prep_j
config = config %>% mutate(output.root = paste0(output_root,"/",version,"/",nid,"/"))
config = config %>% mutate(input.root = paste0(collapse_prep_dir,"/"))
if (!dir.exists(config$output.root)) {
  dir.create(config$output.root, recursive=TRUE)
}
#write config back out
write.csv(config, file.path(ROOT, 'delivery_location_remapping', 'collapse', 'config_new.csv'), row.names = FALSE)
######################################################################################################################

## Settings
logs <- paste0('/share/temp/slurmoutput/', user, '/errors/')## Path to logs

## Launch collapse
ubcov_central <- settings$ubcov_central_root
setwd(ubcov_central)
df <- collapse.launch(
  topic='maternal',
  #nid=nid,
  config.path=paste0(ROOT, '/delivery_location_remapping/', 'collapse/', 'config_new.csv'),
  parallel=FALSE,
   cluster_project='proj_health_sys',
   fthreads=2,
   m_mem_free=6,
   h_rt='00:10:00',
  # logs=file.path(),
  central.root=ubcov_central
)

