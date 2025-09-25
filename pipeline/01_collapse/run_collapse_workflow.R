#!/usr/bin/env Rscript

library(dplyr)
library(readr)
library(stringr)
library(glue)
library(yaml)
user = "mcconrad"
# ---- Load settings ----
config_path <- "/mnt/team/hs/pub/code/gluckt/pcp/config.yml"
settings <- yaml::read_yaml(config_path)

prep_version <- "2025-05-07"
rscript_exec <- "/share/singularity-images/rstudio/shells/execRscript.sh"
project <- "proj_health_sys"

# ---- Load NIDs ----
nid_file <- file.path(settings$pcp_helpers, "nid_lists", "mcconrad_recollapse.csv")
dl_nids <- read_csv(nid_file)
collapse_nids <- unique(dl_nids$nid)

# ---- Get extracted files ----
extract_nid <- function(files) {
  as.integer(str_extract(files, "\\d{3,7}(?=\\.\\w+$|$)"))
  }

extract_j_path <- file.path(settings$dla$extract_j)
extract_l_path <- file.path(settings$dla$extract_l)

j_files <- list.files(extract_j_path)
l_files <- list.files(extract_l_path)

collapse_j_nids <- collapse_nids[collapse_nids %in% extract_nid(j_files)]
collapse_l_nids <- collapse_nids[collapse_nids %in% extract_nid(l_files)]

message(glue("Ready to run collapse for {length(collapse_l_nids)} L-drive"))

## ---- Helpers ----
make_sge_command <- function(job_name, script_path, rscript_args, log_dir = ".", memory = "8G", time = "00:30:00", cpus = 1, dependency = NULL) {
  stdout_file <- file.path(log_dir, glue("{job_name}.out"))
  stderr_file <- file.path(log_dir, glue("{job_name}.err"))
  dep_flag <- if (!is.null(dependency)) glue("-hold_jid {dependency}") else ""
  glue("sbatch ",
       "-A {project} ",
       "-J {job_name} ",
       "-e {stderr_file} ",
       "-o {stdout_file} ",
       "--mem={memory} ",
       "-c {cpus} ",
       "-t {time} ",
       "-p all.q ",
       "{dep_flag} ",
       "{rscript_exec} -s {script_path} {rscript_args}")
}

submit_job <- function(command) {
  cat("Submitting command:\n", command, "\n\n")
  output <- system(command, intern = TRUE)
  job_id <- str_extract(output, "\\d+")
  return(job_id)
}

# ---- Paths to R scripts ----
script_dir <- "/homes/mcconrad/repos/pcp/delivery_location_remapping/collapse/"
prep_script <- file.path(script_dir, "01_collapse_prep.R")
collapse_script <- file.path(script_dir, "02_collapse.R")
concat_script <- file.path(script_dir, "03_concat.R")
log_dir <- paste0("/share/temp/slurmoutput/", user, "/errors/") # Path to logs
dir.create(log_dir, showWarnings = FALSE)

# ---- Submit SLURM collapse prep jobs only ----
prep_job_ids <- list()
for (nid in collapse_l_nids) {
  args_prep <- glue("--nid {nid} --multiple_extract_nid --version {prep_version} --overwrite TRUE")
  prep_job_name <- glue("prep_j_{nid}")
  cmd_prep <- make_sge_command(prep_job_name, prep_script, args_prep, log_dir)
  prep_job_id <- submit_job(cmd_prep)
  prep_job_ids[[paste0("j_", nid)]] <- prep_job_id
}

#now we add on for the collapse
collapse_job_name <- "collapse_all"
collapse_args <- glue("--version {prep_version}")  # no --nid for batch mode

cmd_collapse <- make_sge_command(
  job_name = collapse_job_name,
  script_path = collapse_script,
  rscript_args = collapse_args,
  log_dir = log_dir,
  memory = "16G",
  time = "02:00:00",
 # dependency = dependency_flag
)

submit_job(cmd_collapse)
