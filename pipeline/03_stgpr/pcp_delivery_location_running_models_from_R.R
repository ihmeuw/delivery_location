rm(list = ls())
pacman::p_load(data.table, ggplot2, gtools, tidyverse, haven, labelled)

source('/ihme/code/st_gpr/central/stgpr/r_functions/utilities/utility.r')
source("/ihme/code/st_gpr/central/src/stgpr/api/public.R")
#source("r_functions/registration/sendoff.R")

user <- Sys.getenv('USER')
if (Sys.info()['sysname'] == 'Linux') {
  j <- '/snfs1' # '/home/j' # deprecated
  h <- paste0('/homes/', user)
  k <- '/ihme/cc_resources'
} else { 
  j <- 'J:'
  h <- 'H:'
  k <- 'K:'
}


# Read in config
code_fp <- paste0(h, "/pcp/delivery_location_remapping/pipeline/04_stgpr/")
run_id <- register_stgpr_model(path_to_config = paste0(code_fp, "/pcp_delivery_location_model_configs.csv"), 
                               model_index_id = 174)

stgpr_sendoff(gbd_model_version_id = run_id, 
              project = 'proj_health_sys')

# Check model status every minute (so we don't overwhelm the db!) until it finishes
status <- get_model_status(run_id)
while (status == 2) {
  cat("Model still running! Waiting a minute...\n")
  Sys.sleep(60)
  status <- get_model_status(run_id, verbose = TRUE)
}

print("Model finished!")

# check = get_crosswalk_version(47888)
# check2 = get_crosswalk_version(47813)
