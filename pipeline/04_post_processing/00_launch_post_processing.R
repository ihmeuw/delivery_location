################################################################################
##' 
##' Launch script for Congo rescale
##' Author: Jessica Klusty
##' Updated: 7/11/25
##' 
################################################################################


user <- Sys.info()['user']
code_fp <- file.path('~/repos/pcp/delivery_location_remapping/pipeline/05_post_processing/')
output_file <- paste0('/mnt/share/temp/slurmoutput/', user, '/errors/%x_%j.txt')


## 01. congo rescale ===========================================================

batch_command <- glue::glue(
  'sbatch -o {output_file} -A proj_health_sys -J ',
  'congo_rescale -p all.q -C archive --mem 200G -c 10 -t 6:00:00 ',
  '/ihme/singularity-images/rstudio/shells/execRscript.sh ',
  '-s {code_fp}01_congo_rescale.R ',
  '--num_cores 20'
)
system(batch_command)


## 02. pcp delivery location scaling stgpr =====================================

batch_command <- glue::glue(
  'sbatch -o {output_file} -A proj_health_sys -J ',
  'dla_scaling -p long.q -C archive --mem 200G -c 10 -t 6:00:00 ',
  '/ihme/singularity-images/rstudio/shells/execRscript.sh ',
  '-s {code_fp}02_pcp_delivery_location_scaling_stgpr.R ',
  '--num_cores 20'
)
system(batch_command)


## 03. pcp delivery location location aggregates ===============================

mes <- c("prim_any", "pub_hosp", "priv_hosp", "pub_prim", "priv_prim", "hosp_any")
version_scaled <- "2025-07-14" ## version of scaled st-gpr results

for (me in mes) {
  batch_command <- glue::glue(
    'sbatch -o {output_file} -A proj_health_sys -J ',
    'dla_aggregates -p long.q -C archive --mem 10G -c 10 -t 6:00:00 ',
    '/ihme/singularity-images/rstudio/shells/execRscript.sh ',
    '-s {code_fp}03_pcp_delivery_location_location_aggregates.R ',
    '{me} {version_scaled}'
  )
  system(batch_command) 
  Sys.sleep(3)
}


