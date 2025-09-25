#### Pullling st-gpr results for delivery locations ####
### Chiara Sumich ###
### 12.02.2024 ###

#### Load packages
rm(list = ls())
pacman::p_load(data.table, ggplot2, gtools, tidyverse, haven, labelled)

#### Defining variables
pub_hosp_run_id <- 221415
priv_hosp_run_id <- 221416
pub_prim_run_id <- 221417
priv_prim_run_id <- 221418
#ifd_run_id <- 221119
  
ndraws <- 100
n_draws<- ndraws-1

#### Read in the ST-GPR results
pub_hosp_draw_path <-paste0("/ihme/covariates/ubcov/model/output/", pub_hosp_run_id, "/draws_temp_0/")
priv_hosp_draw_path <-paste0("/ihme/covariates/ubcov/model/output/", priv_hosp_run_id, "/draws_temp_0/")
pub_prim_draw_path <-paste0("/ihme/covariates/ubcov/model/output/", pub_prim_run_id, "/draws_temp_0/")
priv_prim_draw_path <-paste0("/ihme/covariates/ubcov/model/output/", priv_prim_run_id, "/draws_temp_0/")
#ifd_draw_path <- paste0("/ihme/covariates/ubcov/model/output/", ifd_run_id, "/draws_temp_0/")

draw_names <- paste0("draw_", 0:n_draws)

pub_hosp_files <- list.files(pub_hosp_draw_path)
priv_hosp_files <- list.files(priv_hosp_draw_path)
pub_prim_files <- list.files(pub_prim_draw_path)
priv_prim_files <- list.files(priv_prim_draw_path)
#ifd_files <- list.files(ifd_draw_path)

fread_stgpr <- function(file, draw_names) {
  dt <- fread(paste0(file))
  #Create the mean, upper and lower using quantiles and then drop draws. 
  dt[, mean:= apply(.SD, 1, mean), .SDcols = draw_names]
  dt[, lower:= apply(.SD, 1, quantile, c(0.025)), .SDcols = draw_names]
  dt[, upper:= apply(.SD, 1, quantile, c(0.975)), .SDcols = draw_names]
  dt[, (draw_names) := NULL] #Comment out if you want to keep the draws
  return(dt[])
}

pub_hosp_results <- rbindlist(lapply(paste0(pub_hosp_draw_path, pub_hosp_files),
                              FUN = fread_stgpr, 
                              draw_names = draw_names), use.names=TRUE) %>%
  mutate(me_id = "pub_hosp")

priv_hosp_results <- rbindlist(lapply(paste0(priv_hosp_draw_path, priv_hosp_files),
                                      FUN = fread_stgpr, 
                                      draw_names = draw_names), use.names=TRUE) %>%
  mutate(me_id = "priv_hosp")

pub_prim_results <- rbindlist(lapply(paste0(pub_prim_draw_path, pub_prim_files),
                                     FUN = fread_stgpr, 
                                     draw_names = draw_names), use.names=TRUE) %>%
  mutate(me_id = "pub_prim")

priv_prim_results <- rbindlist(lapply(paste0(priv_prim_draw_path, priv_prim_files),
                                      FUN = fread_stgpr, 
                                      draw_names = draw_names), use.names=TRUE) %>%
  mutate(me_id = "priv_prim") 

source("/ihme/cc_resources/libraries/current/r/get_model_results.R")
gbd_ifd <- get_model_results("stgpr", gbd_id = 10651, model_version_id = 221386, release_id = 16)
gbd_ifd <- gbd_ifd %>%
  mutate(me_id = "gbd_ifd") %>%
  select(year_id, location_id, sex_id, age_group_id, mean, lower, upper, me_id)

source('/ihme/cc_resources/libraries/current/r/get_location_metadata.R')
loc <- get_location_metadata(
  location_set_id = 22,
  release_id = 16,
)

results <- bind_rows(pub_hosp_results, priv_hosp_results, pub_prim_results, priv_prim_results, gbd_ifd) %>%
  pivot_wider(names_from = me_id,
              values_from = c("mean", "lower", "upper")) %>%
  mutate(sum = mean_pub_hosp + mean_priv_hosp + mean_pub_prim + mean_priv_prim, 
         difference = sum - mean_gbd_ifd) %>%
  left_join(loc, by = "location_id") %>%
  select(year_id, location_id, location_name, ihme_loc_id, sex_id, age_group_id, mean_pub_hosp, mean_priv_hosp, mean_pub_prim,
         mean_priv_prim, mean_gbd_ifd, sum, difference, lower_pub_hosp, lower_priv_hosp, lower_pub_prim,
         lower_priv_prim, lower_gbd_ifd, upper_pub_hosp, upper_priv_hosp, upper_pub_prim, upper_priv_prim, upper_gbd_ifd) %>%
  filter(year_id >=1990)

### save results ###
write_csv(results, "/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/st-gpr/02_models/st-gpr_results_vetting - mes_add_to_ifd_estimates.csv")
