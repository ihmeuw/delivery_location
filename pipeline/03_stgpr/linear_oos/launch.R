###########################################################################################################
### Author: Will Gardner (wgard@uw.edu)
### Date: 07/31/2019
### Project: ST-GPR
### Purpose: Launch custom first stage prior testing and assemble for upload to ST-GPR
### Overview: This script is for launching two functions: test_prior() and assemble_prior(). 
###           Together they allow the creation of an ensemble linear prior for STGPR
### test_prior()
###   This function runs all combinations of provided covariates on a provided dataset
###   It ranks each model by out-of-sample root-mean-squared error, and marks models as containing insignificant betas and/or betas that represent the 'wrong' relationship with your outcome
###   Each added covariate adds a significant number of models to test. Only include covariates that may have a reasonable relationship with your outcome
###   See full documentation here: "/share/code/mnch/st_gpr/linear_oos/test_prior.R"
###   The output is a list that includes a DT with a row for each model, the fit betas/SE, and whether or not that model violated significance or prior signs
###
### assemble_prior()
###   This function creates out-of-sample RMSE-weighted predictions of your outcome using the results from the test_prior() function
###   This can be included as a 'cv_custom_prior' in an STGPR data upload
###   This function can also plot a time-series of predictions for submodels, top weighted model, and the ensemble model
###   See full documentation here: "/share/code/mnch/st_gpr/linear_oos/assemble_prior.R"
###########################################################################################################

# Environment Setup
user <- Sys.getenv('USER')
if (Sys.info()['sysname'] == 'Linux') {
  j <- '/snfs1' # '/home/j' # deprecated
  h <- paste0('/ihme/homes/', user)
  k <- '/ihme/cc_resources'
} else { 
  j <- 'J:'
  h <- 'H:'
  k <- 'K:'
}

# Imports
pacman::p_load(yaml)

ROOT = file.path(h,'repos', 'pcp')
STGPR_DIR = file.path(ROOT, 'delivery_location_remapping', 'pipeline', '04_stgpr', 'linear_oos')
settings = yaml.load_file(file.path(ROOT, 'config.yml'))

launch_prior <- function(me, release_id=33, crosswalk_version_id=NA, path_to_data=NA, test_mods=TRUE, average=TRUE, pred_ages,
                         n_mods=50, plot_aves=TRUE, plot_betas=TRUE, age_trend=FALSE,
                         cov_ids, 
                         interaction_ids,
                         prior_sign, ban_pairs=NULL, polynoms=NULL, data_transform="logit", modtype="lm", p_value=0.05,
                         rank_method = "oos.rmse", forms_per_job=30, drop_nids=FALSE, fixed_covs=NULL,
                         custom_covs=NULL, random_effects=NULL, username=username, by_sex=by_sex, 
                         years=c(1980:2024), version_label='default_version',
                         scale_covariates=TRUE){
  username <- Sys.info()[["user"]]
  date <- gsub("-", "_", Sys.Date())
  #####################################
  ############### PATHS ###############
  #####################################
  linear_oos_dir <- file.path("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/mcconrad_pre_gpr/", me, '02_linear_oos')
  output_dir <- file.path(linear_oos_dir, 'rmse', version_label)
  message(paste0("Creating output directory at: ", output_dir))
  dir.create(output_dir, recursive=TRUE)
  plot_mods_path <- file.path(linear_oos_dir, version_label)
  message(paste0("Creating plot directory at: ", plot_mods_path))
  dir.create(plot_mods_path, recursive=TRUE)
  
  rmse_output <- file.path(output_dir, paste0(me, "_prior_test_yes_prior_sign_", date, ".csv"))  # where the object returned by test_prior is saved
  data_output <- file.path(plot_mods_path, paste0(me, "_best_data.csv"))
  ensemble_output <- file.path(plot_mods_path, paste0(me, "_custom_prior_", date, ".csv"))
  
  #######################################
  ############### SCRIPTS ###############
  #######################################
  source("/ihme/cc_resources/libraries/current/r/get_ids.R")
  source(file.path(STGPR_DIR, "test_prior.R"))
  source(file.path(STGPR_DIR, "assemble_prior.R"))
  source(file.path(STGPR_DIR, "bind_covariates.R"))
  source(file.path(STGPR_DIR, "helpers.R"))
  
  #######################################
  ############## PULL COVS ##############
  #######################################
  covs <- get_ids("covariate")
  cov_list <- lapply(cov_ids, function(X) covs[covariate_id==X,covariate_name]) %>% unlist
  if (!is.null(interaction_ids)) {
    interaction_list <- lapply(interaction_ids, function(X) lapply(X, function(Y) covs[covariate_id==Y,covariate_name_short])) %>% lapply(unlist)
  } else {
    interaction_list <- NULL
  }
  
  #######################################
  ############ LAUNCH MODELS ############
  #######################################
  
  if(test_mods==T){
    message(paste0("Testing submodels for me ", me))
    rmses_and_data <- test_prior(crosswalk_version_id=crosswalk_version_id, path_to_data=path_to_data, release_id=release_id,
                                 cov_list=cov_list, 
                                 interaction_list=interaction_list,
                                 data_transform=data_transform, rank_method=rank_method, modtype=modtype,
                                 custom_covs=custom_covs, fixed_covs=fixed_covs, random_effects=random_effects, ban_pairs=ban_pairs, 
                                 prior_sign = prior_sign, by_sex=by_sex, polynoms=polynoms, ko_prop=ko_prop, kos=kos, drop_nids=FALSE, 
                                 remove_subnats=T, proj=proj, m_mem_free=m_mem_free, username=username, forms_per_job=forms_per_job, years=years,
                                 p_value=p_value, scale_covariates=scale_covariates)
    
    rmses <- rmses_and_data[[1]]
    print(rmses)
    data <- rmses_and_data[[2]]
    
    # write out data and rmses
    write.csv(data, file=data_output, row.names=F)
    # TODO: Is this rmses saved as expected? cant find it in the output directory
    write.csv(rmses, file=rmse_output, row.names=F)
  }
  
  #####################################################
  ############ CREATE ENSEMBLE PREDICTIONS ############
  #####################################################
  
  if(average==T){
    message("Averaging submodels and predicting")
    # TODO: we've gotten to here so far
    
    rmses <- get_recent(output_dir)  
    data <- fread(data_output)
    
    print(rmses)
    print(dim(data))
    
    ave_models <- assemble_prior(data,
                                 rmses[drop==0,],
                                 cov_list=cov_list,
                                 interaction_list=interaction_list,
                                 data_transform=data_transform,
                                 pred_ages=pred_ages,
                                 custom_cov_list=custom_covs, polynoms=polynoms, n_mods=n_mods,
                                 plot_mods=plot_aves, age_trend=age_trend, plot_mods_path=plot_mods_path, username=username, proj, 
                                 weight_col=ifelse(rank_method=="out.rmse","out_rmse","aic"), by_sex=by_sex, 
                                 location_set_id=22, release_id=release_id, years=years,
                                 scale_covariates=scale_covariates)
    
    setnames(ave_models, "ave_result", "cv_custom_stage_1")
    
    # drop some unneccessary cols for saving
    ave_models[, c("location_name", "region_name", "super_region_name", "age_group_name"):=NULL]
    
    # Save!
    write.csv(ave_models, file=ensemble_output, row.names=F)
    message(paste0("Success! Ensemble prior saved here: ", ensemble_output))
  }
  
  ###########################################
  ############ CREATE BETA PLOTS ############
  ###########################################
  
  if(plot_betas==T){
    source(file.path(STGPR_DIR, 'plot_prior_betas.R'))
  }
}
