###########################################################################################################
### Author: Will Gardner (wgard@uw.edu)
###         Adapted from Simon Yadgir (syadgir)
###.        Adapted for Health Systems by Wes Warriner (warriwes)
### Date: 2024-01-24
### Project: ST-GPR
### Purpose: Test combinations of priors, calculate OOS validity, and generate weights
###########################################################################################################
### Outputs: 
###       -A list of two data tables:
###        -A data.table with each row corresponding to a model, and the following columns:
###             out_rmse: the average out of sample rmse for that model
###             in_rmse: the in sample rmse for that model
###             aic: the in-sample Aikake information criterion value for that model
###             Intercept_fixd: the global intercept for that model
###             Intercept_fixd_se: the standard error of the global intercept for that model
###             'cov'_fixed: a column for each covariate, giving the fixed effect coefficient
###             'cov'_fixed_se: a column for each covariate, giving the standard error of that covariate's fixed effect coefficient
###         -A data.table that is identical to the input data, except with the following extra columns:
###             The covariate estimates specified
###            Columns indicating which rows have been knocked out for each holdout
########################################################################################################################################################


##########################################
########### TEST PRIOR FUNCTION ###########
##########################################

test_prior <- function(
    release_id,
    cov_list,
    interaction_list,
    data_transform,
    crosswalk_version_id=NA,
    path_to_data=NA,
    rank_method="oos.rmse", #"aic"
    modtype="lm",
    offset=0.0001,
    count_mods=FALSE,
    custom_covs=NULL,
    fixed_covs=NULL,
    random_effects=NULL,
    ban_pairs=NULL,
    by_sex=T,
    username=username,
    polynoms=NULL,
    prior_sign=NULL,
    p_value=0.05,
    ko_prop=0.25,
    kos=5,
    remove_subnats=T,
    no_new_ages=T,
    only_data_locs=F,
    drop_nids=FALSE,
    seed=32594,
    location_set_id=22,
    proj="proj_health_sys",
    m_mem_free=2,
    forms_per_job=10,
    years=c(1980:2024),
    version_label='default_version',
    scale_covariates=TRUE
){
  
  if(F){# interactively test
    crosswalk_version_id=NA
    release_id=16
    cov_list=c("ANC1",  "ANC4", "SBA", "C-section", "Fraction OOP", "Domestic Health Expenditure/Capita", 
               "Healthcare Access and Equality Index", "Universal Health Coverage", "Sociodemographic Index",
               "Total Fertility Rate", "Percent Births Over 35"
               ) 
    data_transform="logit" 
    username=Sys.info()[["user"]]
    count_mods=F 
    rank_method="oos.rmse" 
    modtype="lm" 
    offset=0.0001
    custom_covs=NULL 
    fixed_covs=NULL 
    random_effects=NULL
    ban_pairs=NULL
    by_sex=F
    polynoms=NULL
    prior_sign=prior_sign
    p_value=0.05
    ko_prop=0.25 
    kos=5 
    remove_subnats=T # TODO
    no_new_ages=T 
    only_data_locs=F 
    drop_nids=T 
    seed=32394 # TODO
    location_set_id=22
    proj="proj_health_sys" 
    m_mem_free=6
    forms_per_job=10
    years=c(1980:2022) # TODO
  }
  
  ########################################
  ################ SET-UP ################
  ########################################
  os <- .Platform$OS.type
  if (os=="windows") {
    stop("Must be run on cluster!")
  } 
  j <- "/home/j/"
  k <- "/ihme/cc_resources/"
  username <- Sys.info()[["user"]]
  date <- gsub("-", "_", Sys.Date())
  ROOT = file.path(h, 'repos', 'pcp')
  INTERACTION_DELIMITER <- "."
  
  # Load libraries and source functions
  pacman::p_load(data.table, plyr, DBI, dplyr, RMySQL, lme4, ggplot2, ini)
  source(file.path(ROOT, 'delivery_location_remapping', 'pipeline', '04_stgpr', 'linear_oos', 'helpers.R'))
  
  ## Checks
  # Check prior signs
  if(!is.null(prior_sign)){
    if(length(prior_sign)!=length(cov_list)+length(custom_covs)+length(interaction_list)){stop("Length of prior_sign is not equal to length of (cov_list+custom_covs+interaction_list)!!")}
  }
  
  # Check model types
  if(modtype=="lmer" & is.null(random_effects)){stop("You specified modtype as lmer, but did not specify any random effects!")}
  if(modtype=="lm" & !is.null(random_effects)){stop("You specified modtype as lm, and also specified random effects!")}
  
  ## Set Paths
  central <- paste0("/ihme/cc_resources/libraries/current/r/") #path to central functions
  #path to temporarily save model outputs--this folder gets created and deleted during the function
  if (!is.na(crosswalk_version_id)) {
    output_folder <- file.path("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/st-gpr/02_models", me, '02_linear_oos', 'temp', version_label, crosswalk_version_id)
  } else {
    output_folder <- file.path("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/st-gpr/02_models", me, '02_linear_oos', 'temp', version_label, 'data_path')
  }
  
  if(file.exists(output_folder)){
    message(paste("Temporary directory", output_folder, "already exists, deleting contents"))
    unlink(output_folder, recursive = T)
    message("Done deleting")
  }
  
  dir.create(output_folder, recursive = T)
  
  #########################################  
  ################ SCRIPTS ################
  #########################################
  source(file.path(STGPR_DIR, 'bind_covariates.R'))
  source(file.path(STGPR_DIR, 'make_ko.R'))
  source(file.path(STGPR_DIR, 'helpers.R'))
  source(paste0(central, "get_location_metadata.R"))
  source(paste0(central, "get_crosswalk_version.R"))
  
  ##########################################  
  ################ GET DATA ################
  ##########################################
  
  message("Getting data...")
  if (!is.na(crosswalk_version_id)){
    data <- get_crosswalk_version(crosswalk_version_id)
    message("Done")
  } else if (!is.na(path_to_data)){
    data <- fread(path_to_data)
    message("Done")
  } else {
    stop("ERROR: Must supply either a crosswalk version ID or path to data!")
  }
  
  data <- data[is_outlier==0]
  ### Add edit for mean values of 1 in hosp_any here. Include an offset if data is correct
  data[["val"]] <- ifelse(data[["val"]] == 1, data[["val"]] - 0.001, data[["val"]]) ##THIS LINE!
  
  if (!("sex_id" %in% names(data))) data[,sex_id := ifelse(sex=="Both",3,ifelse(sex=="Female",2,1))]
  # TODO: below, what is variance col used for/how?
  if ("val" %in% names(data)) data <- data[, .(nid, survey_name, ihme_loc_id, year_start, year_end, survey_module,
                                                file_path, sample_size, nclust, nstrata, mean, val, standard_error, 
                                                variance, design_effect, delivery_location, year_id, sex_id, age_group_id)] 
  if ("val" %in% names(data)) setnames(data, "val", "data")
  data <- data[, .(nid, survey_name, ihme_loc_id, year_start, year_end, survey_module,
                   file_path, sample_size, nclust, nstrata, data, mean, standard_error, 
                   variance, design_effect, delivery_location, year_id, sex_id, age_group_id)]
  
  
  if("cv_custom_prior" %in% names(data)){
    message("Dropping cv_custom_prior column")
    data[, cv_custom_prior:=NULL]
  }
  
  # check data compatability w/ model
  # if(nrow(data[is.na(data)])>0){
    # message("You have ", nrow(data[is.na(data)]), " NAs in your data, these rows will be dropped")
  data<-na.omit(data)
  # }
  if(length(unique(data$age_group_id))<=1  &  any(grepl("age_group_id", c(fixed_covs, random_effects)))) {stop("You specified age_group_id as a predictor, but you have less than 2 levels of this variable")}
  
  ##############################################################  
  ################ GET LOCATIONS AND COVARIATES ################
  ##############################################################
  
  locs <- get_location_metadata(location_set_id=location_set_id, release_id=release_id)[, .(location_id, super_region_name, region_name, location_name, level)]
  data_and_names <- bind_covariates(data, cov_list=cov_list, custom_cov_list=custom_covs, release_id=release_id)
  data <- data_and_names[[1]] # this is the data w/ bound covariate estimates
  cov_list <- data_and_names[[2]] # these are the covariate_name_shorts
  
  message("Restricting data to years specified")
  # print(years)
  data <- data[year_id %in% years,]
  
  for(cov in cov_list){
    if(nrow(data[is.na(get(cov)),])>0){
      message("There are ",  nrow(data[is.na(get(cov)),])," missing estimates for ", cov, ", these rows will be dropped!")
      Sys.sleep(5)
      data<-data[!is.na(get(cov))]
    }
  }
  
  for(cov in cov_list){
    if(!cov %in% names(data)){ stop(paste(cov, "missing from data... make sure that the covariate has estimates!"))}
  }
  
  # this creates a column in the dataset, and also saves the names of those columns in the 'polynoms' vector
  if(!is.null(polynoms)){
    polys <- strsplit(polynoms, "\\^")
    polynoms.t<-list()
    for(i in 1:length(polys)){
      basecov<-polys[[i]][1]
      if(!basecov %in% names(data)){ stop("ATTEMPTING TO CREATE POLYNOMIAL, MISSING: ", polys[[i]][1])}
      
      data[, paste0(basecov, polys[[i]][2]):=get(basecov)^as.numeric(polys[[i]][2])]
      polynoms.t[[i]]<-paste0(basecov, polys[[i]][2])
    }
    polynoms<-unlist(polynoms.t)
  }
  
  # Create columns for the interaction terms and save the names of those columns in the 'interactions' vector
  print(interaction_list)
  if(!is.null(interaction_list)){
    interactions <- list()
    for(i in 1:length(interaction_list)){
      # This is a vector of covariate names
      interaction_covs <- interaction_list[[i]]
      interaction_name_short <- paste(interaction_covs, collapse = INTERACTION_DELIMITER)
      for (cov in interaction_covs){
        if(!cov %in% names(data)){
          message('Logging names(data)...')
          print(names(data))
          stop(paste("ATTEMPTING TO CREATE INTERACTION ", interaction_name_short, ", BUT MISSING: ", cov))
        }
      }
      data[, eval(interaction_name_short) := Reduce(`*`, lapply(interaction_covs, function(x) get(x)))]
      interactions[[i]] <- interaction_name_short
    }
    interactions <- unlist(interactions)
    cov_list <- c(cov_list, interactions)
  }
  
  message('Logging cov_list...')
  print(cov_list)
  
  # TODO: warriwes - covariates scaling HERE
  # Scale any/all covariates in cov_list, custom_covs, and interactions_names_list to have mean 0 and sd 1
  if (scale_covariates){
    message("Scaling covariates")
    for (cov in cov_list){
      data[, (cov) := scale(get(cov))]
    }
  }
  # TODO: warriwes - commented this out in favor of scaling ALL the same method
  # if ("LDI_pc" %in% cov_list){
  #   message("logging LDI_pc for modeling")
  #   data$LDI_pc <- log(data$LDI_pc) # log LDI for modeling
  #   message('logged LDI_pc')
  # }
  # if ("TFR" %in% cov_list){
  #   message("logging TFR for modeling")
  #   data$TFR <- log(data$TFR)
  # }
  # if("neonatal_modeled_per1000v2" %in% cov_list){
  #   message("logging neonatal mortality for modeling")
  #   data$neonatal_modeled_per1000v2 <- log(data$neonatal_modeled_per1000v2)
  # }
  # if ("HIV_mort_females_10_54" %in% cov_list){
  #   message("logging HIV mortality for modeling")
  #   data$HIV_mort_females_10_54 <- log(data$HIV_mort_females_10_54)
  # }
  # if ("Domestic Health Expenditure/Capita" %in% cov_list){
  #   message("logging Domestic Health Expenditure/Capita for modeling")
  #   data$domestic_he_cap <- log(data$domestic_he_cap + 1) # log LDI for modeling"Domestic Health Expenditure/Capita"
  # }
  
  message('Merging data with location metadata...')
  data <- merge(data, locs, by="location_id")
  print(dim(data))
  message('ST-GPR: Transforming data, offset...')
  # TODO: throws error. this is helpers functin
  data <- offset.data(data, data_transform, offset) # offset function from st-gpr
  message('ST-GPR: offset done.')
  print(dim(data))
  
  ############################################################# 
  ############## GET KOS AND SAVE TO TEMP FOLDER ##############
  #############################################################
  message(paste0('by_sex: ', by_sex))
  if(by_sex==T){
    sex_list<-c("M", "F")
    if (!(1 %in% unique(data$sex_id))) sex_list<-c("F")
    if (!(2 %in% unique(data$sex_id))) sex_list<-c("M")
  }else{
    sex_list<-c("both_sexes")
  }
  
  for(sexchar in sex_list){
    message("Prepping ", sexchar, " data for KO creation")
    
    if(sexchar=="M"){  sex<-1.0}
    if(sexchar=="F"){  sex<-2.0}
    if(sexchar=="both_sexes"){  sex<-c(1.0,2.0,3.0)}
    print(dim(data))
    print(names(data))
    data.s<-data[sex_id %in% sex, ]
    print(dim(data.s))
    
    # set up data set if it's 'all ages'
    if(length(unique(data.s$age_group_id))==1){
      if(unique(data.s$age_group_id)==22  | unique(data.s$age_group_id)==27){
        data.s[, age_group_id:=22]  ##sy: set to 22
        by_age<-0
      }else if(!(unique(data.s$age_group_id) %in% c(2:5, 388:389, 238, 34, 6:20, 30:32, 235, 164))){
        stop("Ask Will about incorporating custom age group")
      }else {
        by_age <- 1
      }
    }else{
      by_age<-1
    }
    if(F){
      remove_subnats<-T
      no_new_ages<-T
      only_data_locs<-F
      drop_nids<-T
      seed<-10
    }
    
    ko_items<-prep_ko(data.s, location_set_id=location_set_id, by_age=by_age, by_sex=ifelse(by_sex, 1, 0), start_year=head(years,n=1), end_year=tail(years,n=1), release_id=release_id)
    message("Done")
    
    message("Generating KOs")
    # generate knockouts.  The arguments are set up to take the output of the prep_ko function directly
    # TODO: ko_items[[2]] must not have missingness
    test<-get_kos(ko_items[[1]], ko_items[[2]], ko_items[[3]], ko_items[[4]], prop_to_hold=ko_prop, kos=kos, seed=seed, no_new_ages=no_new_ages, only_data_locs=only_data_locs, drop_nids = drop_nids)
    message("Done")
    
    # writing the formmated dataset to avoid doing it for each child process
    saveRDS(test, file=file.path(output_folder, paste0(sexchar, "_full_data.rds")),version=2)
    message("Saved ", sexchar, " prepped data to temp folder")
  }
  
  #####################################################
  ############## GET ALL POSSIBLE MODELS ##############
  #####################################################
  
  # check to make sure banned pairs are valid names
  invisible(lapply(unlist(ban_pairs), function(x){
    if(!x %in% names(data)){message("You specified ", x, " as a banned pair, but it is not a valid covariate name short in your data!")}
  }))
  
  # set up formula
  message("Logging cov_list prior to setting up general formula:")
  print(cov_list)
  if(!is.null(random_effects)){
    form<-paste0( data_transform,"(data)~", paste0(cov_list, collapse="+"))
                  # "+", paste0(polynoms, collapse="+"), "+", paste0(fixed_covs, collapse="+"), "+", paste0(random_effects, collapse="+"))
  }else{
    form<-paste0( data_transform,"(data)~", paste0(cov_list, collapse="+"),
                  "+", paste0(polynoms, collapse="+"), "+", paste0(fixed_covs, collapse="+"))
  }
  
  # set up banned set logic--The result of this doesn't get applied, it's only for use w/ dredge() function
  sub<-paste(unlist(lapply(ban_pairs, function(x){
    thing<-c(rep(NA, times=length(x)-1))
    
    for(i in 1:length(x)-1){
      if(i<length(x)){
        bans<-c(rep(NA, times=length(x)-i))
        for(n in i+1:length(x)-i){
          bans[n-i]<-paste0("'", x[n], "'")
        }
        bans.t<-paste(bans, collapse=" | ")
        if(i<length(x)-1){
          thing[i]<-paste0("!('", x[i], "'  &&  (",  bans.t, "))")
        }else{
          thing[i]<-paste0("!('", x[i], "'  &&  ",  bans.t, ")")
          
        }
        
      }
      
    }
    return(paste(thing, collapse=" & "))
    
  })), collapse="  &  ")
  
  message(paste("Getting formulas..."))
  message(paste("General formula:", form))
  message(paste("Banned sets:", sub))
  
  # add polynomials to cov_list:
  # save original cov list for prior_signs later first
  og_cov_list<-cov_list
  cov_list<-c(cov_list, polynoms)
  
  ## Function to remove ban pairs from a list of covariates
  remove.ban <- function(cov_list, ban) {
    out <- lapply(cov_list, function(covs) {
      if (length(covs) == 1 | (!all(sort(intersect(covs, ban)) == sort(ban))) | length(intersect(covs, ban))==0) return(covs)
    })
    return(out[!sapply(out, is.null)])
  }
  
  ## Create all combinations of covariates, including interaction terms
  message(paste("Length of cov_list:", length(cov_list)))
  print(cov_list)
  mod_list <- lapply(1:length(cov_list), function(x) combn(cov_list, x, simplify=FALSE)) %>% unlist(., recursive=F)
  message(paste("length of mod_list:", length(mod_list)))
  # message("mod_list[1], mod_list[1045]")
  # print(mod_list[[1]])
  # Good, checks out at this point:
  # print(mod_list[[1045]])
  # Remove any combinations that include interaction terms that don't include the main effects
  # for example, if we have a main effect "x" and an interaction "x_y", we must drop 
  # any vector in mod_list with "x_y" element but not both "x" and "y" elements
  is_valid_form <- function(vec) {
    # Loop through each element in the vector
    for (element in vec) {
      if (grepl(INTERACTION_DELIMITER, element, fixed = TRUE)) {
        sub_elements <- unlist(strsplit(element, INTERACTION_DELIMITER, fixed = TRUE))
        if (!all(sub_elements %in% vec)) {
          return(FALSE)
        }
      }
    }
    return(TRUE)
  }
  validity_checks <- sapply(mod_list, is_valid_form)
  mod_list <- mod_list[validity_checks]
  message(paste("length of mod_list:", length(mod_list)))
  # message("mod_list[[1]], mod_list[[1045]]")
  # print(mod_list[[1]])
  # TODO: apparently we end up with mod_list length of 1023, so 1045 is out of bounds,
  # however we really should end up with more like >10k models, so something is wrong with
  # the validity checking above
  # print(mod_list[[1045]])
  
  if(!is.null(ban_pairs)){
    ## Create combinations of ban sets
    keepers <- list()
    for(i in 1:length(ban_pairs)){
      x <- ban_pairs[[i]]
      ban_list <- combn(x, 2, simplify=FALSE)
      ## Create lists for each ban set
      banned_lists <- lapply(ban_list, function(ban) remove.ban(mod_list, ban))
      ## Intersect them all
      keepers[[i]]<-Reduce(intersect, banned_lists)
    }
    
    temp_forms<-unlist(lapply(Reduce(intersect, keepers), function(x){ paste0(x, collapse="+")}))
  }else{
    temp_forms<-unlist(lapply(mod_list, function(x){paste0(x, collapse="+")}))
  }
  
  message(paste("length of mod_list:", length(mod_list)))
  message("mod_list[1]")
  print(mod_list[1])
  message(paste("Number of models to test (length of temp_forms):", length(temp_forms)))
  
  # paste all pieces together
  if(!is.null(random_effects)){
    if(!is.null(fixed_covs)){
      forms.n<-paste(
        paste0(data_transform, "(data)"),
        paste(temp_forms, paste0(fixed_covs, collapse="+"), paste0(random_effects, collapse="+"), sep="+"),
        sep="~")
      
      # add on the null mod
      null_mod<-paste(paste0(data_transform,"(data)"),
                      paste(paste0(fixed_covs, collapse="+"), paste0(random_effects, collapse="+"), sep="+"),
                      sep="~")
      
    }else{
      forms.n<-paste(
        paste0(data_transform, "(data)"),
        paste(temp_forms, paste0(random_effects, collapse="+"), sep="+"),
        sep="~")
      
      # add on the null mod
      null_mod<-paste(paste0(data_transform,"(data)"),
                      paste(paste0(random_effects, collapse="+"), sep="+"),
                      sep="~")
    }
  }else{
    forms.n<-paste(
      paste0(data_transform, "(data)"),
      paste(temp_forms, paste0(fixed_covs, collapse="+"), sep="+"),
      sep="~")
    
    # add on the null mod
    null_mod<-paste(paste0(data_transform,"(data)"),
                    paste(paste0(fixed_covs, collapse="+"), sep="+"),
                    sep="~")
    
    if(!is.null(fixed_covs)){
      forms.n<-paste(
        paste0(data_transform, "(data)"),
        paste(temp_forms, paste0(fixed_covs, collapse="+"), sep="+"),
        sep="~")
      
      # add on the null mod
      null_mod<-paste(paste0(data_transform,"(data)"),
                      paste(paste0(fixed_covs, collapse="+"), sep="+"),
                      sep="~")
      
    }else{
      forms.n<-paste(
        paste0(data_transform, "(data)"),
        paste(temp_forms, sep="+"),
        sep="~")
      
      ##sy: add on the null mod
      null_mod<-paste(paste0(data_transform,"(data)"),
                      1,
                      sep="~")
    }
  }
  
  forms<-c(null_mod, forms.n)
  message("Done getting formulas")
  
  if(count_mods==T){
    message("Done--Returning ", length(forms), " formulas")
    if(by_sex==T){
      message(paste("You set by_sex==T, so real number of formulas to be evaluated is", length(forms)*2))
    }
    return(forms)
    stop("Done")
  }
  
  if(by_sex==T){
    message(paste(length(forms)*2, "total formulas to evaluate"))
  }else{
    message(paste(length(forms), "total formulas to evaluate"))
  }
  
  saveRDS(forms, file=file.path(output_folder, "forms.rds"),version=2)
  message(paste0("Model combinations saved to ", paste0(output_folder, "/forms.rds")))
  
  ###############################################
  ############## LAUNCH MODEL JOBS ##############
  ###############################################
  
  file_list<-list()
  for(sexchar in sex_list){
    
    # setup number of jobs to submit
    n <- length(forms)
    n_divisions <- forms_per_job
    # create start and end_ids
    seq <- data.table(start =seq(1, n, by = n_divisions))
    seq[, end:=shift(start, type = "lead") - 1]
    seq[nrow(seq), end:=n]
    seq[, id:=seq(.N)]
    nsubs <- max(seq$id)
    message("Launching ", nsubs, " splitting jobs for sex: ", sexchar)
    data_path <- paste0(output_folder, "/", sexchar, "_full_data.rds")
    forms_path <- paste0(output_folder, "/", "forms.rds")
    
    for(i in 1:max(seq$id)){
      if(F){
        i<-16
        sexchar<-"both_sexes"
        data_transform<-"logit"
      }
      
      # get start and end forms
      date<-gsub("-", "_", Sys.Date())
      start <- seq[id ==i, start]
      end <- seq[id == i, end]
      
      command <- paste0("sbatch -p all.q -t 00:15:00 --mem=", m_mem_free, "G -c 2 -A ", proj, " -J ", paste0("oos_", sexchar, "_", i),
                        " -o /share/temp/slurmoutput/", username, "/output/%x.o%j -e /share/temp/slurmoutput/", username, "/errors/%x.e%j", 
                        " /ihme/singularity-images/rstudio/shells/execRscript.sh -s ", file.path(STGPR_DIR, "linear_oos_process.R"), " ",
                        start, " ", end, " ",
                        sexchar, " ",  data_path, " ", forms_path, " ", output_folder, " ", date, " ", data_transform, " ", 
                        modtype, " ", kos)
      system(command)
    }
  }
  #: job hold
  message("Waiting on jobs...")
  job_hold("oos_")
  message("Finished model testing")
  
  ######################################################
  ############## READ IN RESULTS AND RANK ##############
  ######################################################
  
  # TODO these don't exist, when should they have been created?
  ### CS - this is the error I'm having - rmse file length is 0 so model results are missing
  summary(data$val)  # Gives a quick overview, check if Min, Max are Inf or -Inf
  sum(is.na(data$val))  # Counts how many NA values
  sum(is.nan(data$val))  # Counts how many NaN values
  sum(is.infinite(data$val))  # Counts how many Inf or -Inf values
  
  rmse_files<-list.files(path=output_folder, pattern=".csv", full.names=T)
  message('Length of rmse_files:')
  print(length(rmse_files))
  message("Checking result files...")
  
  file_length<-ifelse(by_sex & 1 %in% unique(data$sex_id) & 2 %in% unique(data$sex_id), 2*length(forms), length(forms))
  if(length(rmse_files)!=file_length){
    stop(paste0(file_length-length(rmse_files), " model results are missing! Check error logs; jobs may have broken"))
  }
  
  ## Read in the results
  message("Reading in results, and ranking models")
  stack<-list()
  if(length(rmse_files>0)){
    for(i in 1:length(rmse_files)){
      stack[[i]]<-fread(rmse_files[i])
    }
  }else{
    stop("All jobs broken")
  }
  rmses<-rbindlist(stack, fill=T)
  
  ## Rank Models
  if(rank_method=="oos.rmse"){
    # sort by oos rmse
    rmses<-setorder(rmses, out_rmse)
  }
  if(rank_method=="aic"){
    rmses<-setorder(rmses, aic)
  }
  
  ## Subset models that violate prior signs
  
  rmses<-restrict_violations(rmses=rmses, prior_sign=prior_sign, covs=cov_list, p_value=p_value)
  message("test_prior() complete. logging rmses (as we have had issues with this piece of test_prior() output in the past)")
  print(rmses)
  
  ##############################################
  ############## FINAL PROCESSING ##############
  ##############################################
  
  message("Cleaning out temp folder...")
  unlink(output_folder, recursive=T)
  
  message("Done")
  return(list(rmses, data))
}

