###########################################################################################################
### Author: Will Gardner (wgard@uw.edu)
###         Adapted from Simon Yadgir (syadgir)
### Date: 06/12/2019
### Project: Maternal Care Indicators
### Purpose: Average the results of the test_prior() function and create ensemble linear predictions
###########################################################################################################
### Arguments: 
###   REQUIRED:
###           -data: dataset used in test_prior() for generating component models
###           -cov_list: A character string of covariates in the covariate_name form.  Must include any custom covariates (see custom_covs argument)
###               ex:   cov_list<-c("Mean BMI", "Alcohol (liters per capita)", "omega 3 unadjusted(g)")
###           -data_transform: Character string, either "log" or "logit" to be inherited by STGPR's transform_data() function
###               ex:   data_transform="log"
###           -username: Character, your IHME username.  For saving outputs and errors to sge
###               ex: username="wgard"
###########################################################################################################
CONCURRENT_TASKS_LIMIT = 1055 / 5

assemble_prior<-function(data,
                         rmses, 
                         cov_list, 
                         interaction_list,
                         data_transform,
                         pred_ages,
                         custom_cov_list=NULL, polynoms=NULL, n_mods=10,
                         plot_mods=F, age_trend=F, plot_mods_path=NULL, username=NULL, proj,
                         weight_col="out_rmse", by_sex=F, scale_covariates=T, location_set_id=22, release_id, years=c(1980:2024)){
  
  if(F){
    plot_mods <- T
    # plot_mods_path <- paste0("/share/covariates/maternal/plot_output/gpr_prior/maternal_anc1/")
    linear_oos_dir <- file.path("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/mcconrad_pre_gpr/", me, '02_linear_oos')
    file.path(linear_oos_dir, me)
    weight_col <- "out_rmse"
    username <- Sys.info()[["user"]]
  }
  
  ########################################
  ################ SET-UP ################
  ########################################
  
  os <- .Platform$OS.type
  if (os=="windows") {
    stop("Must be run on cluster!")
  }
  j <- "/home/j/" 
  date <- gsub("-", "_", Sys.Date())
  
  INTERACTION_DELIMITER <- "."
  
  require(data.table)
  require(plyr)
  require(DBI)
  require(dplyr)
  require(RMySQL)
  require(lme4)
  require(boot)
  require(ggplot2)
  require(rhdf5)
  require(matrixStats)
  
  #########################################  
  ################ SCRIPTS ################
  #########################################
  central<-paste0("/ihme/cc_resources/libraries/current/r/")
  source(paste0(central, "get_location_metadata.R"))
  source(file.path(STGPR_DIR, 'db_tools.R'))
  source(file.path(STGPR_DIR, 'helpers.R'))
  
  ######################################################## 
  ################ DEFINE PRED.LM FUNTION ################
  ########################################################
  
  pred.lm <- function(df, model, predict_re=0) {
    ## RE form
    re.form <- ifelse(predict_re==1, NULL, NA)
    ## Predict
    if (class(model) == "lmerMod") {
      prior <- predict(model, newdata=df, allow.new.levels=T, re.form=re.form)
    } else {
      prior <- predict(model, newdata=df)
    }
    return(prior)
  }
  
  #############################################
  ################ DATA CHECKS ################
  #############################################
  
  nec_cols<-c("data", "location_id", "year_id", "age_group_id", "sex_id")
  invisible(lapply(nec_cols, function(x){
    if(!x %in% names(data)){
      stop(paste0("Missing necessary column:", x))
    }
  }))
  nec_cols<-c(weight_col)
  invisible(lapply(nec_cols, function(x){
    if(!x %in% names(rmses)){
      stop(paste0("Missing necessary column:", x))
    }
  }))
  
  if(plot_mods==T & length(plot_mods_path)!=1){
    stop("plot_mods==T but you have not specified a valid plot_mods_path!")
  }
  
  if(n_mods>nrow(rmses)){
    message("You specified to average over ", n_mods, " models, but only supplied ", nrow(rmses), ". Only the supplied models will be averaged.")
    n_mods<-nrow(rmses)
  }
  
  # drop data NAs (if square already, just make a new one)
  message("Assemble Priors: Dropping NAs from data")
  message("before")
  print(dim(data))
  # data <- data[!is.na(data)]
  data <- na.omit(data)
  message("after")
  print(dim(data))
  locs <- get_location_metadata(location_set_id=location_set_id, release_id=release_id)
  
  # get standard locs for predictions 
  subnat_locs<-get_location_metadata(101, release_id=release_id)$location_id
  
  l_set_v_id<-unique(locs$location_set_version_id)
  
  ################################################
  ################ GET COVARIATES ################
  ################################################
  
  # set up data set if it's 'all ages'
  if(length(unique(data$age_group_id))==1){
    if(unique(data$age_group_id)==22  | unique(data$age_group_id)==27){
      data[, age_group_id:=22]  ##sy: set to 22
      by_age<-0
    }else if(!(unique(data$age_group_id) %in% c(2:5, 388:389, 238, 34, 6:20, 30:32, 235, 164))){
      stop("Ask Will about incorporating custom age group")
    }else {
      by_age <- 1
    }
  }else{
    by_age<-1
  }
  
  # TODO: investigate this fn for potential issues introduced
  sqr <- make_square(location_set_id, release_id=release_id, year_start=head(years,n=1), year_end=tail(years,n=1), covariates=NA,by_age=by_age, by_sex=by_sex,
                     custom_age_group_id=pred_ages)
  
  ## get covariates
  sqr_and_names <- bind_covariates(sqr, cov_list=cov_list, custom_cov_list=custom_covs, release_id=release_id)
  sqr <- sqr_and_names[[1]] # this is the data w/ bound covariate estimates
  cov_list <- sqr_and_names[[2]] # these are the covariate_name_shorts
  
  # Create columns for the interaction terms and save the names of those columns in the 'interactions' vector
  if(!is.null(interaction_list)){
    interactions <- list()
    for(i in 1:length(interaction_list)){
      # This is a vector of covariate names
      interaction_covs <- interaction_list[[i]]
      interaction_name_short <- paste(interaction_covs, collapse = INTERACTION_DELIMITER)
      for (cov in interaction_covs){
        if(!cov %in% names(sqr)){
          message('Logging names(sqr)...')
          print(names(sqr))
          stop(paste("ATTEMPTING TO CREATE INTERACTION ", interaction_name_short, ", BUT MISSING: ", cov))
        }
      }
      sqr[, eval(interaction_name_short) := Reduce(`*`, lapply(interaction_covs, function(x) get(x)))]
      interactions[[i]] <- interaction_name_short
    }
    interactions <- unlist(interactions)
    cov_list <- c(cov_list, interactions)
  }
  
  message('Logging cov_list in assemble_prior...')
  print(cov_list)
  
  # Scale any/all covariates in cov_list, custom_covs, and interactions_names_list to have mean 0 and sd 1
  if (scale_covariates){
    message("Scaling covariates")
    for (cov in cov_list){
      data[, (cov) := scale(get(cov))]
    }
  }
  # if ("LDI_pc" %in% cov_list){
  #   message("logging LDI_pc for modeling")
  #   sqr$LDI_pc <- log(sqr$LDI_pc) # log LDI for modeling
  # }
  # if ("TFR" %in% cov_list){
  #   message("logging TFR for modeling")
  #   sqr$TFR <- log(sqr$TFR)
  # }
  # if("neonatal_modeled_per1000v2" %in% cov_list){
  #   message("logging neonatal mortality for modeling")
  #   sqr$neonatal_modeled_per1000v2 <- log(sqr$neonatal_modeled_per1000v2)
  # }
  # if ("HIV_mort_females_10_54" %in% cov_list){
  #   message("logging HIV mortality for modeling")
  #   sqr$HIV_mort_females_10_54 <- log(sqr$HIV_mort_females_10_54)
  # }
  # if ("Domestic Health Expenditure/Capita" %in% cov_list){
  #   message("logging Domestic Health Expenditure/Capita for modeling")
  #   sqr$domestic_he_cap <- log(sqr$domestic_he_cap + 1) # log LDI for modeling"Domestic Health Expenditure/Capita"
  # }
  
  if(by_sex==T){
    sex_list<-c("M", "F")
    if (!(1 %in% unique(data$sex_id))) sex_list<-c("F")
    if (!(1 %in% unique(data$sex_id))) sqr<-sqr[sex_id==2]
    if (!(2 %in% unique(data$sex_id))) sex_list<-c("M")
    if (!(2 %in% unique(data$sex_id))) sqr<-sqr[sex_id==1]
  }else{
    sex_list<-c("both_sexes")
  }
  
  #################################################
  ################ GET POLYNOMIALS ################
  #################################################
  
  # this creates a column in the dataset, and also saves the names of those columns in the 'polynoms' vector
  if(!is.null(polynoms)){
    polys<-strsplit(polynoms, "\\^")
    
    polynoms.t<-list()
    for(i in 1:length(polys)){
      basecov<-polys[[i]][1]
      if(!basecov %in% names(sqr)){ stop("ATTEMPTING TO CREATE POLYNOMIAL, MISSING: ", polys[[i]][1])}
      
      sqr[, paste0(basecov, polys[[i]][2]):=get(basecov)^as.numeric(polys[[i]][2])]
      polynoms.t[[i]]<-paste0(basecov, polys[[i]][2])
    }
    polynoms<-unlist(polynoms.t)
  }
  # add polynoms to cov_list
  og_cov_list<-cov_list
  cov_list<-c(cov_list, polynoms)
  
  # get locs
  sqr<-merge(sqr, locs[, .(location_name, region_name, super_region_name, location_id)], by="location_id")
  
  #############################################
  ################ LOOP BY SEX ################
  #############################################
  
  output<-list()
  for(sexchar in sex_list){
    # run models
    message("Model averaging for ", sexchar)
    s_id<-ifelse(sexchar=="both_sexes", 3, ifelse(sexchar=="M", 1, 2))
    
    sqr.s<-sqr[sex_id==s_id,]
    data.s<-data[sex_id==s_id,]
    data.s<-data.s[location_id %in% subnat_locs]
    
    rmses.s<-rmses[sex==sexchar]
    
    if(n_mods>nrow(rmses.s)){
      message("You specified to average over ", n_mods, " models for sex ", sexchar, ", but only supplied ", nrow(rmses.s), ". Only the supplied models will be averaged.")
      n_mods.s<-nrow(rmses.s)
    }else{
      n_mods.s<-n_mods
    }
    
    sqr.s <-sqr.s[age_group_id %in% pred_ages]
    
    ####################################################
    ################ CREATE PREDICTIONS ################
    ####################################################
    
    for(n in 1:n_mods.s){
      message("  Predicting for model ", n)
      
      # TODO: do all interaction vars (e.g. 'IFD_coverage_prop.sdi') exist in rmse.s? If not we need to manually add to rmses.s, etc.
      form<-rmses.s[n, covs]
      form<-paste0(paste0(data_transform,"(data)~", form))
      modtype<-ifelse(grepl("\\(1 \\|", form), "lmer", "lm")
      
      if(modtype=="lmer"){
        # try to fit the model. If 0 non-NA cases, skip
        if(nrow(data.s)==0){
          message("No non-NA cases for model ", n, ". Skipping.")
          next
        }
        
        # Perform scaling
        # TODO: untested. Seems like it should be unnecessary given data_transform() helper applied below, but lmer still throws warnings..
        if (scale_covariates == T){
          message("Scaling data for model ", n)
          for (cov in cov_list){
            if (cov %in% names(data.s)){
              data.s[, eval(cov):=scale(get(cov))]
            }
          }
        }
        
        mod<-lmer(as.formula(form), data=data.s)
      }
      if(modtype=="lm"){
        # try to fit the model. If 0 non-NA cases, skip
        if(nrow(data.s)==0){
          message("No non-NA cases for model ", n, ". Skipping.")
          next
        }
        mod<-lm(as.formula(form), data=data.s)
      }
      
      # predict on the square
      sqr.s[, paste0("pred", n):=transform_data(pred.lm(sqr.s, mod), data_transform, reverse=T)]
      sqr.s[, paste0("wt", n):=rmses.s[n, get(weight_col)]]
      
      rm(mod)
    }
    n<-n_mods.s
    
    #####################################################
    ################ AVERAGE PREDICTIONS ################
    #####################################################
    
    message("Averaging Predictions!")
    wts<-1/rmses.s[1:n, get(weight_col)]
    invisible(lapply(1:n, function(x){
      sqr.s[, paste0("numerator",x):=get(paste0("pred", x))*wts[x]]
    }))
    
    sqr.s[, ave_result:=rowSums(.SD)/sum(wts), .SDcols=grep("numerator", names(sqr.s), value=T)]
    
    # get age group id names for plotting to look nice
    message("Getting age group names..")
    age_ids<-get_ids(table="age_group")
    message("Done getting age group names")
    
    sqr.s<-merge(sqr.s, age_ids, by="age_group_id")
    
    saveRDS(sqr.s, file=file.path(plot_mods_path, paste0(sexchar, "_data.rds")), version=2)
    
    #####################################################
    ################ AVERAGE PREDICTIONS ################
    #####################################################
    if(plot_mods==T){
      
      param_map <- data.table(unique(sqr.s$location_id))
      setnames(param_map,"V1","location_id")
      param_map_filepath <- file.path(plot_mods_path, "params.csv")
      write.csv(param_map, param_map_filepath, row.names=F)
      
      # Setting slurm array params
      print(paste0("N rows in param_map: ", nrow(param_map)))
      print(paste0("N Concurrent Tasks Limit: ", CONCURRENT_TASKS_LIMIT))
      n_parallel <- min(nrow(param_map), CONCURRENT_TASKS_LIMIT)
      print(paste0("N parallel jobs: ", n_parallel))
      
      n_jobs <- paste0("1-", nrow(param_map),"%", n_parallel)
      
      command<-paste0("sbatch -p all.q -c 1 -t 00:05:00 --mem=2G", " -A ", proj, " -J ", paste0("average_", sexchar),
                      " -o /share/temp/sgeoutput/", username, "/output/%x.o%j -e /share/temp/sgeoutput/", username, "/errors/%x.o%j", 
                      " -a ", n_jobs, " /ihme/singularity-images/rstudio/shells/execRscript.sh -s ", file.path(STGPR_DIR, 'plot_mod_aves.R'), ' ', param_map_filepath, " ", sexchar, " ", plot_mods_path, " ", age_trend)
      print(paste0("Launching command: ", command))
      system(command)
      
      message("  Waiting on plots...")
      job_hold("average_")
      message("Done plotting")
      
      ## check for missingness
      files<-list.files(path=plot_mods_path, pattern="subplot")
      if(length(files)<length(unique(sqr.s$location_id))){
        message(paste0(length(unique(sqr.s$location_id))-length(files)), " output pdfs are missing!")
        Sys.sleep(6)
      }
      message("  Appending PDFs and deleting tempfiles")
      
      full_plot_output<-file.path(plot_mods_path, 
                                  paste0(sexchar, "_full_mod_ave_",
                                                         ifelse(age_trend==T, "agetrend", "timetrend"),
                                         ".pdf"))
      append_pdfs(plot_mods_path, pattern="subplot_", output=full_plot_output, rm=T)
      message("  Done appending/deleting")
      message("  Final plot output saved here: ", full_plot_output)
    }
    
    sqr.s[, c(grep("numerator|pred|wt", names(sqr.s), value=T), cov_list):=NULL]
    
    output[[length(output)+1]]<-sqr.s
    message("Done averaging for ", sexchar)
  }
  
  output<-rbindlist(output)
  return(output)
}