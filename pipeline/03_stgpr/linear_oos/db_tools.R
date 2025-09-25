###########################################################
### Author: Patrick Liu
### Date: 1/26/2015
### Project: ST-GPR
### Purpose: Database utility functions
###########################################################

###################
### Setting up ####
###################
library(data.table)
library(stringr)
library(plyr)
library(RMySQL)
library(parallel)
source("/ihme/cc_resources/libraries/current/r/get_age_metadata.R")
source("/ihme/cc_resources/libraries/current/r/get_ids.R")
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

####################################################################################################################################################
# 															   Table of Contents
####################################################################################################################################################

## Base
## query

## Pulls
## get_ages
## get_sexes
## get_location_hierarchy
## get_demographics
## get_covariate_metadata
## get_covariates

## Utilities
## detect_demographics
## age_sex_spec
## age_sex_merge
## get_names
## make_square


# ####################################################################################################################################################
# # 																	 Base
# ####################################################################################################################################################
# 
# query <- function(query,conn_def) {
#   odbc <- read.ini("/home/j/temp/central_comp/credentials/.odbc.ini")
#   conn <- dbConnect(RMySQL::MySQL(), 
#                     host = odbc[[conn_def]]$server, 
#                     username = odbc[[conn_def]]$user, 
#                     password = odbc[[conn_def]]$password)
#   dt <- dbGetQuery(conn,query) %>% data.table
#   dbDisconnect(conn)
#   return(dt)
# }

####################################################################################################################################################
# 																	 Pulls
####################################################################################################################################################

age.list <- get_age_metadata(age_group_set_id = 24, release_id = 16)

#####################################################################################################################################################

sex.list <- get_ids("sex")

#####################################################################################################################################################

get_location_hierarchy <- function(location_set_id = 22, release_id = 10, standard_location_set_id = 101) {
  
  #pull location hierarchy based on best location_set_version_id for given hierarchy and release_id
  df <- get_location_metadata(location_set_id, release_id=release_id)
  
  #pull standard location hierarchy
  std_locs <- get_location_metadata(standard_location_set_id, release_id=release_id)$location_id
  
  #assert existing location_set
  if(nrow(df) == 0){
    stop("Locations dataframe is empty! Make sure your location_set_id and release_id are legit.")
  }
  
  ## Create hierarchy
  hierarchy <- str_split_fixed(df$path_to_top_parent, ",", max(df$level) + 1) %>% data.table
  hierarchy <- hierarchy[, lapply(.SD, as.numeric)]
  setnames(hierarchy, names(hierarchy), paste0("level_", seq(0, max(df$level))))
  df <- cbind(df, hierarchy)
  
  #Create indicator column for standard locations, using the janky setup from GBD2017 for all rounds before GBD2019
  df[, standard_location:=ifelse(location_id %in% std_locs, 1, 0)]
  
  return(df)
}

#####################################################################################################################################################

get_demographics <- function(location_set_id,
                             release_id=16,
                             year_start, year_end,
                             by_sex=1, by_age=1,
                             custom_sex_id=NULL, custom_age_group_id=NULL) {
  ## Locations
  locs <- get_location_hierarchy(location_set_id, release_id)[level >= 3]$location_id
  ## Years
  years <- seq(year_start, year_end,1)
  ## Sexes
  if (is.blank(custom_sex_id)) {
    if (by_sex == 1) {
      sexes <- c(1,2)
    } else if (by_sex == 0) {
      sexes <- 3
    }
  } else {
    sexes <- custom_sex_id
  }
  ## Ages
  if (is.blank(custom_age_group_id)) {
    if (by_age==1) {
      ages <- age.list$age_group_id
    } else if (by_age==0) {
      ages <- 22
    }
  } else {
    ages <- custom_age_group_id
  }
  
  ## Expand
  df <- data.table(expand.grid(location_id = locs, year_id=years, sex_id=sexes, age_group_id=ages))
  ## Force integer
  df <- df[, lapply(.SD, as.character)]
  df <- df[, lapply(.SD, as.integer)]
  
  return(df)
}

#####################################################################################################################################################

get_covariate_metadata <- function(list=NULL) {
  ## Where clause
  if (is.blank(list)) {
    where <- ""
  } else {
    where <- paste0("WHERE LOWER(covariate_name_short) in (", tolower(toString(shQuote(list))), ")")
  }
  
  ## Pull
  host <- "cod"
  q <- paste0("SELECT * FROM shared.covariate ", where)
  df <- query(q, host)
  
  ## Convert names to lowercase
  df$covariate_name_short <- tolower(df$covariate_name_short)
  
  ## Throw an error check if not int output
  if (!is.blank(list)) {
    return <- df$covariate_name_short
    if (length(list[which(!list %in% return)]) > 0) {
      stop(paste0("The following covariates do not exist in the db: ", toString(list[which(!list %in% return)])))
    }
  }
  return(df)
}

#####################################################################################################################################################

pull_covariate <- function(cov, ci=FALSE, covariate_name_short, release_id) {
  
  source('/ihme/cc_resources/libraries/current/r/get_covariate_estimates.R')
  
  data<-get_covariate_estimates(covariate_id = cov, location_set_id = location_set_id, release_id = release_id)
  data<-data[,.(location_id, year_id, age_group_id, sex_id, mean_value)]
  setnames(data, "mean_value", paste0(covariate_name_short))
  
  
  return(data)
  
}

#####################################################################################################################################################

get_covariates <- function(list, location_set_id, prediction_years, ci=FALSE, release_id = 10) {
  
  ## Get metadata
  meta <- get_covariate_metadata(list)
  
  ## Parse into age/sex specific, age specific, sex specific
  meta <- data.table(meta)
  meta <- meta[, .(covariate_id, covariate_name_short, by_sex, by_age)]
  age_sex <- meta[by_sex == 1 & by_age == 1, .(covariate_id, covariate_name_short)]
  age <- meta[by_sex == 0 & by_age==1, .(covariate_id, covariate_name_short)]
  sex <- meta[by_sex == 1 & by_age==0, .(covariate_id, covariate_name_short)]
  all <- meta[by_sex == 0 & by_age==0, .(covariate_id, covariate_name_short)]
  
  ## Reorder from the most specific to most general
  frame <- unique(rbind(age_sex, age, sex, all)[, .(covariate_id, covariate_name_short)])
  #list <- unique(rbind(age_sex, age, sex, all)$covariate_name_short)
  
  # Pull and Merge Covariates
  flag <- 0
  output <- NULL
  for (cov in frame$covariate_id) {
    
    cov_name_short <- frame[cov == covariate_id, covariate_name_short]
    data <- pull_covariate(cov, ci, covariate_name_short = cov_name_short, release_id = release_id)
    
    #make sure covariate has estimates for all prediction years
    if(!all(prediction_years %in% unique(data$year_id))){
      stop(sprintf('Covariate %s does not have estimates for all years between %i and %i!',
                   cov_name_short, prediction_years[1], prediction_years[length(prediction_years)]))
    }
    
    if (flag == 0) {
      output <- data
      flag <- 1
    } else {
      output <- age_sex_merge(output, data)
    }
  }
  
  return(output)
}

####################################################################################################################################################
# 																	 Utility
####################################################################################################################################################

is.blank <- function(x) {
  any(is.null(x))  || any(is.na(x))  || any(is.nan(x)) 
}

#####################################################################################################################################################


detect_demographics <- function(df) {
  vars <- c("location_id", "year_id", "age_group_id", "sex_id")
  ## Check if data frame has id's and only return on those that exist
  vars <- names(df)[names(df) %in% vars]
  demos <- lapply(vars, function(x) unique(df[[x]]))
  names(demos) <- vars
  return(demos)
}

#####################################################################################################################################################


age_sex_spec <- function(df) {	
  demos <- detect_demographics(df)
  by_age <- ifelse(((22 %in% demos$age_group_id) | (27 %in% demos$age_group_id)), 0, 1)
  by_sex <- ifelse(3 %in% demos$sex_id, 0 , 1)
  spec <- cbind(by_age, by_sex) %>% data.table
  return(spec)
}

#####################################################################################################################################################

age_sex_merge <- function(df1, df2) {
  
  ## Find specificity
  spec1 <- age_sex_spec(df1)
  spec2 <- age_sex_spec(df2)
  test <- data.table(spec1 == spec2)
  
  ## Merge
  cols <- c("location_id", "year_id", "age_group_id", "sex_id")
  drop_cols <- NULL
  merge_cols <- col
  
  ## If age and sex match
  if (test$by_age & test$by_sex) {
    df <- merge(df1, df2, by=cols)
  } else {
    ## If age matches but sex doesn't match
    if (test$by_age & !test$by_sex) {
      drop_cols <- "sex_id"
    }	
    ## If age doesnt match and sex matches
    else if (!test$by_age & test$by_sex) {
      drop_cols <- "age_group_id"
    }
    ## If neither match
    else {
      drop_cols <- c("sex_id", "age_group_id")
    }
    ## Merge
    merge_cols <- cols[!cols %in% drop_cols]
    df <- merge(df1, df2[, drop_cols := NULL, with=F], by=merge_cols)
  }
  return(df)
}

#####################################################################################################################################################

get_names <- function(df) {
  
  ## Given a data.table with location, age_group, sex, merge on namescu
  required <- c("location_id", "age_group_id", "sex_id") 
  missing <- required[!required %in% names(df)]
  if (length(missing) > 0) stop(paste0("Missing required columns: ", toString(required)))
  
  ## Detect what needs names
  cols <- c("ihme_loc_id", "location_name", "age_group_name", "sex")
  need <- cols[!cols %in% names(df)]
  
  ## Names
  if ("ihme_loc_id" %in% need) df <- merge(df, get_location_hierarchy(41)[,.(location_id, ihme_loc_id)], by="location_id", all.x=T)
  if ("location_name" %in% need) df <- merge(df, get_location_hierarchy(41)[,.(location_id, location_name)], by="location_id", all.x=T)
  if ("age_group_name" %in% need) df <- merge(df, age.list[,.(age_group_id, age_group_name)], by="age_group_id", all.x=T)
  if ("sex" %in% need) df <- merge(df, sex.list[,.(sex_id, sex)], by="sex_id", all.x=T)
  
  return(df)
}

#####################################################################################################################################################

make_square <- function(location_set_id, release_id,
                        year_start, year_end,
                        by_sex=1, by_age=1,
                        custom_sex_id=NULL, custom_age_group_id=NULL,
                        covariates=NULL, population=FALSE) {	
  
  ## Skeleton
  df <- get_demographics(location_set_id, release_id, 
                         year_start, year_end, 
                         by_sex, by_age, 
                         custom_sex_id, custom_age_group_id)
  
  ## Covariates
  prediction_years <- seq(year_start, year_end)
  
  if (!is.null(covariates) & !is.na(covariates)) {
    covs <- get_covariates(list = covariates, location_set_id = location_set_id, release_id = release_id, prediction_years = prediction_years)
    df <- age_sex_merge(df, covs)
  }
  
  return(df)
  
}


####################################################################################################################################################
####################################################################################################################################################

# ## 4749, 44533 fix to create if missing
janky.covariate.fix <- function(df) {
  
  locs <-unique(df$location_id)
  
  childs 	<- c(4749, 44533) 
  parents	<- c(95, 6)
  
  for (i in 1:length(childs)) {
    child <- childs[[i]]
    parent <- parents[[i]]
    if (!(child %in% locs)) {
      df.parent <- df[location_id==parent]
      df.parent <- df.parent[, location_id := child]
      df <- rbind(df, df.parent)
    }
  }
  
  return(df)
}

china.hierarchy.fix <- function(df) {
  
  ## Reroute China subnationals to level 4, make CHN w/o HKG and MAC, HKG, MAC level 3
  ## Remove China
  df <- df[location_id != 6]
  ## Remove from path to top parent
  df <- df[, path_to_top_parent := gsub(",6,", ",", path_to_top_parent)]
  ## Reroute 44533, 354, 361 to level 3
  df <- df[location_id %in% c(44533, 354, 361), level := 3]
  ## Reroute the other subnationals to level 4
  df <- df[grepl("CHN", df$ihme_loc_id) & level == 5, level := 4]
  
  return(df)
  
}

get.subnat.locs.and.lvls <- function(location_set_id, release_id, get.subnats = F, national_level = 3){
  
  ids <- c("all_subnat_locs","lvl6_subnat_locs", "lvl5_subnat_locs", "lvl4_subnat_locs")
  
  df <- get_location_hierarchy(location_set_id, release_id = release_id)
  
  #set max level observed
  max_level <- df[, level] %>% max
  df <- df[level >= national_level]
  df[, c("level_0"):=NULL]
  
  #get max level by grabbing level-3 location_id of locs at subnat levels (>lvl4)
  grps <- c()
  all_subnat_locs <- c()
  if(max_level == national_level){
    for (x in ids) assign(x, NA)
    grps <- ids
  } else{
    for(n in seq(max_level,4, -1)){
      assign("lvl", paste0("level_", n))
      out <- paste0("lvl", n, "_subnat_locs")
      
      assign(out, df[!is.na(get(lvl)), str_split_fixed(path_to_top_parent, ",", max_level+1)[, 4]] %>% unique %>% as.numeric)
      
      #make sure that sets are disjoint (ie only loc_ids with MAX lvl6 in lvl6_subnat_locs)
      if(n < max_level){
        for (j in c(1:(max_level - n))){
          assign(out, get(out)[!(get(out) %in% get(paste0("lvl", n+j, "_subnat_locs")))])
        } #this is an excellent moment for a jojo reference...
      }
      
      #add lists to a full vector so we can iterate through + grab subnats
      grps <- append(out, grps)
      
    }
    
    #append all disjoint subnat levels together for a full subnat grouping
    all_subnat_locs <- c()
    for(grp in grps){
      all_subnat_locs <- dplyr::union(all_subnat_locs, get(grp))
    }
    
    #make sure all outputs exist, even if those levels not in location hierarchy
    grps <- append(grps, "all_subnat_locs")
    for(item in ids){
      #if(exception_locs == item){
      if(!(item %in% grps)) assign(item, NA)
    }
  }
  
  #finally, return subnats (either to global environment or to a named list)
  if(get.subnats==F){
    for(item in ids){
      #if(exception_locs == item){
      if(!(item %in% grps)) assign(item, NA)
      assign(item, get(item), envir = globalenv())
    }
  } else{
    sublocs <- list(all_subnat_locs = all_subnat_locs, lvl4_subnat_locs = lvl4_subnat_locs, lvl5_subnat_locs = lvl5_subnat_locs, lvl6_subnat_locs = lvl6_subnat_locs)
    return(sublocs)
  }
  
  
}