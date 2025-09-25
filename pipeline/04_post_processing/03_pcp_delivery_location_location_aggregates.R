#### Delivery Location Mapping: Country Aggregates Weighted by Births ####
### Chiara Sumich ###
### 01.02.2025 ###

#### Load packages
rm(list = ls())
pacman::p_load(data.table, ggplot2, gtools, tidyverse, haven, labelled, ini)
'%ni%' <- Negate('%in%')
library(readxl)
## Set Paths
date <- Sys.Date()
indir <- file.path("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/post_processing/scaled_data/")
outdir <- file.path("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/post_processing/aggregates") ##; 
# if (!dir.exists(outdir)) dir.create(outdir, recursive=TRUE)

if (interactive()) {
  me <- "prim_any" # "pub_hosp" "priv_hosp" "pub_prim" "priv_prim" "hosp_any"  ## me name or abbreviation for file paths
  VERSION <- "2025-07-14" ## version of scaled st-gpr results
} else {
  args <- commandArgs(trailingOnly = TRUE)
  me <- args[1]
  VERSION <- args[2]
}

## Source
release <- 33

source('/ihme/cc_resources/libraries/current/r/get_population.R')
source('/ihme/cc_resources/libraries/current/r/get_location_metadata.R')
source('/ihme/cc_resources/libraries/current/r/get_covariate_estimates.R')
source("/share/code/st_gpr/central/stgpr/r_functions/utilities/utility.r")
#source("/ihme/code/st_gpr/central/src/stgpr/api/public.R")

# read in births from covariate data base for super region aggregation
birth_dt <- get_covariate_estimates(covariate_id = 60, release_id = release)
birth_dt <- birth_dt[, c("location_id", "year_id", "mean_value")]
setnames(birth_dt, "mean_value", "births")
## we only need births from 1980 for the delivery location aggregates
birth_dt <- birth_dt %>%
  filter(year_id >= 1980)

# read in location data
locs <- get_location_metadata(release_id = release, location_set_id = 22)
#read in wb data to do an aggregation limited to LMICs
wb = read_excel("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/figures/mcconrad_deliver_location_paper_figures/CLASS.xlsx")
wb = wb %>% mutate(`Income group` = ifelse(Code == "VEN", "Upper middle income", `Income group`))

df <- read.csv(file.path(indir, paste0(me, "_st-gpr_results_scaled_", VERSION, ".csv")))

df <- merge(df, locs[, c("super_region_name", "level", "region_name", "location_id", "super_region_id", "region_id","parent_id","ihme_loc_id")], by=c("location_id"))

df = df %>% mutate(super_region_name = if_else(ihme_loc_id == "ARG","Latin America and Caribbean",super_region_name),
                   super_region_id = if_else(ihme_loc_id == "ARG", 103, super_region_id))
########################################################## for paper plugging only
df <- df %>% left_join(wb, by = c("ihme_loc_id" = "Code"))

#right now not doing kenya(179) and ethiopia (180)
df1 = df %>% filter(`Income group` != "High income" | parent_id %in% c(163, 214) | location_id %in% c(413,320,374)) %>% filter(ihme_loc_id != "CHN") ## limit to just LMICs: global now will be for all lmics

df = df1 %>% mutate(`Income group` = if_else(location_id == 413,"Upper middle income",`Income group`),
                   `Income group` = if_else(location_id == 320,"Upper middle income",`Income group`),
                   `Income group` = if_else(location_id == 374,"Upper middle income",`Income group`))

check = df %>% filter(location_id == 179)
#df_subnat <- df %>% filter((parent_id %in% c(163, 179, 214, 180) & level == 4))

# # #### Defining variables
# gbd_ifd_run_id <- 224626
# ndraws <- 1000
# n_draws<- ndraws-1
# draw_names <- paste0("draw_", 0:n_draws)
# 
# #### Read in the ST-GPR results
# gbd_ifd_draw_path <- paste0("/ihme/covariates/ubcov/model/output/", gbd_ifd_run_id, "/draws_temp_0/")
# gbd_ifd_files <- list.files(gbd_ifd_draw_path)
# fread_stgpr <- function(file, draw_names) {
#   dt <- fread(paste0(file))
#   #Create the mean, upper and lower using quantiles and then drop draws.
#   dt[, mean:= apply(.SD, 1, mean), .SDcols = draw_names]
#   dt[, lower:= apply(.SD, 1, quantile, c(0.025)), .SDcols = draw_names]
#   dt[, upper:= apply(.SD, 1, quantile, c(0.975)), .SDcols = draw_names]
#   ##  dt[, (draw_names) := NULL] #Comment out if you want to keep the draws
#   return(dt[])
# }
# gbd_ifd_results <- rbindlist(lapply(paste0(gbd_ifd_draw_path, gbd_ifd_files),
#                                     FUN = fread_stgpr,
#                                     draw_names = draw_names), use.names=TRUE)
# gbd_ifd_results <- dplyr::select(gbd_ifd_results, -mean,-lower,-upper)
# ## keeping only the variables we need to simplify things
# # gbd_ifd_results <- gbd_ifd_results %>%
# #   dplyr::select("year_id", "location_id", "sex_id", "age_group_id", "mean", "lower", "upper") %>%
# #   dplyr::rename(ifd_mean = "mean", 
# #                 ifd_lower = "lower", 
# #                 ifd_upper = "upper")
########################################################### 
draw_cols <- names(df)[grepl("draw_[0-9]*", names(df))]

## Aggregate estimates
aggregate_estimates <- function(df, vars) {
  # Convert df to a data.table
  setDT(df)
  # merge on level, super region name, region name
  # filter out draws that don't need to be aggregated (level >2)
  nonagg_draws <- copy(df)
  nonagg_draws <- nonagg_draws[level > 2 & location_id %ni% c(163,214,179)]
  agg <- copy(df)
  #only want to aggregate from level 3 locations 
  agg <- agg[level == 3 | (parent_id %in% c(163,214,179) & level == 4)]
  ## Merge on births
  agg <- merge(agg, birth_dt, by= c("location_id", "year_id") )
  agg[, (vars) := .SD * births, .SDcols = vars]

  global_draws <- agg[, lapply(.SD, sum), by = c("year_id", "age_group_id", "sex_id"), .SDcols = c(vars, "births")]
  super_region_draws <- agg[!is.na(super_region_name), lapply(.SD, sum), by = c("year_id", "age_group_id", "super_region_name", "super_region_id", "sex_id"), .SDcols = c(vars, "births")]
  region_draws <- agg[!is.na(region_name), lapply(.SD, sum), by = c("year_id", "age_group_id", "super_region_name", "region_name", "region_id", "sex_id"), .SDcols = c(vars, "births")]
  nat_draws <- agg[level == 4, lapply(.SD, sum), by = c("year_id", "age_group_id", "parent_id", "sex_id"), .SDcols = c(vars, "births")]

  
  global_draws[, (vars) := .SD / births, .SDcols = vars]
  super_region_draws[, (vars) := .SD / births, .SDcols = vars]
  region_draws[, (vars) := .SD / births, .SDcols = vars]
  nat_draws[, (vars) := .SD / births, .SDcols = vars]

  # Add back on location info
  global_draws[, location_id := 1]
  super_region_draws[, location_id := super_region_id]
  region_draws[, location_id := region_id]
  nat_draws[, location_id := parent_id]

  # Combine draws
  df_return <- rbindlist(list(global_draws, super_region_draws, region_draws, nat_draws,nonagg_draws), fill=T)
  df_return <- df_return %>% dplyr::select(-c(births, level, super_region_name, super_region_id, region_name, region_id, parent_id, ihme_loc_id))
  
  # if not among births, divide by in facility delivery to get among ifd
  # df_return_livebirths <- merge(
  #   df_return,
  #   gbd_ifd_results,
  #   by = c("location_id", "year_id", "age_group_id", "sex_id"),
  #   suffixes = c("", "_ifd")
  # )
  # draw_cols <- grep("^draw_[0-9]+$", names(df_return_livebirths), value = TRUE)
  # ifd_cols <- paste0(draw_cols, "_ifd")
  # 
  # df_return_livebirths[, (draw_cols) := Map(`/`, .SD, mget(ifd_cols)), .SDcols = draw_cols]
  # #drop _ifd columns
  
  # df_return_livebirths[, (ifd_cols) := NULL]
  # 
  # return(df_return_livebirths)
}

# get summary values
## Collapse draws
collapse.draws <- function(df) {
  cols <- grep("draw_", names(df), value=TRUE)
  df <- df[, mean_value := rowMeans(.SD), .SDcols=cols]
  df$lower_value <- apply(df[, (cols), with=F], 1, quantile, probs=0.025)
  df$upper_value <- apply(df[, (cols), with=F], 1, quantile, probs=0.975)
  df <- df[, -(cols), with=F]
  return(df)
}
df1 <- aggregate_estimates(df, draw_cols)
df1 = df1 %>% dplyr::select(-c(mean,lower,upper))
#check = df1 %>% filter(location_id == 179)
message("aggregating")
df_summary <- collapse.draws(df1)
#check = df_summary %>% filter(location_id == 214)
message("summarizing")

## merge country and super region/region aggregates all
df2 <- merge(df1, df_summary) %>%
#  mutate(true = case_when(mean == mean_value ~ 1,
#                          TRUE ~ 0)) %>%
# #select all the way to draw_999
# dplyr::select("year_id", "age_group_id", "sex_id", "location_id", "mean_value", "lower_value", "upper_value", # "true",
#               "draw_0", "draw_1", "draw_10", "draw_11", "draw_12", "draw_13", "draw_14", "draw_15", "draw_16", "draw_17",
#               "draw_18", "draw_19", "draw_2", "draw_20", "draw_21", "draw_22", "draw_23", "draw_24", "draw_25", "draw_26",
#               "draw_27", "draw_28", "draw_29", "draw_3", "draw_30", "draw_31", "draw_32", "draw_33", "draw_34", "draw_35",
#               "draw_36", "draw_37", "draw_38", "draw_39", "draw_4", "draw_40", "draw_41", "draw_42", "draw_43", "draw_44",
#               "draw_45", "draw_46", "draw_47", "draw_48", "draw_49", "draw_5", "draw_50", "draw_51", "draw_52", "draw_53",
#               "draw_54", "draw_55", "draw_56", "draw_57", "draw_58", "draw_59", "draw_6", "draw_60", "draw_61", "draw_62",
#               "draw_63", "draw_64", "draw_65", "draw_66", "draw_67", "draw_68", "draw_69", "draw_7", "draw_70", "draw_71",
#               "draw_72", "draw_73", "draw_74", "draw_75", "draw_76", "draw_77", "draw_78", "draw_79", "draw_8", "draw_80" ,
#               "draw_81", "draw_82", "draw_83", "draw_84", "draw_85", "draw_86", "draw_87", "draw_88", "draw_89", "draw_9",
#               "draw_90", "draw_91", "draw_92", "draw_93", "draw_94", "draw_95", "draw_96", "draw_97", "draw_98", "draw_99",
#               "") %>%
  #dplyr::select(-c("births","mean","lower","upper")) %>% #"super_region_id", "region_id", "super_region_name", "region_name", "level"
  dplyr::rename(mean = "mean_value", lower = "lower_value", upper = "upper_value")
## writing out weighted aggregates
message("writing out")
write_csv(df2, file.path(outdir, paste0(me, "_bothsteps_st-gpr_results_weighted_aggregates_lmics_", date, ".csv")))

