#### Scaling st-gpr results for delivery locations ####
### Chiara Sumich ###
### 11.12.2024 ###

#### Load packages
rm(list = ls())
pacman::p_load(data.table, ggplot2, gtools, tidyverse, haven, labelled)

VERSION <- Sys.Date()
vers_congo <- "2025-07-11" ## congo rescale version

####Scaling hospital any sector and primary any sector to ifd envelope ####
## Pulling hosp_any and prim_any st-gpr results
#### Defining variables
message("hosp and prim any")
hosp_any_run_id <- 224715 # 224804 eth only
prim_any_run_id <- 224712 # 224807 eth only
gbd_ifd_run_id <- 224626
congo = 170
ndraws <- 1000
n_draws<- ndraws-1

#### Read in the ST-GPR results
hosp_any_draw_path <- paste0("/ihme/covariates/ubcov/model/output/", hosp_any_run_id, "/draws_temp_0/")
prim_any_draw_path <- paste0("/ihme/covariates/ubcov/model/output/", prim_any_run_id, "/draws_temp_0/")
gbd_ifd_draw_path <- paste0("/ihme/covariates/ubcov/model/output/", gbd_ifd_run_id, "/draws_temp_0/")

draw_names <- paste0("draw_", 0:n_draws)

hosp_any_files <- list.files(hosp_any_draw_path)
prim_any_files <- list.files(prim_any_draw_path)
gbd_ifd_files <- list.files(gbd_ifd_draw_path)

fread_stgpr <- function(file, draw_names) {
  dt <- fread(paste0(file))
  #Create the mean, upper and lower using quantiles and then drop draws. 
  dt[, mean:= apply(.SD, 1, mean), .SDcols = draw_names]
  dt[, lower:= apply(.SD, 1, quantile, c(0.025)), .SDcols = draw_names]
  dt[, upper:= apply(.SD, 1, quantile, c(0.975)), .SDcols = draw_names]
##  dt[, (draw_names) := NULL] #Comment out if you want to keep the draws
  return(dt[])
}

hosp_any_results <- rbindlist(lapply(paste0(hosp_any_draw_path, hosp_any_files),
                                     FUN = fread_stgpr, 
                                     draw_names = draw_names), use.names=TRUE) %>%
  mutate(me_id = "hosp_any")
hosp_any_mean = hosp_any_results %>% dplyr::select(year_id,location_id,mean) %>% dplyr::rename(mean_old = mean)

prim_any_results <- rbindlist(lapply(paste0(prim_any_draw_path, prim_any_files),
                                     FUN = fread_stgpr, 
                                     draw_names = draw_names), use.names=TRUE) %>%
  mutate(me_id = "prim_any")
prim_any_mean = prim_any_results %>% dplyr::select(year_id,location_id,mean) %>% dplyr::rename(mean_old = mean)

gbd_ifd_results <- rbindlist(lapply(paste0(gbd_ifd_draw_path, gbd_ifd_files),
                                     FUN = fread_stgpr, 
                                     draw_names = draw_names), use.names=TRUE) %>%
  mutate(me_id = "gbd_ifd")
message("done reading in data")
## selecting just the first 100 draws until the delivery location models are finalized
# gbd_ifd_results <- gbd_ifd_results %>%
#   dplyr::select("year_id", "location_id", "sex_id", "age_group_id", "draw_0", "draw_1", "draw_2", "draw_3", "draw_4",
#          "draw_5", "draw_6", "draw_7", "draw_8", "draw_9", "draw_10", "draw_11", "draw_12", "draw_13", "draw_14",
#          "draw_15", "draw_16", "draw_17", "draw_18", "draw_19", "draw_20", "draw_21", "draw_22", "draw_23",
#          "draw_24", "draw_25", "draw_26", "draw_27", "draw_28", "draw_29", "draw_30", "draw_31", "draw_32",
#          "draw_33", "draw_34", "draw_35", "draw_36", "draw_37", "draw_38", "draw_39", "draw_40", "draw_41",
#          "draw_42", "draw_43", "draw_44", "draw_45", "draw_46", "draw_47", "draw_48", "draw_49", "draw_50",
#          "draw_51", "draw_52", "draw_53", "draw_54", "draw_55", "draw_56", "draw_57", "draw_58", "draw_59",
#          "draw_60", "draw_61", "draw_62", "draw_63", "draw_64", "draw_65", "draw_66", "draw_67", "draw_68",
#          "draw_69", "draw_70", "draw_71", "draw_72", "draw_73", "draw_74", "draw_75", "draw_76", "draw_77",
#          "draw_78", "draw_79", "draw_80", "draw_81", "draw_82", "draw_83", "draw_84", "draw_85", "draw_86",
#          "draw_87", "draw_88", "draw_89", "draw_90", "draw_91", "draw_92", "draw_93", "draw_94", "draw_95",
#          "draw_96", "draw_97", "draw_98", "draw_99", "mean", "lower", "upper", "me_id")




#Pivot all data to long with draws, i.e.
hosp_any_results <- pivot_longer(hosp_any_results, cols = starts_with("draw_"), names_to = "draw", values_to = "val")
prim_any_results <- pivot_longer(prim_any_results, cols = starts_with("draw_"), names_to = "draw", values_to = "val")
gbd_ifd_results <- pivot_longer(gbd_ifd_results, cols = starts_with("draw_"), ## 1004 when we use the full 1000 draws
                                names_to = "draw", values_to = "val")

#Rename key col for each of the datasets so it's identifiable 
hosp_any_results <- setnames(hosp_any_results, "val", "val_hosp_any")
prim_any_results <- setnames(prim_any_results, "val", "val_prim_any")
gbd_ifd_results <- setnames(gbd_ifd_results, "val", "val_gbd_ifd")

#Merge all datasets together, i.e.
message("merging data")
df_combined <- merge(hosp_any_results, prim_any_results, by = c("age_group_id", "location_id", "sex_id", "year_id", "draw"))
df_combined <- merge(df_combined, gbd_ifd_results, by = c("age_group_id", "location_id", "sex_id", "year_id", "draw"))
message("done merging data")
#Scale vals
df_combined <- as.data.table(df_combined)
df_combined <- df_combined[, val_sum := val_hosp_any+val_prim_any]
df_combined <- df_combined[, scale_factor := val_gbd_ifd/val_sum]

#Remake each indicator, i.e.
df_combined <- df_combined[, hosp_any_scaled := val_hosp_any * scale_factor]
df_combined <- df_combined[, prim_any_scaled := val_prim_any * scale_factor]

#Split back out and pivot back to wide, i.e
hosp_any_results <- subset(df_combined, select = c(age_group_id, location_id, sex_id, year_id, draw, hosp_any_scaled))
hosp_any_results <- setnames(hosp_any_results, "hosp_any_scaled", "val")
hosp_any_results <- pivot_wider(hosp_any_results, names_from = "draw", values_from = "val")

prim_any_results <- subset(df_combined, select = c(age_group_id, location_id, sex_id, year_id, draw, prim_any_scaled))
prim_any_results <- setnames(prim_any_results, "prim_any_scaled", "val")
prim_any_results <- pivot_wider(prim_any_results, names_from = "draw", values_from = "val")

#Summarize draws from here to get CIs
#Create the mean, upper and lower using quantiles  
hosp_any_results <- as.data.table(hosp_any_results)
hosp_any_results[, mean:= apply(.SD, 1, mean), .SDcols = draw_names]
hosp_any_results[, lower:= apply(.SD, 1, quantile, c(0.025)), .SDcols = draw_names]
hosp_any_results[, upper:= apply(.SD, 1, quantile, c(0.975)), .SDcols = draw_names]

prim_any_results <- as.data.table(prim_any_results)
prim_any_results[, mean:= apply(.SD, 1, mean), .SDcols = draw_names]
prim_any_results[, lower:= apply(.SD, 1, quantile, c(0.025)), .SDcols = draw_names]
prim_any_results[, upper:= apply(.SD, 1, quantile, c(0.975)), .SDcols = draw_names]

#congo
#remove congo data, read in other scaled congo data
hosp_any_results <- subset(hosp_any_results, location_id != congo)
rescaled_congo <-read_csv(paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/post_processing/scaled_data/congo_hosp_any_scaled_", vers_congo, ".csv"))
rescaled_congo <- rescaled_congo %>% dplyr::select(-me_id)
hosp_any_results <- rbind(hosp_any_results, rescaled_congo)

prim_any_results <- subset(prim_any_results, location_id != congo)
rescaled_congo <-read_csv(paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/post_processing/scaled_data/congo_prim_any_scaled_", vers_congo, ".csv"))
rescaled_congo <- rescaled_congo %>% dplyr::select(-me_id)
prim_any_results <- rbind(prim_any_results, rescaled_congo)


prim_any_mean_new = prim_any_results %>% dplyr::select(year_id,location_id,mean) %>% dplyr::rename(mean_new = mean)
hosp_any_mean_new = hosp_any_results %>% dplyr::select(year_id,location_id,mean) %>% dplyr::rename(mean_new = mean)
#combine old and new
prim_any_mean = merge(prim_any_mean, prim_any_mean_new, by = c("year_id", "location_id"))
hosp_any_mean = merge(hosp_any_mean, hosp_any_mean_new, by = c("year_id", "location_id"))
#calculate difference
prim_any_mean = prim_any_mean %>% mutate(diff = mean_new - mean_old)
hosp_any_mean = hosp_any_mean %>% mutate(diff = mean_new - mean_old)
#save differences
message("writing out differences")
write_csv(prim_any_mean, paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/post_processing/scaled_data/prim_any_mean_diff_", VERSION, ".csv"))
write_csv(hosp_any_mean, paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/post_processing/scaled_data/hosp_any_mean_diff_", VERSION, ".csv"))

message("writing out scaled results")
##saving scaled results
write_csv(hosp_any_results, paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/post_processing/scaled_data/hosp_any_st-gpr_results_scaled_", VERSION, ".csv"))
write_csv(prim_any_results, paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/post_processing/scaled_data/prim_any_st-gpr_results_scaled_", VERSION, ".csv"))

####Scaling pub_hosp and priv_hosp to hospital any sector envelope ####
## Pulling pub_hosp and priv_hosp st-gpr results
#### Defining variables
message("pub and priv hosp")
pub_hosp_run_id <- 225379  ##'[jklusty: updated version with Brazil SIH admin data]
priv_hosp_run_id <- 224710 # 224809 eth only



#### Read in the ST-GPR results
#hosp_any_results = read_csv("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/mcconrad_post_gpr/scaled data/hosp_any_st-gpr_results_scaled_2025-05-30.csv")
pub_hosp_draw_path <- paste0("/ihme/covariates/ubcov/model/output/", pub_hosp_run_id, "/draws_temp_0/")
priv_hosp_draw_path <- paste0("/ihme/covariates/ubcov/model/output/", priv_hosp_run_id, "/draws_temp_0/")

pub_hosp_files <- list.files(pub_hosp_draw_path)
priv_hosp_files <- list.files(priv_hosp_draw_path)

pub_hosp_results <- rbindlist(lapply(paste0(pub_hosp_draw_path, pub_hosp_files),
                                     FUN = fread_stgpr, 
                                     draw_names = draw_names), use.names=TRUE) %>%
  mutate(me_id = "pub_hosp")
pub_hosp_mean = pub_hosp_results %>% dplyr::select(year_id,location_id,mean) %>% dplyr::rename(mean_old = mean)
priv_hosp_results <- rbindlist(lapply(paste0(priv_hosp_draw_path, priv_hosp_files),
                                     FUN = fread_stgpr, 
                                     draw_names = draw_names), use.names=TRUE) %>%
  mutate(me_id = "priv_hosp")
priv_hosp_mean = priv_hosp_results %>% dplyr::select(year_id,location_id,mean) %>% dplyr::rename(mean_old = mean)
#Pivot all data to long with draws, i.e.
pub_hosp_results <- pivot_longer(pub_hosp_results, cols = starts_with("draw_"), names_to = "draw", values_to = "val")
priv_hosp_results <- pivot_longer(priv_hosp_results, cols = starts_with("draw_"), names_to = "draw", values_to = "val")
hosp_any_results <- pivot_longer(hosp_any_results, cols = starts_with("draw_"), names_to = "draw", values_to = "val")

#Rename key col for each of the datasets so it's identifiable 
pub_hosp_results <- setnames(pub_hosp_results, "val", "val_pub_hosp")
priv_hosp_results <- setnames(priv_hosp_results, "val", "val_priv_hosp")
hosp_any_results <- setnames(hosp_any_results, "val", "val_hosp_any")


#Merge all datasets together, i.e.
df_combined <- merge(pub_hosp_results, priv_hosp_results, by = c("age_group_id", "location_id", "sex_id", "year_id", "draw"))
df_combined <- merge(df_combined, hosp_any_results, by = c("age_group_id", "location_id", "sex_id", "year_id", "draw"))


#Scale vals
df_combined <- as.data.table(df_combined)
df_combined <- df_combined[, val_sum := val_pub_hosp+val_priv_hosp]
df_combined[, scale_factor := val_hosp_any / val_sum]         # normal envelope scaling

#Remake each indicator, i.e.
df_combined <- df_combined[, priv_hosp_scaled := ifelse(location_id == 135, val_hosp_any - val_pub_hosp, val_priv_hosp * scale_factor)]
df_combined <- df_combined[, pub_hosp_scaled := ifelse(location_id == 130 | location_id == 135,
                                                       val_hosp_any - priv_hosp_scaled, val_pub_hosp * scale_factor)]


#Split back out and pivot back to wide, i.e
pub_hosp_results <- subset(df_combined, select = c(age_group_id, location_id, sex_id, year_id, draw, pub_hosp_scaled))
pub_hosp_results <- setnames(pub_hosp_results, "pub_hosp_scaled", "val")
pub_hosp_results <- pivot_wider(pub_hosp_results, names_from = "draw", values_from = "val")

priv_hosp_results <- subset(df_combined, select = c(age_group_id, location_id, sex_id, year_id, draw, priv_hosp_scaled))
priv_hosp_results <- setnames(priv_hosp_results, "priv_hosp_scaled", "val")
priv_hosp_results <- pivot_wider(priv_hosp_results, names_from = "draw", values_from = "val")

#Summarize draws from here to get CIs
#Create the mean, upper and lower using quantiles  
pub_hosp_results <- as.data.table(pub_hosp_results)
pub_hosp_results[, mean:= apply(.SD, 1, mean), .SDcols = draw_names]
pub_hosp_results[, lower:= apply(.SD, 1, quantile, c(0.025)), .SDcols = draw_names]
pub_hosp_results[, upper:= apply(.SD, 1, quantile, c(0.975)), .SDcols = draw_names]

priv_hosp_results <- as.data.table(priv_hosp_results)
priv_hosp_results[, mean:= apply(.SD, 1, mean), .SDcols = draw_names]
priv_hosp_results[, lower:= apply(.SD, 1, quantile, c(0.025)), .SDcols = draw_names]
priv_hosp_results[, upper:= apply(.SD, 1, quantile, c(0.975)), .SDcols = draw_names]

#remove congo data, read in other scaled congo data
pub_hosp_results <- subset(pub_hosp_results, location_id != congo)
rescaled_congo <-read_csv(paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/post_processing/scaled_data/congo_pub_hosp_scaled_", vers_congo, ".csv"))
rescaled_congo <- rescaled_congo %>% dplyr::select(-me_id)
pub_hosp_results <- rbind(pub_hosp_results, rescaled_congo)

priv_hosp_results <- subset(priv_hosp_results, location_id != congo)
rescaled_congo <-read_csv(paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/post_processing/scaled_data/congo_priv_hosp_scaled_", vers_congo, ".csv"))
rescaled_congo <- rescaled_congo %>% dplyr::select(-me_id)
priv_hosp_results <- rbind(priv_hosp_results, rescaled_congo)

pub_hosp_mean_new = pub_hosp_results %>% dplyr::select(year_id,location_id,mean) %>% dplyr::rename(mean_new = mean)
priv_hosp_mean_new = priv_hosp_results %>% dplyr::select(year_id,location_id,mean) %>% dplyr::rename(mean_new = mean)
#combine old and new
pub_hosp_mean = merge(pub_hosp_mean, pub_hosp_mean_new, by = c("year_id", "location_id"))
priv_hosp_mean = merge(priv_hosp_mean, priv_hosp_mean_new, by = c("year_id", "location_id"))

#calculate difference
pub_hosp_mean = pub_hosp_mean %>% mutate(diff = mean_new - mean_old)
priv_hosp_mean = priv_hosp_mean %>% mutate(diff = mean_new - mean_old)
#save differences
write_csv(pub_hosp_mean, paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/post_processing/scaled_data/pub_hosp_mean_diff_", VERSION, ".csv"))
write_csv(priv_hosp_mean, paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/post_processing/scaled_data/priv_hosp_mean_diff_", VERSION, ".csv"))
##saving scaled results
write_csv(pub_hosp_results, paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/post_processing/scaled_data/pub_hosp_st-gpr_results_scaled_", VERSION, ".csv"))
write_csv(priv_hosp_results, paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/post_processing/scaled_data/priv_hosp_st-gpr_results_scaled_", VERSION, ".csv"))

####Scaling pub_prim and priv_prim to hospital any sector envelope ####
## Pulling pub_prim and priv_prim st-gpr results
#### Defining variables
message("pub and priv prim")
pub_prim_run_id <- 224711 # 224808 eth only
priv_prim_run_id <- 224714 # 224805 eth only


#### Read in the ST-GPR results
pub_prim_draw_path <- paste0("/ihme/covariates/ubcov/model/output/", pub_prim_run_id, "/draws_temp_0/")
priv_prim_draw_path <- paste0("/ihme/covariates/ubcov/model/output/", priv_prim_run_id, "/draws_temp_0/")

pub_prim_files <- list.files(pub_prim_draw_path)
priv_prim_files <- list.files(priv_prim_draw_path)

pub_prim_results <- rbindlist(lapply(paste0(pub_prim_draw_path, pub_prim_files),
                                     FUN = fread_stgpr, 
                                     draw_names = draw_names), use.names=TRUE) %>%
  mutate(me_id = "pub_prim")
pub_prim_mean = pub_prim_results %>% dplyr::select(year_id,location_id,mean) %>% dplyr::rename(mean_old = mean)
priv_prim_results <- rbindlist(lapply(paste0(priv_prim_draw_path, priv_prim_files),
                                      FUN = fread_stgpr, 
                                      draw_names = draw_names), use.names=TRUE) %>%
  mutate(me_id = "priv_prim")
priv_prim_mean = priv_prim_results %>% dplyr::select(year_id,location_id,mean) %>% dplyr::rename(mean_old = mean)
#Pivot all data to long with draws, i.e.
pub_prim_results <- pivot_longer(pub_prim_results, cols = starts_with("draw_"), names_to = "draw", values_to = "val")
priv_prim_results <- pivot_longer(priv_prim_results, cols = starts_with("draw_"), names_to = "draw", values_to = "val")
prim_any_results <- pivot_longer(prim_any_results, cols = starts_with("draw_"), names_to = "draw", values_to = "val")

#Rename key col for each of the datasets so it's identifiable 
pub_prim_results <- setnames(pub_prim_results, "val", "val_pub_prim")
priv_prim_results <- setnames(priv_prim_results, "val", "val_priv_prim")
prim_any_results <- setnames(prim_any_results, "val", "val_prim_any")

#Merge all datasets together, i.e.
df_combined <- merge(pub_prim_results, priv_prim_results, by = c("age_group_id", "location_id", "sex_id", "year_id", "draw"))
df_combined <- merge(df_combined, prim_any_results, by = c("age_group_id", "location_id", "sex_id", "year_id", "draw"))
#Scale vals
df_combined <- as.data.table(df_combined)
df_combined <- df_combined[, val_sum := val_pub_prim+val_priv_prim]
df_combined[, scale_factor :=           # direct scaling to IFD
                                      val_prim_any / val_sum]         # normal envelope scaling

#Remake each indicator, i.e.
df_combined <- df_combined[, pub_prim_scaled := val_pub_prim * scale_factor]
df_combined <- df_combined[, priv_prim_scaled := val_priv_prim * scale_factor]


#Split back out and pivot back to wide, i.e
pub_prim_results <- subset(df_combined, select = c(age_group_id, location_id, sex_id, year_id, draw, pub_prim_scaled))
pub_prim_results <- setnames(pub_prim_results, "pub_prim_scaled", "val")
pub_prim_results <- pivot_wider(pub_prim_results, names_from = "draw", values_from = "val")

priv_prim_results <- subset(df_combined, select = c(age_group_id, location_id, sex_id, year_id, draw, priv_prim_scaled))
priv_prim_results <- setnames(priv_prim_results, "priv_prim_scaled", "val")
priv_prim_results <- pivot_wider(priv_prim_results, names_from = "draw", values_from = "val")

#Summarize draws from here to get CIs
#Create the mean, upper and lower using quantiles  
pub_prim_results <- as.data.table(pub_prim_results)
pub_prim_results[, mean:= apply(.SD, 1, mean), .SDcols = draw_names]
pub_prim_results[, lower:= apply(.SD, 1, quantile, c(0.025)), .SDcols = draw_names]
pub_prim_results[, upper:= apply(.SD, 1, quantile, c(0.975)), .SDcols = draw_names]

priv_prim_results <- as.data.table(priv_prim_results)
priv_prim_results[, mean:= apply(.SD, 1, mean), .SDcols = draw_names]
priv_prim_results[, lower:= apply(.SD, 1, quantile, c(0.025)), .SDcols = draw_names]
priv_prim_results[, upper:= apply(.SD, 1, quantile, c(0.975)), .SDcols = draw_names]

#remove congo data, read in other scaled congo data
pub_prim_results <- subset(pub_prim_results, location_id != congo)
rescaled_congo <-read_csv(paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/post_processing/scaled_data/congo_pub_prim_scaled_", vers_congo, ".csv"))
rescaled_congo <- rescaled_congo %>% dplyr::select(-me_id)
pub_prim_results <- rbind(pub_prim_results, rescaled_congo)

priv_prim_results <- subset(priv_prim_results, location_id != congo)
rescaled_congo <-read_csv(paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/post_processing/scaled_data/congo_priv_prim_scaled_", vers_congo, ".csv"))
rescaled_congo <- rescaled_congo %>% dplyr::select(-me_id)
priv_prim_results <- rbind(priv_prim_results, rescaled_congo)

pub_prim_mean_new = pub_prim_results %>% dplyr::select(year_id,location_id,mean) %>% dplyr::rename(mean_new = mean)
priv_prim_mean_new = priv_prim_results %>% dplyr::select(year_id,location_id,mean) %>% dplyr::rename(mean_new = mean)
# #combine old and new
pub_prim_mean = merge(pub_prim_mean, pub_prim_mean_new, by = c("year_id", "location_id"))
priv_prim_mean = merge(priv_prim_mean, priv_prim_mean_new, by = c("year_id", "location_id"))
#calculate difference
pub_prim_mean = pub_prim_mean %>% mutate(diff = mean_new - mean_old)
priv_prim_mean = priv_prim_mean %>% mutate(diff = mean_new - mean_old)
#save differences
write_csv(pub_prim_mean, paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/post_processing/scaled_data/pub_prim_mean_diff_", VERSION, ".csv"))
write_csv(priv_prim_mean, paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/post_processing/scaled_data/priv_prim_mean_diff_", VERSION, ".csv"))
##saving scaled results
write_csv(pub_prim_results, paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping//post_processing/scaled_data/pub_prim_st-gpr_results_scaled_", VERSION, ".csv"))
write_csv(priv_prim_results, paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping//post_processing/scaled_data/priv_prim_st-gpr_results_scaled_", VERSION, ".csv"))
message("done!")
