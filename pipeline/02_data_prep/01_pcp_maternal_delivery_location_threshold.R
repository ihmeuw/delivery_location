#### Delivery location thresholds ####
#### written by Chiara Sumich ####
#### 09.16.2024 ####

#### Load packages ####
rm(list = ls())
pacman::p_load(data.table, ggplot2, gtools, tidyverse, haven, labelled)

## Environment set up ##
VERSION <- "2025-07-10"

#### Loading datasets ####
# indir <- file.path("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse/2025-02-26")
indir <- file.path("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse/")
maternal_file <- paste0("collapse_maternal_", VERSION, ".csv") # "collapse_maternal_2025-05-23.csv"
maternal <- read.csv(file.path(indir, maternal_file))
outdir <- file.path("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/mcconrad_pre_gpr/")

#### Creating year_id
maternal <- maternal %>%
  mutate(year_id = year_start - age_start)

#### Remove duplicate nids ####
maternal_no_duplicates <- maternal %>%
  distinct(nid, survey_name, ihme_loc_id, year_id, survey_module, var, .keep_all = TRUE)

maternal_filtered <- maternal_no_duplicates %>% 
  group_by(nid, survey_name, ihme_loc_id, year_start, year_end, survey_module, file_path, age_start, age_end) %>% 
  # filter(any(var == "ifd") & any(var == "ifd" & mean != 0)) %>% 
  ##'[jklusty: update 7/9/25 to keep the Brazil SIH data]
  filter(survey_name == "SIH_admin_data" | (any(var == "ifd") & any(var == "ifd" & mean != 0))) %>% 
  ungroup()

#### Reshaping file to wide ####
maternal_wide <- pivot_wider(maternal_filtered,
                             names_from = "var",
                             values_from = c("sample_size", "nclust", "nstrata", "mean",
                                             "standard_error", "design_effect"))

#### Filter out NIDS with more missingness than our thresholds for sector and level ####
maternal_wide <- maternal_wide %>%  
  rowwise() %>% 
  dplyr::mutate(spec_outlier = ifelse(sum(mean_pub_hosp, mean_pub_prim, mean_priv_hosp, mean_priv_prim, mean_ngo_hosp, mean_ngo_prim, na.rm = TRUE)/mean_ifd<0.85, 1 , 0),   
                level_outlier = ifelse(sum(mean_hospital, mean_primary, na.rm = TRUE)/mean_ifd<0.95, 1, 0))

## updating pre-processing to merge public and ngo facilities rather than after preliminary ST-GPR models
maternal_wide <- maternal_wide %>%
  dplyr::rename(original_mean_pub_hosp = 'mean_pub_hosp',
                original_mean_pub_prim = 'mean_pub_prim') %>%
  ## merging ngo_hosp with public_hosp
  # dplyr::mutate(mean_pub_hosp = original_mean_pub_hosp + mean_ngo_hosp,
  #               mean_pub_prim = original_mean_pub_prim + mean_ngo_prim)
  dplyr::mutate(mean_pub_hosp = ifelse(is.na(mean_ngo_hosp), original_mean_pub_hosp, original_mean_pub_hosp + mean_ngo_hosp),
                mean_pub_prim = ifelse(is.na(mean_ngo_prim), original_mean_pub_prim, original_mean_pub_prim + mean_ngo_prim))

## Exporting maternal collapse dataset
maternal_wide <- maternal_wide %>%
write_csv(file.path(outdir, paste0("maternal_delivery_location_CLEAN_", VERSION, ".csv")))


  