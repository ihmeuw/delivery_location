#### Uploading bundle data for maternal delivery location ####
### Chiara Sumich ###
### 10.25.2024 ###

#### Load packages ####
rm(list = ls())
pacman::p_load(data.table, ggplot2, gtools, tidyverse, haven, labelled)

#### loading datasets ####
VERSION <- "2025-07-10"
indir <- file.path("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/mcconrad_pre_gpr/")
maternal_file <- "maternal_missing_covariates_excluded_"
maternal <- read.csv(file.path(indir, paste0(maternal_file, VERSION, ".csv")))
outdir <- file.path("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/mcconrad_pre_gpr")
                              
## loading functions
source("/ihme/cc_resources/libraries/current/r/upload_bundle_data.R")
source("/ihme/cc_resources/libraries/current/r/save_bundle_version.R")
source("/ihme/cc_resources/libraries/current/r/get_bundle_data.R")
source("/ihme/cc_resources/libraries/current/r/get_elmo_ids.R")

## Defining bundle ids
bundle_id_pub_hosp <- 10530
bundle_id_priv_hosp <- 10554
bundle_id_ngo_hosp <- 10555
bundle_id_pub_prim <- 10556
bundle_id_priv_prim <-10557
bundle_id_ngo_prim <- 10558
bundle_id_hosp_any <- 10634
bundle_id_prim_any <- 10635

## Emptying current data bundles
# get current bundle data
old_pub_hosp <- get_bundle_data(bundle_id = bundle_id_pub_hosp)
old_priv_hosp <- get_bundle_data(bundle_id = bundle_id_priv_hosp)
old_pub_prim <- get_bundle_data(bundle_id = bundle_id_pub_prim)
old_priv_prim <- get_bundle_data(bundle_id = bundle_id_priv_prim)
old_hosp_any <- get_bundle_data(bundle_id = bundle_id_hosp_any)
old_prim_any <- get_bundle_data(bundle_id = bundle_id_prim_any)
old_ngo_hosp <- get_bundle_data(bundle_id = bundle_id_ngo_hosp)
old_ngo_prim <- get_bundle_data(bundle_id = bundle_id_ngo_prim)
# empty data table 
old_pub_hosp <- data.table("seq" = old_pub_hosp[, seq])
old_priv_hosp <- data.table("seq" = old_priv_hosp[, seq])
old_pub_prim <- data.table("seq" = old_pub_prim[, seq])
old_priv_prim <- data.table("seq" = old_priv_prim[, seq])
old_hosp_any <- data.table("seq" = old_hosp_any[, seq])
old_prim_any <- data.table("seq" = old_prim_any[, seq])
old_ngo_hosp <- data.table("seq" = old_ngo_hosp[, seq])
old_ngo_prim <- data.table("seq" = old_ngo_prim[, seq])
#write out file
outdir_pub_hosp <- file.path(outdir, "pub_hosp", VERSION)
dir.create(outdir_pub_hosp, recursive = TRUE)
openxlsx::write.xlsx(old_pub_hosp, file.path(outdir_pub_hosp, "empty_pub_hosp.csv"), sheetName = "extraction")

outdir_priv_hosp <- file.path(outdir, "priv_hosp", VERSION)
dir.create(outdir_priv_hosp, recursive = TRUE)
openxlsx::write.xlsx(old_priv_hosp, file.path(outdir_priv_hosp, "empty_priv_hosp.csv"), sheetName = "extraction")

outdir_pub_prim <- file.path(outdir, "pub_prim", VERSION)
dir.create(outdir_pub_prim, recursive = TRUE)
openxlsx::write.xlsx(old_pub_prim, file.path(outdir_pub_prim, "empty_pub_prim.csv"), sheetName = "extraction")

outdir_priv_prim <- file.path(outdir, "priv_prim", VERSION)
dir.create(outdir_priv_prim, recursive = TRUE)
openxlsx::write.xlsx(old_priv_prim, file.path(outdir_priv_prim, "empty_priv_prim.csv"), sheetName = "extraction")

outdir_hosp_any <- file.path(outdir, "hosp_any", VERSION)
dir.create(outdir_hosp_any, recursive = TRUE)
openxlsx::write.xlsx(old_hosp_any, file.path(outdir_hosp_any, "empty_hosp_any.csv"), sheetName = "extraction")

outdir_prim_any <- file.path(outdir, "prim_any", VERSION)
dir.create(outdir_prim_any, recursive = TRUE)
openxlsx::write.xlsx(old_prim_any, file.path(outdir_prim_any, "empty_prim_any.csv"), sheetName = "extraction")

outdir_ngo_hosp <- file.path(outdir, "ngo_hosp", VERSION)
dir.create(outdir_ngo_hosp, recursive = TRUE)
openxlsx::write.xlsx(old_ngo_hosp, file.path(outdir_ngo_hosp, "empty_ngo_hosp.csv"), sheetName = "extraction")

outdir_ngo_prim <- file.path(outdir, "ngo_prim", VERSION)
dir.create(outdir_ngo_prim, recursive = TRUE)
openxlsx::write.xlsx(old_ngo_prim, file.path(outdir_ngo_prim, "empty_ngo_prim.csv"), sheetName = "extraction")

#upload empty bundle
upload_bundle_data(bundle_id_pub_hosp, filepath = paste0(outdir_pub_hosp, "/", "empty_pub_hosp.csv"))
upload_bundle_data(bundle_id_priv_hosp, filepath = paste0(outdir_priv_hosp, "/", "empty_priv_hosp.csv"))
upload_bundle_data(bundle_id_pub_prim, filepath = paste0(outdir_pub_prim, "/", "empty_pub_prim.csv"))
upload_bundle_data(bundle_id_priv_prim, filepath = paste0(outdir_priv_prim, "/", "empty_priv_prim.csv"))
upload_bundle_data(bundle_id_hosp_any, filepath = paste0(outdir_hosp_any, "/", "empty_hosp_any.csv"))
upload_bundle_data(bundle_id_prim_any, filepath = paste0(outdir_prim_any, "/", "empty_prim_any.csv"))
upload_bundle_data(bundle_id_ngo_hosp, filepath = paste0(outdir_ngo_hosp, "/", "empty_ngo_hosp.csv"))
upload_bundle_data(bundle_id_ngo_prim, filepath = paste0(outdir_ngo_prim, "/", "empty_ngo_prim.csv"))

## Splitting data into modelable entity datasets for ensemble models & bundles
# 4 datasets of:
# Percent of deliveries in public hospital
# Ensemble data
#if nid in addtl mexico nids (107174,107172,107123,236187,236213,265259,335931,387644), remove from all except priv hospital and hospital
pub_hosp <- maternal %>%
  dplyr::select("nid", "survey_name", "location_id", "location_name", "location_set_version_id", "location_set_id",
         "parent_id", "path_to_top_parent", "level", "is_estimate", "most_detailed", "sort_order",                                
         "location_ascii_name", "location_name_short", "location_name_medium", "location_type_id",                          
         "location_type", "map_id", "super_region_id", "super_region_name", "region_id", "region_name",       
         "ihme_loc_id", "local_id", "developed", "lancet_label", "who_label", "year_start", "year_end", 
         "year_id", "survey_module", "file_path", "sample_size_pub_hosp", "nclust_pub_hosp", "nstrata_pub_hosp", 
         "mean_pub_hosp", "standard_error_pub_hosp", "design_effect_pub_hosp", "spec_outlier") %>%
  dplyr::rename(sample_size = "sample_size_pub_hosp", nclust = "nclust_pub_hosp", nstrata = "nstrata_pub_hosp", 
                mean = "mean_pub_hosp", standard_error = "standard_error_pub_hosp", 
                design_effect = "design_effect_pub_hosp", is_outlier = "spec_outlier") %>%
  ## there is one nid (468566) with a missing standard error that appears to be there in the original collapsed data
  ## removing this for now
  filter(!is.na(standard_error)) %>%
  dplyr::mutate(delivery_location = "pub_hosp",
                sex_id = 3,
                age_group_id = 22,
                val = mean, 
                variance = standard_error^2, 
                measure_id = "continuous",  ## according to https://scicomp-docs.ihme.washington.edu/elmo/current/validations/stgpr_validations.html
                ## variance (currently using standard error value here) must be between 0 and 1 for proportion (18)
                ## so changing measure to continuous (19) where the variance only needs to be a positive value
                seq = NA) %>%
  mutate(variance = if_else(variance <= 1e-12, 1e-12, variance)) %>%
  filter(!(nid %in% c(107174,107172,107123,236187,236213,265259,335931,387644))) %>%
  ##'[jklusty: add outlier 0 for Brazil data to pass validations]
  mutate(is_outlier = ifelse(survey_name == "SIH_admin_data", 0, is_outlier)) %>%
  write_csv(file.path(outdir_pub_hosp, paste0("delivery_location_", "pub_hosp_", VERSION, ".csv")))

# Bundle dataset
pub_hosp_bundle <- pub_hosp %>%
  dplyr::select("nid", "year_id", "location_id", "val", "sample_size", "standard_error", "sex_id", "age_group_id", "variance", "measure_id", "is_outlier", "seq") %>%
  dplyr::mutate(underlying_nid = nid,
                sex_id = "Both") %>%
  dplyr::rename(sex = "sex_id",
                measure = "measure_id") %>%
  openxlsx::write.xlsx(file.path(outdir_pub_hosp, paste0("delivery_location_", "bundle_", bundle_id_pub_hosp, "_", "pub_hosp_", VERSION, ".xlsx")), 
                       sheetName = "extraction",
                       overwrite = TRUE)

## Percent of deliveries in private for-profit hospital
# Ensemble dataset
priv_hosp <- maternal %>%
  select("nid", "survey_name", "location_id", "location_name", "location_set_version_id", "location_set_id",
         "parent_id", "path_to_top_parent", "level", "is_estimate", "most_detailed", "sort_order",                                
         "location_ascii_name", "location_name_short", "location_name_medium", "location_type_id",                          
         "location_type", "map_id", "super_region_id", "super_region_name", "region_id", "region_name",       
         "ihme_loc_id", "local_id", "developed", "lancet_label", "who_label", "year_start", "year_end", 
         "year_id", "survey_module", "file_path", "sample_size_priv_hosp", "nclust_priv_hosp", "nstrata_priv_hosp", 
         "mean_priv_hosp", "standard_error_priv_hosp", "design_effect_priv_hosp", "spec_outlier") %>%
  dplyr::rename(sample_size = "sample_size_priv_hosp", nclust = "nclust_priv_hosp", nstrata = "nstrata_priv_hosp", 
                mean = "mean_priv_hosp", standard_error = "standard_error_priv_hosp", 
                design_effect = "design_effect_priv_hosp", is_outlier = "spec_outlier") %>%
  dplyr::mutate(delivery_location = "priv_hosp",
                sex_id = 3,
                age_group_id = 22,
                val = mean, 
                variance = standard_error^2, 
                measure_id = "continuous", 
                seq = NA) %>%
  mutate(variance = if_else(variance <= 1e-12, 1e-12, variance)) %>%
  write_csv(file.path(outdir_priv_hosp, paste0("delivery_location_", "priv_hosp_", VERSION, ".csv")))

# Bundle dataset
priv_hosp_bundle <- priv_hosp %>%
  select("nid", "year_id", "location_id", "val", "sample_size", "standard_error", "sex_id", "age_group_id", "variance", "measure_id", "is_outlier", "seq") %>%
  dplyr::mutate(underlying_nid = nid,
                sex_id = "Both") %>%
  dplyr::rename(sex = "sex_id",
                measure = "measure_id") %>%
  openxlsx::write.xlsx(file.path(outdir_priv_hosp, paste0("delivery_location_", "bundle_", bundle_id_priv_hosp, "_", "priv_hosp_", VERSION, ".xlsx")),
                       sheetName = "extraction",
                       overwrite = TRUE)

## Percent of deliveries in private non-profit hospital 
## merged with pub_hosp ME
# outdir_ngo_hosp <- file.path(outdir, "ngo_hosp", VERSION)
# dir.create(outdir_ngo_hosp, recursive = TRUE)

# Ensemble dataset
ngo_hosp <- maternal %>%
  dplyr::select("nid", "survey_name", "location_id", "location_name", "location_set_version_id", "location_set_id",
         "parent_id", "path_to_top_parent", "level", "is_estimate", "most_detailed", "sort_order",
         "location_ascii_name", "location_name_short", "location_name_medium", "location_type_id",
         "location_type", "map_id", "super_region_id", "super_region_name", "region_id", "region_name",
         "ihme_loc_id", "local_id", "developed", "lancet_label", "who_label", "year_start", "year_end",
         "year_id", "survey_module", "file_path", "sample_size_ngo_hosp", "nclust_ngo_hosp", "nstrata_ngo_hosp",
         "mean_ngo_hosp", "mean_ngo_prim", "standard_error_ngo_hosp", "design_effect_ngo_hosp", "spec_outlier") %>%
  dplyr::rename(sample_size = "sample_size_ngo_hosp", nclust = "nclust_ngo_hosp", nstrata = "nstrata_ngo_hosp",
                mean = "mean_ngo_hosp", standard_error = "standard_error_ngo_hosp",
                design_effect = "design_effect_ngo_hosp", is_outlier = "spec_outlier") %>%
  subset(!is.na(is_outlier) & !is.na(mean) & !is.na(nid) & !is.na(standard_error)) %>%
  dplyr::mutate(delivery_location = "ngo_hosp",
                sex_id = 3,
                age_group_id = 22,
                val = case_when(nid==464593 & year_id %in% c(2018, 2019) ~ mean_ngo_prim,
                                nid %in% c(161590, 21139, 157066, 55992) ~ mean_ngo_prim,
                                TRUE ~ mean),
                variance = standard_error^2,
                measure_id = "continuous",
                seq = row_number())%>%
  mutate(variance = if_else(variance <= 1e-12, 1e-11, variance)) %>%
  filter(!(nid %in% c(107174,107172,107123,236187,236213,265259,335931,387644, 26919, 235046))) %>%
  write_csv(file.path(outdir_ngo_hosp, paste0("delivery_location_", "ngo_hosp_", VERSION, ".csv")))

# Bundle dataset
ngo_hosp_bundle <- ngo_hosp %>%
  select("nid", "year_id", "location_id", "val", "sample_size", "standard_error", "sex_id", "age_group_id", "variance", "measure_id", "is_outlier", "seq") %>%
  dplyr::mutate(underlying_nid = nid,
                sex_id = "Both") %>%
  dplyr::rename(sex = "sex_id",
                measure = "measure_id") %>%
  openxlsx::write.xlsx(file.path(outdir_ngo_hosp, paste0("delivery_location_", "bundle_", bundle_id_ngo_hosp, "_", "ngo_hosp_", VERSION, ".xlsx")),
                       sheetName = "extraction",
                       overwrite = TRUE)

## Percent of deliveries in public lower level facility
# Ensemble dataset
pub_prim <- maternal %>%
  select("nid", "survey_name", "location_id", "location_name", "location_set_version_id", "location_set_id",
         "parent_id", "path_to_top_parent", "level", "is_estimate", "most_detailed", "sort_order",                                
         "location_ascii_name", "location_name_short", "location_name_medium", "location_type_id",                          
         "location_type", "map_id", "super_region_id", "super_region_name", "region_id", "region_name",       
         "ihme_loc_id", "local_id", "developed", "lancet_label", "who_label", "year_start", "year_end", 
         "year_id", "survey_module", "file_path", "sample_size_pub_prim", "nclust_pub_prim", "nstrata_pub_prim", 
         "mean_pub_prim", "standard_error_pub_prim", "design_effect_pub_prim", "spec_outlier") %>%
  dplyr::rename(sample_size = "sample_size_pub_prim", nclust = "nclust_pub_prim", nstrata = "nstrata_pub_prim", 
                mean = "mean_pub_prim", standard_error = "standard_error_pub_prim", 
                design_effect = "design_effect_pub_prim", is_outlier = "spec_outlier") %>%
  ## there is one nid (468566) with a missing standard error that appears to be there in the original collapsed data
  ## removing this for now
  filter(!is.na(standard_error)) %>%
  dplyr::mutate(delivery_location = "pub_prim",
                sex_id = 3,
                age_group_id = 22,
                val = mean, 
                variance = standard_error^2, 
                measure_id = "continuous", 
                seq = NA) %>% 
  mutate(variance = if_else(variance <= 1e-12, 1e-12, variance)) %>%
  filter(!(nid %in% c(107174,107172,107123,236187,236213,265259,335931,387644))) %>%
  write_csv(file.path(outdir_pub_prim, paste0("delivery_location_", "pub_prim_", VERSION, ".csv")))

# Bundle dataset
pub_prim_bundle <- pub_prim %>%
  select("nid", "year_id", "location_id", "val", "sample_size", "standard_error", "sex_id", "age_group_id", "variance", "measure_id", "is_outlier", "seq") %>%
  dplyr::mutate(underlying_nid = nid,
                sex_id = "Both") %>%
  dplyr::rename(sex = "sex_id",
                measure = "measure_id") %>%
  openxlsx::write.xlsx(file.path(outdir_pub_prim, paste0("delivery_location_", "bundle_", bundle_id_pub_prim, "_", "pub_prim_", VERSION, ".xlsx")),
                       sheetName = "extraction",
                       overwrite = TRUE)

## Percent of deliveries in private for-profit lower level facility
# Ensemble dataset
priv_prim <- maternal %>%
  select("nid", "survey_name", "location_id", "location_name", "location_set_version_id", "location_set_id",
         "parent_id", "path_to_top_parent", "level", "is_estimate", "most_detailed", "sort_order",                                
         "location_ascii_name", "location_name_short", "location_name_medium", "location_type_id",                          
         "location_type", "map_id", "super_region_id", "super_region_name", "region_id", "region_name",       
         "ihme_loc_id", "local_id", "developed", "lancet_label", "who_label", "year_start", "year_end", 
         "year_id", "survey_module", "file_path", "sample_size_priv_prim", "nclust_priv_prim", "nstrata_priv_prim", 
         "mean_priv_prim", "standard_error_priv_prim", "design_effect_priv_prim", "spec_outlier") %>%
  dplyr::rename(sample_size = "sample_size_priv_prim", nclust = "nclust_priv_prim", nstrata = "nstrata_priv_prim", 
                mean = "mean_priv_prim", standard_error = "standard_error_priv_prim", 
                design_effect = "design_effect_priv_prim", is_outlier = "spec_outlier") %>%
  dplyr::mutate(delivery_location = "priv_prim",
                sex_id = 3,
                age_group_id = 22,
                val = mean, 
                variance = standard_error^2, 
                measure_id = "continuous", 
                seq = NA) %>% 
  mutate(variance = if_else(variance <= 1e-12, 1e-12, variance)) %>%
  filter(!(nid %in% c(107174,107172,107123,236187,236213,265259,335931,387644))) %>%
  write_csv(file.path(outdir_priv_prim, paste0("delivery_location_", "priv_prim_", VERSION, ".csv")))

# Bundle dataset
priv_prim_bundle <- priv_prim %>%
  select("nid", "year_id", "location_id", "val", "sample_size", "standard_error", "sex_id", "age_group_id", "variance", "measure_id", "is_outlier", "seq") %>%
  dplyr::mutate(underlying_nid = nid,
                sex_id = "Both") %>%
  dplyr::rename(sex = "sex_id",
                measure = "measure_id") %>%
  openxlsx::write.xlsx(file.path(outdir_priv_prim, paste0("delivery_location_", "bundle_", bundle_id_priv_prim, "_", "priv_prim_", VERSION, ".xlsx")),
                       sheetName = "extraction",
                       overwrite = TRUE)

## Percent of deliveries in private non-profit lower level facility
## merged with pub_prim
# outdir_ngo_prim <- file.path(outdir, "ngo_prim", VERSION)
# dir.create(outdir_ngo_prim, recursive = TRUE)

ngo_prim <- maternal %>%
  select("nid", "survey_name", "location_id", "location_name", "location_set_version_id", "location_set_id",
         "parent_id", "path_to_top_parent", "level", "is_estimate", "most_detailed", "sort_order",
         "location_ascii_name", "location_name_short", "location_name_medium", "location_type_id",
         "location_type", "map_id", "super_region_id", "super_region_name", "region_id", "region_name",
         "ihme_loc_id", "local_id", "developed", "lancet_label", "who_label", "year_start", "year_end",
         "year_id", "survey_module", "file_path", "sample_size_ngo_prim", "nclust_ngo_prim", "nstrata_ngo_prim",
         "mean_ngo_prim", "mean_ngo_hosp", "standard_error_ngo_prim", "design_effect_ngo_prim", "spec_outlier") %>%
  dplyr::rename(sample_size = "sample_size_ngo_prim", nclust = "nclust_ngo_prim", nstrata = "nstrata_ngo_prim",
                mean = "mean_ngo_prim", standard_error = "standard_error_ngo_prim",
                design_effect = "design_effect_ngo_prim", is_outlier = "spec_outlier") %>%
  subset(!is.na(is_outlier) & !is.na(mean) & !is.na(nid) & !is.na(standard_error)) %>%
  dplyr::mutate(delivery_location = "ngo_prim",
                sex_id = 3,
                age_group_id = 22,
                val = case_when(nid==464593 & year_id %in% c(2018, 2019) ~ mean_ngo_hosp,
                                nid %in% c(161590, 21139, 157066, 55992) ~ mean_ngo_hosp,
                                TRUE ~ mean),
                variance = standard_error^2,
                measure_id = "continuous",
                seq = row_number()) %>%
  mutate(variance = if_else(variance <= 1e-12, 1e-11, variance)) %>%
  filter(!(nid %in% c(107174,107172,107123,236187,236213,265259,335931,387644, 26919, 235046))) %>%
  write_csv(file.path(outdir_ngo_prim, paste0("delivery_location_", "ngo_prim_", VERSION, ".csv")))

# Bundle dataset
ngo_prim_bundle <- ngo_prim %>%
  select("nid", "year_id", "location_id", "val", "sample_size", "standard_error", "sex_id", "age_group_id", "variance", "measure_id", "is_outlier", "seq") %>%
  dplyr::mutate(underlying_nid = nid,
                sex_id = "Both") %>%
  dplyr::rename(sex = "sex_id",
                measure = "measure_id") %>%
  openxlsx::write.xlsx(file.path(outdir_ngo_prim, paste0("delivery_location_", "bundle_", bundle_id_ngo_prim, "_", "ngo_prim_", VERSION, ".xlsx")),
                       sheetName = "extraction",
                       overwrite = TRUE)

# Ensemble data
outdir_hosp_any <- file.path(outdir, "hosp_any", VERSION)
dir.create(outdir_hosp_any, recursive = TRUE)


hosp_any <- maternal %>%
  select("nid", "survey_name", "location_id", "location_name", "location_set_version_id", "location_set_id",
         "parent_id", "path_to_top_parent", "level", "is_estimate", "most_detailed", "sort_order",                                
         "location_ascii_name", "location_name_short", "location_name_medium", "location_type_id",                          
         "location_type", "map_id", "super_region_id", "super_region_name", "region_id", "region_name",       
         "ihme_loc_id", "local_id", "developed", "lancet_label", "who_label", "year_start", "year_end", 
         "year_id", "survey_module", "file_path", "sample_size_hospital", "nclust_hospital", "nstrata_hospital", 
         "mean_hospital", "standard_error_hospital", "design_effect_hospital", "level_outlier") %>%
  dplyr::rename(sample_size = "sample_size_hospital", nclust = "nclust_hospital", nstrata = "nstrata_hospital", 
                                 mean = "mean_hospital", standard_error = "standard_error_hospital", 
                                 design_effect = "design_effect_hospital", is_outlier = "level_outlier") %>%
  ## there are two nids (468566 & 45942) with a missing standard error that appears to be there in the original collapsed data
  ## removing this for now
  filter(!is.na(standard_error)) %>%
  dplyr::mutate(delivery_location = "hospital",
                sex_id = 3,
                age_group_id = 22,
                val = mean,
                variance = standard_error^2, 
                measure_id = "continuous",  
                seq = NA) %>%
  mutate(variance = if_else(variance <= 1e-12, 1e-12, variance)) %>%
  write_csv(file.path(outdir_hosp_any, paste0("delivery_location_", "hosp_any_", VERSION, ".csv")))

# Bundle dataset
hosp_any_bundle <- hosp_any %>%
  select("nid", "year_id", "location_id", "val", "sample_size", "standard_error", "sex_id", "age_group_id", "variance", "measure_id", "is_outlier", "seq") %>%
  dplyr::mutate(underlying_nid = nid,
                sex_id = "Both") %>%
  dplyr::rename(sex = "sex_id",
                measure = "measure_id") %>%
  openxlsx::write.xlsx(file.path(outdir_hosp_any, paste0("delivery_location_", "bundle_", bundle_id_hosp_any, "_", "hosp_any_", VERSION, ".xlsx")), 
                       sheetName = "extraction",
                       overwrite = TRUE)

# Ensemble data
outdir_prim_any <- file.path(outdir, "prim_any", VERSION)
dir.create(outdir_prim_any, recursive = TRUE)

prim_any <- maternal %>%
  select("nid", "survey_name", "location_id", "location_name", "location_set_version_id", "location_set_id",
         "parent_id", "path_to_top_parent", "level", "is_estimate", "most_detailed", "sort_order",                                
         "location_ascii_name", "location_name_short", "location_name_medium", "location_type_id",                          
         "location_type", "map_id", "super_region_id", "super_region_name", "region_id", "region_name",       
         "ihme_loc_id", "local_id", "developed", "lancet_label", "who_label", "year_start", "year_end", 
         "year_id", "survey_module", "file_path", "sample_size_primary", "nclust_primary", "nstrata_primary", 
         "mean_primary", "standard_error_primary", "design_effect_primary", "level_outlier") %>%
  dplyr::rename(sample_size = "sample_size_primary", nclust = "nclust_primary", nstrata = "nstrata_primary", 
                mean = "mean_primary", standard_error = "standard_error_primary", 
                design_effect = "design_effect_primary", is_outlier = "level_outlier") %>%
  ## there are two nids (468566 & 45942) with a missing standard error that appears to be there in the original collapsed data
  ## removing this for now
  filter(!is.na(standard_error)) %>%
  dplyr::mutate(delivery_location = "primary",
                sex_id = 3,
                age_group_id = 22,
                val = mean,
                variance = standard_error^2, 
                measure_id = "continuous",  
                seq = NA) %>%
  mutate(variance = if_else(variance <= 1e-12, 1e-12, variance)) %>%
  filter(!(nid %in% c(107174,107172,107123,236187,236213,265259,335931,387644))) %>%
  write_csv(file.path(outdir_prim_any, paste0("delivery_location_", "prim_any_", VERSION, ".csv")))

# Bundle dataset
prim_any_bundle <- prim_any %>%
  select("nid", "year_id", "location_id", "val", "sample_size", "standard_error", "sex_id", "age_group_id", "variance", "measure_id", "is_outlier", "seq") %>%
  dplyr::mutate(underlying_nid = nid,
                sex_id = "Both") %>%
  dplyr::rename(sex = "sex_id",
                measure = "measure_id") %>%
  openxlsx::write.xlsx(file.path(outdir_prim_any, paste0("delivery_location_", "bundle_", bundle_id_prim_any, "_", "prim_any_", VERSION, ".xlsx")), 
                       sheetName = "extraction",
                       overwrite = TRUE)

#### Uploading bundle data ####
## defining bundle data files
path_pub_hosp <- file.path(paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/mcconrad_pre_gpr/pub_hosp/", VERSION, "/delivery_location_bundle_", bundle_id_pub_hosp, "_pub_hosp_", VERSION, ".xlsx"))
path_priv_hosp <- file.path(paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/mcconrad_pre_gpr/priv_hosp/", VERSION, "/delivery_location_bundle_", bundle_id_priv_hosp, "_priv_hosp_", VERSION, ".xlsx"))
path_ngo_hosp <- file.path(paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/mcconrad_pre_gpr/ngo_hosp/", VERSION, "/delivery_location_bundle_", bundle_id_ngo_hosp, "_ngo_hosp_", VERSION, ".xlsx"))
path_pub_prim <- file.path(paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/mcconrad_pre_gpr/pub_prim/", VERSION, "/delivery_location_bundle_", bundle_id_pub_prim, "_pub_prim_", VERSION, ".xlsx"))
path_priv_prim <- file.path(paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/mcconrad_pre_gpr/priv_prim/",  VERSION, "/delivery_location_bundle_", bundle_id_priv_prim, "_priv_prim_", VERSION, ".xlsx"))
path_ngo_prim <- file.path(paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/mcconrad_pre_gpr/ngo_prim/", VERSION, "/delivery_location_bundle_", bundle_id_ngo_prim, "_ngo_prim_", VERSION, ".xlsx"))
path_hosp_any <- file.path(paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/mcconrad_pre_gpr/hosp_any/", VERSION, "/delivery_location_bundle_", bundle_id_hosp_any, "_hosp_any_", VERSION, ".xlsx"))
path_prim_any <- file.path(paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/mcconrad_pre_gpr/prim_any/", VERSION, "/delivery_location_bundle_", bundle_id_prim_any, "_prim_any_", VERSION, ".xlsx"))

## Uploading bundle data
pub_hosp <- upload_bundle_data(bundle_id_pub_hosp, filepath = path_pub_hosp)
priv_hosp <- upload_bundle_data(bundle_id_priv_hosp, filepath = path_priv_hosp)
ngo_hosp <- upload_bundle_data(bundle_id_ngo_hosp, filepath = path_ngo_hosp)
pub_prim <- upload_bundle_data(bundle_id_pub_prim, filepath = path_pub_prim)
priv_prim <- upload_bundle_data(bundle_id_priv_prim, filepath = path_priv_prim)
ngo_prim <- upload_bundle_data(bundle_id_ngo_prim, filepath = path_ngo_prim)
hosp_any <- upload_bundle_data(bundle_id_hosp_any, filepath = path_hosp_any)
prim_any <- upload_bundle_data(bundle_id_prim_any, filepath = path_prim_any)

## Saving bundle version
pub_hosp_version <- save_bundle_version(bundle_id = bundle_id_pub_hosp, automatic_crosswalk = TRUE)
priv_hosp_version <- save_bundle_version(bundle_id = bundle_id_priv_hosp, automatic_crosswalk = TRUE)
ngo_hosp_version <- save_bundle_version(bundle_id = bundle_id_ngo_hosp, automatic_crosswalk = TRUE)
pub_prim_version <- save_bundle_version(bundle_id = bundle_id_pub_prim, automatic_crosswalk = TRUE)
priv_prim_version <- save_bundle_version(bundle_id = bundle_id_priv_prim, automatic_crosswalk = TRUE)
ngo_prim_version <- save_bundle_version(bundle_id = bundle_id_ngo_prim, automatic_crosswalk = TRUE)
hosp_any_version <- save_bundle_version(bundle_id = bundle_id_hosp_any, automatic_crosswalk = TRUE)
prim_any_version <- save_bundle_version(bundle_id = bundle_id_prim_any, automatic_crosswalk = TRUE)
