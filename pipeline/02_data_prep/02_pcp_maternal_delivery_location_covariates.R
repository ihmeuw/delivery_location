#### Delivery location covariates ####
#### written by Chiara Sumich ####
#### adapted from code from Wes Warriner ####
#### 09.16.2024 ####

#### Load packages ####
rm(list = ls())
pacman::p_load(data.table, ggplot2, gtools, tidyverse, haven, labelled)

#### loading datasets ####
VERSION <- "2025-07-10"
indir <- file.path("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/mcconrad_pre_gpr/")
maternal <- read_csv(file.path(paste0(indir, "maternal_delivery_location_CLEAN_", VERSION, ".csv")))
outdir <- file.path("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/mcconrad_pre_gpr")

# Environment Setup
user <- Sys.getenv('USER')
if (Sys.info()['sysname'] == 'Linux') {
  j <- '/snfs1' # '/home/j' # deprecated
  h <- paste0('/homes/', user)
  k <- '/ihme/cc_resources'
} else { 
  j <- 'J:'
  h <- 'H:'
  k <- 'K:'
}
ROOT = file.path(h, 'repos', 'pcp')

# Imports
source(file.path(ROOT, 'global_helpers', 'viz', 'map_estimates.R'))
pacman::p_load(data.table, dplyr, ggplot2, ggrepel, corrplot, yaml, scales)

# Local Constants
settings = yaml.load_file(file.path(ROOT, 'config.yml'))
PREP_VERSION = '2024-05-23'
USE_SAMPLE_WEIGHTS = TRUE
# paths since prep version should be sufficient
DATASETS_DATE = PREP_VERSION
COMPARISON_DATASET_DATE = PREP_VERSION
COLOR_SCHEME_3 <- c('#BDD7E7', '#6AAED6', '#3182BD')
COLOR_SCHEME_4 = c('#EFF3FF', '#BDD7E7', '#6AAED6', '#3182BD')
HIGHER_IS_WORSE_COLOR_SCHEME = c('#FFF3D5', '#FEE090', '#FC8D59', '#D73027')

# Super-regions color scheme
SR_COLORS_MAP <- c(
  'Central Europe, Eastern Europe, and Central Asia' = '#7E0018',
  'Global' = '#FF9DC8',
  'High-income' = '#A6A6A6',
  'Latin America and Caribbean' = '#0079FA',
  'North Africa and Middle East' = '#00C2F9',
  'South Asia' = '#FFDC3D',
  'Southeast Asia, East Asia, and Oceania' = '#CD022D',
  'Sub-Saharan Africa' = '#003D30'
)

## Helper functions
cmc_to_year_month <- function(cmc) {
  year <- 1900 + ((cmc - 1) %/% 12)
  month <- cmc - (12 * (year - 1900))
  
  return ( list(year = year, month = month) )
}

# Locations
source('/ihme/cc_resources/libraries/current/r/get_location_metadata.R')
loc <- get_location_metadata(
  location_set_id = 22,
  release_id = 16,
)

## Covariates
source("/ihme/cc_resources/libraries/current/r/get_covariate_estimates.R")
# Set up arguments
years <- c(1980:2024)
release <- 16

# covariates required: 
# ANC1, ANC4, SBA, IFD, c-section, 
mat_care_covariates <- c (7, 8, 143, 51, 2381)
# health expenditure per capita, fraction out of pocket, government health spending, domestic health expenditure per capita, 
spending_covariates <-c (1089, 1093)
# HAQ, UHC, SDI, lag distributed income per capita, TFR, percent births over 35
other_covariates <- c(1099, 1097, 881, 57, 149, 84)
all_covariates <-  c(mat_care_covariates, spending_covariates, other_covariates)

## Pull covariates
# get_covariate_estimates does not accept lists of covariate ids, need to pull separately and append
covariates_df <- data.frame()
for(i in 1:length(all_covariates)) {
  ind <- all_covariates[i]
  ind_df <- get_covariate_estimates(covariate_id = ind,
                                    year_id = years, 
                                    release_id = release) 
  covariates_df <- rbind(covariates_df, ind_df)                                     
}

# join covariates dataframe to location dataframe 
covariates_df2 <- covariates_df %>%
  left_join(loc, by = c("location_id"))
check = covariates_df2 %>% filter(is.na(location_id))
# join covariates dataframe to maternal wide data frame by start year and location
maternal_covariates <- maternal %>%
  left_join(covariates_df2)
#  left_join(covariates_df, by = c("ihme_loc_id", "year_id"))
check = maternal_covariates %>% filter(is.na(location_id))
maternal_covariates <- maternal_covariates %>% filter(!is.na(location_id)) %>% dplyr::rename(location_name = location_name.x)

# pivot maternal covariates wide
maternal_covariates_wide <- maternal_covariates %>%
  # reorder columns
  dplyr::select("nid", "survey_name", "location_id", "location_name", "location_set_version_id", "location_set_id", "parent_id",                  
         "path_to_top_parent", "level", "is_estimate", "most_detailed", "sort_order", "location_ascii_name",        
         "location_name_short", "location_name_medium", "location_type_id", "location_type", "map_id", "super_region_id",            
         "super_region_name", "region_id", "region_name", "ihme_loc_id", "local_id", "developed", "lancet_label",               
         "who_label", "year_start", "year_end", "age_start", "age_end", "year_id", "survey_module", "file_path", "sample_size_ifd", "sample_size_home", 
         "sample_size_other", "sample_size_unknown", "sample_size_pub_hosp", "sample_size_pub_prim", "sample_size_pub_split", 
         "sample_size_pub_none", "sample_size_priv_hosp", "sample_size_priv_prim", "sample_size_priv_split", "sample_size_priv_none", 
         "sample_size_ngo_hosp", "sample_size_ngo_prim", "sample_size_ngo_split", "sample_size_ngo_none", "sample_size_no_hosp",        
         "sample_size_no_prim", "sample_size_no_split", "sample_size_no_none", "sample_size_public", "sample_size_private", 
         "sample_size_ngo", "sample_size_no_sector", "sample_size_hospital", "sample_size_primary", "sample_size_split", 
         "sample_size_no_level", "nclust_ifd", "nclust_home", "nclust_other", "nclust_unknown", "nclust_pub_hosp", "nclust_pub_prim",
         "nclust_pub_split", "nclust_pub_none", "nclust_priv_hosp", "nclust_priv_prim", "nclust_priv_split", "nclust_priv_none", 
         "nclust_ngo_hosp", "nclust_ngo_prim", "nclust_ngo_split", "nclust_ngo_none", "nclust_no_hosp", "nclust_no_prim", 
         "nclust_no_split", "nclust_no_none", "nclust_public", "nclust_private", "nclust_ngo", "nclust_no_sector", "nclust_hospital",            
         "nclust_primary", "nclust_split", "nclust_no_level", "nstrata_ifd", "nstrata_home", "nstrata_other", "nstrata_unknown", 
         "nstrata_pub_hosp", "nstrata_pub_prim", "nstrata_pub_split", "nstrata_pub_none", "nstrata_priv_hosp", "nstrata_priv_prim", 
         "nstrata_priv_split", "nstrata_priv_none", "nstrata_ngo_hosp", "nstrata_ngo_prim", "nstrata_ngo_split", "nstrata_ngo_none",
         "nstrata_no_hosp", "nstrata_no_prim", "nstrata_no_split", "nstrata_no_none", "nstrata_public", "nstrata_private", "nstrata_ngo", 
         "nstrata_no_sector", "nstrata_hospital", "nstrata_primary", "nstrata_split", "nstrata_no_level", "mean_ifd", "mean_home", 
         "mean_other", "mean_unknown", "original_mean_pub_hosp","original_mean_pub_prim", "mean_pub_split", "mean_pub_none", "mean_priv_hosp", "mean_priv_prim", 
         "mean_priv_split", "mean_priv_none", "mean_ngo_hosp", "mean_ngo_prim", "mean_ngo_split", "mean_ngo_none", "mean_no_hosp",
         "mean_no_prim", "mean_no_split", "mean_no_none", "mean_public", "mean_private", "mean_ngo", "mean_no_sector", "mean_hospital", 
         "mean_primary", "mean_split", "mean_no_level", "standard_error_ifd", "standard_error_home", "standard_error_other", 
         "standard_error_unknown", "standard_error_pub_hosp", "standard_error_pub_prim", "standard_error_pub_split", "standard_error_pub_none", 
         "standard_error_priv_hosp", "standard_error_priv_prim", "standard_error_priv_split", "standard_error_priv_none", "standard_error_ngo_hosp",    
         "standard_error_ngo_prim", "standard_error_ngo_split", "standard_error_ngo_none", "standard_error_no_hosp", "standard_error_no_prim", 
         "standard_error_no_split", "standard_error_no_none", "standard_error_public", "standard_error_private", "standard_error_ngo", 
         "standard_error_no_sector", "standard_error_hospital", "standard_error_primary", "standard_error_split", "standard_error_no_level", 
         "design_effect_ifd", "design_effect_home", "design_effect_other", "design_effect_unknown", "design_effect_pub_hosp", 
         "design_effect_pub_prim", "design_effect_pub_split", "design_effect_pub_none", "design_effect_priv_hosp", "design_effect_priv_prim", 
         "design_effect_priv_split", "design_effect_priv_none", "design_effect_ngo_hosp", "design_effect_ngo_prim", "design_effect_ngo_split",    
         "design_effect_ngo_none", "design_effect_no_hosp", "design_effect_no_prim", "design_effect_no_split", "design_effect_no_none", 
         "design_effect_public", "design_effect_private", "design_effect_ngo", "design_effect_no_sector", "design_effect_hospital", 
         "design_effect_primary", "design_effect_split", "design_effect_no_level", "spec_outlier", "level_outlier",
         "mean_pub_hosp", "mean_pub_prim", "covariate_name_short", "age_group_id", "age_group_name", "sex_id", "sex", "model_version_id",  
         "covariate_id", "mean_value", "lower_value", "upper_value") %>%
  pivot_wider(names_from = "covariate_name_short",
              values_from = c("model_version_id", "age_group_id", "age_group_name", "sex_id", "sex", 
                              "covariate_id", "mean_value", "lower_value", "upper_value"))

## missing values all seem to be from ihme_loc_id ETH_44858 (7) and TCA (1)
## we have values for these in the maternal collapse data but there is no covariate information or location information available for them
mis_anc1 <- subset(maternal_covariates_wide, is.na(mean_value_ANC1_coverage_prop))
mis_anc4 <- subset(maternal_covariates_wide, is.na(mean_value_ANC4_coverage_prop))
mis_sba <- subset(maternal_covariates_wide, is.na(mean_value_SBA_coverage_prop))
mis_IFD <- subset(maternal_covariates_wide, is.na(mean_value_IFD_coverage_prop))
mis_csection <- subset(maternal_covariates_wide, is.na(mean_value_csection_coverage_prop))
mis_he <- subset(maternal_covariates_wide, is.na(mean_value_he_cap))
mis_oop <- subset(maternal_covariates_wide, is.na(mean_value_frac_oop_hexp))
mis_domestic_he <- subset(maternal_covariates_wide, is.na(mean_value_domestic_he_cap))
mis_haqi <- subset(maternal_covariates_wide, is.na(mean_value_haqi))
mis_uhc <- subset(maternal_covariates_wide, is.na(mean_value_universal_health_coverage))
mis_sdi <- subset(maternal_covariates_wide, is.na(mean_value_sdi))
mis_ldi <- subset(maternal_covariates_wide, is.na(mean_value_LDI_pc))
mis_TRF <- subset(maternal_covariates_wide, is.na(mean_value_TFR))
mis_births_35plus <- subset(maternal_covariates_wide, is.na(mean_value_pct_births_in_over35s))

numberNIDsexcluded <- length(unique(mis_anc1$nid))
missing_covariates <- mis_anc1 %>%
  distinct(nid, .keep_all = TRUE)

maternal_covariates_corr <- maternal_covariates_wide %>%
  dplyr::select("mean_pub_hosp","mean_pub_prim", "mean_priv_hosp", "mean_priv_prim", "mean_ngo_hosp", "mean_ngo_prim", "mean_hospital", "mean_primary",
         "mean_value_ANC1_coverage_prop", "mean_value_ANC4_coverage_prop", "mean_value_SBA_coverage_prop", "mean_value_IFD_coverage_prop", 
         "mean_value_csection_coverage_prop", "mean_value_he_cap", "mean_value_frac_oop_hexp", "mean_value_haqi", 
         "mean_value_universal_health_coverage", "mean_value_sdi", "mean_value_LDI_pc", "mean_value_TFR", "mean_value_pct_births_in_over35s") %>%
  # rm rows where means are NA
  filter(!is.na(mean_value_ANC1_coverage_prop) & !is.na(mean_value_ANC4_coverage_prop) & !is.na(mean_value_SBA_coverage_prop) & !is.na(mean_value_IFD_coverage_prop) & 
           !is.na(mean_value_csection_coverage_prop) & !is.na(mean_value_he_cap) & !is.na(mean_value_frac_oop_hexp)  & 
           !is.na(mean_value_haqi) & !is.na(mean_value_universal_health_coverage) & !is.na(mean_value_sdi) & !is.na(mean_value_LDI_pc) & !is.na(mean_value_TFR) &
           !is.na(mean_value_pct_births_in_over35s))

cairo_pdf(paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/figures/delivery_location_covariate_correlations_", VERSION, ".pdf"), width=17, height=8.5)
corrplot::corrplot(
  cor(maternal_covariates_corr),
  # method = 'number',
  col = COL2('PiYG'),
  addCoef.col = 'black',
  type = 'lower',
  tl.col = 'black',
  tl.srt = 45,
  tl.cex = 1,
  tl.offset = 0.5,
  # addCoefasPercent = T,
  number.cex = 0.75,
  number.digits = 2,
  diag = F,
  cl.pos = 'n',
  cl.length = 0,
  cl.cex = 0.5
)
mtext("Correlation Coefficients of Key Estimates and Covariates", at=2.5, line=-0.5, cex=1.2)
dev.off()

#### Save dataset with missing covariate nids excluded ####
maternal_covariates_wide_missing_covariates_excluded <- maternal_covariates_wide %>%
  mutate(spec_outlier = case_when(is.na(mean_value_ANC1_coverage_prop) ~ 1,
                                  TRUE ~ spec_outlier),
         level_outlier = case_when(is.na(mean_value_ANC1_coverage_prop) ~ 1,
                                   TRUE ~ level_outlier)) %>%
  dplyr::select("nid", "survey_name", "location_id", "location_name", "location_set_version_id", "location_set_id", "parent_id",                  
         "path_to_top_parent", "level", "is_estimate", "most_detailed", "sort_order", "location_ascii_name",        
         "location_name_short", "location_name_medium", "location_type_id", "location_type", "map_id", "super_region_id",            
         "super_region_name", "region_id", "region_name", "ihme_loc_id", "local_id", "developed", "lancet_label",               
         "who_label", "year_start", "year_end", "age_start", "age_end", "year_id", "survey_module", "file_path", "sample_size_ifd", "sample_size_home", 
         "sample_size_other", "sample_size_unknown", "sample_size_pub_hosp", "sample_size_pub_prim", "sample_size_pub_split", 
         "sample_size_pub_none", "sample_size_priv_hosp", "sample_size_priv_prim", "sample_size_priv_split", "sample_size_priv_none", 
         "sample_size_ngo_hosp", "sample_size_ngo_prim", "sample_size_ngo_split", "sample_size_ngo_none", "sample_size_no_hosp",        
         "sample_size_no_prim", "sample_size_no_split", "sample_size_no_none", "sample_size_public", "sample_size_private", 
         "sample_size_ngo", "sample_size_no_sector", "sample_size_hospital", "sample_size_primary", "sample_size_split", 
         "sample_size_no_level", "nclust_ifd", "nclust_home", "nclust_other", "nclust_unknown", "nclust_pub_hosp", "nclust_pub_prim",
         "nclust_pub_split", "nclust_pub_none", "nclust_priv_hosp", "nclust_priv_prim", "nclust_priv_split", "nclust_priv_none", 
         "nclust_ngo_hosp", "nclust_ngo_prim", "nclust_ngo_split", "nclust_ngo_none", "nclust_no_hosp", "nclust_no_prim", 
         "nclust_no_split", "nclust_no_none", "nclust_public", "nclust_private", "nclust_ngo", "nclust_no_sector", "nclust_hospital",            
         "nclust_primary", "nclust_split", "nclust_no_level", "nstrata_ifd", "nstrata_home", "nstrata_other", "nstrata_unknown", 
         "nstrata_pub_hosp", "nstrata_pub_prim", "nstrata_pub_split", "nstrata_pub_none", "nstrata_priv_hosp", "nstrata_priv_prim", 
         "nstrata_priv_split", "nstrata_priv_none", "nstrata_ngo_hosp", "nstrata_ngo_prim", "nstrata_ngo_split", "nstrata_ngo_none",
         "nstrata_no_hosp", "nstrata_no_prim", "nstrata_no_split", "nstrata_no_none", "nstrata_public", "nstrata_private", "nstrata_ngo", 
         "nstrata_no_sector", "nstrata_hospital", "nstrata_primary", "nstrata_split", "nstrata_no_level", "mean_ifd", "mean_home", 
         "mean_other", "mean_unknown", "original_mean_pub_hosp","original_mean_pub_prim", "mean_pub_split", "mean_pub_none", "mean_priv_hosp", "mean_priv_prim", 
         "mean_priv_split", "mean_priv_none", "mean_ngo_hosp", "mean_ngo_prim", "mean_ngo_split", "mean_ngo_none", "mean_no_hosp",
         "mean_no_prim", "mean_no_split", "mean_no_none", "mean_public", "mean_private", "mean_ngo", "mean_no_sector", "mean_hospital", 
         "mean_primary", "mean_split", "mean_no_level", "standard_error_ifd", "standard_error_home", "standard_error_other", 
         "standard_error_unknown", "standard_error_pub_hosp", "standard_error_pub_prim", "standard_error_pub_split", "standard_error_pub_none", 
         "standard_error_priv_hosp", "standard_error_priv_prim", "standard_error_priv_split", "standard_error_priv_none", "standard_error_ngo_hosp",    
         "standard_error_ngo_prim", "standard_error_ngo_split", "standard_error_ngo_none", "standard_error_no_hosp", "standard_error_no_prim", 
         "standard_error_no_split", "standard_error_no_none", "standard_error_public", "standard_error_private", "standard_error_ngo", 
         "standard_error_no_sector", "standard_error_hospital", "standard_error_primary", "standard_error_split", "standard_error_no_level", 
         "design_effect_ifd", "design_effect_home", "design_effect_other", "design_effect_unknown", "design_effect_pub_hosp", 
         "design_effect_pub_prim", "design_effect_pub_split", "design_effect_pub_none", "design_effect_priv_hosp", "design_effect_priv_prim", 
         "design_effect_priv_split", "design_effect_priv_none", "design_effect_ngo_hosp", "design_effect_ngo_prim", "design_effect_ngo_split",    
         "design_effect_ngo_none", "design_effect_no_hosp", "design_effect_no_prim", "design_effect_no_split", "design_effect_no_none", 
         "design_effect_public", "design_effect_private", "design_effect_ngo", "design_effect_no_sector", "design_effect_hospital", 
         "design_effect_primary", "design_effect_split", "design_effect_no_level", "spec_outlier", "level_outlier",
         "mean_pub_hosp", "mean_pub_prim") %>%
  filter(!is.na(location_id)) %>%
  write_csv(file.path(indir, paste0("/maternal_missing_covariates_excluded_", VERSION, ".csv")))

missing_nids = maternal_covariates_wide_missing_covariates_excluded %>% filter(is.na(location_id))

