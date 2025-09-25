###########################################################################################################
### Author: Will Gardner (wgard@uw.edu)
### Date: 07/31/2019
### Project: ST-GPR
### Purpose: Launch custom first stage prior testing and assemble for upload to ST-GPR
### Overview: This script launches the function launch_prior() that in turn runs two functions: test_prior() and assemble_prior(). 
###           Together they allow the creation of an ensemble linear prior for STGPR.
### test_prior()
###   This function runs all combinations of provided covariates on a provided dataset
###   It ranks each sub-model by out-of-sample predictive validity and marks models containing insignificant betas and/or betas that represent the 'wrong' relationship with your outcome
###   Each added covariate adds a significant number of models to test. Only include covariates that may have a reasonable relationship with your outcome
###   The output is a list that includes a dataframe with a row for each model, the fit betas/SE, and whether or not that model violated significance or prior signs
###
### assemble_prior()
###   This function creates out-of-sample predictive validity-weighted predictions of your outcome using the results from the test_prior() function
###   This can be included as a 'cv_custom_prior' in an STGPR data upload
###   This function can also plot a time-series of predictions for submodels, top weighted model, and the ensemble model
###########################################################################################################

######################################
############### SET-UP ###############
######################################
rm(list=ls())

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

pacman::p_load(ini, yaml)

ROOT = file.path(h,'repos', 'pcp')
STGPR_DIR = file.path(ROOT, 'delivery_location_remapping', 'pipeline', '04_stgpr', 'linear_oos')
settings = yaml.load_file(file.path(ROOT, 'config.yml'))
PREP_VERSION <- '2025-05-20'

# Load Packages and Functions
source(file.path(STGPR_DIR, 'launch.R'))

######################################
############## SET ARGS ##############
######################################

## Required
#don't forget to go into test prior and look at whether to comment out that one line for hosp_any or not
me <- "hosp_any" # "pub_hosp" "priv_hosp" "pub_prim" "priv_prim" "hosp_any"  ## me name or abbreviation for file paths
# "ngo_hosp" & "ngo_prim" are merged with pub_hosp and pub_prim respectively
release_id <- 33        # Release_id (for pulling covariates/metadata)
crosswalk_version_id <- NA    # Crosswalk version id (for pulling input data)
version_label <- paste0('alpha_', Sys.Date()) # Version label for output files
linear_oos_dir <- file.path("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/mcconrad_pre_gpr/", me, '02_linear_oos')
# Path to data (custom data csv if not using a crosswalk version)
path_to_data <- file.path("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/mcconrad_pre_gpr/", me, PREP_VERSION, paste0('delivery_location_', me, '_', PREP_VERSION, '.csv'))

## Functions
test_mods <- TRUE               # Whether to run the test_prior() function.  Not necessary if you've saved a previous run and just want to run assemble_prior()
average <- TRUE                 # Whether to run the assemble_prior() function. Not necessary if you just want to run test_prior()
plot_aves <- FALSE               # Whether to plot diagnostics for model averages
age_trend <- FALSE              # Whether to plot diagnostics for model averages by age instead of year
plot_betas <- TRUE              # Whether to plot diagnostics for ensemble betas

## Model Covariates
cov_ids <- c(
  7,    # ANC1_coverage_prop (Proportion of pregnant women receiving any antenatal care from a skilled provider)
  8,    # ANC4_coverage_prop (Proportion of pregnant women receiving 4 or more antenatal care visits including 1 or more from a skilled provider)
  143,  # SBA_coverage_prop (Percent of women giving birth with a skilled birth attendant (mainly nurses, doctors, midwives))
  2381, # csection_coverage_prop (Proportion of live births delivered by Caesarean Section (c-section))
  
 # 1093, # frac_oop_hexp (Fraction of out-of-pocket health expenditure out of total health expenditure, from FGH April 2019)
 #2525, # domestic_he_cap (Domestic health expenditure per capita, in 2010 international dollars)
  
  1099, # HAQI (Healthcare access and quality index)
  1097, # UHC (Coverage of universal health coverage tracer interventions for prevention and treatment services, percent; created for GBD 2015 SDGs paper.)
  881,  # SDI (Socio-Demographic Index. 	A measure of development estimated via principal component analysis using log-transformed LDI, TFR (<25), and education years per capita over age 15)
  149,  # TFR (Total fertility rate)
  84    # pct_births_in_over35s (Proportion of live births by mothers age 35 and older)
)   # List of covariate IDs to test for predictive validity in ensemble
custom_covs <- NULL             # List of character vectors containing 1) the custom covariate_name_short and 2) the filepath where the custom covariate is found
#     E.g. custom_covs=list(c("hemog_mean", "/share/mnch/st_gpr/custom_covs/hemog_mean.csv"), c("sbr_mean", "/share/mnch/st_gpr/custom_covs/sbr_mean.csv"))

# Custom covariate prior directions should be included in order _after_ the cov_ids covariate
polynoms <- NULL                # String of polynomial transformations for covariates using covariate_name_short (e.g. c("sdi^2"))
ban_pairs <- NULL               # List of covariate IDs you wish to ban from occurring together in a sub-model (list of covariate IDs)
fixed_covs <- NULL              # Any covariates you want included in every model (i.e. age fixed effects)
# random_effects <- c("(1|super_region_name/region_name/location_name)")  # Any random effects you want included in every model
random_effects <- NULL
# Any interaction terms you want to test.  Each interaction term should be a vector of two covariate IDs
interaction_ids <- NULL
# interaction_ids <- list(
#  c(51, 881),  # ifd coverage prop * SDI
  #   c(51, 1097), # ifd coverage prop * UHC
  #   c(51, 1099), # ifd coverage prop * HAQI
#  c(51, 143),  # ifd coverage prop * SBA_coverage_prop
#  c(51, 1089), # ifd coverage prop * Health expenditure (per capita)
#  c(51, 57),   # ifd coverage prop * LDI
  #   c(51, 8),    # ifd coverage prop * ANC_4
#  c(51, 463)   # ifd coverage prop * Maternal education (years per-capita)
# )
# assert that any ids in interaction_ids are also in cov_ids
# for (i in interaction_ids) {
#  if (!all(i %in% cov_ids)) {
#    stop("All covariate IDs in interaction_ids must also be in cov_ids")
#  }
#}

# prior_sign <- c(-1, -1, -1, 1, -1, -1, -1, -1, 1, 1)     
# Prior directions for covariates, in order of list cov_ids, then custom_covs, then interaction_ids
# (-1 is negative correlation, 1 is positive correlation, 0 is no prior)
# prior_sign <- c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
# prior_sign <- c(1, -1, -1, -1, 1, 0, 0, 1, 1, 0, -1, -1, 0, 0)
# prior_sign <- c(1, -1, -1, -1, 1, 0, 0, 1, 0, 1, 0, -1, -1, 0, 0, 0)

# priors for public hospital ensemble model
# prior_sign <- c(1,  # ANC1_coverage_prop
#                1,  # ANC4_coverage_prop
#                1,  # SBA_coverage_prop
#                1,  # csection_coverage_prop
#                1,  # HAQI
#                1,  # UHC_coverage
#                1,  # SDI
#                -1, # TFR
#                -1  # pct_births_in_over35s
# )
# priors for private hospital ensemble model
# prior_sign <- c(1,  # ANC1_coverage_prop
#                1,  # ANC4_coverage_prop
#                1,  # SBA_coverage_prop
#                1,  # csection_coverage_prop
#                1,  # HAQI
#                1,  # UHC_coverage
#                1,  # SDI
#                -1, # TFR
#                -1  # pct_births_in_over35s
# )
# now modeled with pub_hosp
# priors for ngo hospital ensemble model
#prior_sign <- c(1,  # ANC1_coverage_prop
#                0,  # ANC4_coverage_prop
#                0,  # SBA_coverage_prop
#                0,  # csection_coverage_prop
#                -1, # frac_oop_hexp
#                0,  # HAQI
#                0,  # UHC_coverage
#                0,  # SDI
#                0, # TFR
#                0  # pct_births_in_over35s
#)
# priors for public primary ensemble model
# prior_sign <- c(1,  # ANC1_coverage_prop
#                1,  # ANC4_coverage_prop
#                1,  # SBA_coverage_prop
#                0,  # csection_coverage_prop
#                0,  # HAQI
#                0,  # UHC_coverage
#                0,  # SDI
#                0,  # TFR
#                -1   # pct_births_in_over35s
# )
# priors for private primary ensemble model
# prior_sign <- c(1,  # ANC1_coverage_prop
#                1,  # ANC4_coverage_prop
#                1,  # SBA_coverage_prop
#                0,  # csection_coverage_prop
#                0,  # HAQI
#                0,  # UHC_coverage
#                1,  # SDI
#                -1, # TFR
#                1  # pct_births_in_over35s
# )
# now modeled with pub_prim
# priors for ngo primary ensemble model
# prior_sign <- c(0,  # ANC1_coverage_prop
#                0,  # ANC4_coverage_prop
#                0,  # SBA_coverage_prop
#                0,  # csection_coverage_prop
#                -1, # frac_oop_hexp
#                0,  # HAQI
#                0,  # UHC_coverage
#                0,  # SDI
#                0, # TFR
#                0  # pct_births_in_over35s
# )
# priors for hospital of any sector ensemble model
prior_sign <- c(1,  # ANC1_coverage_prop
               1,  # ANC4_coverage_prop
               1,  # SBA_coverage_prop
               1,  # csection_coverage_prop
               1,  # HAQI
               1,  # UHC_coverage
               1,  # SDI
               -1, # TFR
               -1  # pct_births_in_over35s
)
# priors for primary facility of any sector ensemble model
# prior_sign <- c(1,  # ANC1_coverage_prop
#                 1,  # ANC4_coverage_prop
#                 1,  # SBA_coverage_prop
#                 0,  # csection_coverage_prop
#                 0,  # HAQI
#                 0,  # UHC_coverage
#                 1,  # SDI
#                 0, # TFR
#                 1  # pct_births_in_over35s
# )
if (length(prior_sign) != length(cov_ids)+length(custom_covs)+length(interaction_ids)) {
  stop("Length of prior_sign must match length of cov_ids+custom_covs+interaction_ids")
}

## Model Settings
modtype <- "lm"               # Linear model type (lmer if using random effects, lm otherwise)
rank_method <- "oos.rmse"       # OOS error method by which to rank sub-models. Options are Out-of-Sample RMSE ("oos.rmse") or Akaike Information Criterion ("aic")
n_mods <- 50                    # Number of top models to average over. More models is more computationally intensive, and there is a diminishing return
forms_per_job <- 15             # Number of model forms to test for each parallelized cluster job
years <- c(1980:2024)           # Years for which you want to predict out in final stage 1 estimate
by_sex <- FALSE                  # Whether your model is sex-specific
# pred_ages <- c(2:3, 388:389, 238, 34, 6:20, 30:32, 235)
pred_ages <- c(22)         # TODO: Relevant at all for us?
data_transform <- "logit"       # Transform function for your data (usually "logit" for proportions, "log" for continuous variables)
kos <- 5                       # Number of knock-outs for each sub-model
ko_prop <- 0.20                 # Proportion of data you want held out for each ko

## Cluster
proj <- "proj_health_sys"       # Cluster project for launching jobs
m_mem_free <- 3                # Gigabytes of memory per job launched (depends on forms_per_job)
scale_covariates <- FALSE       # Whether to scale covariates before running models

######################################
############### LAUNCH ###############
######################################

launch_prior(me=me, release_id=release_id, crosswalk_version_id=crosswalk_version_id, test_mods=test_mods, path_to_data=path_to_data,
             pred_ages=pred_ages, average=average, n_mods=n_mods, plot_aves=plot_aves, plot_betas=plot_betas, age_trend=age_trend,
             cov_ids=cov_ids, 
             interaction_ids=interaction_ids,
             prior_sign=prior_sign, ban_pairs=ban_pairs, polynoms=polynoms, modtype=modtype,
             rank_method=rank_method, forms_per_job=forms_per_job, drop_nids=FALSE, fixed_covs=fixed_covs,
             custom_covs=custom_covs, random_effects=random_effects, username=username, by_sex=by_sex, years=years, 
             version_label=version_label,
             data_transform=data_transform,
             scale_covariates=scale_covariates)

# END

