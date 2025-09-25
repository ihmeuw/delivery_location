#### Figure 1 for delivery location paper ####
#### written by Chiara Sumich ####
#### 2025-01-03 ####

#### Load packages ####
rm(list = ls())
pacman::p_load(data.table, ggplot2, gtools, tidyverse, haven, labelled, tidyselect, sf, weights, survey, scales, viridis, ggrepel, ggpubr, sp, patchwork,readxl)

#### Defining variables 
VERSION <- "2025-07-14" ## version of scaled st-gpr results
date <- Sys.Date()

#### setting filepaths ####
indir <- file.path("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/post_processing/scaled_data")
outdir <- file.path(paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/figures/", date, "/"))

#### Load functions
source("/snfs1/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2021/inset_maps/gbd2021_map.R")
source('/ihme/cc_resources/libraries/current/r/get_location_metadata.R')
## read in location data and world bank income data
locs <- get_location_metadata(release_id = 16, location_set_id = 22)
wb_groups <- read_excel("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/figures/mcconrad_deliver_location_paper_figures/CLASS.xlsx")
#venezuela is upper middle income
wb_groups <- wb_groups %>% mutate(`Income group` = ifelse(Code == "VEN", "Upper middle income", `Income group`))

#### loading datasets
hosp_any <- read_csv(file.path(indir, paste0("hosp_any_st-gpr_results_scaled_", VERSION, ".csv"))) %>%
  #### keeping only variables that we need to simplify things
  dplyr::select(year_id, age_group_id, sex_id, location_id, mean, lower, upper) %>%
  #### updating variable names so we know which results correspond to each 
  dplyr::rename(hosp_any_mean = "mean",
                hosp_any_lower = "lower",
                hosp_any_upper = "upper")

prim_any <- read_csv(file.path(indir, paste0("prim_any_st-gpr_results_scaled_", VERSION, ".csv"))) %>%
  #### keeping only variables that we need to simplify things
  dplyr::select(year_id, age_group_id, sex_id, location_id, mean, lower, upper) %>%
  #### updating variable names so we know which results correspond to each 
  dplyr::rename(prim_any_mean = "mean",
                prim_any_lower = "lower",
                prim_any_upper = "upper")
  
pub_hosp <- read_csv(file.path(indir, paste0("pub_hosp_st-gpr_results_scaled_", VERSION, ".csv"))) %>%
    #### keeping only variables that we need to simplify things
    dplyr::select(year_id, age_group_id, sex_id, location_id, mean, lower, upper) %>%
    #### updating variable names so we know which results correspond to each 
    dplyr::rename(pub_hosp_mean = "mean",
                  pub_hosp_lower = "lower",
                  pub_hosp_upper = "upper")

pub_prim <- read_csv(file.path(indir, paste0("pub_prim_st-gpr_results_scaled_", VERSION, ".csv"))) %>%
  #### keeping only variables that we need to simplify things
  dplyr::select(year_id, age_group_id, sex_id, location_id, mean, lower, upper) %>%
  #### updating variable names so we know which results correspond to each 
  dplyr::rename(pub_prim_mean = "mean",
                pub_prim_lower = "lower",
                pub_prim_upper = "upper")

priv_hosp <- read_csv(file.path(indir, paste0("priv_hosp_st-gpr_results_scaled_", VERSION, ".csv"))) %>%
  #### keeping only variables that we need to simplify things
  dplyr::select(year_id, age_group_id, sex_id, location_id, mean, lower, upper) %>%
  #### updating variable names so we know which results correspond to each 
  dplyr::rename(priv_hosp_mean = "mean",
                priv_hosp_lower = "lower",
                priv_hosp_upper = "upper")

priv_prim <- read_csv(file.path(indir, paste0("priv_prim_st-gpr_results_scaled_", VERSION, ".csv"))) %>%
  #### keeping only variables that we need to simplify things
  dplyr::select(year_id, age_group_id, sex_id, location_id, mean, lower, upper) %>%
  #### updating variable names so we know which results correspond to each 
  dplyr::rename(priv_prim_mean = "mean",
                priv_prim_lower = "lower",
                priv_prim_upper = "upper")
  
## Pulling ifd st-gpr results
#### Defining variables
gbd_ifd_run_id <- 224626
  
ndraws <- 1000
n_draws<- ndraws-1
draw_names <- paste0("draw_", 0:n_draws)
  
#### Read in the ST-GPR results
gbd_ifd_draw_path <- paste0("/ihme/covariates/ubcov/model/output/", gbd_ifd_run_id, "/draws_temp_0/")


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
  
gbd_ifd_results <- rbindlist(lapply(paste0(gbd_ifd_draw_path, gbd_ifd_files),
                                    FUN = fread_stgpr, 
                                    draw_names = draw_names), use.names=TRUE) 
## keeping only the variables we need to simplify things
gbd_ifd_results <- gbd_ifd_results %>%
  dplyr::select("year_id", "location_id", "sex_id", "age_group_id", "mean", "lower", "upper") %>%
  dplyr::rename(ifd_mean = "mean", 
                ifd_lower = "lower", 
                ifd_upper = "upper")
  
#Merging datasets together, i.e.
#public hospital as share of in facility delivery
pub_hosp_combined <- merge(pub_hosp, gbd_ifd_results, by = c("age_group_id", "location_id","sex_id","year_id")) %>%
  ##divide pub_hosp mean by ifd_mean to get percent public sector hospital deliveries among ifd 
  dplyr::mutate(pub_hosp_among_ifd = pub_hosp_mean/ifd_mean)

#all hospital as share of in facility delivery
hosp_combined <- merge(hosp_any, gbd_ifd_results, by = c("age_group_id", "location_id", "sex_id", "year_id")) %>%
  ##divide hosp_any mean by ifd_mean to get percent hospital deliveries among ifd 
  dplyr::mutate(hosp_among_ifd = hosp_any_mean/ifd_mean)
#all prime as share of infacility delivery
prim_combined <- merge(prim_any, gbd_ifd_results, by = c("age_group_id", "location_id", "sex_id", "year_id")) %>%
  ##divide hosp_any mean by ifd_mean to get percent primary facility deliveries among ifd 
  dplyr::mutate(prim_among_ifd = prim_any_mean/ifd_mean)
#public primary among primary
pubprim_combined <- merge(pub_prim, gbd_ifd_results, by = c("age_group_id","location_id","sex_id","year_id")) %>%
  #divide pub prim mean by ifd mean to get percent public sector primary facility deliveries among ifd
  dplyr::mutate(pubprim_among_ifd = pub_prim_mean/ifd_mean)
#public hospital among hospital
privprim_combined <- merge(priv_prim, gbd_ifd_results) %>%
  #divide pub hosp mean by any_hosp mean to get percent public sector hospital deliveries among all hospital deliveries
  dplyr::mutate(privprim_among_ifd = priv_prim_mean/ifd_mean)
#private hospital among in facility delivery
priv_hosp_combined <- merge(priv_hosp, gbd_ifd_results, by = c("age_group_id", "location_id","sex_id","year_id")) %>%
  ##divide priv_hosp mean by ifd_mean to get percent private sector hospital deliveries among ifd 
  dplyr::mutate(priv_among_ifd = priv_hosp_mean/ifd_mean)

#
# pub_hosp_combined = pub_hosp
# pubprim_combined = pub_prim
# priv_hosp_combined = priv_hosp
# privprim_combined = priv_prim

## merge location data to delivery location aggregates
pub_hosp_combined <- left_join(pub_hosp_combined, locs, by = "location_id")
pub_hosp_combined <- left_join(pub_hosp_combined, wb_groups, by = c("ihme_loc_id" = "Code"))
priv_hosp_combined <- left_join(priv_hosp_combined, locs, by = "location_id")
priv_hosp_combined <- left_join(priv_hosp_combined, wb_groups, by = c("ihme_loc_id" = "Code"))
hosp_combined <- left_join(hosp_combined, locs, by = "location_id")
hosp_combined <- left_join(hosp_combined, wb_groups, by = c("ihme_loc_id" = "Code"))
prim_combined <- left_join(prim_combined, locs, by = "location_id")
prim_combined <- left_join(prim_combined, wb_groups, by = c("ihme_loc_id" = "Code"))
privprim_combined <- left_join(privprim_combined, locs, by = "location_id")
privprim_combined <- left_join(privprim_combined, wb_groups, by = c("ihme_loc_id" = "Code"))
pubprim_combined <- left_join(pubprim_combined, locs, by = "location_id")
pubprim_combined <- left_join(pubprim_combined, wb_groups, by = c("ihme_loc_id" = "Code"))

## creating mapping data sets
#absolute change among ifd
#take public hospital data, filter to 1995 and 2023, and find the absolute change in pub_hosp_among_ifd between the two years for low, lower middle and upper middle income countries
pub_hosp_ifd_change_mapdata <- pub_hosp_combined %>%
  dplyr::select(year_id, location_id, `Income group`, level,pub_hosp_mean) %>%
  filter(year_id == 2023 | year_id == 1995) %>%
  pivot_wider(names_from = year_id, values_from = pub_hosp_mean) %>%
  dplyr::mutate(change = `2023` - `1995`) %>%
  mutate(mapvar = change) %>%
  filter(level == 3) %>%
  filter(location_id != 6) %>%
  filter(`Income group` %in% c("Low income", "Lower middle income", "Upper middle income"))

#take public primary data, filter to 1995 and 2023, and find the absolute change in pub_hosp_among_ifd between the two years for low, lower middle and upper middle income countries
pubprim_ifd_change_mapdata <- pubprim_combined %>%
  dplyr::select(year_id, location_id, `Income group`, level,pub_prim_mean) %>%
  filter(year_id == 2023 | year_id == 1995) %>%
  pivot_wider(names_from = year_id, values_from = pub_prim_mean) %>%
  dplyr::mutate(change = `2023` - `1995`) %>%
  mutate(mapvar = change) %>%
  filter(level == 3) %>%
  filter(location_id != 6) %>%
  filter(`Income group` %in% c("Low income", "Lower middle income", "Upper middle income"))
#take prim combined data, filter to 1995 and 2023, and find the absolute change in pub_hosp_among_ifd between the two years for low, lower middle and upper middle income countries
prim_ifd_change_mapdata <- prim_combined %>%
  dplyr::select(year_id, location_id, `Income group`, level,prim_among_ifd) %>%
  filter(year_id == 2023 | year_id == 1995) %>%
  pivot_wider(names_from = year_id, values_from = prim_among_ifd) %>%
  dplyr::mutate(change = `2023` - `1995`) %>%
  mutate(mapvar = change) %>%
  filter(level == 3) %>%
  filter(location_id != 6) %>%
  filter(`Income group` %in% c("Low income", "Lower middle income", "Upper middle income"))
#take priv prim combined data, filter to 1995 and 2023, and find the absolute change in pub_hosp_among_ifd between the two years for low, lower middle and upper middle income countries
privprim_ifd_change_mapdata <- privprim_combined %>%
  dplyr::select(year_id, location_id, `Income group`, level, priv_prim_mean) %>%
  filter(year_id == 2023 | year_id == 1995) %>%
  pivot_wider(names_from = year_id, values_from = priv_prim_mean) %>%
  dplyr::mutate(change = `2023` - `1995`) %>%
  mutate(mapvar = change) %>%
  filter(level == 3) %>%
  filter(location_id != 6) %>%
  filter(`Income group` %in% c("Low income", "Lower middle income", "Upper middle income"))
#take priv_hosp combined data, filter to 1995 and 2023, and find the absolute change in priv_hosp_among_ifd between the two years for low, lower middle and upper middle income countries
priv_hosp_ifd_change_mapdata <- priv_hosp_combined %>%
  dplyr::select(year_id, location_id, `Income group`, level, priv_hosp_mean) %>%
  filter(year_id == 2023 | year_id == 1995) %>%
  pivot_wider(names_from = year_id, values_from = priv_hosp_mean) %>%
  dplyr::mutate(change = `2023` - `1995`) %>%
  mutate(mapvar = change) %>%
  filter(level == 3) %>%
  filter(location_id != 6) %>%
  filter(`Income group` %in% c("Low income", "Lower middle income", "Upper middle income"))
#take hosp combined data, filter to 1995 and 2023, and find the absolute change in hosp_among_ifd between the two years for low, lower middle and upper middle income countries
hosp_ifd_change_mapdata <- hosp_combined %>%
  dplyr::select(year_id, location_id, `Income group`, level, hosp_among_ifd) %>%
  filter(year_id == 2023 | year_id == 1995) %>%
  pivot_wider(names_from = year_id, values_from = hosp_among_ifd) %>%
  dplyr::mutate(change = `2023` - `1995`) %>%
  mutate(mapvar = change) %>%
  filter(level == 3) %>%
  filter(location_id != 6) %>%
  filter(`Income group` %in% c("Low income", "Lower middle income", "Upper middle income"))


#public hospital among ifd in 2023
pub_hosp_mapdata_2023 = pub_hosp_combined %>%
  dplyr::select(year_id, location_id,`Income group`, pub_hosp_among_ifd) %>%
  dplyr::mutate(mapvar = pub_hosp_among_ifd) %>%
  filter(year_id == 2023) %>%
  filter(location_id != 6) %>%
  filter(`Income group` %in% c("Low income", "Lower middle income", "Upper middle income"))
#private hospital among ifd in 2023
priv_hosp_mapdata_2023 = priv_hosp_combined %>%
  dplyr::select(year_id, location_id,`Income group`, priv_among_ifd) %>%
  dplyr::mutate(mapvar = priv_among_ifd) %>%
  filter(year_id == 2023) %>%
  filter(location_id != 6) %>%
  filter(`Income group` %in% c("Low income", "Lower middle income", "Upper middle income"))
#all primary among ifd in 2023
prim_mapdata_2023 = prim_combined %>%
  dplyr::select(year_id, location_id,`Income group`, prim_among_ifd) %>%
  dplyr::mutate(mapvar = prim_among_ifd) %>%
  filter(year_id == 2023) %>%
  filter(location_id != 6) %>%
  filter(`Income group` %in% c("Low income", "Lower middle income", "Upper middle income"))
pubprim_mapdata_2023 = pubprim_combined %>%
  dplyr::select(year_id, location_id,`Income group`, pubprim_among_ifd) %>%
  dplyr::mutate(mapvar = pubprim_among_ifd) %>%
  filter(year_id == 2023) %>%
  filter(location_id != 6) %>%
  filter(`Income group` %in% c("Low income", "Lower middle income", "Upper middle income"))
hosp_mapdata_2023 = hosp_combined %>%
  dplyr::select(year_id, location_id,`Income group`, hosp_among_ifd) %>%
  dplyr::mutate(mapvar = hosp_among_ifd) %>%
  filter(year_id == 2023) %>%
  filter(location_id != 6) %>%
  filter(`Income group` %in% c("Low income", "Lower middle income", "Upper middle income"))
privprim_mapdata_2023 = privprim_combined %>%
  dplyr::select(year_id, location_id,`Income group`, privprim_among_ifd) %>%
  dplyr::mutate(mapvar = privprim_among_ifd) %>%
  filter(year_id == 2023) %>%
  filter(location_id != 6) %>%
  filter(`Income group` %in% c("Low income", "Lower middle income", "Upper middle income"))

# # hospital deliveries
# hosp_combined <- hosp_combined %>%
#   dplyr::mutate(hosp_among_ifd = case_when(super_region_id == 64 ~ -1, 
#                                            TRUE ~ hosp_among_ifd))
# 
# hosp_mapdata_1990 <- hosp_combined %>%
#   dplyr::select(year_id, location_id, hosp_among_ifd) %>%
#   dplyr::mutate(mapvar = hosp_among_ifd) %>%
#   filter(year_id == 1990)
# 
# hosp_mapdata_1995 <- hosp_combined %>%
#   dplyr::select(year_id, location_id, hosp_among_ifd) %>%
#   dplyr::mutate(mapvar = hosp_among_ifd) %>%
#   filter(year_id == 1995)
# 
# hosp_mapdata_2000 <- hosp_combined %>%
#   dplyr::select(year_id, location_id, hosp_among_ifd) %>%
#   dplyr::mutate(mapvar = hosp_among_ifd) %>%
#   filter(year_id == 2000)
# 
# hosp_mapdata_2005 <- hosp_combined %>%
#   dplyr::select(year_id, location_id, hosp_among_ifd) %>%
#   dplyr::mutate(mapvar = hosp_among_ifd) %>%
#   filter(year_id == 2005)
# 
# hosp_mapdata_2010 <- hosp_combined %>%
#   dplyr::select(year_id, location_id, hosp_among_ifd) %>%
#   dplyr::mutate(mapvar = hosp_among_ifd) %>%
#   filter(year_id == 2010)
# 
# hosp_mapdata_2015 <- hosp_combined %>%
#   dplyr::select(year_id, location_id, hosp_among_ifd) %>%
#   dplyr::mutate(mapvar = hosp_among_ifd) %>%
#   filter(year_id == 2015)
# 
# hosp_mapdata_2020 <- hosp_combined %>%
#   dplyr::select(year_id, location_id, hosp_among_ifd) %>%
#   dplyr::mutate(mapvar = hosp_among_ifd) %>%
#   filter(year_id == 2020)
# 
# hosp_mapdata_2024 <- hosp_combined %>%
#   dplyr::select(year_id, location_id, hosp_among_ifd) %>%
#   dplyr::mutate(mapvar = hosp_among_ifd) %>%
#   filter(year_id == 2024)
# 
# # primary facility deliveries
# prim_combined <- prim_combined%>%
#   dplyr::mutate(prim_among_ifd = case_when(super_region_id == 64 ~ -1, 
#                                            TRUE ~ prim_among_ifd))
# 
# prim_mapdata_1990 <- prim_combined %>%
#   dplyr::select(year_id, location_id, prim_among_ifd) %>%
#   dplyr::mutate(mapvar = prim_among_ifd) %>%
#   filter(year_id == 1990)
# 
# prim_mapdata_1995 <- prim_combined %>%
#   dplyr::select(year_id, location_id, prim_among_ifd) %>%
#   dplyr::mutate(mapvar = prim_among_ifd) %>%
#   filter(year_id == 1995)
# 
# prim_mapdata_2000 <- prim_combined %>%
#   dplyr::select(year_id, location_id, prim_among_ifd) %>%
#   dplyr::mutate(mapvar = prim_among_ifd) %>%
#   filter(year_id == 2000)
# 
# prim_mapdata_2005 <- prim_combined %>%
#   dplyr::select(year_id, location_id, prim_among_ifd) %>%
#   dplyr::mutate(mapvar = prim_among_ifd) %>%
#   filter(year_id == 2005)
# 
# prim_mapdata_2010 <- prim_combined %>%
#   dplyr::select(year_id, location_id, prim_among_ifd) %>%
#   dplyr::mutate(mapvar = prim_among_ifd) %>%
#   filter(year_id == 2010)
# 
# prim_mapdata_2015 <- prim_combined %>%
#   dplyr::select(year_id, location_id, prim_among_ifd) %>%
#   dplyr::mutate(mapvar = prim_among_ifd) %>%
#   filter(year_id == 2015)
# 
# prim_mapdata_2020 <- prim_combined %>%
#   dplyr::select(year_id, location_id, prim_among_ifd) %>%
#   dplyr::mutate(mapvar = prim_among_ifd) %>%
#   filter(year_id == 2020)
# 
# prim_mapdata_2023 <- prim_combined %>%
#   dplyr::select(year_id, location_id, prim_among_ifd) %>%
#   dplyr::mutate(mapvar = prim_among_ifd) %>%
#   filter(year_id == 2023)
# 
# 
# # public sector deliveries
# pub_combined <- pub_combined %>%
#   dplyr::mutate(pub_among_ifd = case_when(super_region_id == 64 ~ -1, 
#                                           TRUE ~ pub_among_ifd))
# 
# pub_mapdata_1990 <- pub_combined %>%
#   dplyr::select(year_id, location_id, pub_among_ifd) %>%
#   dplyr::mutate(mapvar = pub_among_ifd) %>%
#   filter(year_id == 1990)
# 
# pub_mapdata_1995 <- pub_combined %>%
#   dplyr::select(year_id, location_id, pub_among_ifd) %>%
#   dplyr::mutate(mapvar = pub_among_ifd) %>%
#   filter(year_id == 1995)
# 
# pub_mapdata_2000 <- pub_combined %>%
#   dplyr::select(year_id, location_id, pub_among_ifd) %>%
#   dplyr::mutate(mapvar = pub_among_ifd) %>%
#   filter(year_id == 2000)
# 
# pub_mapdata_2005 <- pub_combined %>%
#   dplyr::select(year_id, location_id, pub_among_ifd) %>%
#   dplyr::mutate(mapvar = pub_among_ifd) %>%
#   filter(year_id == 2005)
# 
# pub_mapdata_2010 <- pub_combined %>%
#   dplyr::select(year_id, location_id, pub_among_ifd) %>%
#   dplyr::mutate(mapvar = pub_among_ifd) %>%
#   filter(year_id == 2010)
# 
# pub_mapdata_2015 <- pub_combined %>%
#   dplyr::select(year_id, location_id, pub_among_ifd) %>%
#   dplyr::mutate(mapvar = pub_among_ifd) %>%
#   filter(year_id == 2015)
# 
# pub_mapdata_2020 <- pub_combined %>%
#   dplyr::select(year_id, location_id, pub_among_ifd) %>%
#   dplyr::mutate(mapvar = pub_among_ifd) %>%
#   filter(year_id == 2020)
# 
# pub_mapdata_2024 <- pub_combined %>%
#   dplyr::select(year_id, location_id, pub_among_ifd) %>%
#   dplyr::mutate(mapvar = pub_among_ifd) %>%
#   filter(year_id == 2024)
# 
# # private sector deliveries
# priv_combined <- priv_combined %>%
#   dplyr::mutate(priv_among_ifd = case_when(super_region_id == 64 ~ -1, 
#                                           TRUE ~ priv_among_ifd))
# 
# priv_mapdata_1990 <- priv_combined %>%
#   dplyr::select(year_id, location_id, priv_among_ifd) %>%
#   dplyr::mutate(mapvar = priv_among_ifd) %>%
#   filter(year_id == 1990)
# 
# priv_mapdata_1995 <- priv_combined %>%
#   dplyr::select(year_id, location_id, priv_among_ifd) %>%
#   dplyr::mutate(mapvar = priv_among_ifd) %>%
#   filter(year_id == 1995)
# 
# priv_mapdata_2000 <- priv_combined %>%
#   dplyr::select(year_id, location_id, priv_among_ifd) %>%
#   dplyr::mutate(mapvar = priv_among_ifd) %>%
#   filter(year_id == 2000)
# 
# priv_mapdata_2005 <- priv_combined %>%
#   dplyr::select(year_id, location_id, priv_among_ifd) %>%
#   dplyr::mutate(mapvar = priv_among_ifd) %>%
#   filter(year_id == 2005)
# 
# priv_mapdata_2010 <- priv_combined %>%
#   dplyr::select(year_id, location_id, priv_among_ifd) %>%
#   dplyr::mutate(mapvar = priv_among_ifd) %>%
#   filter(year_id == 2010)
# 
# priv_mapdata_2015 <- priv_combined %>%
#   dplyr::select(year_id, location_id, priv_among_ifd) %>%
#   dplyr::mutate(mapvar = priv_among_ifd) %>%
#   filter(year_id == 2015)
# 
# priv_mapdata_2020 <- priv_combined %>%
#   dplyr::select(year_id, location_id, priv_among_ifd) %>%
#   dplyr::mutate(mapvar = priv_among_ifd) %>%
#   filter(year_id == 2020)
# 
# priv_mapdata_2024 <- priv_combined %>%
#   dplyr::select(year_id, location_id, priv_among_ifd) %>%
#   dplyr::mutate(mapvar = priv_among_ifd) %>%
#   filter(year_id == 2024)
# 
# ## limits and labels
hosp_lims <- c(0, 0.11, 0.21, 0.31, 0.41, 0.51, 0.61, 0.71, 0.81, 0.91,1.01)
hosp_labs <- c("0 - 0.1", "0.11 - 0.2", "0.21 - 0.3", "0.31 - 0.4", "0.41 - 0.5", "0.51 - 0.6", "0.61 - 0.7", "0.71 - 0.8", "0.81 - 0.9","0.91 - 1")

prim_lims <- c(0, 0.11, 0.21, 0.31, 0.41, 0.51, 0.61, 0.71, 0.81, 0.91)
prim_labs <- c("0 - 0.1", "0.11 - 0.2", "0.21 - 0.3", "0.31 - 0.4", "0.41 - 0.5", "0.51 - 0.6", "0.61 - 0.7", "0.71 - 0.8", "0.81 - 0.9")

pub_lims <- c(0, 0.11, 0.21, 0.31, 0.41, 0.51, 0.61, 0.71, 0.81, 0.91, 1.01)
pub_labs <- c("0 - 0.1", "0.11 - 0.2", "0.21 - 0.3", "0.31 - 0.4", "0.41 - 0.5", "0.51 - 0.6", "0.61 - 0.7", "0.71 - 0.8", "0.81 - 0.9", "0.91 - 1.0")

priv_lims <- c(0, 0.11, 0.21, 0.31, 0.41, 0.51, 0.61, 0.71, 0.81, 0.91)
priv_labs <- c("0 - 0.1", "0.11 - 0.2", "0.21 - 0.3", "0.31 - 0.4", "0.41 - 0.5", "0.51 - 0.6", "0.61 - 0.7", "0.71 - 0.8", "0.81 - 0.9")

change_lims <- c(-.51,-.41, -.31, -0.21, -0.11, -0.01, 0.1, 0.21, 0.31, 0.41)
change_labs <- c("-0.5 - -0.41","-.4 - -0.31", "-0.3 - -0.21", "-0.2 - -0.11", "-0.1 - -0.01", "0 - 0.1", "0.11 - 0.2", "0.21 - 0.3", "0.31 - 0.4")

change2_lims <- c(-.41, -.31, -0.21, -0.11, -0.01, 0.1, 0.21, 0.31, 0.41,0.51)
change2_labs <- c("-.4 - -0.31", "-0.3 - -0.21", "-0.2 - -0.11", "-0.1 - -0.01", "0 - 0.1", "0.11 - 0.2", "0.21 - 0.3", "0.31 - 0.4","0.41 - 0.5")
# 
# #cairo_pdf(paste0(outdir, "delivery_location_maps_5year_intervals", date, ".pdf"), width = 17, height = 8.5, onefile = TRUE)
# ## absolute change public hospital hospital deliveries
# gbd_map(data=prim_mapdata,
#         inset = F,
#         sub_nat="none",
#         limits=change_lims, # change to whatever bins make sense for your data
#         label=change_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Absolute change in primary facility deliveries as share of in facility deliveries", # map title
#         legend.title="Change") # save as .tif .eps or .pdf
# ## public hospital hospital deliveries
# ## absolute change public hospital hospital deliveries
# gbd_map(data=pub_hosp_mapdata,
#         inset = F,
#         sub_nat="none",
#         limits=change2_lims, # change to whatever bins make sense for your data
#         label=change2_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Absolute change in public hospital deliveries as share of in facility deliveries", # map title
#         legend.title="Change") # save as .tif .eps or .pdf
# ## public hospital hospital deliveries
# gbd_map(data=pub_hosp_mapdata,
#         inset = F,
#         sub_nat="none",
#         limits=hosp_lims, # change to whatever bins make sense for your data
#         label=hosp_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Public hospital deliveries as share of in facility deliveries 2023", # map title
#         legend.title="Percent public hospital deliveries") # save as .tif .eps or .pdf
# ## hospital deliveries
# gbd_map(data=hosp_mapdata_1990,
#         inset = F,
#         sub_nat="none",
#         limits=hosp_lims, # change to whatever bins make sense for your data
#         label=hosp_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in a hospital 1990", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# gbd_map(data=hosp_mapdata_1995,
#         inset = F,
#         sub_nat="none",
#         limits=hosp_lims, # change to whatever bins make sense for your data
#         label=hosp_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in a hospital 1995", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# gbd_map(data=hosp_mapdata_2000,
#         inset = F,
#         sub_nat="none",
#         limits=hosp_lims, # change to whatever bins make sense for your data
#         label=hosp_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in a hospital 2000", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# gbd_map(data=hosp_mapdata_2005,
#         inset = F,
#         sub_nat="none",
#         limits=hosp_lims, # change to whatever bins make sense for your data
#         label=hosp_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in a hospital 2005", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# gbd_map(data=hosp_mapdata_2010,
#         inset = F,
#         sub_nat="none",
#         limits=hosp_lims, # change to whatever bins make sense for your data
#         label=hosp_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in a hospital 2010", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# gbd_map(data=hosp_mapdata_2015,
#         inset = F,
#         sub_nat="none",
#         limits=hosp_lims, # change to whatever bins make sense for your data
#         label=hosp_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in a hospital 2015", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# gbd_map(data=hosp_mapdata_2020,
#         inset = F,
#         sub_nat="none",
#         limits=hosp_lims, # change to whatever bins make sense for your data
#         label=hosp_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in a hospital 2020", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# gbd_map(data=hosp_mapdata_2024,
#         inset = F,
#         sub_nat="none",
#         limits=hosp_lims, # change to whatever bins make sense for your data
#         label=hosp_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in a hospital 2024", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# ## primary deliveries
# gbd_map(data=prim_mapdata_1990,
#         inset = F,
#         sub_nat="none",
#         limits=hosp_lims, # change to whatever bins make sense for your data
#         label=hosp_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in a primary facility 1990", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# gbd_map(data=prim_mapdata_1995,
#         inset = F,
#         sub_nat="none",
#         limits=hosp_lims, # change to whatever bins make sense for your data
#         label=hosp_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in a primary facility 1995", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# gbd_map(data=prim_mapdata_2000,
#         inset = F,
#         sub_nat="none",
#         limits=hosp_lims, # change to whatever bins make sense for your data
#         label=hosp_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in a primary facility 2000", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# gbd_map(data=prim_mapdata_2005,
#         inset = F,
#         sub_nat="none",
#         limits=hosp_lims, # change to whatever bins make sense for your data
#         label=hosp_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in a primary facility 2005", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# gbd_map(data=prim_mapdata_2010,
#         inset = F,
#         sub_nat="none",
#         limits=hosp_lims, # change to whatever bins make sense for your data
#         label=hosp_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in a primary facility 2010", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# gbd_map(data=prim_mapdata_2015,
#         inset = F,
#         sub_nat="none",
#         limits=hosp_lims, # change to whatever bins make sense for your data
#         label=hosp_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in a primary facility 2015", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# gbd_map(data=prim_mapdata_2020,
#         inset = F,
#         sub_nat="none",
#         limits=hosp_lims, # change to whatever bins make sense for your data
#         label=hosp_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in a primary facility 2020", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# gbd_map(data=prim_mapdata_2023,
#         inset = F,
#         sub_nat="none",
#         limits=hosp_lims, # change to whatever bins make sense for your data
#         label=hosp_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Primary deliveries as share of in facility deliveries 2023", # map title
#         legend.title="Percent primary deliveries") # save as .tif .eps or .pdf
# 
# ## public sector deliveries
# gbd_map(data=pub_mapdata_1990,
#         inset = F,
#         sub_nat="none",
#         limits=pub_lims, # change to whatever bins make sense for your data
#         label=pub_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in a public sector 1990", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# gbd_map(data=pub_mapdata_1995,
#         inset = F,
#         sub_nat="none",
#         limits=pub_lims, # change to whatever bins make sense for your data
#         label=pub_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in a public sector 1995", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# gbd_map(data=pub_mapdata_2000,
#         inset = F,
#         sub_nat="none",
#         limits=pub_lims, # change to whatever bins make sense for your data
#         label=pub_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in a public sector 2000", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# gbd_map(data=pub_mapdata_2005,
#         inset = F,
#         sub_nat="none",
#         limits=pub_lims, # change to whatever bins make sense for your data
#         label=pub_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in a public sector 2005", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# gbd_map(data=pub_mapdata_2010,
#         inset = F,
#         sub_nat="none",
#         limits=pub_lims, # change to whatever bins make sense for your data
#         label=pub_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in a public sector 2010", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# gbd_map(data=pub_mapdata_2015,
#         inset = F,
#         sub_nat="none",
#         limits=pub_lims, # change to whatever bins make sense for your data
#         label=pub_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in a public sector 2015", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# gbd_map(data=pub_mapdata_2020,
#         inset = F,
#         sub_nat="none",
#         limits=pub_lims, # change to whatever bins make sense for your data
#         label=pub_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in public sector 2020", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# gbd_map(data=pub_mapdata_2024,
#         inset = F,
#         sub_nat="none",
#         limits=pub_lims, # change to whatever bins make sense for your data
#         label=pub_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in a public sector 2024", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# ## private sector deliveries
# gbd_map(data=priv_mapdata_1990,
#         inset = F,
#         sub_nat="none",
#         limits=pub_lims, # change to whatever bins make sense for your data
#         label=pub_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in a private sector 1990", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# gbd_map(data=priv_mapdata_1995,
#         inset = F,
#         sub_nat="none",
#         limits=pub_lims, # change to whatever bins make sense for your data
#         label=pub_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in a private sector 1995", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# gbd_map(data=priv_mapdata_2000,
#         inset = F,
#         sub_nat="none",
#         limits=pub_lims, # change to whatever bins make sense for your data
#         label=pub_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in a private sector 2000", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# gbd_map(data=priv_mapdata_2005,
#         inset = F,
#         sub_nat="none",
#         limits=pub_lims, # change to whatever bins make sense for your data
#         label=pub_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in a private sector 2005", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# gbd_map(data=priv_mapdata_2010,
#         inset = F,
#         sub_nat="none",
#         limits=pub_lims, # change to whatever bins make sense for your data
#         label=pub_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in a private sector 2010", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# gbd_map(data=priv_mapdata_2015,
#         inset = F,
#         sub_nat="none",
#         limits=pub_lims, # change to whatever bins make sense for your data
#         label=pub_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in a private sector 2015", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# gbd_map(data=priv_mapdata_2020,
#         inset = F,
#         sub_nat="none",
#         limits=pub_lims, # change to whatever bins make sense for your data
#         label=pub_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in private sector 2020", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# gbd_map(data=priv_mapdata_2024,
#         inset = F,
#         sub_nat="none",
#         limits=pub_lims, # change to whatever bins make sense for your data
#         label=pub_labs, # label bins in the legend
#         col="RdYlBu", # choose palette
#         col.reverse=FALSE, #reverse palette if you want
#         na.color="grey",
#         title="Percent of IFD deliveries in a private sector 2024", # map title
#         legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# dev.off()
# 
# 
# ## paper figure
# hosp <- gbd_map(data=hosp_mapdata_2023,
#                 inset = F,
#                 sub_nat="none",
#                 limits=pub_lims, # change to whatever bins make sense for your data
#                 label=pub_labs, # label bins in the legend
#                 col="RdYlBu", # choose palette
#                 col.reverse=FALSE, #reverse palette if you want
#                 na.color="grey",
#                 fname = paste0(outdir, "hosp_any_map_2023", date, ".pdf"), # save as .tif .eps or .pdf
#                 title="Percentage of IFD deliveries in a hospital 2024", # map title
#                 legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# 
# pub <- gbd_map(data=pub_mapdata_2024,
#               inset = F,
#               sub_nat="none",
#               limits=pub_lims, # change to whatever bins make sense for your data
#               label=pub_labs, # label bins in the legend
#               col="RdYlBu", # choose palette
#               col.reverse=FALSE, #reverse palette if you want
#               na.color="grey",
#               title="Percentage of IFD deliveries in a public sector 2024", # map title
#               legend.title="Percent IFD deliveries") # save as .tif .eps or .pdf
# gbd_map(data=pub_hosp_mapdata_2023,
#                inset = F,
#                sub_nat="none",
#                limits=hosp_lims, # change to whatever bins make sense for your data
#                label=hosp_labs, # label bins in the legend
#                col="RdYlBu", # choose palette
#                col.reverse=FALSE, #reverse palette if you want
#                na.color="grey",
#                title="Public hospital deliveries as share of in facilityscree
#         ", # map title
#                legend.title="Percent public hospital deliveries") # save as .tif .eps or .pdf
# pp_prim <- gbd_map(data=pp_prim_mapdata_2024,
#                   inset = F,
#                   sub_nat="none",
#                   limits=pub_lims, # change to whatever bins make sense for your data
#                   label=pub_labs, # label bins in the legend
#                   col="RdYlBu", # choose palette
#                   col.reverse=FALSE, #reverse palette if you want
#                   na.color="grey",
#                   title="Percentage of primary public deliveries in a primary facility ", # map title
#                   legend.title="Percent public hospital deliveries") # save as .tif .eps or .pdf
# 
# ## this isn't working yet. hosp and pub objects are null after running the above code and I'm not sure why
# # paper_figure <- ggarrange(hosp,
# #                           pub,
# #                           nrow = 1,
# #                           legend = "right") 
# # print(paper_figure)
# # 
# # cairo_pdf(paste0(outdir, "delivery_location_figure_one_", date, ".pdf"), width = 17, height = 8.5, onefile = TRUE)
# # dev.off()


##Absolute change maps for supplement
lims = c(-0.6, -.21,-0.06, 0.06, 0.21, 0.6)
labs = c("-0.2+","-0.2 - -0.05", "-0.05 - 0.05", "0.05 - 0.2", "0.2+")
pubprim_ifd_change_map <- gbd_map(data=pubprim_ifd_change_mapdata,
                   inset = F,
                   sub_nat="none",
                   limits=lims, # change to whatever bins make sense for your data
                   label=labs, # label bins in the legend
                   fname = paste0(outdir, "pubprim_change_map_", date, ".pdf"),
                   col="RdYlBu", # choose palette
                   col.reverse=FALSE, #reverse palette if you want
                   na.color="grey",
                   title="Absolute change in public lower-level deliveries, 1995-2023", # map title
                   legend.title="Change in percent public lower level deliveries") # save as .tif .eps or .pdf
priv_hosp_ifd_change_map <- gbd_map(data=priv_hosp_ifd_change_mapdata,
                   inset = F,
                   sub_nat="none",
                   limits=lims, # change to whatever bins make sense for your data
                   label=labs, # label bins in the legend
                   col="RdYlBu", # choose palette
                   col.reverse=FALSE, #reverse palette if you want
                   na.color="grey",
                   fname = paste0(outdir, "priv_hosp_change_map_", date, ".pdf"),
                   title="Absolute change in private hospital deliveries, 1995-2023", # map title
                   legend.title="Change in percent private hospital deliveries") # save as .tif .eps or .pdf

prim_ifd_change_map <- gbd_map(data=prim_ifd_change_mapdata,
                                    inset = F,
                                    sub_nat="none",
                                    limits=lims, # change to whatever bins make sense for your data
                                    label=labs, # label bins in the legend
                                    col="RdYlBu", # choose palette
                                    col.reverse=FALSE, #reverse palette if you want
                                    na.color="grey",
                                    fname = paste0(outdir, "prim_ifd_change_map_", date, ".pdf"),
                                    title="Absolute change in lower-level deliveries, 1995-2023", # map title
                                    legend.title="Change in percent lower-level deliveries") # save as .tif .eps or .pdf
pub_hosp_ifd_change_map_map <- gbd_map(data=pub_hosp_ifd_change_mapdata,
                   inset = F,
                   sub_nat="none",
                   limits=lims, # change to whatever bins make sense for your data
                   label=labs, # label bins in the legend
                   col="RdYlBu", # choose palette
                   col.reverse=FALSE, #reverse palette if you want
                   na.color="grey",
                   fname = paste0(outdir, "pub_hosp_change_map_", date, ".pdf"),
                   title="Absolute change in public hospital deliveries, 1995-2023", # map title
                   legend.title="Change in percent public hospital deliveries") # save as .tif .eps or .pdf
privprim_ifd_change_map <- gbd_map(data=privprim_ifd_change_mapdata,
                   inset = F,
                   sub_nat="none",
                   limits=lims, # change to whatever bins make sense for your data
                   label=labs, # label bins in the legend
                   col="RdYlBu", # choose palette
                   col.reverse=FALSE, #reverse palette if you want
                   na.color="grey",
                   fname = paste0(outdir, "privprim_change_map_", date, ".pdf"),
                   title="Absolute change in private lower-level deliveries, 1995-2023", # map title
                   legend.title="Change in percent private lower-level deliveries") # save as .tif .eps or .pdf
hosp_ifd_change_map <- gbd_map(data=hosp_ifd_change_mapdata,
                   inset = F,
                   sub_nat="none",
                   limits=lims, # change to whatever bins make sense for your data
                   label=labs, # label bins in the legend
                   col="RdYlBu", # choose palette
                   col.reverse=FALSE, #reverse palette if you want
                   na.color="grey",
                   fname = paste0(outdir, "hosp_ifd_change_map_", date, ".pdf"),
                   title="Absolute change in hospital deliveries, 1995-2023", # map title
                   legend.title="Change in percent hospital deliveries") # save as .tif .eps or .pdf

# priv_lims <- c(0, 0.11, 0.21, 0.31, 0.41, 0.51, 0.61, 0.71)
# priv_labs <- c("0 - 0.1", "0.11 - 0.2", "0.21 - 0.3", "0.31 - 0.4", "0.41 - 0.5", "0.51 - 0.6", "0.61 - 0.7")
# gbd_map(data=priv_hosp_mapdata_2023,
#                    inset = F,
#                    sub_nat="none",
#                    limits=hosp_lims, # change to whatever bins make sense for your data
#                    label=hosp_labs, # label bins in the legend
#                    col="RdYlBu", # choose palette
#                    col.reverse=FALSE, #reverse palette if you want
#                    na.color="grey",
#                    #title="Percent of IFD deliveries in a public hospital 2024", # map title
#                    legend.title="Percent private hospital deliveries") # save as .tif .eps or .pdf
# prim_lims <- c(0, 0.11, 0.21, 0.31, 0.41, 0.51, 0.61, 0.71, 0.81, 0.91)
# prim_labs <- c("0 - 0.1", "0.11 - 0.2", "0.21 - 0.3", "0.31 - 0.4", "0.41 - 0.5", "0.51 - 0.6", "0.61 - 0.7", "0.71 - 0.8", "0.81 - 0.9")
# 
# prim_2023_map <- gbd_map(data=privprim_mapdata_2023,
#                    inset = F,
#                    sub_nat="none",
#                    limits=hosp_lims, # change to whatever bins make sense for your data
#                    label=hosp_labs, # label bins in the legend
#                    col="RdYlBu", # choose palette
#                    col.reverse=FALSE, #reverse palette if you want
#                    na.color="grey",
#                    #title="Percent of IFD deliveries in a public hospital 2024", # map title
#                    legend.title="Percent private lower-level deliveries") # save as .tif .eps or .pdf
# 
# pub_lims <- c(0, 0.11, 0.21, 0.31, 0.41, 0.51, 0.61, 0.71, 0.81, 0.91, 1.01)
# pub_labs <- c("0 - 0.1", "0.11 - 0.2", "0.21 - 0.3", "0.31 - 0.4", "0.41 - 0.5", "0.51 - 0.6", "0.61 - 0.7", "0.71 - 0.8", "0.81 - 0.9", "0.91 - 1.0")
# pub_hosp_all_2023_map <- gbd_map(data=pub_hosp_mapdata_2023_mean,
#                    inset = F,
#                    sub_nat="none",
#                    limits=pub_lims, # change to whatever bins make sense for your data
#                    label=pub_labs, # label bins in the legend
#                    col="RdYlBu", # choose palette
#                    col.reverse=FALSE, #reverse palette if you want
#                    na.color="grey",
#                    #title="Percent of IFD deliveries in a public hospital 2024", # map title
#                    legend.title="Percent public hospital deliveries") # save as .tif .eps or .pdf
# 
# priv_hosp_all_2023_map <- gbd_map(data=priv_hosp_mapdata_2023_mean,
#                    inset = F,
#                    sub_nat="none",
#                    limits=priv_lims, # change to whatever bins make sense for your data
#                    label=priv_labs, # label bins in the legend
#                    col="RdYlBu", # choose palette
#                    col.reverse=FALSE, #reverse palette if you want
#                    na.color="grey",
#                    #title="Percent of IFD deliveries in a public hospital 2024", # map title
#                    legend.title="Percent private hospital deliveries") # save as .tif .eps or .pdf
# prim_all_2023_map <- gbd_map(data=pubprim_mapdata_2023,
#                    inset = F,
#                    sub_nat="none",
#                    limits=pub_lims, # change to whatever bins make sense for your data
#                    label=pub_labs, # label bins in the legend
#                    col="RdYlBu", # choose palette
#                    col.reverse=FALSE, #reverse palette if you want
#                    na.color="grey",
#                    #title="Percent of IFD deliveries in a public hospital 2024", # map title
#                    legend.title="Percent primary deliveries") # save as .tif .eps or .pdf
# change3_lims <- c(-.31, -0.21, -0.11, -0.01, 0.1, 0.21, 0.31, 0.41,0.51, 0.61)
# change3_labs <- c("-0.3 - -0.21", "-0.2 - -0.11", "-0.1 - -0.01", "0 - 0.1", "0.11 - 0.2", "0.21 - 0.3", "0.31 - 0.4","0.41 - 0.5", "0.51 - 0.6")
# 
# pub_hosp_all_change <- gbd_map(data=pub_hosp_change_mapdata,
#                    inset = F,
#                    sub_nat="none",
#                    limits=change3_lims, # change to whatever bins make sense for your data
#                    label=change3_labs, # label bins in the legend
#                    col="RdYlBu", # choose palette
#                    col.reverse=FALSE, #reverse palette if you want
#                    na.color="grey",
#                    #title="Percent of IFD deliveries in a public hospital 2024", # map title
#                    legend.title="Percent public hospital deliveries") # save as .tif .eps or .pdf
# 
# lims = c(-0.2,-0.1, 0, 0.1, 0.2,0.3)
# labs = c("-0.2 - -0.1", "-0.1 - 0", "0 - 0.1", "0.1 - 0.2", "0.2 - 0.3")
# priv_hosp_all_change <- gbd_map(data=priv_hosp_change_mapdata,
#                    inset = F,
#                    sub_nat="none",
#                    limits=lims, # change to whatever bins make sense for your data
#                    label=labs, # label bins in the legend
#                    col="RdYlBu", # choose palette
#                    col.reverse=FALSE, #reverse palette if you want
#                    na.color="grey",
#                    #title="Percent of IFD deliveries in a public hospital 2024", # map title
#                    legend.title="Percent private hospital deliveries") # save as .tif .eps or .pdf
# 
# lims = c(-0.4,-0.3,-0.2,-0.1, 0, 0.1, 0.2,0.3,0.4,0.5,0.6)
# labs = c("-0.4 - -0.3","-0.3 - -0.2","-0.2 - -0.1", "-0.1 - 0", "0 - 0.1", "0.1 - 0.2", "0.2 - 0.3", "0.3 - 0.4", "0.4 - 0.5", "0.5 - 0.6")
# prim_hosp_all_change <- gbd_map(data=prim_change_mapdata,
#                    inset = F,
#                    sub_nat="none",
#                    limits=lims, # change to whatever bins make sense for your data
#                    label=labs, # label bins in the legend
#                    col="RdYlBu", # choose palette
#                    col.reverse=FALSE, #reverse palette if you want
#                    na.color="grey",
#                    #title="Percent of IFD deliveries in a public hospital 2024", # map title
#                    legend.title="Percent primary deliveries") # save as .tif .eps or .pdf


### For paper figure 1: pub_hosp, priv_hosp, pub_primary
library(patchwork)
cairo_pdf(paste0(outdir, "Figure_1_", date, ".pdf"), width = 17, height = 50, onefile = TRUE)
pub_hosp_2023_map = gbd_map(data=pub_hosp_mapdata_2023,
                             inset = F,
                             sub_nat="none",
                             limits=pub_lims, # change to whatever bins make sense for your data
                             label=pub_labs, # label bins in the legend
                             col="Blues", # choose palette
                             col.reverse=FALSE, #reverse palette if you want
                             na.color="grey",
                             title="Figure 1a: Public and non-profit hospital deliveries as a share of facility deliveries, 2023", # map title
                             legend.title="Percent public hospital deliveries",
        fname = paste0(outdir, "Figure_1a", date, ".pdf"))
         # save as .tif .eps or .pdf
priv_hosp_2023_map = gbd_map(data=priv_hosp_mapdata_2023,
                              inset = F,
                              sub_nat="none",
                              limits=pub_lims, # change to whatever bins make sense for your data
                              label=pub_labs, # label bins in the legend
                              col="Blues", # choose palette
                              col.reverse=FALSE, #reverse palette if you want
                              na.color="grey",
                              title="Figure 1b: Private for-profit hospital deliveries as a share of facility deliveries, 2023", # map title
                              legend.title="Percent private hospital deliveries",
        fname = paste0(outdir, "Figure_1b", date, ".pdf")) # save as .tif .eps or .pdf
prim_2023_map = gbd_map(data=pubprim_mapdata_2023,
                         inset = F,
                         sub_nat="none",
                         limits=pub_lims, # change to whatever bins make sense for your data
                         label=pub_labs, # label bins in the legend
                         col="RdPu", # choose palette
                         col.reverse=FALSE, #reverse palette if you want
                         na.color="grey",
                         title="Figure 1c: Public and non-profit lower-level deliveries as a share of facility deliveries, 2023", # map title
                         legend.title="Percent public lower-level deliveries",
        fname = paste0(outdir, "Figure_1c", date, ".pdf")) # save as .tif .eps or .pdf
paper_figure <- pub_hosp_2023_map / priv_hosp_2023_map / prim_2023_map
paper_figure = paper_figure + plot_annotation(title = "Figure 1: Delivery location as a share of facility deliveries, 2023")
print(paper_figure)
dev.off()

### private primary map for supplement:
prim_2023_map <- gbd_map(data=privprim_mapdata_2023,
                                            inset = F,
                                            sub_nat="none",
                                            limits=pub_lims, # change to whatever bins make sense for your data
                                            label=pub_labs, # label bins in the legend
                                            col="RdPu", # choose palette
                                            col.reverse=FALSE, #reverse palette if you want
                                            na.color="grey",
                                            fname = paste0(outdir, "privprim_2023_map_", date, ".pdf"), # save as .tif .eps or .pdf
                                            title="Private lower-level deliveries as share of facility deliveries, 2023", # map title
                                            legend.title="Percent private lower-level deliveries") # save as .tif .eps or .pdf

