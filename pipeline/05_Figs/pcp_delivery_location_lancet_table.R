##################################
## lancet table creation        #
##################################

rm(list = ls())

#libraries and packages
repo_dir <- paste0("/homes/", Sys.getenv("USER"), "/repos/0table-and-figure-templates/tables/lancet_table/")
source(paste0(repo_dir, "as_lancet_table.R"))
source('/ihme/cc_resources/libraries/current/r/get_location_metadata.R')
source('/ihme/cc_resources/libraries/current/r/get_covariate_estimates.R')
library(data.table)
library(tidyverse)
library(openxlsx)
library(dplyr)
library(extrafont)
library(readxl)
loadfonts()
f1 <- "Times"
f2 <- "ScalaLancetPro"
f3 <- "Shaker 2 Lancet Regular"

locs <- get_location_metadata(release_id = 16, location_set_id = 35)
locs = locs %>% dplyr::select(location_id,location_name,ihme_loc_id,level,super_region_name,super_region_id,sort_order)

date <- Sys.Date()
indir <- file.path("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/post_processing/aggregates")
outdir <- paste0("/share/scratch/projects/hssa/pcp/delivery_location_remapping/figures/", date, "/")
VERSION <- "2025-07-16" # version of aggregated ST-GPR results

#get data: want all six models, plus our created IFD
pub_hosp_lmic = read_csv(paste0(indir, "/pub_hosp_bothsteps_st-gpr_results_weighted_aggregates_lmics_", VERSION, ".csv"))
pub_hosp_lmic = pub_hosp_lmic %>% 
  filter(year_id %in% c(1995, 2023)) %>% 
  #filter(!is.na(ihme_loc_id))%>% 
  #dplyr::select(location_id, year_id, mean, lower, upper)%>%
  dplyr::rename( mean_pub_hosp = mean, lower_pub_hosp = lower, upper_pub_hosp = upper)
#read in private hospital among lmics
priv_hosp_lmic = read_csv(paste0(indir, "/priv_hosp_bothsteps_st-gpr_results_weighted_aggregates_lmics_", VERSION, ".csv"))
priv_hosp_lmic = priv_hosp_lmic %>% 
  filter(year_id %in% c(1995, 2023)) %>% 
 # dplyr::select(location_id, year_id, mean, lower, upper) %>%
  dplyr::rename( mean_priv_hosp = mean, lower_priv_hosp = lower, upper_priv_hosp = upper)
#read in priv prim among lmics
priv_prim_lmic = read_csv(paste0(indir, "/priv_prim_bothsteps_st-gpr_results_weighted_aggregates_lmics_", VERSION, ".csv"))
priv_prim_lmic = priv_prim_lmic %>% 
  filter(year_id %in% c(1995, 2023)) %>% 
 # dplyr::select(location_id, year_id, mean, lower, upper) %>%
  dplyr::rename( mean_priv_prim = mean, lower_priv_prim = lower, upper_priv_prim = upper)
#read in public prim any among lmics
pub_prim_lmic = read_csv(paste0(indir, "/pub_prim_bothsteps_st-gpr_results_weighted_aggregates_lmics_", VERSION, ".csv"))
pub_prim_lmic = pub_prim_lmic %>% 
  filter(year_id %in% c(1995, 2023)) %>% 
 # dplyr::select(location_id, year_id, mean, lower, upper) %>%
  dplyr::rename( mean_pub_prim = mean, lower_pub_prim = lower, upper_pub_prim = upper)
# #read in hospital any among lmics
# hosp_any_lmic = read_csv(paste0(indir, "/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/mcconrad_post_gpr/scaled data/hosp_any_st-gpr_results_scaled_2025-05-12.csv"))
# hosp_any_lmic = hosp_any_lmic %>% 
#   filter(year_id == 2023) %>% 
#   dplyr::select(location_id, year_id, mean, lower, upper) %>%
#   dplyr::rename( mean_hosp_any = mean, lower_hosp_any = lower, upper_hosp_any = upper)
# #read in prim any among lmics
# prim_any_lmic = read_csv(paste0(indir, "/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/mcconrad_post_gpr/scaled data/prim_any_st-gpr_results_scaled_2025-05-12.csv"))
# prim_any_lmic = prim_any_lmic %>% 
#   filter(year_id == 2023) %>% 
#   dplyr::select(location_id, year_id, mean, lower, upper) %>%
#   dplyr::rename( mean_prim_any = mean, lower_prim_any = lower, upper_prim_any = upper)

#make IFD table
dts <- list(pub_prim_lmic, pub_hosp_lmic, priv_prim_lmic, priv_hosp_lmic)
me_ids <- c("pub_prim",    "pub_hosp",    "priv_prim",    "priv_hosp")
keyvars  <- c("location_id","age_group_id","sex_id","year_id","me_id")
# 2. define the draw columns
draw_cols <- paste0("draw_", 0:999)

# 3. extract just the keys + draw columns from each and rbind them together,
#    tagging by facility type so we can sum later
dts_tagged <- lapply(seq_along(dts), function(i) {
  dt <- as.data.table(dts[[i]])      
  dt[, me_id := me_ids[i]]           
  dt
})
ifd_long <- rbindlist(lapply(dts_tagged, function(dt) {
  # now dt is a data.table, so data.table::melt will be used
  data.table::melt(
    dt,
    id.vars        = keyvars,
    measure.vars   = draw_cols,
    variable.name  = "draw",
    value.name     = "val"
  )
}), use.names = TRUE)

# sum across facility types to get total IFD per draw
ifd_sum <- ifd_long[
  , .(ifd = sum(val)), 
  by=.(location_id, age_group_id, sex_id, year_id, draw)
]
ifd_sum = ifd_sum %>% mutate(non_ifd = 1-ifd) # get non facility 
# pivot wider so that each row–year has one column per draw
ifd_sum = ifd_sum %>%
  pivot_wider(
    names_from = draw,
    id_cols = c("location_id","year_id"),
    values_from = non_ifd
  )

#get mean, lower, and upper
cols <- grep("draw_", names(ifd_sum), value=TRUE)
ifd_sum = as.data.table(ifd_sum)
ifd_sum <- ifd_sum[, mean_value := rowMeans(.SD), .SDcols=cols]
ifd_sum$lower_value <- apply(ifd_sum[, (cols), with=F], 1, quantile, probs=0.025)
ifd_sum$upper_value <- apply(ifd_sum[, (cols), with=F], 1, quantile, probs=0.975)

#filter to just mean lower upper
pub_hosp_lmic = pub_hosp_lmic %>% 
  filter(year_id %in% c(1995, 2023)) %>% 
  dplyr::select(location_id, year_id, mean_pub_hosp, lower_pub_hosp, upper_pub_hosp)
priv_hosp_lmic = priv_hosp_lmic %>%
  filter(year_id %in% c(1995, 2023)) %>% 
  dplyr::select(location_id, year_id, mean_priv_hosp, lower_priv_hosp, upper_priv_hosp)
priv_prim_lmic = priv_prim_lmic %>%
  filter(year_id %in% c(1995, 2023)) %>% 
  dplyr::select(location_id, year_id, mean_priv_prim, lower_priv_prim, upper_priv_prim)
pub_prim_lmic = pub_prim_lmic %>%
  filter(year_id %in% c(1995, 2023)) %>% 
  dplyr::select(location_id, year_id, mean_pub_prim, lower_pub_prim, upper_pub_prim)
ifd_sum = ifd_sum %>%
  dplyr::select(location_id, year_id, mean_value, lower_value, upper_value)

#merge all the data together
maternal = pub_hosp_lmic %>%
  dplyr::left_join(priv_hosp_lmic, by = c("location_id", "year_id")) %>%
  dplyr::left_join(priv_prim_lmic, by = c("location_id", "year_id")) %>%
  dplyr::left_join(pub_prim_lmic, by = c("location_id", "year_id")) %>%
  dplyr::left_join(ifd_sum, by = c("location_id", "year_id")) %>%
  dplyr::left_join(locs, by = c("location_id")) 
#multiply all columns by 100
maternal = maternal %>%
  dplyr::mutate(mean_pub_hosp = round(mean_pub_hosp * 100, 1),
         lower_pub_hosp = round(lower_pub_hosp * 100, 1),
         upper_pub_hosp = round(upper_pub_hosp * 100, 1),
         mean_priv_hosp = round(mean_priv_hosp * 100, 1),
         lower_priv_hosp = round(lower_priv_hosp * 100, 1),
         upper_priv_hosp = round(upper_priv_hosp * 100, 1),
         mean_pub_prim = round(mean_pub_prim * 100, 1),
         lower_pub_prim = round(lower_pub_prim * 100, 1),
         upper_pub_prim = round(upper_pub_prim * 100, 1),
         mean_priv_prim = round(mean_priv_prim * 100, 1),
         lower_priv_prim = round(lower_priv_prim * 100, 1),
         upper_priv_prim = round(upper_priv_prim * 100, 1),
         mean_value = round(mean_value * 100, 1),
         lower_value = round(lower_value * 100, 1),
         upper_value = round(upper_value * 100, 1))
##check
check = maternal %>% mutate(sum1 = mean_pub_hosp + mean_priv_hosp + mean_pub_prim +mean_priv_prim+mean_value)
#add in world bank data an dfilter out high income countries
wb_data = read_excel("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/figures/mcconrad_deliver_location_paper_figures/CLASS.xlsx")
# add in venezuela
wb_data = wb_data %>% dplyr:: mutate(`Income group` = if_else(Code == "VEN", "Upper middle income", `Income group`))

maternal = maternal %>%
  dplyr::left_join(wb_data, by = c("ihme_loc_id" = "Code")) %>%
  dplyr::filter(location_name != "China") %>%
  dplyr::filter(location_id == 1 | level == 1 | (level == 3 &`Income group` != "High income")) %>%
  dplyr::mutate(super_region_name = if_else(ihme_loc_id == "ARG","Latin America and Caribbean", super_region_name)) %>%
  dplyr::filter(super_region_name != "High-income")

v1995<- subset(maternal, year_id == 1995)
maternal<- subset(maternal, year_id == 2023)
#rename the variable to be lancet table friendly, arrange the variables so it's pub hospital, priv hospital, public primary, private primary, hospital any, prim any, and then IFD
maternal = maternal %>%
  dplyr::select(-c(location_id,Economy,Region,ihme_loc_id,`Income group`,`Lending category`, year_id))
maternal = maternal %>%
  dplyr::rename(
    Country = location_name,
    `val.Public and private non-profit hospital` = mean_pub_hosp,
    `upper.Public and private non-profit hospital` = upper_pub_hosp,
    `lower.Public and private non-profit hospital` = lower_pub_hosp,
    `val.Private for-profit hospital` = mean_priv_hosp,
    `upper.Private for-profit hospital` = upper_priv_hosp,
    `lower.Private for-profit hospital` = lower_priv_hosp,
    `val.Public and private non-profit lower-level` = mean_pub_prim,
    `upper.Public and private non-profit lower-level` = upper_pub_prim,
    `lower.Public and private non-profit lower-level` = lower_pub_prim,
    `val.Private for-profit lower-level` = mean_priv_prim,
    `upper.Private for-profit lower-level` = upper_priv_prim,
    `lower.Private for-profit lower-level` = lower_priv_prim,
    `val.Non-facility` = mean_value,
    `upper.Non-facility` = upper_value,
    `lower.Non-facility` = lower_value) %>%
 dplyr::select(Country,level,
                `val.Public and private non-profit hospital`,
                `upper.Public and private non-profit hospital`,
                `lower.Public and private non-profit hospital`,
                `val.Private for-profit hospital`,
                `upper.Private for-profit hospital`,
                `lower.Private for-profit hospital`,
                `val.Public and private non-profit lower-level`,
                `upper.Public and private non-profit lower-level`,
                `lower.Public and private non-profit lower-level`,
                `val.Private for-profit lower-level`,
                `upper.Private for-profit lower-level`,
                `lower.Private for-profit lower-level`,
                `val.Non-facility`, 
                `upper.Non-facility`, 
                `lower.Non-facility`)
df_ordered <- maternal %>%
  # Assign sort order: 1 = region, 2 = country
  dplyr::mutate(
    sort_order = ifelse(level == 1, 1, 2),
    region = ifelse(level == 1, Country, NA)
  ) %>%
  # Fill region name *downward* so countries inherit it
  fill(region, .direction = "down") %>%
  # Custom country sort (Argentina first in its region)
  dplyr::mutate(country_sort = ifelse(Country == "Argentina", 0, 1)) %>%
  # Final sort: by region (alphabetically), then region before countries, then Argentina-first, then name
  dplyr::arrange(region, sort_order, country_sort, Country) %>%
  dplyr::select(-country_sort) %>%
  dplyr::mutate(sort_order = row_number()) %>%
  dplyr::mutate(sort_order = if_else(Country == "Argentina", 20.5,sort_order)) %>%
  dplyr::mutate(sort_order = if_else(Country == "Global",0.5,sort_order))%>%
  dplyr::mutate(Country = if_else(Country == "Global", "All study countries", Country)) %>%
  dplyr::arrange(sort_order) %>%
  dplyr::select(-sort_order,-level,-region) %>%
  dplyr::rename(Location = Country)
check = df_ordered %>% 
  dplyr::mutate(sum1 = `val.Public and private non-profit hospital` + `val.Private for-profit hospital` + `val.Public and private non-profit lower-level` + `val.Private for-profit lower-level` + `val.Non-facility`) %>%
  dplyr::select(Location, sum1)

as_lancet_table(df_ordered,
                label = "Location",
                with.ui = T,
                hierarchy = F,
                sdi = F,
                alternate = T,
                rounded = F,
                lancet_font = T,
                non.numeric = F,
                outfile = paste0(outdir, "table_1.xlsx"))

####another table with births
birth_dt <- get_covariate_estimates(covariate_id = 60, release_id = 16)
birth_dt <- birth_dt[, c("location_id", "year_id", "mean_value")]
setnames(birth_dt, "mean_value", "births")
## we only need births from 2023 
birth_dt <- birth_dt %>%
  filter(year_id == 2023)
birth_dt = left_join(birth_dt,locs)
birth_dt = left_join(birth_dt,wb_data, by = c("ihme_loc_id" = "Code"))
birth_dt = birth_dt %>% mutate(`Income group` = if_else(ihme_loc_id == "VEN", "Upper middle income", `Income group`))
birth_dt = birth_dt %>% filter(`Income group` %in% c("Upper middle income","Low income","Lower middle income"))
birth_dt = birth_dt %>% mutate(super_region_name = if_else(ihme_loc_id == "ARG","Latin America and Caribbean", super_region_name)) %>%
  filter(super_region_name != "High-income") %>% filter(location_id != 6)
global = sum(birth_dt$births)
global_row = data.table(year_id = 2023, location_id = 1,births = global)
super_region = birth_dt %>% 
  dplyr::group_by(super_region_name,super_region_id) %>% 
  dplyr::summarise(births = sum(births)) %>% dplyr::ungroup() %>% 
  dplyr::mutate(year_id = 2023) %>%
  dplyr::rename(location_id = super_region_id) %>%
  dplyr::select(-super_region_name)
birth_dt = birth_dt %>% select(location_id,year_id,births)

birth_dt = rbind(birth_dt, global_row)
birth_dt = rbind(birth_dt, super_region)

##Run the beginning up to the end of IFD sum (line 104)
#join all tables by births and multiply draws
draw_cols <- paste0("draw_", 0:999)
ifd_sum = ifd_sum %>%
  dplyr::left_join(birth_dt, by = c("location_id", "year_id")) 
ifd_sum = ifd_sum %>% dplyr::filter(!is.na(births))
ifd_sum = as.data.table(ifd_sum)
ifd_sum <- ifd_sum[, (draw_cols) := lapply(.SD, function(x) x * births), .SDcols=draw_cols]

pub_hosp_lmic = pub_hosp_lmic %>%
  left_join(locs) %>%
  dplyr::filter(level %in% c(0,1,3)) %>%
  dplyr::left_join(birth_dt, by = c("location_id", "year_id")) %>%
  filter(!is.na(births))
pub_hosp_lmic = as.data.table(pub_hosp_lmic)
pub_hosp_lmic <- pub_hosp_lmic[, (draw_cols) := lapply(.SD, function(x) x * births), .SDcols=draw_cols]

priv_hosp_lmic = priv_hosp_lmic %>%
  left_join(locs) %>%
  dplyr::filter(level %in% c(0,1,3)) %>%
  dplyr::left_join(birth_dt, by = c("location_id", "year_id"))%>%
  filter(!is.na(births))
priv_hosp_lmic = as.data.table(priv_hosp_lmic)
priv_hosp_lmic <- priv_hosp_lmic[, (draw_cols) := lapply(.SD, function(x) x * births), .SDcols=draw_cols]

priv_prim_lmic = priv_prim_lmic %>%
  left_join(locs) %>%
  dplyr::filter(level %in% c(0,1,3)) %>%
  dplyr::left_join(birth_dt, by = c("location_id", "year_id"))%>%
  filter(!is.na(births))
priv_prim_lmic = as.data.table(priv_prim_lmic)
priv_prim_lmic <- priv_prim_lmic[, (draw_cols) := lapply(.SD, function(x) x * births), .SDcols=draw_cols]

pub_prim_lmic = pub_prim_lmic %>%
  left_join(locs) %>%
  dplyr::filter(level %in% c(0,1,3)) %>%
  dplyr::left_join(birth_dt, by = c("location_id", "year_id"))%>%
  filter(!is.na(births))
pub_prim_lmic = as.data.table(pub_prim_lmic)
pub_prim_lmic <- pub_prim_lmic[, (draw_cols) := lapply(.SD, function(x) x * births), .SDcols=draw_cols]

#get mean lower and upper
cols <- grep("draw_", names(ifd_sum), value=TRUE)
ifd_sum = as.data.table(ifd_sum)
ifd_sum <- ifd_sum[, mean_value := rowMeans(.SD), .SDcols=cols]
ifd_sum$lower_value <- apply(ifd_sum[, (cols), with=F], 1, quantile, probs=0.025)
ifd_sum$upper_value <- apply(ifd_sum[, (cols), with=F], 1, quantile, probs=0.975)
ifd_sum = ifd_sum %>%
  dplyr::select(location_id, year_id, mean_value, lower_value, upper_value)
pub_hosp_lmic = as.data.table(pub_hosp_lmic)
pub_hosp_lmic <- pub_hosp_lmic[, mean_pub_hosp := rowMeans(.SD), .SDcols=cols]
pub_hosp_lmic$lower_pub_hosp <- apply(pub_hosp_lmic[, (cols), with=F], 1, quantile, probs=0.025)
pub_hosp_lmic$upper_pub_hosp <- apply(pub_hosp_lmic[, (cols), with=F], 1, quantile, probs=0.975)
pub_hosp_lmic = pub_hosp_lmic %>%
  dplyr::select(location_id, year_id, mean_pub_hosp, lower_pub_hosp, upper_pub_hosp)
priv_hosp_lmic = as.data.table(priv_hosp_lmic)
priv_hosp_lmic <- priv_hosp_lmic[, mean_priv_hosp := rowMeans(.SD), .SDcols=cols]
priv_hosp_lmic$lower_priv_hosp <- apply(priv_hosp_lmic[, (cols), with=F], 1, quantile, probs=0.025)
priv_hosp_lmic$upper_priv_hosp <- apply(priv_hosp_lmic[, (cols), with=F], 1, quantile, probs=0.975)
priv_hosp_lmic = priv_hosp_lmic %>%
  dplyr::select(location_id, year_id, mean_priv_hosp, lower_priv_hosp, upper_priv_hosp)
priv_prim_lmic = as.data.table(priv_prim_lmic)
priv_prim_lmic <- priv_prim_lmic[, mean_priv_prim := rowMeans(.SD), .SDcols=cols]
priv_prim_lmic$lower_priv_prim <- apply(priv_prim_lmic[, (cols), with=F], 1, quantile, probs=0.025)
priv_prim_lmic$upper_priv_prim <- apply(priv_prim_lmic[, (cols), with=F], 1, quantile, probs=0.975)
priv_prim_lmic = priv_prim_lmic %>%
  dplyr::select(location_id, year_id, mean_priv_prim, lower_priv_prim, upper_priv_prim)
pub_prim_lmic = as.data.table(pub_prim_lmic)
pub_prim_lmic <- pub_prim_lmic[, mean_pub_prim := rowMeans(.SD), .SDcols=cols]
pub_prim_lmic$lower_pub_prim <- apply(pub_prim_lmic[, (cols), with=F], 1, quantile, probs=0.025)
pub_prim_lmic$upper_pub_prim <- apply(pub_prim_lmic[, (cols), with=F], 1, quantile, probs=0.975)
pub_prim_lmic = pub_prim_lmic %>%
  dplyr::select(location_id, year_id, mean_pub_prim, lower_pub_prim, upper_pub_prim)

maternal2 = pub_hosp_lmic %>%
  dplyr::left_join(priv_hosp_lmic, by = c("location_id", "year_id")) %>%
  dplyr::left_join(priv_prim_lmic, by = c("location_id", "year_id")) %>%
  dplyr::left_join(pub_prim_lmic, by = c("location_id", "year_id")) %>%
  dplyr::left_join(ifd_sum, by = c("location_id", "year_id")) %>%
  dplyr::left_join(locs, by = c("location_id")) 
maternal2 = maternal2 %>%
  dplyr::left_join(wb_data, by = c("ihme_loc_id" = "Code")) %>%
  dplyr::filter(location_name != "China") %>%
  dplyr::filter(location_id == 1 | level == 1 | (level == 3 &`Income group` != "High income")) %>%
  dplyr::mutate(super_region_name = if_else(ihme_loc_id == "ARG","Latin America and Caribbean", super_region_name)) %>%
  dplyr::filter(super_region_name != "High-income") 
maternal2 = maternal2 %>% dplyr::left_join(birth_dt, by = c("location_id", "year_id"))
#check that 5 columns sum to births column
check = maternal2 %>% mutate(sum1 = mean_pub_hosp + mean_priv_hosp + mean_pub_prim +mean_priv_prim+mean_value)


#rename the variable to be lancet table friendly, arrange the variables so it's pub hospital, priv hospital, public primary, private primary, hospital any, prim any, and then IFD
maternal2 = maternal2 %>%
  dplyr::select(-c(location_id,Economy,Region,ihme_loc_id,`Income group`,`Lending category`,year_id,births))
maternal2 = maternal2 %>%
  dplyr::rename(
    Country = location_name,
    `val.Public and private non-profit hospital` = mean_pub_hosp,
    `upper.Public and private non-profit hospital` = upper_pub_hosp,
    `lower.Public and private non-profit hospital` = lower_pub_hosp,
    `val.Private for-profit hospital` = mean_priv_hosp,
    `upper.Private for-profit hospital` = upper_priv_hosp,
    `lower.Private for-profit hospital` = lower_priv_hosp,
    `val.Public and private non-profit lower-level` = mean_pub_prim,
    `upper.Public and private non-profit lower-level` = upper_pub_prim,
    `lower.Public and private non-profit lower-level` = lower_pub_prim,
    `val.Private for-profit lower-level` = mean_priv_prim,
    `upper.Private for-profit lower-level` = upper_priv_prim,
    `lower.Private for-profit lower-level` = lower_priv_prim,
    `val.Non-facility` = mean_value,
    `upper.Non-facility` = upper_value,
    `lower.Non-facility` = lower_value) %>%
  dplyr::select(Country,level,
                `val.Public and private non-profit hospital`,
                `upper.Public and private non-profit hospital`,
                `lower.Public and private non-profit hospital`,
                `val.Private for-profit hospital`,
                `upper.Private for-profit hospital`,
                `lower.Private for-profit hospital`,
                `val.Public and private non-profit lower-level`,
                `upper.Public and private non-profit lower-level`,
                `lower.Public and private non-profit lower-level`,
                `val.Private for-profit lower-level`,
                `upper.Private for-profit lower-level`,
                `lower.Private for-profit lower-level`,
                `val.Non-facility`, 
                `upper.Non-facility`, 
                `lower.Non-facility`)
df_ordered <- maternal2 %>%
  # Assign sort order: 1 = region, 2 = country
  dplyr::mutate(
    sort_order = ifelse(level == 1, 1, 2),
    region = ifelse(level == 1, Country, NA)
  ) %>%
  # Fill region name *downward* so countries inherit it
  fill(region, .direction = "down") %>%
  # Custom country sort (Argentina first in its region)
  dplyr::mutate(country_sort = ifelse(Country == "Argentina", 0, 1)) %>%
  # Final sort: by region (alphabetically), then region before countries, then Argentina-first, then name
  dplyr::arrange(region, sort_order, country_sort, Country) %>%
  dplyr::select(-country_sort) %>%
  dplyr::mutate(sort_order = row_number()) %>%
  dplyr::mutate(sort_order = if_else(Country == "Argentina", 20.5,sort_order)) %>%
  dplyr::mutate(sort_order = if_else(Country == "Global",0.5,sort_order))%>%
  dplyr::mutate(Country = if_else(Country == "Global", "All study countries", Country)) %>%
  dplyr::arrange(sort_order) %>%
  dplyr::select(-sort_order,-level,-region) %>%
  dplyr::rename(Location = Country)
df_ordered <- df_ordered %>%
  mutate(across(where(is.numeric), ~ ifelse(. > 100, round(., 0), round(., 1))))
as_lancet_table(df_ordered,
                label = "Location",
                with.ui = T,
                hierarchy = F,
                sdi = F,
                alternate = T,
                rounded = F,
                lancet_font = T,
                non.numeric = F,
                outfile = paste0(outdir, "supplementary_table_2.xlsx"))

#1995 table
v1995 = v1995 %>%
  dplyr::select(-c(location_id,Economy,Region,ihme_loc_id,`Income group`,`Lending category`, year_id))
v1995 = v1995 %>%
  dplyr::rename(
    Country = location_name,
    `val.Public and private non-profit hospital` = mean_pub_hosp,
    `upper.Public and private non-profit hospital` = upper_pub_hosp,
    `lower.Public and private non-profit hospital` = lower_pub_hosp,
    `val.Private for-profit hospital` = mean_priv_hosp,
    `upper.Private for-profit hospital` = upper_priv_hosp,
    `lower.Private for-profit hospital` = lower_priv_hosp,
    `val.Public and private non-profit lower-level` = mean_pub_prim,
    `upper.Public and private non-profit lower-level` = upper_pub_prim,
    `lower.Public and private non-profit lower-level` = lower_pub_prim,
    `val.Private for-profit lower-level` = mean_priv_prim,
    `upper.Private for-profit lower-level` = upper_priv_prim,
    `lower.Private for-profit lower-level` = lower_priv_prim,
    `val.Non-facility` = mean_value,
    `upper.Non-facility` = upper_value,
    `lower.Non-facility` = lower_value) %>%
  dplyr::select(Country,level,
                `val.Public and private non-profit hospital`,
                `upper.Public and private non-profit hospital`,
                `lower.Public and private non-profit hospital`,
                `val.Private for-profit hospital`,
                `upper.Private for-profit hospital`,
                `lower.Private for-profit hospital`,
                `val.Public and private non-profit lower-level`,
                `upper.Public and private non-profit lower-level`,
                `lower.Public and private non-profit lower-level`,
                `val.Private for-profit lower-level`,
                `upper.Private for-profit lower-level`,
                `lower.Private for-profit lower-level`,
                `val.Non-facility`, 
                `upper.Non-facility`, 
                `lower.Non-facility`)
df_ordered_1995 <- v1995 %>%
  # Assign sort order: 1 = region, 2 = country
  dplyr::mutate(
    sort_order = ifelse(level == 1, 1, 2),
    region = ifelse(level == 1, Country, NA)
  ) %>%
  # Fill region name *downward* so countries inherit it
  fill(region, .direction = "down") %>%
  # Custom country sort (Argentina first in its region)
  dplyr::mutate(country_sort = ifelse(Country == "Argentina", 0, 1)) %>%
  # Final sort: by region (alphabetically), then region before countries, then Argentina-first, then name
  dplyr::arrange(region, sort_order, country_sort, Country) %>%
  dplyr::select(-country_sort) %>%
  dplyr::mutate(sort_order = row_number()) %>%
  dplyr::mutate(sort_order = if_else(Country == "Argentina", 20.5,sort_order)) %>%
  dplyr::mutate(sort_order = if_else(Country == "Global",0.5,sort_order))%>%
  dplyr::mutate(Country = if_else(Country == "Global", "All study countries", Country)) %>%
  dplyr::arrange(sort_order) %>%
  dplyr::select(-sort_order,-level,-region) %>%
  dplyr::rename(Location = Country)
check = df_ordered %>% 
  dplyr::mutate(sum1 = `val.Public and private non-profit hospital` + `val.Private for-profit hospital` + `val.Public and private non-profit lower-level` + `val.Private for-profit lower-level` + `val.Non-facility`) %>%
  dplyr::select(Location, sum1)

as_lancet_table(df_ordered_1995,
                label = "Location",
                with.ui = T,
                hierarchy = F,
                sdi = F,
                alternate = T,
                rounded = F,
                lancet_font = T,
                non.numeric = F,
                outfile = paste0(outdir, "1995_table.xlsx"))
