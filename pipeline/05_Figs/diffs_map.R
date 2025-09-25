########looking at differences in scaling

source("/snfs1/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2021/inset_maps/gbd2021_map.R")
source('/ihme/cc_resources/libraries/current/r/get_location_metadata.R')
date <- Sys.Date()

#read in data
indir <- "/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/post_processing/scaled_data/"
outdir <- file.path(paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/figures/", date, "/"))
VERSION <- "2025-07-14" #version of scaled st-gpr results
pub_prim = read_csv(paste0(indir, "/pub_prim_mean_diff_", VERSION, ".csv"))
priv_prim = read_csv(paste0(indir, "/priv_prim_mean_diff_", VERSION, ".csv"))
pub_hosp = read_csv(paste0(indir, "/pub_hosp_mean_diff_", VERSION, ".csv"))
priv_hosp = read_csv(paste0(indir, "/priv_hosp_mean_diff_", VERSION, ".csv"))

locs = get_location_metadata(location_set_id = 35, release_id = 16)
wb = read_excel("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/figures/mcconrad_deliver_location_paper_figures/CLASS.xlsx")
wb = wb %>% mutate(`Income group` = ifelse(Code == "VEN", "Upper middle income", `Income group`))

##limit to 2023, append locs, and limit to level 3, calculate difference
locs = locs %>% dplyr::select(location_id, location_name,ihme_loc_id,level)
pub_prim = pub_prim %>%
  filter(year_id == 2023) %>%
  left_join(locs) 
priv_prim = priv_prim %>%
  filter(year_id == 2023) %>%
  left_join(locs)
pub_hosp = pub_hosp %>%
  filter(year_id == 2023) %>%
  left_join(locs)
priv_hosp = priv_hosp %>%
  filter(year_id == 2023) %>%
  left_join(locs)

#explicitly name columns in each table, then join and sum to find total difference
pub_prim = pub_prim %>%
  dplyr::select(location_id, location_name,ihme_loc_id, level, diff) %>%
  mutate(type = "public primary")
priv_prim = priv_prim %>%
  dplyr::select(location_id, location_name,ihme_loc_id, level, diff) %>%
  mutate(type = "private primary")
pub_hosp = pub_hosp %>%
  dplyr::select(location_id, location_name, ihme_loc_id,level, diff) %>%
  mutate(type = "public hospital")
priv_hosp = priv_hosp %>%
  dplyr::select(location_id, location_name, ihme_loc_id,level, diff) %>%
  mutate(type = "private hospital")
diffs = rbind(pub_prim, priv_prim, pub_hosp, priv_hosp)
diffs = diffs %>% filter(level == 3)
diffs = diffs %>% pivot_wider(names_from = type, values_from = diff)
diffs = diffs %>% mutate(
  mapvar = `public primary` + `private primary` + `public hospital` + `private hospital`
)
diffs = left_join(diffs, wb %>% dplyr::select(Code, `Income group`), by = c("ihme_loc_id" = "Code"))
diffs = diffs %>% filter(`Income group` != "High income")
diffs = diffs %>% filter(ihme_loc_id != "CHN")
source("/snfs1/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2021/inset_maps/gbd2021_map.R")
lims = c(-.3,-.11,-0.06,0.06,.11,31)
labs = c("-0.25 - -0.1", "-0.1 - -0.05","-0.05 - 0.05", "0.05 - 0.1","0.1- 0.3")

lims2 = c(-0.3,-0.1,0.1,0.31)
labs2 = c("-0.3 - -0.1", "-0.1 - 0.1", "0.1 - 0.3")
gbd_map(data=diffs,
        inset = F,
        sub_nat="none",
        limits=lims2, # change to whatever bins make sense for your data
        label=labs2, # label bins in the legend
        col="RdYlBu", # choose palette
        col.reverse=FALSE, #reverse palette if you want
        na.color="grey",
        fname = paste0(outdir, "diffs_map.pdf"), # output file name
        title = "Absolute difference in scaling in 2023 across all locations",
        legend.title = "Absolute difference")
