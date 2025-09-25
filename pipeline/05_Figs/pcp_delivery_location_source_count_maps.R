
source("/snfs1/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2021/inset_maps/gbd2021_map.R")
source('/ihme/cc_resources/libraries/current/r/get_location_metadata.R')
source("/ihme/cc_resources/libraries/current/r/get_crosswalk_version.R")

date <- Sys.Date()
outdir <- file.path(paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/figures/", date, "/"))

pub_hosp<- get_crosswalk_version(49720)
locs = get_location_metadata(location_set_id = 35, release_id = 16)
wb = read_excel("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/figures/mcconrad_deliver_location_paper_figures/CLASS.xlsx")
wb = wb %>% mutate(`Income group` = ifelse(Code == "VEN", "Upper middle income", `Income group`))
wb_lmics = wb %>% filter(`Income group` %in% c("Upper middle income","Low income","Lower middle income"))


source_count<- left_join(pub_hosp, locs)
source_count <- source_count %>%
  dplyr::left_join(wb_lmics,by = c("ihme_loc_id" = "Code")) %>%
  dplyr::filter(!is.na(`Income group`)) %>%
  dplyr::filter(location_id!=6 & is_outlier==0 & year_id>=1995)
# source_count<- source_count %>%
#   dplyr::group_by(location_id, location_name, year_id) %>%
#   dplyr::summarize(count = n())
# source_count<- source_count %>%
#   dplyr::group_by(location_id, location_name) %>%
#   dplyr::summarize(mapvar = sum(count))
source_count<- source_count %>%
  distinct(location_id, location_name, year_id) %>%
  dplyr::group_by(location_id, location_name) %>%
  dplyr::summarize(mapvar = n())

lims = c(1, 6, 11, 16, 20, 79)
labs = c("1-5", "6-10", "11-15", "16-20", "21+")

gbd_map(source_count,
        inset = T,
        sub_nat="none",
        limits=lims, # change to whatever bins make sense for your data
        label=labs, # label bins in the legend
        col="RdYlBu", # choose palette
        col.reverse=FALSE, #reverse palette if you want
        na.color="grey",
        fname = paste0(outdir, "source_count_map.pdf"), # output file name
        title = "",
        legend.title = "Country-years")
