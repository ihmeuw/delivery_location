##########################################
#Reappend new collapse nids to old concatenated data file
##########################################
library(tidyverse)
'%!in%' <- function(x,y)!('%in%'(x,y))

##'[JK: as of 7/9/25, can skip to version3 new concatenated data]

version = "2025-02-26"
date1 = "2025-03-19"
date2 = "2025-03-18"
date3 = "2025-03-16"
date4 = "2025-03-24"
date5 = "2025-05-17"
date6 = "2025-05-23"
ROOT = "/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse/"
#first read in the data files for each NID and concatenate them together
nids1 = c(467681,23219)
nids2 = c(157050,21331,31750,20865,77395,341838,12455,234733,45777,23183,19787,19950,234733,341838)
nids3 = c(107174,107172,107123,236187,236213,265259,335931,387644)#mexico
nids4 = c(154897,159617,26842,19521)
nids5 = c(18834,223669,20947)
nids6 = c(20954,32421)
nids7 = c(440151)

concat = data.table()
for (nid in nids1){
  #read in the data
  data = read.csv(paste0(ROOT,version,"/",nid,"/collapse_maternal_",date1,".csv"))
  concat = rbind(concat, data)
}
for (nid in nids2){
  #read in the data
  data = read.csv(paste0(ROOT,version,"/",nid,"/collapse_maternal_",date2,".csv"))
  concat = rbind(concat, data)
}
for (nid in nids3){
  #read in the data
  data = read.csv(paste0(ROOT,version,"/",nid,"/collapse_maternal_",date3,".csv"))
  concat = rbind(concat, data)
}
for (nid in nids4){
  #read in the data
  data = read.csv(paste0(ROOT,version,"/",nid,"/collapse_maternal_",date4,".csv"))
  concat = rbind(concat, data)
}
for (nid in nids5){
  #read in the data
  data = read.csv(paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse/2025-05-17/",nid,"/collapse_maternal_20225.csv"))
  concat = rbind(concat, data)
}
for (nid in nids6){
  #read in the data
  data = read.csv(paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse/2025-05-17/",nid,"/collapse_maternal_20226.csv"))
  concat = rbind(concat, data)
}
for (nid in nids7){
  #read in the data
  data = read.csv(paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/data/collapse/2025-05-23/",nid,"/collapse_maternal_20231.csv"))
  concat = rbind(concat, data)
}
new_data = read.csv(paste0(ROOT,"2025-05-07/collapse_maternal_20215.csv"))
new_data = new_data %>% filter(nid != 27215)
concat = rbind(concat, new_data)

#read in old concatenated data
version2 = "2024-11-08"
old_concat = read.csv(paste0(ROOT,version2,"/collapse_maternal_",version2,".csv"))

#filter data to only include nids not in the list, and add our new nids to this data
new_nids = unique(concat$nid)
old_concat = old_concat %>% filter(nid %!in% new_nids)
old_concat = old_concat %>% filter(nid %!in% nids1) %>% filter(nid %!in% nids2) %>% filter(nid %!in% nids3) %>% filter(nid %!in% nids4)

#append new data to old data
new_concat = rbind(old_concat, concat)

## append in new Brazil public hospital data, JK 7/9/25
version3 = "2025-05-23"
new_concat <- read.csv(paste0(ROOT,version,"/collapse_maternal_",version3,".csv"))
## add brazil data
brazil_data <- read.csv("/mnt/team/hs/pub/code/mknight4/pcp/brazil_comp/bra_share_public_births_2000_2021.csv")
brazil_data <- brazil_data %>%
  mutate(var = "pub_hosp",
         survey_name = "SIH_admin_data",
         survey_module = "none",
         age_start = 0,
         standard_error = standard_error * 10, ## attempt to address tail fit in ST-GPR issue, 2015-07-10 data date
         file_path = "/mnt/team/hs/pub/code/mknight4/pcp/brazil_comp/bra_share_public_births_2000_2021.csv") %>%
  dplyr::rename(mean = share_public_births) %>%
  dplyr::select(-c(year_id, is_outlier, measure_id, sex_id, age_group_id, location_id))
## add
new_concat_w_brazil <- bind_rows(new_concat, brazil_data)

#write out
# version3 = "2025-05-23"
version_bra <- "2025-07-10"
# write.csv(new_concat_w_brazil, paste0(ROOT,version,"/collapse_maternal_",version_bra,".csv"), row.names = FALSE)
write.csv(new_concat_w_brazil, paste0(ROOT,"/collapse_maternal_",version_bra,".csv"), row.names = FALSE)

