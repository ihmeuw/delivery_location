###########################################
# Number plugging for DLA #################
############################################

#libraries and packages
library(readxl)

indir <- file.path("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/post_processing/aggregates")
VERSION <- "2025-07-16" # version of aggregated ST-GPR results

#load wb data
wb = read_excel("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/figures/mcconrad_deliver_location_paper_figures/CLASS.xlsx")
wb = wb %>% mutate(`Income group` = ifelse(Code == "VEN", "Upper middle income", `Income group`))
wb = wb %>% filter(`Income group` != "High income") 
#1 and 2 WB has has 131 LMICs - 1 for China - 1
#3 number of data sources
#pull data from hospital any
source('/ihme/cc_resources/libraries/current/r/get_crosswalk_version.R')
hosp_any_data = get_crosswalk_version(crosswalk_version_id = 48504)
unique(hosp_any_data$nid)
#4 out of deliveries or live births?
# read in public hospital among lmics
pub_hosp_lmic = read_csv(paste0(indir, "/pub_hosp_bothsteps_st-gpr_results_weighted_aggregates_lmics_", VERSION, ".csv"))
pub_hosp_lmic = pub_hosp_lmic %>% filter(year_id%in% c(1995,2023) & location_id == 1) %>% select(-c(Economy,Region,`Income group`,`Lending category`))
#read in private hospital among lmics
priv_hosp_lmic = read_csv(paste0(indir, "/priv_hosp_bothsteps_st-gpr_results_weighted_aggregates_lmics_", VERSION, ".csv"))
priv_hosp_lmic = priv_hosp_lmic %>% filter(year_id%in% c(1995,2023) & location_id == 1) %>% select(-c(Economy,Region,`Income group`,`Lending category`)) 
#read in prim any among lmics
prim_any_lmic = read_csv(paste0(indir, "/prim_any_bothsteps_st-gpr_results_weighted_aggregates_lmics_", VERSION, ".csv"))
prim_any_lmic = prim_any_lmic %>% filter(year_id%in% c(1995,2023)& location_id == 1) %>% select(-c(Economy,Region,`Income group`,`Lending category`)) 
#read in priv prim any among lmics
priv_prim_lmic = read_csv(paste0(indir, "/priv_prim_bothsteps_st-gpr_results_weighted_aggregates_lmics_", VERSION, ".csv"))
priv_prim_lmic = priv_prim_lmic %>% filter(year_id%in% c(1995,2023) & location_id == 1) %>% select(-c(Economy,Region,`Income group`,`Lending category`)) 
#read in public prim any among lmics
pub_prim_lmic = read_csv(paste0(indir, "/pub_prim_bothsteps_st-gpr_results_weighted_aggregates_lmics_", VERSION, ".csv"))
pub_prim_lmic = pub_prim_lmic %>% filter(year_id%in% c(1995,2023) & location_id == 1) %>% select(-c(Economy,Region,`Income group`,`Lending category`)) 

#to find change in ifd and pub hosp as percentage of that
#need to combine data sets and make long to add all together to get ifd
pub_hosp_long = pub_hosp_lmic %>% 
  select(-c(mean,lower,upper)) %>%
  pivot_longer(cols = starts_with("draw_"), 
               names_to = "draw", 
               values_to = "val_pubhosp") %>%
  mutate(draw = as.integer(gsub("draw_", "", draw)))
priv_hosp_long = priv_hosp_lmic %>%
  select(-c(mean,lower,upper)) %>%
  pivot_longer(cols = starts_with("draw_"), 
               names_to = "draw", 
               values_to = "val_priv_hosp") %>%
  mutate(draw = as.integer(gsub("draw_", "", draw)))
pub_prim_long = pub_prim_lmic %>%
  select(-c(mean,lower,upper)) %>%
  pivot_longer(cols = starts_with("draw_"), 
               names_to = "draw", 
               values_to = "val_pubprim") %>%
  mutate(draw = as.integer(gsub("draw_", "", draw)))
priv_prim_long = priv_prim_lmic %>%
  select(-c(mean,lower,upper)) %>%
  pivot_longer(cols = starts_with("draw_"), 
               names_to = "draw", 
               values_to = "val_priv_prim") %>%
  mutate(draw = as.integer(gsub("draw_", "", draw)))
df = left_join(pub_hosp_long,priv_hosp_long)
df = left_join(df,pub_prim_long)
df = left_join(df,priv_prim_long) %>% select(year_id,age_group_id,location_id,draw,val_pubhosp,val_priv_hosp,val_pubprim,val_priv_prim) 
df = df %>% mutate(total = val_pubhosp + val_priv_hosp + val_pubprim + val_priv_prim) 
df_1995 <- df %>% filter(year_id == 1995)
df_2023 <- df %>% filter(year_id == 2023)

df_diff <- df_2023 %>%
  arrange(draw) %>%
  mutate(
    ifd_diff = total - df_1995 %>% arrange(draw) %>% pull(total),
    pubhosp_diff = val_pubhosp - df_1995 %>% arrange(draw) %>% pull(val_pubhosp),
    share_pubhosp = pubhosp_diff / ifd_diff
  )

summary_stats <- df_diff %>%
  summarise(
    mean_ifd_diff = mean(ifd_diff),
    q025_ifd_diff = quantile(ifd_diff, 0.025),
    q975_ifd_diff = quantile(ifd_diff, 0.975),
    
    mean_share = mean(share_pubhosp, na.rm = TRUE),
    q025_share = quantile(share_pubhosp, 0.025, na.rm = TRUE),
    q975_share = quantile(share_pubhosp, 0.975, na.rm = TRUE)
  )

#6 public lmics without regions and super regions
pub_hosp_lmic_num = pub_hosp_lmic %>% filter(!location_id %in% c(1,4,31,64,103,158,166,5,9,21,32,42,56,65,70,96,100,104,120,124,134,138,159,167,174,192,199))
pub_hosp_lmic_num = pub_hosp_lmic_num %>% filter(year_id %in%c(2023))  %>% dplyr::select(location_id, year_id, mean, lower, upper)
#7 private lmics without regions and super regions
priv_hosp_lmic = priv_hosp_lmic %>% filter(!location_id %in% c(1,4,31,64,103,158,166,5,9,21,32,42,56,65,70,96,100,104,120,124,134,138,159,167,174,192,199))
priv_hosp_lmic = priv_hosp_lmic %>% filter(year_id %in%c(2023))  %>% dplyr::select(location_id, year_id, mean, lower, upper)
#prim any without regions and super regions
prim_any_lmic = prim_any_lmic %>% filter(!location_id %in% c(1,4,31,64,103,158,166,5,9,21,32,42,56,65,70,96,100,104,120,124,134,138,159,167,174,192,199))
prim_any_lmic = prim_any_lmic %>% filter(year_id %in%c(2023))  %>% dplyr::select(location_id, year_id, mean, lower, upper)
#8 public lower level lmics without regions and super regions
pub_prim_lmic = read_csv(paste0(indir, "/pub_prim_bothsteps_st-gpr_results_weighted_aggregates_lmics_", VERSION, ".csv"))
pub_prim_lmic = pub_prim_lmic %>% filter(year_id == 2023) %>% dplyr::select(location_id, year_id, mean, lower, upper)
pub_prim_lmic = pub_prim_lmic %>% filter(!location_id %in% c(1,4,31,64,103,158,166,5,9,21,32,42,56,65,70,96,100,104,120,124,134,138,159,167,174,192,199))
#private lower level lmics without regions and super regions
priv_prim_lmic = read_csv(paste0(indir, "/priv_prim_bothsteps_st-gpr_results_weighted_aggregates_lmics_", VERSION, ".csv"))
priv_prim_lmic = priv_prim_lmic %>% filter(year_id == 2023) %>% dplyr::select(location_id, year_id, mean, lower, upper)
priv_prim_lmic = priv_prim_lmic %>% filter(!location_id %in% c(1,4,31,64,103,158,166,5,9,21,32,42,56,65,70,96,100,104,120,124,134,138,159,167,174,192,199))

#take take all models, filter to 1995 and 2023
pub_hosp_lmic = pub_hosp_lmic %>% filter(year_id %in% c(2023)) %>% filter(location_id == 1)
priv_hosp_lmic = priv_hosp_lmic %>% filter(year_id %in% c(2023))  %>% filter(location_id == 1)
pub_prim_lmic = pub_prim_any_lmic %>% filter(year_id %in% c(2023))  %>% filter(location_id == 1)
priv_prim_lmic = priv_prim_any_lmic %>% filter(year_id %in% c(2023))  %>% filter(location_id == 1)

# 1. put your four DTs in a list (replace these names as needed)
dts <- list(pub_prim_lmic, pub_hosp_lmic, priv_prim_lmic, priv_hosp_lmic)
me_ids <- c("pub_prim",    "pub_hosp",    "priv_prim",    "priv_hosp")
keyvars  <- c("location_id","age_group_id","sex_id","year_id","me_id")
# 2. define the draw columns
draw_cols <- paste0("draw_", 0:999)

# 3. extract just the keys + draw columns from each and rbind them together,
#    tagging by facility type so we can sum later
dts_tagged <- lapply(seq_along(dts), function(i) {
  dt <- as.data.table(dts[[i]])      # drop tibble class
  dt[, me_id := me_ids[i]]           # add the model identifier
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

# 4. sum across facility types to get total IFD per draw
ifd_sum <- ifd_long[
  , .(ifd = sum(val)), 
  by=.(location_id, age_group_id, sex_id, year_id, draw)
]

# 5. cast back so that each row–year has one column per draw
diff_wide <- dcast(
  ifd_sum[year_id %in% c(1995,2023)],  # keep only the two years
  draw ~ year_id,                       # one row per draw
  value.var = "ifd"                     # pivot ifd into two columns: `1995`, `2023`
)

diff_wide[, diff := `2023` - `1995`]    # new column = difference
diff_vec <- diff_wide$diff
result <- data.table(
  mean_diff = mean(diff_vec),
  lower95   = quantile(diff_vec, 0.025),
  upper95   = quantile(diff_vec, 0.975)
)
##########public hospital only
# 1) compute total IFD per draw & year
ifd_sum <- ifd_long[
  , .(ifd = sum(val)), 
  by = .(location_id, age_group_id, sex_id, year_id, draw)
]

# 2) public‐hospital IFD per draw & year
pub_sum <- ifd_long[me_id == "pub_hosp",
                    .(pub_ifd = sum(val)),    # in case you had >1 row per draw
                    by = .(location_id, age_group_id, sex_id, year_id, draw)
]

# 3) get the 1995 vs 2023 difference for both series
diff_ifd <- dcast(
  ifd_sum[year_id %in% c(1995,2023)],
  draw ~ year_id,
  value.var = "ifd"
)[, diff_ifd := `2023` - `1995`]

diff_pub <- dcast(
  pub_sum[year_id %in% c(1995,2023)],
  draw ~ year_id,
  value.var = "pub_ifd"
)[, diff_pub := `2023` - `1995`]

# 4) merge them, compute % driven by pub_hosp
pct_pub <- merge(
  diff_pub[, .(draw, diff_pub)],
  diff_ifd[, .(draw, diff_ifd)],
  by = "draw"
)[, pct := diff_pub / diff_ifd * 100 ]

# 5) summarise mean & 95% UI
res <- pct_pub[, .(
  mean_pct   = mean(pct),
  lower95pct = quantile(pct, 0.025),
  upper95pct = quantile(pct, 0.975)
)]

###central europe and southeast asia 2023 estimates for public hospitals only
pub_hosp_lmic = pub_hosp_lmic %>% filter(location_id %in% c(31,103))
priv_hosp_lmic = priv_hosp_lmic %>% filter(location_id %in% c(31,103,158,159,137,138))

#public lower level SSA
pub_prim_lmic = pub_prim_any_lmic %>% filter(location_id %in% c(166))

##############
#public hospital among ifd mean and uncertainty
diff_vec <- diff_wide$`2023`
result <- data.table(
  mean_diff = mean(diff_vec),
  lower95   = quantile(diff_vec, 0.025),
  upper95   = quantile(diff_vec, 0.975)
)
pub_hosp_lmic = pub_hosp_lmic %>% filter(location_id == 1 & year_id == 2023) %>% dplyr::select(location_id, year_id, mean, lower, upper)

#SSA public lower level among ifd with mean and uncertainty
# 1) total‐IFD per draw & year
ifd_sum <- ifd_long[
  , .(ifd = sum(val)), 
  by = .(location_id, age_group_id, sex_id, year_id, draw)
]

# 2) public‐hospital IFD per draw & year
pub_sum <- ifd_long[me_id == "pub_hosp",
                    .(pub_ifd = sum(val)),
                    by = .(location_id, age_group_id, sex_id, year_id, draw)
]

# 3) merge and filter to 2023
share23_dt <- merge(pub_sum, ifd_sum,
                    by = c("location_id","age_group_id","sex_id","year_id","draw")
)[year_id == 2023]

# 4) compute share per draw (0–1)
share23_dt[, share := pub_ifd / ifd]

# 5) summarise into percent points
res_share23 <- share23_dt[
  , .(
    mean_share = mean(share) * 100,
    lower95    = quantile(share, 0.025) * 100,
    upper95    = quantile(share, 0.975) * 100
  )
]

print(res_share23)

##total non facility births
pub_hosp_lmic = pub_hosp_lmic %>% filter(year_id == 2023) %>% mutate(me_id = "pub_hosp")
priv_hosp_lmic = priv_hosp_lmic %>% filter(year_id == 2023) %>% mutate(me_id = "priv_hosp")
pub_prim_lmic = pub_prim_lmic %>% filter(year_id == 2023) %>% mutate(me_id = "pub_prim")
priv_prim_lmic = priv_prim_lmic %>% filter(year_id == 2023) %>% mutate(me_id = "priv_prim")
df = rbind(pub_hosp_lmic, priv_hosp_lmic, pub_prim_lmic, priv_prim_lmic)
#pivot longer by draws
draw_cols <- paste0("draw_", 0:999)
df_long <- df %>%
  # 1) pivot the draws into long form
  pivot_longer(
    cols        = starts_with("draw_"),
    names_to    = "draw",
    names_prefix= "draw_",
    values_to   = "value"
  ) %>%
  # optional: turn draw into an integer
  mutate(draw = as.integer(draw)) %>%
  # keep only the grouping columns + me_id + draw + the value
  select(year_id, age_group_id, sex_id, location_id, me_id, draw, value) %>%
  # 2) pivot me_id into its own columns
  pivot_wider(
    names_from = me_id,
    values_from = value
  )
df_long = df_long %>% 
  mutate(ifd = pub_hosp+pub_prim+priv_hosp+priv_prim,
         non_facility = 1 - ifd)
nonfac_summary <- df_long %>%
  group_by(year_id, age_group_id, sex_id, location_id) %>%
  summarise(
    mean_nonfac  = mean(non_facility, na.rm = TRUE),
    lower_nonfac = quantile(non_facility, probs = 0.025, na.rm = TRUE),
    upper_nonfac = quantile(non_facility, probs = 0.975, na.rm = TRUE),
    .groups      = "drop"
  )


