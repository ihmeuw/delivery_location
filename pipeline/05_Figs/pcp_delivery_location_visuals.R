#### Figures for delivery location paper ####
#### written by Chiara Sumich ####
#### 2025-01-03 ####

##choose either geom_col(width =1 ) or geom_area(stat = "identity)
#### Load packages ####
rm(list = ls())
pacman::p_load(data.table, ggplot2, gtools, tidyverse, haven, labelled, tidyselect, sf, weights, survey, scales, viridis, ggrepel, ggpubr,patchwork,matrixStats,readxl)
#source("/ihme/code/st_gpr/central/src/stgpr/api/public.R")
source('/ihme/cc_resources/libraries/current/r/get_covariate_estimates.R')
source('/ihme/cc_resources/libraries/current/r/get_outputs.R')
source('/ihme/cc_resources/libraries/current/r/get_location_metadata.R')

date <- Sys.Date()

library(reticulate)
reticulate::use_python("/ihme/code/mscm/miniconda3/envs/mrtool_0.0.1/bin/python")
mr <- reticulate::import("mrtool")

#### setting filepaths ####
indir <- file.path("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/post_processing/aggregates/")
outdir <- file.path(paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/figures/", date, "/"))


#### Defining variables 
VERSION <- "2025-07-16" ## version of weighted aggregates st-gpr results

#### loading datasets
draw_cols <- paste0("draw_", 0:999)
pub_hosp <- read_csv(file.path(indir, paste0("pub_hosp_bothsteps_st-gpr_results_weighted_aggregates_lmics_", VERSION, ".csv"))) %>%
  #### keeping only variables that we need to simplify things
  #dplyr::select(-Economy,-Region,-`Income group`,-`Lending category`,-parent_id,-ihme_loc_id) %>%
  #dplyr::select(year_id, age_group_id, sex_id, location_id,ihme_loc_id, mean, lower, upper) %>%
  #### creating a me_id variable so we know which results correspond to each 
  dplyr:: mutate(me_id = "pub_hosp") %>% filter(year_id == 2023)

priv_hosp <- read_csv(file.path(indir, paste0("priv_hosp_bothsteps_st-gpr_results_weighted_aggregates_lmics_", VERSION, ".csv"))) %>%
  #### keeping only variables that we need to simplify things
  #dplyr::select(-Economy,-Region,-`Income group`,-`Lending category`,-parent_id,-ihme_loc_id) %>%
  #dplyr::select(year_id, age_group_id, sex_id, location_id,ihme_loc_id, mean, lower, upper) %>%
  #### creating a me_id variable so we know which results correspond to each 
  dplyr:: mutate(me_id = "priv_hosp")%>% filter(year_id == 2023)

pub_prim <- read_csv(file.path(indir, paste0("pub_prim_bothsteps_st-gpr_results_weighted_aggregates_lmics_", VERSION, ".csv"))) %>%
  #### keeping only variables that we need to simplify things
  #dplyr::select(-Economy,-Region,-`Income group`,-`Lending category`,-parent_id,-ihme_loc_id) %>%
  #dplyr::select(year_id, age_group_id, sex_id, location_id,ihme_loc_id, mean, lower, upper) %>%
  #### creating a me_id variable so we know which results correspond to each 
  dplyr:: mutate(me_id = "pub_prim")%>% filter(year_id == 2023)

priv_prim <- read_csv(file.path(indir, paste0("priv_prim_bothsteps_st-gpr_results_weighted_aggregates_lmics_", VERSION, ".csv"))) %>%
  #### keeping only variables that we need to simplify things
 # dplyr::select(-Economy,-Region,-`Income group`,-`Lending category`,-parent_id,-ihme_loc_id) %>%
  #dplyr::select(year_id, age_group_id, sex_id, location_id,ihme_loc_id, mean, lower, upper) %>%
  dplyr:: mutate(me_id = "priv_prim")%>% filter(year_id == 2023)
#draws <- paste0("draw_", 0:999)
df_means <-
  pub_hosp %>%
  dplyr::select(year_id, age_group_id, sex_id, location_id, mean) %>%
  dplyr::rename(mean_pub_hosp = mean) %>%
  dplyr::left_join(
    priv_hosp  %>% dplyr::select(location_id, mean) %>% dplyr::rename(mean_priv_hosp  = mean),
    by = "location_id"
  ) %>%
  dplyr::left_join(
    pub_prim   %>% dplyr::select(location_id, mean) %>% dplyr::rename(mean_pub_prim   = mean),
    by = "location_id"
  ) %>%
  dplyr::left_join(
    priv_prim  %>% dplyr::select(location_id, mean) %>% dplyr::rename(mean_priv_prim  = mean),
    by = "location_id"
  ) %>%
  dplyr::mutate(
    # now get non-facility mean
    mean_non_facility = 1 - (mean_pub_hosp +
                               mean_priv_hosp +
                               mean_pub_prim +
                               mean_priv_prim)
  )

# helper to grab & rename draw cols
rename_draws <- function(df, suffix) {
  df %>%
    dplyr::select(location_id, starts_with("draw_")) %>%
    rename_with(~ paste0(.x, "_", suffix), starts_with("draw_"))
}

d_pub_hosp  <- rename_draws(pub_hosp,  "pub_hosp")
d_priv_hosp <- rename_draws(priv_hosp, "priv_hosp")
d_pub_prim  <- rename_draws(pub_prim,  "pub_prim")
d_priv_prim <- rename_draws(priv_prim, "priv_prim")

df_draws <-
  d_pub_hosp %>%
  dplyr::left_join(d_priv_hosp,  by = "location_id") %>%
  dplyr::left_join(d_pub_prim,   by = "location_id") %>%
  dplyr::left_join(d_priv_prim,  by = "location_id")

# now add non-facility draws
draw_ids <- 0:999
for (i in draw_ids) {
  ph <- paste0("draw_", i, "_pub_hosp")
  prh <- paste0("draw_", i, "_priv_hosp")
  pp <- paste0("draw_", i, "_pub_prim")
  prp <- paste0("draw_", i, "_priv_prim")
  fac <- df_draws[[ph]] + df_draws[[prh]] + df_draws[[pp]] + df_draws[[prp]]
  
  df_draws[[paste0("draw_", i, "_non_facility")]] <- 1 - fac
}

# find each block of draw‐columns by suffix
draw_cols <- names(df_draws)
pub_hosp_cols    <- grep("_pub_hosp$",    draw_cols, value = TRUE)
priv_hosp_cols   <- grep("_priv_hosp$",   draw_cols, value = TRUE)
pub_prim_cols    <- grep("_pub_prim$",    draw_cols, value = TRUE)
priv_prim_cols   <- grep("_priv_prim$",   draw_cols, value = TRUE)
non_fac_cols     <- grep("_non_facility$", draw_cols, value = TRUE)

df_ses <-
  df_draws %>%
  dplyr::transmute(
    location_id       = location_id,
    se_pub_hosp       = rowSds(as.matrix(dplyr::select(., all_of(pub_hosp_cols)))),
    se_priv_hosp      = rowSds(as.matrix(dplyr::select(., all_of(priv_hosp_cols)))),
    se_pub_prim       = rowSds(as.matrix(dplyr::select(., all_of(pub_prim_cols)))),
    se_priv_prim      = rowSds(as.matrix(dplyr::select(., all_of(priv_prim_cols)))),
    se_non_facility   = rowSds(as.matrix(dplyr::select(., all_of(non_fac_cols))))
  )
df_final <-
  df_means %>%
  left_join(df_ses, by = "location_id")

wb = read_excel("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/figures/mcconrad_deliver_location_paper_figures/CLASS.xlsx")
wb = wb %>% mutate(`Income group` = if_else(Code == "VEN", "Upper middle income", `Income group`))
wb_lmics = wb %>% filter(`Income group` %in% c("Upper middle income","Low income","Lower middle income"))
locs <- get_location_metadata(release_id = 16, location_set_id = 35)
sdi = get_covariate_estimates(covariate_id = 881, release_id = 16, year_id = c(2023), location_id = locs$location_id)
######## add in facility delivery
df <- df_final %>%
  dplyr::left_join(locs[,c("location_id","ihme_loc_id")], by = "location_id") %>%
  dplyr::left_join(wb_lmics,by = c("ihme_loc_id" = "Code")) %>%
  dplyr::filter(!is.na(`Income group`))
  # pivot_wider(names_from = "me_id", values_from = c("mean", "lower", "upper")) %>%
  # dplyr::group_by(location_id,year_id) %>%
  # # Calculate the total facility births proportion
  # dplyr::mutate(facility_total = mean_pub_hosp + mean_priv_hosp + mean_pub_prim + mean_priv_prim,
  #        no_facility = 1-facility_total)
# df_mean_only = df %>% 
  # dplyr::select(year_id,location_id,sex_id,age_group_id,
  #               mean_pub_hosp, mean_priv_hosp, mean_pub_prim, mean_priv_prim,_facility)
together = merge(df, sdi, by = c("location_id","year_id","sex_id","age_group_id"), all.x = TRUE)
#pivot df longer so we have location, year, sdi, me identifier, me mean
df_long <- together %>%
  dplyr::select(year_id,location_id,mean_pub_hosp, mean_priv_hosp, mean_pub_prim, mean_priv_prim,mean_non_facility, mean_value) %>%
  pivot_longer(cols = c("mean_pub_hosp", "mean_priv_hosp", "mean_pub_prim", "mean_priv_prim","mean_non_facility"),
               names_to = "me_id", values_to = "me_mean",names_prefix = "mean_") 
together_se = together %>%
  dplyr::select(year_id,location_id,se_pub_hosp, se_priv_hosp, se_pub_prim, se_priv_prim,se_non_facility) %>%
  pivot_longer(cols = c("se_pub_hosp", "se_priv_hosp", "se_pub_prim", "se_priv_prim","se_non_facility"),
               names_to = "me_id", values_to = "me_se",names_prefix = "se_") 
df_long = dplyr::left_join(df_long, together_se, by = c("year_id","location_id","me_id"))
# Quick sanity‐check:
glimpse(df_long)
#first attempt for geom area for 2023 global
dat_1 = df_long %>% filter(me_id != "prim_any_scaled") %>% filter(me_id != "hosp_any_scaled") %>% filter(year_id == 2023)

# Step 2: Define shared SDI grid
sdi_grid <- seq(min(dat_1$mean_value, na.rm = TRUE),
                max(dat_1$mean_value, na.rm = TRUE),
                length.out = 200)
# # Step 2 option b: MR-BRT
df_smooth <- dat_1 %>%
  dplyr::group_by(me_id) %>%
  group_map(~{
    df_i <- .x %>%
      dplyr::mutate(y = log(me_mean/(1-me_mean)), #logit
                    y_se = 0.01)
    
    # wrap into MRData
    dat_i <- mr$MRData()
    dat_i$load_df(
      data       = df_i,
      col_obs    = "y",
      col_obs_se = "y_se",
      col_covs   = list("mean_value")
    )
    
    # 1) minimal‐flex spline: 
    mod_i <- mr$MRBRT(
      data       = dat_i,
      cov_models = list(
        mr$LinearCovModel("intercept"),
        mr$LinearCovModel(
          "mean_value",
          use_spline         = TRUE,
          spline_degree      = 2L,                       # quadratic
          spline_knots       = array(seq(0, 1, length.out = 7)),
          spline_knots_type  = "frequency",               # density‐spaced
          spline_l_linear    = TRUE,                     # linear left tail
          spline_r_linear    = TRUE                      # linear right tail
          
        )
      )
    )
    mod_i$fit_model()
    
    # make predictions on the shared grid
    pred_df <- tibble(mean_value = sdi_grid)
    pred_dat <- mr$MRData()
    pred_dat$load_df(data = pred_df, col_covs = list("mean_value"))
    
    logit_preds <- mod_i$predict(data = pred_dat)
    prop_preds   <- plogis(logit_preds)   # inverse‐logit

    tibble(me_id = .y$me_id, mean_value = sdi_grid, smooth_mean = prop_preds)
  }) %>% bind_rows()

df_prop <- df_smooth %>%
  dplyr:: group_by(mean_value) %>%
  dplyr:: mutate(proportion = smooth_mean / sum(smooth_mean)) %>%
  dplyr:: ungroup()
df_prop <- df_prop %>% 
  mutate(me_id = factor(me_id,
                        levels = c("non_facility","pub_prim","pub_hosp","priv_prim","priv_hosp")))
sdi_plot <- ggplot(df_prop, aes(x = mean_value, y = proportion, fill = (me_id),group = me_id,order = as.numeric(me_id))) +
  geom_area(position = position_stack(reverse = TRUE), alpha = 0.9) +
  scale_fill_manual(
    values = c(
      "non_facility"     = "grey",
      "pub_prim"  = "maroon",
      "pub_hosp"  = "darkblue",
      "priv_prim" = "lightpink",
      "priv_hosp" = "lightblue"
    ),
    labels = c(
      "pub_prim"  = "Public Lower‐level",
      "pub_hosp"  = "Public hospital",
      "priv_prim" = "Private Lower‐level",
      "priv_hosp" = "Private hospital",
      "non_facility"     = "No facility"
    )
  ) +
  labs(
    x = "Socio-demographic index",
    y = "Proportion",
    title = "Figure 4a: Facility delivery location mix versus SDI, 2023"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(sdi_plot)
#################################################################################
###maternal mortality
mmr = get_outputs("cause",release_id = 16, cause_id = 366,measure_id = 25,location_id = locs$location_id, year_id = c(2023),metric_id = 3,compare_version_id = 8234,sex_id = c(2))
mmr <- mmr %>%
  dplyr::mutate(sex_id = 3) %>%
  mutate(val = log(val))
together = merge(df, mmr, by = c("location_id","year_id","sex_id","year_id"))
df_long <- together %>%
  dplyr::select(year_id,location_id,mean_pub_hosp, mean_priv_hosp, mean_pub_prim, mean_priv_prim,mean_non_facility, val) %>%
  pivot_longer(cols = c("mean_pub_hosp", "mean_priv_hosp", "mean_pub_prim", "mean_priv_prim","mean_non_facility"),
               names_to = "me_id", values_to = "me_mean",names_prefix = "mean_")
together_se = together %>%
  dplyr::select(year_id,location_id,se_pub_hosp, se_priv_hosp, se_pub_prim, se_priv_prim,se_non_facility) %>%
  pivot_longer(cols = c("se_pub_hosp", "se_priv_hosp", "se_pub_prim", "se_priv_prim","se_non_facility"),
               names_to = "me_id", values_to = "me_se",names_prefix = "se_")
df_long = dplyr::left_join(df_long, together_se, by = c("year_id","location_id","me_id"))

# Quick sanity‐check:
glimpse(df_long)

#first attempt for geom area for 2023 global
dat_1 = df_long %>% filter(me_id != "prim_any_scaled") %>% filter(me_id != "hosp_any_scaled") %>% filter(year_id == 2023)

# Step 2: Create shared log-MMR grid
mmr_grid <- seq(min(dat_1$val, na.rm = TRUE),
                max(dat_1$val, na.rm = TRUE),
                length.out = 200)
# #################################################################################
# Using MRBRT
df_smooth <- dat_1 %>%
  dplyr::group_by(me_id) %>%
  group_map(~{
    df_i <- .x %>%
      dplyr::mutate(
        y    = log(me_mean/(1-me_mean)), #logit
        y_se = 0.01
      )
    
    dat_i <- mr$MRData()
    dat_i$load_df(
      data       = df_i,
      col_obs    = "y",
      col_obs_se = "y_se",
      col_covs   = list("val")
    )
    
    mod_i <- mr$MRBRT(
      data       = dat_i,
      cov_models = list(
        mr$LinearCovModel("intercept"),
        mr$LinearCovModel(
          "val",
          use_spline        = TRUE,
          spline_degree     = 2L,
          spline_knots      = array(seq(0, 1, length.out = 5)),
          spline_knots_type = "frequency",
          spline_l_linear   = TRUE,
          spline_r_linear   = TRUE
        )
      )
    )
    mod_i$fit_model()
    
    pred_df <- tibble(val = mmr_grid)
    pred_dat <- mr$MRData()
    pred_dat$load_df(data = pred_df, col_covs = list("val"))
    #preds <- mod_i$predict(data = pred_dat)
    
    logit_preds <- mod_i$predict(data = pred_dat) # back transform the mean
    prop_preds   <- inv.logit(logit_preds)

    tibble(
      me_id       = unique(.y$me_id),
      val     = mmr_grid,
      smooth_mean = prop_preds
    )
  }) %>% dplyr::bind_rows()

df_prop <- df_smooth %>%
  #dplyr::mutate(smooth_mean = pmax(smooth_mean, 0)) %>%
  dplyr::group_by(val) %>%
  dplyr::mutate(
    proportion = smooth_mean / sum(smooth_mean, na.rm = TRUE)
  ) %>%
  dplyr::ungroup()

df_prop = df_prop %>%
  mutate(me_id = factor(me_id,
                        levels = c("non_facility","pub_prim","pub_hosp","priv_prim","priv_hosp")))
obstetric_stages = c(log(1000),log(300),log(100),log(50))
obstetric_stages
mmr_plot <- ggplot(df_prop, aes(x = val, y = proportion, fill = me_id)) +
  geom_area(position = position_stack(rev = TRUE), alpha = 0.9) +
  scale_x_reverse(
    breaks = c(7, 6, 5, 4, 3, 2),
    labels = c(
      "7\n(1,096)",
      "6\n(403)",
      "5\n(148)",
      "4\n(55)",
      "3\n(20)",
      "2\n(7)"
    )
  )+
  scale_fill_manual(
    values = c(
      "pub_prim"  = "maroon",
      "pub_hosp"  = "darkblue",
      "priv_prim" = "lightpink",
      "priv_hosp" = "lightblue",
      "non_facility"     = "grey"
    ),
    labels = c(
      "pub_prim"  = "Public Lower‐level",
      "pub_hosp"  = "Public hospital",
      "priv_prim" = "Private Lower‐level",
      "priv_hosp" = "Private hospital",
      "non_facility"     = "Non-facility"
    )
  ) + 
  geom_vline(xintercept = 6.90,linetype = "dashed") +
  geom_vline(xintercept = 5.70,linetype = "dashed") +
  geom_vline(xintercept = 4.60,linetype = "dashed") +
  annotate("text",
           x = 6.9,
           y = 0.25,
           label = "Stage 1",
           angle = 90,         # <-- vertical text
           vjust = -0.5,       # tweak vertical position
           hjust = 1.1,        # to place it left of the line
           color = "black")+
  annotate("text",
           x = 5.75,
           y = 0.25,
           label = "Stage 2",
           angle = 90,         # <-- vertical text
           vjust = -0.5,       # tweak vertical position
           hjust = 1.1,        # to place it left of the line
           color = "black")+
  annotate("text",
           x = 4.6,
           y = 0.25,
           label = "Stage 3",
           angle = 90,         # <-- vertical text
           vjust = -0.5,       # tweak vertical position
           hjust = 1.1,        # to place it left of the line
           color = "black")+
  annotate("text",
           x = 1.6,
           y = 0.25,
           label = "Stage 4",
           angle = 90,         # <-- vertical text
           vjust = -0.5,       # tweak vertical position
           hjust = 1.1,        # to place it left of the line
           color = "black")+
  labs(
    x     = "Log Maternal Mortality Ratio (Linear Maternal Mortality Ratio)",
    y     = "Proportion",
    fill  = "Facility type",
    title = "Figure 4b: Facility delivery mix versus MMR, 2023"
  ) +
  theme_minimal() +
  theme(legend.position = "right")
print(mmr_plot)

##neonatal mortality for appendix
nmr = get_covariate_estimates(release_id = 16, 209,location_id = locs$location_id,year_id = c(2023))
birth_dt <- get_covariate_estimates(covariate_id = 1106, release_id = 16,year_id = 2023)
birth_dt <- birth_dt[, c("location_id","sex_id", "year_id", "mean_value")]
setnames(birth_dt, "mean_value", "births")
#merge nmr and births
nmr = merge(nmr, birth_dt, by = c("location_id","sex_id","year_id"))
nmr <- nmr %>%
  # compute deaths by sex
  dplyr::mutate(deaths = mean_value * births) %>%
  # collapse to both sexes
  dplyr::group_by(location_id, year_id) %>%
  dplyr::summarise(
    deaths_total = sum(deaths),
    births_total = sum(births),
    nmr_both     = deaths_total / births_total,
    .groups      = "drop"
  ) %>%
  dplyr::mutate(sex_id = 3) 
together = merge(df, nmr, by = c("location_id","year_id","sex_id","year_id"))
df_long <- together %>%
  dplyr::select(year_id,location_id,mean_pub_hosp, mean_priv_hosp, mean_pub_prim, mean_priv_prim,mean_non_facility, nmr_both) %>%
  pivot_longer(cols = c("mean_pub_hosp", "mean_priv_hosp", "mean_pub_prim", "mean_priv_prim","mean_non_facility"),
               names_to = "me_id", values_to = "me_mean",names_prefix = "mean_")
together_se = together %>%
  dplyr::select(year_id,location_id,se_pub_hosp, se_priv_hosp, se_pub_prim, se_priv_prim,se_non_facility) %>%
  pivot_longer(cols = c("se_pub_hosp", "se_priv_hosp", "se_pub_prim", "se_priv_prim","se_non_facility"),
               names_to = "me_id", values_to = "me_se",names_prefix = "se_")
df_long = dplyr::left_join(df_long, together_se, by = c("year_id","location_id","me_id"))

# Quick sanity‐check:
glimpse(df_long)
dat_1 = df_long %>% filter(me_id != "prim_any_scaled") %>% filter(me_id != "hosp_any_scaled") %>% filter(year_id == 2023)

# Step 2: Create shared log-MMR grid
nmr_grid <- seq(min(dat_1$nmr_both, na.rm = TRUE),
                max(dat_1$nmr_both, na.rm = TRUE),
                length.out = 200)
# #################################################################################
# Using MRBRT
df_smooth <- dat_1 %>%
  dplyr::group_by(me_id) %>%
  group_map(~{
    df_i <- .x %>%
      dplyr::mutate(
        y    = log(me_mean/(1-me_mean)), #logit
        y_se = 0.01 #constant se
      )
    
    dat_i <- mr$MRData()
    dat_i$load_df(
      data       = df_i,
      col_obs    = "y",
      col_obs_se = "y_se",
      col_covs   = list("nmr_both")
    )
    
    mod_i <- mr$MRBRT(
      data       = dat_i,
      cov_models = list(
        mr$LinearCovModel("intercept"),
        mr$LinearCovModel(
          "nmr_both",
          use_spline        = TRUE,
          spline_degree     = 2L,
          spline_knots      = array(seq(0, 1, length.out = 7)),
          spline_knots_type = "frequency",
          spline_l_linear   = TRUE,
          spline_r_linear   = TRUE
        )
      )
    )
    mod_i$fit_model()
    
    pred_df <- tibble(nmr_both = nmr_grid)
    pred_dat <- mr$MRData()
    pred_dat$load_df(data = pred_df, col_covs = list("nmr_both"))
    
    logit_preds <- mod_i$predict(data = pred_dat)
    prop_preds   <- plogis(logit_preds)   # inverse‐logit
    
    tibble(
      me_id       = unique(.y$me_id),
      val     = nmr_grid,
      smooth_mean = prop_preds
    )
  }) %>% dplyr::bind_rows()

df_prop <- df_smooth %>%
  #dplyr::mutate(smooth_mean = pmax(smooth_mean, 0)) %>%
  dplyr::group_by(val) %>%
  dplyr::mutate(
    proportion = smooth_mean / sum(smooth_mean, na.rm = TRUE)
  ) %>%
  dplyr::ungroup()

df_prop = df_prop %>%
  mutate(me_id = factor(me_id,
                        levels = c("non_facility","pub_prim","pub_hosp","priv_prim","priv_hosp")))
df_prop = df_prop %>% mutate( val = val * 1000)

#cairo_pdf(file.path(paste0(outdir, "Supplementary_Figure_17",date,".pdf")), width = 8, height = 6)
nmr_plot <- ggplot(df_prop, aes(x = val, y = proportion, fill = me_id)) +
  geom_area(position = position_stack(rev = TRUE), alpha = 0.9) +
  scale_x_reverse() +
  scale_fill_manual(
    values = c(
      "pub_prim"  = "maroon",
      "pub_hosp"  = "darkblue",
      "priv_prim" = "lightpink",
      "priv_hosp" = "lightblue",
      "non_facility"     = "grey"
    ),
    labels = c(
      "pub_prim"  = "Public Lower‐level",
      "pub_hosp"  = "Public hospital",
      "priv_prim" = "Private Lower‐level",
      "priv_hosp" = "Private hospital",
      "non_facility"     = "Non-facility"
    )
  ) + 
  labs(
    x     = "Neonatal deaths per 1000 live births",
    y     = "Proportion",
    fill  = "Facility type",
    title = "Figure 4b: Facility delivery mix versus NMR, 2023"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

print(nmr_plot)
#dev.off()

cairo_pdf(file.path(paste0(outdir, "Figure_4",date,".pdf")), width = 16, height = 6)
together = sdi_plot+nmr_plot
together = together + plot_annotation(
  title = "Figure 4: Facility delivery location mix versus social and economic development and neonatal health, 2023",
) 
#Figure 4: In-facility delivery location mix versus social and economic development and maternal health, 2023
together = together + plot_layout(guides = "collect")
print(together)
dev.off()

####By super region for appendix####
sr<- dplyr::select(locs, location_id, super_region_name)
dat_1<- left_join(dat_1, sr) %>%
  mutate(super_region_name = case_when(location_id==97~ "Latin America and Caribbean", 
                                       super_region_name == "Central Europe, Eastern Europe, and Central Asia" ~ "Central Europe, Eastern Europe,\n and Central Asia", 
                                       super_region_name == "Southeast Asia, East Asia, and Oceania" ~ "Southeast Asia, East Asia,\n and Oceania", 
                                       TRUE ~ super_region_name))
sr_dat<- data.frame()
# Step 2: Define shared SDI grid
for (supreg in unique(dat_1$super_region_name)) {
  dat_i <- subset(dat_1, super_region_name==supreg)
  sdi_grid <- seq(min(dat_i$mean_value, na.rm = TRUE),
                  max(dat_i$mean_value, na.rm = TRUE),
                  length.out = 200)
  
  df_smooth <- dat_i %>%
    dplyr::group_by(me_id) %>%
    group_map(~{
      df_i <- .x %>%
        dplyr::mutate(y = log(me_mean/(1-me_mean)), #logit
                      y_se = 0.01)
      # wrap into MRData
      dat_i <- mr$MRData()
      dat_i$load_df(
        data       = df_i,
        col_obs    = "y",
        col_obs_se = "y_se",
        col_covs   = list("mean_value")
      )
      
      # 1) minimal‐flex spline: 
      mod_i <- mr$MRBRT(
        data       = dat_i,
        cov_models = list(
          mr$LinearCovModel("intercept"),
          mr$LinearCovModel(
            "mean_value",
            use_spline         = TRUE,
            spline_degree      = 2L,                       # quadratic
            spline_knots       = array(seq(0, 1, length.out = 4)),
            spline_knots_type  = "frequency",               # density‐spaced
            spline_l_linear    = TRUE,                     # linear left tail
            spline_r_linear    = TRUE                      # linear right tail
            
          )
        )
      )
      mod_i$fit_model()
      
      # make predictions on the shared grid
      pred_df <- tibble(mean_value = sdi_grid)
      pred_dat <- mr$MRData()
      pred_dat$load_df(data = pred_df, col_covs = list("mean_value"))
      
      logit_preds <- mod_i$predict(data = pred_dat)
      prop_preds   <- plogis(logit_preds)   # inverse‐logit
      
      tibble(me_id = .y$me_id, mean_value = sdi_grid, smooth_mean = prop_preds)
    }) %>% bind_rows()
  
  df_prop <- df_smooth %>%
    dplyr:: group_by(mean_value) %>%
    dplyr:: mutate(proportion = smooth_mean / sum(smooth_mean)) %>%
    dplyr:: ungroup()
  df_prop <- df_prop %>% 
    mutate(me_id = factor(me_id,
                          levels = c("non_facility","pub_prim","pub_hosp","priv_prim","priv_hosp")), 
           super_region = sr)
  sr_dat<- rbind(sr_dat, df_prop)
}

    
    
sr_sdi_plot <- ggplot(sr_dat, aes(x = mean_value, y = proportion, fill = (me_id),group = me_id,order = as.numeric(me_id))) +
  geom_area(position = position_stack(reverse = TRUE), alpha = 0.9) +
  scale_fill_manual(
    values = c(
      "non_facility"     = "grey",
      "pub_prim"  = "maroon",
      "pub_hosp"  = "darkblue",
      "priv_prim" = "lightpink",
      "priv_hosp" = "lightblue"
    ),
    labels = c(
      "pub_prim"  = "Public Lower‐level",
      "pub_hosp"  = "Public hospital",
      "priv_prim" = "Private Lower‐level",
      "priv_hosp" = "Private hospital",
      "non_facility"     = "No facility"
    )
  ) +
  labs(
    x = "Socio-demographic index",
    y = "Proportion",
    title = "Facility delivery location mix versus SDI by region, 2023"
  ) +
  facet_wrap( ~super_region) +
  theme_minimal() +
  theme(legend.position = "none")

print(sr_sdi_plot)

#NMR
sr<- dplyr::select(locs, location_id, super_region_name)
dat_1<- left_join(dat_1, sr) %>%
  mutate(super_region_name = case_when(location_id==97~ "Latin America and Caribbean", 
                                       super_region_name == "Central Europe, Eastern Europe, and Central Asia" ~ "Central Europe, Eastern Europe,\n and Central Asia", 
                                       super_region_name == "Southeast Asia, East Asia, and Oceania" ~ "Southeast Asia, East Asia,\n and Oceania", 
                                       TRUE ~ super_region_name))
sr_dat<- data.frame()
for (sr in unique(dat_1$super_region_name)) {
  dat_i <- subset(dat_1, super_region_name==sr)
  nmr_grid <- seq(min(dat_i$nmr_both, na.rm = TRUE),
                max(dat_i$nmr_both, na.rm = TRUE),
                length.out = 200)
  # Using MRBRT
  df_smooth <- dat_i %>%
    dplyr::group_by(me_id) %>%
    group_map(~{
      df_i <- .x %>%
        dplyr::mutate(
          y    = log(me_mean/(1-me_mean)), #logit
          y_se = 0.01 #constant se
        )
      
      dat_i <- mr$MRData()
      dat_i$load_df(
        data       = df_i,
        col_obs    = "y",
        col_obs_se = "y_se",
        col_covs   = list("nmr_both")
      )
      
      mod_i <- mr$MRBRT(
        data       = dat_i,
        cov_models = list(
          mr$LinearCovModel("intercept"),
          mr$LinearCovModel(
            "nmr_both",
            use_spline        = TRUE,
            spline_degree     = 2L,
            spline_knots      = array(seq(0, 1, length.out = 4)),
            spline_knots_type = "frequency",
            spline_l_linear   = TRUE,
            spline_r_linear   = TRUE
          )
        )
      )
      mod_i$fit_model()
      
      pred_df <- tibble(nmr_both = nmr_grid)
      pred_dat <- mr$MRData()
      pred_dat$load_df(data = pred_df, col_covs = list("nmr_both"))
      
      logit_preds <- mod_i$predict(data = pred_dat)
      prop_preds   <- plogis(logit_preds)   # inverse‐logit
      
      tibble(
        me_id       = unique(.y$me_id),
        val     = nmr_grid,
        smooth_mean = prop_preds
      )
    }) %>% dplyr::bind_rows()
  
  df_prop <- df_smooth %>%
    #dplyr::mutate(smooth_mean = pmax(smooth_mean, 0)) %>%
    dplyr::group_by(val) %>%
    dplyr::mutate(
      proportion = smooth_mean / sum(smooth_mean, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
  
  df_prop = df_prop %>%
    mutate(me_id = factor(me_id,
                          levels = c("non_facility","pub_prim","pub_hosp","priv_prim","priv_hosp")))
  df_prop = df_prop %>% mutate( val = val * 1000, region = sr)
  sr_dat<- rbind(sr_dat, df_prop)
}
  
  sr_nmr_plot <- ggplot(sr_dat, aes(x = val, y = proportion, fill = me_id)) +
    geom_area(position = position_stack(rev = TRUE), alpha = 0.9) +
    scale_x_reverse() +
    scale_fill_manual(
      values = c(
        "pub_prim"  = "maroon",
        "pub_hosp"  = "darkblue",
        "priv_prim" = "lightpink",
        "priv_hosp" = "lightblue",
        "non_facility"     = "grey"
      ),
      labels = c(
        "pub_prim"  = "Public Lower‐level",
        "pub_hosp"  = "Public hospital",
        "priv_prim" = "Private Lower‐level",
        "priv_hosp" = "Private hospital",
        "non_facility"     = "Non-facility"
      )
    ) + 
    labs(
      x     = "Neonatal deaths per 1000 live births",
      y     = "Proportion",
      fill  = "Facility type",
      title = "Facility delivery mix versus NMR by region, 2023"
    ) +
    facet_wrap(~region) +
    theme_minimal() +
    theme(legend.position = "right")
  
  print(sr_nmr_plot)
