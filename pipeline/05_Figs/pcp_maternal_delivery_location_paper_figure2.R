#### Figure 2 for delivery location paper ####
#### written by Chiara Sumich ####
#### 2025-01-03 ####

#### Load packages ####
rm(list = ls())
pacman::p_load(data.table, ggplot2, gtools, tidyverse, haven, labelled, tidyselect, sf, weights, survey, scales, viridis, ggrepel, ggpubr,patchwork,cowplot)
source("/ihme/code/st_gpr/central/src/stgpr/api/public.R")

date <- Sys.Date()

#### setting filepaths ####
indir <- file.path("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/post_processing/aggregates")
outdir <- file.path(paste0("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/figures/", date, "/"))

#### Defining variables #### 
VERSION <- "2025-07-15" ## version of weighted aggregates st-gpr results


#### location data #### 
source('/ihme/cc_resources/libraries/current/r/get_location_metadata.R')
locs <- get_location_metadata(release_id = 16, location_set_id = 35)
locs_super_lmics = locs %>% filter(location_id %in% c(4,31,103,137,158,166))    
wb = read_excel("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/figures/mcconrad_deliver_location_paper_figures/CLASS.xlsx")
wb = wb %>% mutate(`Income group` = if_else(Code == "VEN", "Upper middle income", `Income group`))

#### loading datasets #### 
pub_hosp <- read_csv(file.path(indir, paste0("pub_hosp_bothsteps_st-gpr_results_weighted_aggregates_lmics_", VERSION, ".csv"))) %>%
  #### keeping only variables that we need to simplify things
  dplyr::select(year_id, age_group_id, sex_id, location_id, mean, lower, upper) %>%
  #### creating a me_id variable so we know which results correspond to each 
  dplyr:: mutate(me_id = "pub_hosp") 

priv_hosp <- read_csv(file.path(indir, paste0("priv_hosp_bothsteps_st-gpr_results_weighted_aggregates_lmics_", VERSION, ".csv"))) %>%
  #### keeping only variables that we need to simplify things
  dplyr::select(year_id, age_group_id, sex_id, location_id, mean, lower, upper) %>%
  #### creating a me_id variable so we know which results correspond to each 
  dplyr:: mutate(me_id = "priv_hosp")

pub_prim <- read_csv(file.path(indir, paste0("pub_prim_bothsteps_st-gpr_results_weighted_aggregates_lmics_", VERSION, ".csv"))) %>%
  #### keeping only variables that we need to simplify things
  dplyr::select(year_id, age_group_id, sex_id, location_id, mean, lower, upper) %>%
  #### creating a me_id variable so we know which results correspond to each 
  dplyr:: mutate(me_id = "pub_prim")

priv_prim <- read_csv(file.path(indir, paste0("priv_prim_bothsteps_st-gpr_results_weighted_aggregates_lmics_", VERSION, ".csv"))) %>%
  #### keeping only variables that we need to simplify things
  dplyr::select(year_id, age_group_id, sex_id, location_id, mean, lower, upper) %>%
  #### creating a me_id variable so we know which results correspond to each 
  dplyr:: mutate(me_id = "priv_prim")

hosp_any <- read_csv(file.path(indir, paste0("hosp_any_bothsteps_st-gpr_results_weighted_aggregates_lmics_", VERSION, ".csv"))) %>%
  #### keeping only variables that we need to simplify things
  dplyr::select(year_id, age_group_id, sex_id, location_id, mean, lower, upper) %>%
  #### creating a me_id variable so we know which results correspond to each 
  dplyr:: mutate(me_id = "hosp_any")

prim_any <- read_csv(file.path(indir, paste0("prim_any_bothsteps_st-gpr_results_weighted_aggregates_lmics_", VERSION, ".csv"))) %>%
  #### keeping only variables that we need to simplify things
  dplyr::select(year_id, age_group_id, sex_id, location_id, mean, lower, upper) %>%
  #### creating a me_id variable so we know which results correspond to each 
  dplyr:: mutate(me_id = "prim_any")

## creating one dataset #### 
df <- bind_rows(pub_hosp, priv_hosp, pub_prim, priv_prim,hosp_any, prim_any) %>%
  ## pivoting wider 
  pivot_wider(names_from = "me_id", values_from = c("mean", "lower", "upper")) %>%
  ## calculating non facility deliveries by summing facility deliveries and subtracting from 1
  mutate(mean_non_facility = 1 - (mean_pub_hosp + mean_priv_hosp + mean_pub_prim + mean_priv_prim))

#### get data points for vetting. might have to restart R session, update MEs! #### 
source("/ihme/code/st_gpr/central/src/stgpr/api/public.R")
pub_hosp_data = get_input_data(version_id = 225379, data_stage_name = "original") %>% mutate(me_id = "mean_pub_hosp")
priv_hosp_data = get_input_data(version_id = 224710, data_stage_name = "original") %>% mutate(me_id = "mean_priv_hosp")
pub_prim_data = get_input_data(version_id = 224711, data_stage_name = "original") %>% mutate(me_id = "mean_pub_prim")
priv_prim_data = get_input_data(version_id = 224714, data_stage_name = "original") %>% mutate(me_id = "mean_priv_prim")
hosp_any_data = get_input_data(version_id = 224715, data_stage_name = "original") %>% mutate(me_id = "mean_hosp_any")
prim_any_data = get_input_data(version_id = 224712, data_stage_name = "original") %>% mutate(me_id = "mean_prim_any")
df_data <- bind_rows(pub_hosp_data, priv_hosp_data, pub_prim_data, priv_prim_data,hosp_any_data,prim_any_data) %>% select(nid,year_id, location_id, sex_id, age_group_id, val, is_outlier, me_id)
df_data = df_data %>% left_join(locs, by = "location_id")

#### merge location data to delivery location aggregates #### 
df_super = df %>% filter(location_id %in% c(4,31,103,137,158,166)) %>% left_join(locs_super_lmics, by = "location_id") 
df = df %>% left_join(locs, by = "location_id")

#### breaking everything up into super regions, regions, countries, and subnats #### 
super_regions <- df_super %>%
  filter(year_id >= 1995 & year_id <= 2024) %>%
  dplyr::select(year_id, location_id, level, location_name, mean_pub_hosp, mean_priv_hosp, mean_pub_prim, mean_priv_prim, mean_non_facility,mean_hosp_any,mean_prim_any) %>%
  pivot_longer(cols = c("mean_pub_hosp", "mean_priv_hosp", "mean_pub_prim", "mean_priv_prim", "mean_non_facility","mean_hosp_any","mean_prim_any"), names_to = "me_id") %>%
  mutate(me_id = factor(me_id, levels = c("mean_pub_hosp", "mean_priv_hosp", "mean_pub_prim", "mean_priv_prim", "mean_non_facility","mean_hosp_any","mean_prim_any")))
regions <- df %>%
  filter(location_type == "region") %>%
  filter(!is.na(region_id)) %>%
  filter(super_region_id != 64) %>%
  filter(year_id >= 1995 & year_id <= 2023) %>%
  dplyr::select(year_id, location_id, level, location_name, mean_pub_hosp, mean_priv_hosp, mean_pub_prim, mean_priv_prim, mean_non_facility,mean_prim_any,mean_hosp_any) %>%
  pivot_longer(cols = c("mean_pub_hosp", "mean_priv_hosp", "mean_pub_prim", "mean_priv_prim", "mean_non_facility","mean_prim_any","mean_hosp_any"), names_to = "me_id") %>%
  mutate(me_id = factor(me_id, levels = c("mean_pub_hosp", "mean_priv_hosp", "mean_pub_prim", "mean_priv_prim", "mean_non_facility","mean_prim_any","mean_hosp_any")))
countries <- df %>%
  filter(location_name == "Ethiopia") %>%
  left_join(wb,by = c("ihme_loc_id" = "Code")) %>% # for paper
  filter(`Income group` %in% c("Low income", "Lower middle income", "Upper middle income")) %>%
  filter(ihme_loc_id != "CHN") %>%
  #filter(super_region_name != "High-income") %>%
  filter(location_type != "global" & location_type != "superregion" & location_type != "region") %>%
  filter(year_id >=1995 & year_id <= 2024) %>%
  filter(level == 3) %>%
  dplyr::select(year_id, location_id,ihme_loc_id, location_name, level, mean_pub_hosp, mean_priv_hosp, mean_pub_prim, mean_priv_prim, ,mean_hosp_any,mean_prim_any,mean_non_facility) %>%
  pivot_longer(cols = c("mean_pub_hosp", "mean_priv_hosp", "mean_pub_prim", "mean_priv_prim","mean_hosp_any","mean_prim_any","mean_non_facility"), names_to = "me_id") %>%
  mutate(me_id = factor(me_id, levels = c("mean_pub_hosp", "mean_priv_hosp", "mean_pub_prim", "mean_priv_prim","mean_hosp_any","mean_prim_any", "mean_non_facility"))) %>%
  arrange(location_name)
#update to specific subnats if you want
subnats <- df %>%
  filter(location_type != "global" & location_type != "superregion" & location_type != "region") %>%
  filter(year_id >=1995 & year_id <= 2024) %>%
  filter(level == 4 & parent_id %in% c(179, 180, 214, 165, 163, 11)) %>%
  dplyr::select(year_id, location_id,ihme_loc_id, location_name, level, mean_pub_hosp, mean_priv_hosp, mean_pub_prim, mean_priv_prim, ,mean_hosp_any,mean_prim_any,mean_non_facility) %>%
  pivot_longer(cols = c("mean_pub_hosp", "mean_priv_hosp", "mean_pub_prim", "mean_priv_prim","mean_hosp_any","mean_prim_any","mean_non_facility"), names_to = "me_id") %>%
  mutate(me_id = factor(me_id, levels = c("mean_pub_hosp", "mean_priv_hosp", "mean_pub_prim", "mean_priv_prim","mean_hosp_any","mean_prim_any", "mean_non_facility")))

#### Figure 2 for paper #### 
super_regions_dat = super_regions %>% filter(me_id != "mean_hosp_any" & me_id != "mean_prim_any")
super_regions_plot_dat = super_regions_dat %>%
  group_by(location_name,year_id,me_id)

cairo_pdf(paste0(outdir, "Figure_2_", date, ".pdf"), width=17, height=8.5, onefile = TRUE)
p <- ggplot(data = super_regions_dat,
            mapping = aes(x = year_id,
                          y = value,
                          fill = factor(me_id, levels = rev(c("mean_pub_hosp", "mean_priv_hosp", "mean_pub_prim", "mean_priv_prim", "mean_non_facility"))))) +
  geom_area(stat = "identity") +
  theme_bw() +
  theme(plot.title = element_text(size = 20),
        axis.title = element_text(size = 15),
        #axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.text = element_text(size = 12),
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.position = "bottom", # Move legend to the bottom
        panel.grid.major = element_blank(), # Remove major gridlines
        panel.grid.minor = element_blank(), # Remove minor gridlines
        ) +
  labs(y = "Proportion",
       x = "Time (years)",
       title = "Figure 2: Proportion of deliveries by delivery location by region, 1995-2023",
       fill = "Delivery location") +
  scale_fill_manual(values = c("mean_pub_hosp" = "darkblue", "mean_priv_hosp" = "lightblue", "mean_pub_prim" = "maroon", "mean_priv_prim" = "pink", "mean_non_facility" = "grey"),
                    labels = c("mean_pub_hosp" = "Public hospital",
                               "mean_priv_hosp" = "Private hospital",
                               "mean_pub_prim" = "Public Lower-level",
                               "mean_priv_prim" = "Private Lower-level", 
                               "mean_non_facility" = "Non-facility")) +
  guides(fill = guide_legend(nrow = 2)) + # Arrange legend items into two rows 
  facet_wrap(~location_name)
print(p)
dev.off()

## plot by regions
# cairo_pdf(paste0(outdir, "smooth_delivery_location__regions", date, ".pdf"), width=17, height=8.5, onefile = TRUE)
# p <- ggplot(data = regions,
#             mapping = aes(x = year_id,
#                           y = value,
#                           fill = factor(me_id, levels = rev(c("mean_pub_hosp", "mean_priv_hosp", "mean_pub_prim", "mean_priv_prim", "mean_non_facility"))))) +
#   geom_area(stat = "identity") +
#   theme_bw() +
#   theme(plot.title = element_text(size = 20),
#         axis.title = element_text(size = 15),
#         axis.text = element_text(size = 12),
#         legend.title = element_text(size = 15),
#         legend.text = element_text(size = 15),
#         legend.position = "bottom", # Move legend to the bottom
#         panel.grid.major = element_blank(), # Remove major gridlines
#         panel.grid.minor = element_blank(), # Remove minor gridlines
#   ) +
#   labs(y = "Percent",
#        x = "Time (years)",
#        title = "Percent of deliveries by delivery location in each region from 1990 to 2022",
#        fill = "Delivery location") +
#   scale_fill_manual(values = c("mean_pub_hosp" = "darkblue", "mean_priv_hosp" = "lightblue", "mean_pub_prim" = "maroon", "mean_priv_prim" = "pink", "mean_non_facility" = "white"),
#                     labels = c("mean_pub_hosp" = "Public & Non-profit hospital",
#                                "mean_priv_hosp" = "Private hospital", 
#                                "mean_pub_prim" = "Public & Non-profit Lower-level", 
#                                "mean_priv_prim" = "Private Lower-level", 
#                                "mean_non_facility" = "Non-facility")) +
#   guides(fill = guide_legend(nrow = 2)) + # Arrange legend items into two rows 
#   facet_wrap(~location_name)
# print(p)
# dev.off()

# 3 plot vetting visuals. Gives 4 submodels on top left, 2 overall models on bottom left, and the six models with scaled trend lines and data points on the right
countries_w_subnats = rbind(countries,subnats)
countries_w_subnats = countries_w_subnats %>% filter(str_detect(ihme_loc_id, "ETH_") | ihme_loc_id == "ETH")
countries_w_subnats = arrange(countries_w_subnats,ihme_loc_id)
countries_w_subnats = countries_w_subnats %>% filter(location_name == "United Arab Emirates")
year_limits <- c(1995:2024)
# df_data = df_data %>% mutate(is_outlier = if_else(nid == 56151 & (me_id == "mean_priv_hosp"), 1,is_outlier),
#                              is_outlier = if_else(nid == 234733 & (me_id == "mean_priv_hosp"), 0,is_outlier))
# df_data = df_data %>% mutate(is_outlier = if_else(nid == 234733 & me_id== "mean_priv_hosp",0,is_outlier))
cairo_pdf(paste0(outdir, "ETH_ONE", date, ".pdf"), width=16, height=8.5, onefile = TRUE)
for (i in (unique(countries_w_subnats$location_name))){
  print(i)
  plot_dat <- subset(countries_w_subnats, (location_name == i & year_id %in% year_limits & me_id != "mean_hosp_any" & me_id != "mean_prim_any"))
  #plot_dat2 <- df_data %>% filter(location_name == i & year_id %in% year_limits)
  plot_dat3 <- subset(countries_w_subnats, (location_name == i & year_id %in% year_limits & (me_id == "mean_hosp_any" | me_id == "mean_prim_any" | me_id == "mean_non_facility")))
  plot_dat4<- subset(countries_w_subnats, (location_name == i & year_id %in% year_limits))
  p <- ggplot(data = plot_dat,
              mapping = aes(x = year_id,
                            y = value,
                            fill = factor(me_id, levels = rev(c( "mean_priv_hosp","mean_pub_hosp","mean_pub_prim","mean_priv_prim","mean_prim_any","mean_hosp_any", "mean_non_facility"))))) +
    geom_area(stat = "identity") +
    theme_bw() +
    theme(plot.title = element_text(size = 20),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size = 15),
          legend.position = "bottom", # Move legend to the bottom
          panel.grid.major = element_blank(), # Remove major gridlines
          panel.grid.minor = element_blank(), # Remove minor gridlines
          ) +
    labs(
         x = "Time (years)",
        y = "Percent") +
    scale_fill_manual(values = c("mean_pub_hosp" = "darkblue", "mean_priv_hosp" = "lightblue", "mean_pub_prim" = "maroon", "mean_priv_prim" = "pink", "mean_non_facility" = "white","mean_hosp_any" = "lemonchiffon","mean_prim_any" = "purple"),
                      labels = c("mean_pub_hosp" = "Public hospital",
                                 "mean_priv_hosp" = "Private hospital",
                                 "mean_pub_prim" = "Public Lower-level",
                                 "mean_priv_prim" = "Private Lower-level",
                                 "mean_hosp_any" = "Hospital any sector",
                                 "mean_prim_any" = "Any primary level",
                                 "mean_non_facility" = "Non-facility")) +
    guides(fill = guide_legend(nrow = 2),) # Arrange legend items into two rows
  p2 <- ggplot(data = plot_dat3,
              mapping = aes(x = year_id,
                            y = value,
                            fill = factor(me_id, levels = rev(c("mean_prim_any","mean_hosp_any", "mean_non_facility"))))) +
    geom_area(stat = "identity") +
    theme_bw() +
    theme(plot.title = element_text(size = 20),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size = 15),
          legend.position = "bottom", # Move legend to the bottom
          panel.grid.major = element_blank(), # Remove major gridlines
          panel.grid.minor = element_blank(), # Remove minor gridlines
    ) +
    labs(
      x = "Time (years)",
    y = "Percent") +
    scale_fill_manual(values = c("mean_non_facility" = "white","mean_hosp_any" = "lemonchiffon","mean_prim_any" = "purple"),
                      labels = c(
                                 "mean_hosp_any" = "Hospital any sector",
                                 "mean_prim_any" = "Any primary level",
                                 "mean_non_facility" = "Non-facility")) +
    guides(fill = guide_legend(nrow = 2),) # Arrange legend items into two row
  p3 = ggplot(data = plot_dat4,
              mapping = aes(x = year_id,
                            y = value,
                            fill = factor(me_id, levels = rev(c("mean_pub_hosp", "mean_priv_hosp", "mean_pub_prim", "mean_priv_prim", "mean_non_facility","mean_pub_any","mean_prim_any"))))) +
    geom_line(aes(x = year_id, y = value),show.legend = F) +
    #geom_point(data = plot_dat2[!is.na(val)], aes(x = year_id, y = val, color = as.character(is_outlier)),show.legend = T) +
    scale_y_continuous("Deliveries", labels = percent, limits = c(0, 1), expand = c(0.03,0.03)) +
    scale_x_continuous("Year", breaks = c(seq(years[1], years[2], 10)), limits = years, expand = c(0.03,0.03)) +
    theme_bw() + theme(axis.text.x = element_text(angle = 65, hjust = 1),
                       legend.position = "bottom",
                       legend.direction = "horizontal") +
    #scale_color_manual(values = c("O" = "purple", "1" = "red"))+
    facet_wrap(~me_id, labeller = labeller(me_id =
                                             c("mean_pub_hosp" = "Public hospital",
                                               "mean_priv_hosp" = "Private hospital",
                                               "mean_pub_prim" = "Public Lower-level",
                                               "mean_priv_prim" = "Private Lower-level",
                                               "mean_hosp_any" = "Hospital any sector",
                                               "mean_prim_any" = "Any primary level",
                                               "mean_non_facility" = "Non-facility")
    ))+
    labs(y = "Percent",
         x = "Time (years)",
    ) +
    guides(color = guide_legend(title = "Outlier",direction = "vertical",),fill = "none") # Arrange legend items into two rows

  final_plot = (p / p2 |p3 )
  final_plot = final_plot + plot_annotation(title = i,
                                            theme = theme(plot.title = element_text(size = 20,hjust = 0.5)))
  final_plot
  print(final_plot)
  p
}
dev.off()

# appendix figure of all LMIC country trends 
plots <- list()
year_limits <- c(1995:2024)
# 1. Build list of plots (with their own legends)
plots <- lapply(unique(subnats$location_name), function(loc) {
  plot_dat <- subset(subnats,
                     location_name == loc &
                       year_id %in% year_limits &
                       ! me_id %in% c("mean_hosp_any", "mean_prim_any"))
  
  ggplot(plot_dat, aes(year_id, value,
                       fill = factor(me_id, levels = rev(c(
                         "mean_priv_hosp","mean_pub_hosp",
                         "mean_pub_prim","mean_priv_prim",
                         "mean_non_facility"
                       ))))) +
    geom_area(stat = "identity") +
    scale_fill_manual(
      name   = NULL,  # <-- drops legend title
      values = c(
        mean_pub_hosp     = "darkblue",
        mean_priv_hosp    = "lightblue",
        mean_pub_prim     = "maroon",
        mean_priv_prim    = "pink",
        mean_non_facility = "grey"
      ),
      labels = c(
        mean_pub_hosp     = "Public hospital",
        mean_priv_hosp    = "Private hospital",
        mean_pub_prim     = "Public lower-level",
        mean_priv_prim    = "Private lower-level",
        mean_non_facility = "Non-facility"
      )
    ) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    theme_bw() +
    theme(
      plot.title  = element_text(size = 16, hjust = 0.5),
      panel.grid.major = element_blank(), # Remove major gridlines
      panel.grid.minor = element_blank(), # Remove minor gridlines
      axis.title  = element_blank(),    # remove per‐panel axis titles
      axis.text   = element_text(size = 10),
      legend.text = element_text(size = 11)
    ) +
    ggtitle(loc)
})

shared_legend <- get_legend(plots[[1]]) #+ theme(legend.position = "bottom"))

#make into pages of 6
plots <- lapply(plots, function(p) p + guides(fill="none"))
plot_pages <- split(plots, ceiling(seq_along(plots) / 6))

#write to PDF:
pdf(paste0(outdir, "/phc_pst_subnats", date, ".pdf"), width = 16, height = 10)
for(page_plots in plot_pages) {
  # a) make the 2×3 grid
  grid_plots <- plot_grid(plotlist = page_plots,
                          ncol     = 2,
                          nrow     = 3,
                          align    = "hv")
  
  # b) shared x‐axis label grob
  x_lab <- ggdraw() +
    draw_label("Time (years)", fontface = "plain", size = 14)
  
  # c) shared y‐axis label grob
  y_lab <- ggdraw() +
    draw_label("Percent", fontface = "plain", size = 14, angle = 90)
  
  # d) stack them: [ grid | xlab | legend ], with ylab on the left
  final_page <- plot_grid(
    y_lab, 
    plot_grid(
      grid_plots,
      x_lab,
      shared_legend,
      ncol        = 1,
      rel_heights = c(10, 0.5, 1)
    ),
    ncol        = 2,
    rel_widths  = c(0.3, 10)    # give some room for the y‐axis label
  )
  
  print(final_page)
}

dev.off()

##### Figure 3: Absolute change over time by super region from 1995-2023 ##### 
df_abs = df %>%
  filter(location_id %in% c(4,31,103,137,158,166)) %>% left_join(locs_super_lmics, by = "location_id") %>%
  filter(year_id == 1995 | year_id == 2023) %>%
  dplyr::select(year_id, location_id,location_name.x, mean_pub_hosp, mean_priv_hosp, mean_pub_prim, mean_priv_prim)

#for each country, find the absolute change, then find the average change for each super region
sum = df_abs %>%
  dplyr::group_by(location_name.x) %>% dplyr::summarise(abs_change_pub_hosp = mean_pub_hosp[year_id == 2023] - mean_pub_hosp[year_id == 1995],
                                      abs_change_priv_hosp = mean_priv_hosp[year_id == 2023] - mean_priv_hosp[year_id == 1995],
                                      abs_change_pub_prim = mean_pub_prim[year_id == 2023] - mean_pub_prim[year_id == 1995],
                                      abs_change_priv_prim = mean_priv_prim[year_id == 2023] - mean_priv_prim[year_id == 1995])

sum = sum %>% pivot_longer(cols = c("abs_change_pub_hosp", "abs_change_priv_hosp", "abs_change_pub_prim", "abs_change_priv_prim"))
cairo_pdf(paste0(outdir, "Figure_3_", date, ".pdf"), width=17, height=8.5, onefile = TRUE)
mockup <- ggplot(data = sum,
            mapping = aes(x = factor(location_name.x), #levels = c("Sub-Saharan Africa","Western Sub-Saharan Africa","Nigeria")
                          y = value,
                          fill = factor(name, levels = rev(c( "abs_change_pub_hosp", "abs_change_priv_hosp", "abs_change_pub_prim", "abs_change_priv_prim", "abs_change_non_facility"))))) +
  geom_col() +
  theme_bw() +
  theme(plot.title = element_text(size = 20),
        axis.title = element_text(size = 15),
        legend.title = element_blank(),
        legend.text = element_text(size = 15),
        legend.position = "bottom", # Move legend to the bottom
        panel.grid.major = element_blank(), # Remove major gridlines
        panel.grid.minor = element_blank(), # Remove minor gridlines
  ) +
  scale_x_discrete(labels = label_wrap(20))+
  ### dahsed line at y axis 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  ggtitle("Figure 3: Absolute change in facility delivery by facility type, 1995-2023") +
  labs(
    y = "Absolute change in proportion", x = "") +
  scale_fill_manual(labels = c("abs_change_pub_hosp" = "Public hospital",
                              "abs_change_priv_hosp" = "Private hospital",
                              "abs_change_pub_prim" = "Public lower-level",
                              "abs_change_priv_prim" = "Private lower-level"),
                    values = c("abs_change_pub_hosp" = "darkblue", "abs_change_priv_hosp" = "lightblue", "abs_change_pub_prim" = "maroon", "abs_change_priv_prim" = "pink")) +

  guides(fill = guide_legend(nrow = 2),) # Arrange legend items into two rows
print(mockup)
dev.off()




#######This is all old vetting code ############
# ##Absolute change over time for Burkina Faso, Ethiopia, Kenya, and Nigeria from 1995-2023 in a stacked bar plot for pub_prim,priv_prim,pub_hosp, and priv_hosp
# df_four = df %>%
#   filter(location_name == "Burkina Faso" | location_name == "Ethiopia" | location_name == "Kenya" | location_name == "Nigeria") %>%
#   filter(year_id >= 1995 & year_id <= 2023) %>%
#   ## selecting only variables we need to simplify things
#   dplyr::select(year_id, location_id, location_name, mean_pub_hosp, mean_priv_hosp, mean_pub_prim, mean_priv_prim)
# sum_four = df_four %>%
#   dplyr::group_by(location_name) %>% dplyr::summarise(abs_change_pub_hosp = mean_pub_hosp[year_id == 2023] - mean_pub_hosp[year_id == 1995],
#                                                       abs_change_priv_hosp = mean_priv_hosp[year_id == 2023] - mean_priv_hosp[year_id == 1995],
#                                                       abs_change_pub_prim = mean_pub_prim[year_id == 2023] - mean_pub_prim[year_id == 1995],
#                                                       abs_change_priv_prim = mean_priv_prim[year_id == 2023] - mean_priv_prim[year_id == 1995]) 
# 
# sum_four = sum_four %>% pivot_longer(cols = c("abs_change_pub_hosp", "abs_change_priv_hosp", "abs_change_pub_prim", "abs_change_priv_prim"), names_to = "me_id", values_to = "value")
# #plot stacked bar
# cairo_pdf(paste0(outdir, "absolute_change_four_countries", date, ".pdf"), width=17, height=8.5, onefile = TRUE)
# plot = ggplot(data = sum_four,
#             mapping = aes(x = location_name,
#                           y = value,
#                           fill = factor(me_id, levels = rev(c("abs_change_pub_hosp", "abs_change_priv_hosp", "abs_change_pub_prim", "abs_change_priv_prim"))))) +
#   geom_col() +
#   theme_bw() +
#   theme(plot.title = element_text(size = 20),
#         axis.title = element_text(size = 15),
#         axis.title.y = element_text(angle = 0, vjust = 0.5),
#         axis.text = element_text(size = 12),
#         legend.title = element_blank(),
#         legend.text = element_text(size = 15),
#         legend.position = "bottom", # Move legend to the bottom
#         panel.grid.major = element_blank(), # Remove major gridlines
#         panel.grid.minor = element_blank(), # Remove minor gridlines
#   ) +
#   labs(y = "Absolute Change",
#        x = "") +
#   scale_fill_manual(values = c("abs_change_pub_hosp" = "darkblue", "abs_change_priv_hosp" = "lightblue", "abs_change_pub_prim" = "maroon", "abs_change_priv_prim" = "pink"),
#                     labels = c("abs_change_pub_hosp" = "Public hospital",
#                                "abs_change_priv_hosp" = "Private hospital",
#                                "abs_change_pub_prim" = "Public Lower-level",
#                                "abs_change_priv_prim" = "Private Lower-level")) +
#   guides(fill = guide_legend(nrow = 2)) # Arrange legend items into two rows
# plot
# dev.off()
# 
# ##Super region time trend plot. line plots of the six types of me_ids from 1995-2024 without non facility
# super_regions_plot_dat = super_regions %>% filter(year_id %in%(1995:2024) & me_id != "mean_non_facility")%>%
#   left_join(locs) %>%
#   #left_join(wb_groups, by = c("ihme_loc_id" = "Code")) %>%
#   #filter(`Income group` %in% c("Low income", "Lower middle income", "Upper middle income")) %>%
#   group_by(super_region_name,year_id,me_id) %>%
#   dplyr::summarise(value = mean(value, na.rm = TRUE))
# 
# cairo_pdf(paste0(outdir, "super_region_time_trend", date, ".pdf"), width=17, height=8.5, onefile = TRUE)
# p = ggplot(super_regions_plot_dat,aes(x = year_id, y = value)) +
#   geom_line(aes(color = me_id),size = 1) +
#   facet_wrap(~super_region_name) +
#   theme_bw() +
#   theme(plot.title = element_text(size = 20),
#         axis.title = element_text(size = 15),
#         axis.text = element_text(size = 12),
#         legend.title = element_text(size = 15),
#         legend.text = element_text(size = 15),
#         legend.position = "bottom", # Move legend to the bottom
#         panel.grid.major = element_blank(), # Remove major gridlines
#         panel.grid.minor = element_blank(), # Remove minor gridlines
#   ) +
#   labs(y = "Percent",
#        x = "Time (years)",
#        title = "Percent of deliveries by delivery location in each super-region from 1995 to 2023",
#        color = "Delivery location") +
#   scale_color_manual(values = c("mean_pub_hosp" = "darkblue", "mean_priv_hosp" = "lightblue", "mean_pub_prim" = "maroon", "mean_priv_prim" = "pink", "mean_hosp_any" = "orange","mean_prim_any" = "purple"),
#                     labels = c("mean_pub_hosp" = "Public hospital",
#                                "mean_priv_hosp" = "Private hospital",
#                                "mean_pub_prim" = "Public Lower-level",
#                                "mean_priv_prim" = "Private Lower-level",
#                                "mean_hosp_any" = "Hospital any sector",
#                                "mean_prim_any" = "Any primary level",
#                                "mean_non_facility" = "Non-facility")) +
#   guides(color = guide_legend(nrow = 2)) # Arrange legend items into two rows
# p
# # print(p)
# # dev.off()
# # #same as above but reversed
# super_regions_plot_dat = super_regions %>% filter(year_id %in%(1995:2024) & me_id != "mean_non_facility") %>% filter(location_name == "South Asia" | location_name == "Sub-Saharan Africa")
# cairo_pdf(paste0(outdir, "super_region_time_trend_2_free", date, ".pdf"), width=17, height=8.5, onefile = TRUE)
# p = ggplot(super_regions_plot_dat,aes(x = year_id, y = value)) +
#   geom_line(aes(color = location_name),size = 1) +
#   facet_wrap(~me_id,scales = "free") +
#   theme_bw() +
#   theme(plot.title = element_text(size = 20),
#         axis.title = element_text(size = 15),
#         axis.text = element_text(size = 12),
#         legend.title = element_text(size = 15),
#         legend.text = element_text(size = 15),
#         legend.position = "bottom", # Move legend to the bottom
#         panel.grid.major = element_blank(), # Remove major gridlines
#         panel.grid.minor = element_blank(), # Remove minor gridlines
#   ) +
#   labs(y = "Percent",
#        x = "Time (years)",
#        title = "Percent of deliveries by delivery location in each super-region from 1995 to 2023",
#        color = "") +
#   # scale_color_manual(values = c("mean_pub_hosp" = "darkblue", "mean_priv_hosp" = "lightblue", "mean_pub_prim" = "maroon", "mean_priv_prim" = "pink", "mean_hosp_any" = "orange","mean_prim_any" = "purple"),
#   #                    labels = c("mean_pub_hosp" = "Public hospital",
#   #                               "mean_priv_hosp" = "Private hospital",
#   #                               "mean_pub_prim" = "Public Lower-level",
#   #                               "mean_priv_prim" = "Private Lower-level",
#   #                               "mean_hosp_any" = "Hospital any sector",
#   #                               "mean_prim_any" = "Any primary level",
#   #                               "mean_non_facility" = "Non-facility")) +
#   guides(color = guide_legend(nrow = 2)) # Arrange legend items into two rows
# 
# print(p)
# dev.off()
# 
# 
# ################################################################################################################################################
# ################################################################################################################################################
# ################################################################################################################################################
# #reading scaled data with subnats for countries of interest
# VERSION = "2025-05-30"
# indir = "/ihme/scratch/projects/hssa/pcp/delivery_location_remapping//mcconrad_post_gpr/scaled data/"
# pub_hosp <- read_csv(file.path(indir, paste0("pub_hosp_st-gpr_results_scaled_", VERSION, ".csv"))) %>%
#   #### keeping only variables that we need to simplify things
#   dplyr::select(year_id, age_group_id, sex_id, location_id, mean, lower, upper) %>%
#   #### updating variable names so we know which results correspond to each 
#   mutate(me_id = "pub_hosp")
# 
# pub_prim <- read_csv(file.path(indir, paste0("pub_prim_st-gpr_results_scaled_", VERSION, ".csv"))) %>%
#   #### keeping only variables that we need to simplify things
#   dplyr::select(year_id, age_group_id, sex_id, location_id, mean, lower, upper) %>%
#   #### updating variable names so we know which results correspond to each 
#   mutate(me_id = "pub_prim")
# 
# 
# priv_hosp <- read_csv(file.path(indir, paste0("priv_hosp_st-gpr_results_scaled_", VERSION, ".csv"))) %>%
#   #### keeping only variables that we need to simplify things
#   dplyr::select(year_id, age_group_id, sex_id, location_id, mean, lower, upper) %>%
#   #### updating variable names so we know which results correspond to each 
#   mutate(me_id = "priv_hosp")
# 
# priv_prim <- read_csv(file.path(indir, paste0("priv_prim_st-gpr_results_scaled_", VERSION, ".csv"))) %>%
#   #### keeping only variables that we need to simplify things
#   dplyr::select(year_id, age_group_id, sex_id, location_id, mean, lower, upper) %>%
#   #### updating variable names so we know which results correspond to each 
#   mutate(me_id = "priv_prim")
# 
# df_nat_subnat = rbind(pub_prim,pub_hosp,priv_prim,priv_hosp)
# df_nat_subnat = df_nat_subnat %>% pivot_wider(names_from = me_id, values_from = c(mean, lower, upper)) 
# df_nat_subnat = df_nat_subnat %>% left_join(locs,by = c("location_id"))
# #Just one country geom_area plot with large national plot on left and facet wrap smaller state plots on right without prim_any and hosp_any
# wb_groups <- read_excel("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/figures/mcconrad_deliver_location_paper_figures/CLASS.xlsx")
# nat = df_nat_subnat %>%
#   # left_join(wb_groups, by = c("ihme_loc_id" = "Code")) %>%
#   # filter(`Income group` %in% c("Low income", "Lower middle income", "Upper middle income")) %>%
#  #filter(location_name == "Kenya") %>%
#   filter(location_name == "Mexico") %>%
#   filter(year_id >=1995 & year_id <= 2024) %>%
#   filter(level == 3) %>%
#   dplyr::select(year_id, location_id, location_name, level, mean_pub_hosp, mean_priv_hosp, mean_pub_prim, mean_priv_prim) %>%
#   pivot_longer(cols = c("mean_pub_hosp", "mean_priv_hosp", "mean_pub_prim", "mean_priv_prim"), names_to = "me_id") %>%
#   mutate(me_id = factor(me_id, levels = c("mean_pub_hosp", "mean_priv_hosp", "mean_pub_prim", "mean_priv_prim"))) %>%
#   filter(me_id != "mean_hosp_any" & me_id != "mean_prim_any")
# subnat = df_nat_subnat %>%
#   #filter(location_name == "Uttar Pradesh" | location_name == "Bihar") %>%
#   filter(str_detect(ihme_loc_id,"IND_")) %>%
#   filter(year_id >=1995 & year_id <= 2024) %>%
#   filter(level == 4) %>%
#   dplyr::select(year_id, location_id, location_name, level, mean_pub_hosp, mean_priv_hosp, mean_pub_prim, mean_priv_prim) %>%
#   pivot_longer(cols = c("mean_pub_hosp", "mean_priv_hosp", "mean_pub_prim", "mean_priv_prim"), names_to = "me_id") %>%
#   mutate(me_id = factor(me_id, levels = c("mean_pub_hosp", "mean_priv_hosp", "mean_pub_prim", "mean_priv_prim"))) %>%
#   filter(me_id != "mean_hosp_any" & me_id != "mean_prim_any")
# cairo_pdf(paste0(outdir, "India_subnat", date, ".pdf"), width=16, height=8.5, onefile = TRUE)
# p = ggplot(data = nat,
#             mapping = aes(x = year_id,
#                           y = value,
#                           fill = factor(me_id, levels = rev(c("mean_priv_hosp", "mean_pub_hosp", "mean_pub_prim", "mean_priv_prim", "mean_non_facility"))))) +
#   geom_area(stat = "identity") +
#   theme_bw() +
#   facet_wrap(~location_name) + #adjust this
#   theme(plot.title = element_text(size = 20),
#         axis.title = element_text(size = 15),
#         axis.text = element_text(size = 12),
#         legend.title = element_text(size = 15),
#         legend.text = element_text(size = 15),
#         legend.position = "bottom", # Move legend to the bottom
#         panel.grid.major = element_blank(), # Remove major gridlines
#         panel.grid.minor = element_blank(), # Remove minor gridlines
#   ) +
#   labs(y = "Percent",
#        x = "Time (years)",
#        title = "Percent of deliveries by delivery location in India from 1995 to 2023",
#        fill = "Delivery location") +
#   scale_fill_manual(values = c("mean_pub_hosp" = "darkblue", "mean_priv_hosp" = "lightblue", "mean_pub_prim" = "maroon", "mean_priv_prim" = "pink", "mean_non_facility" = "white"),
#                     labels = c("mean_pub_hosp" = "Public hospital",
#                                "mean_priv_hosp" = "Private hospital",
#                                "mean_pub_prim" = "Public Lower-level",
#                                "mean_priv_prim" = "Private Lower-level",
#                                "mean_non_facility" = "Non-facility")) +
#   guides(fill = guide_legend(nrow = 2)) # Arrange legend items into two rows
# print(p)
# p2 = ggplot(data = subnat,
#             mapping = aes(x = year_id,
#                           y = value,
#                           fill = factor(me_id, levels = rev(c("mean_pub_hosp", "mean_priv_hosp", "mean_pub_prim", "mean_priv_prim", "mean_non_facility"))))) +
#   geom_area(stat = "identity") +
#   theme_bw() +
#   facet_wrap(~location_name) + #adjust this
#   theme(plot.title = element_text(size = 20),
#         axis.title = element_text(size = 15),
#         axis.text = element_text(size = 12),
#         legend.title = element_text(size = 15),
#         legend.text = element_text(size = 15),
#         legend.position = "bottom", # Move legend to the bottom
#         panel.grid.major = element_blank(), # Remove major gridlines
#         panel.grid.minor = element_blank(), # Remove minor gridlines
#   ) +
#   labs(y = "",
#        x = "Time (years)",
#        #title = "Percent of deliveries by delivery location in India from 1995 to 2023",
#        fill = "Delivery location") +
#   scale_fill_manual(values = c("mean_pub_hosp" = "darkblue", "mean_priv_hosp" = "lightblue", "mean_pub_prim" = "maroon", "mean_priv_prim" = "pink", "mean_non_facility" = "white"),
#                     labels = c("mean_pub_hosp" = "Public hospital",
#                                "mean_priv_hosp" = "Private hospital",
#                                "mean_pub_prim" = "Public Lower-level",
#                                "mean_priv_prim" = "Private Lower-level",
#                                "mean_non_facility" = "Non-facility")) +
#   guides(fill = guide_legend(nrow = 2)) # Arrange legend items into two rows
# p2
# 
# final = (p | p2)
# final = (final+plot_layout(guides = "collect") & theme(legend.position = "bottom"))
# print(final)
# dev.off()
# 
# #four panel plot of India, Ethiopia, Nigeria, and Kenya at the national level showing the geom area plots for pub_prim, priv_prim,pub_hosp,priv_hosp, and non facility over time
# nats = df %>%
#   filter(location_name == "India") %>%
#   filter(year_id >=1995 & year_id <= 2024) %>%
#   filter(level == 3) %>%
#   dplyr::select(year_id, location_id, location_name, level, mean_pub_hosp, mean_priv_hosp, mean_pub_prim, mean_priv_prim, ,mean_hosp_any,mean_prim_any,mean_non_facility) %>%
#   pivot_longer(cols = c("mean_pub_hosp", "mean_priv_hosp", "mean_pub_prim", "mean_priv_prim","mean_hosp_any","mean_prim_any","mean_non_facility"), names_to = "me_id") %>%
#   mutate(me_id = factor(me_id, levels = c("mean_pub_hosp", "mean_priv_hosp", "mean_pub_prim", "mean_priv_prim","mean_hosp_any","mean_prim_any", "mean_non_facility"))) %>%
#   filter(me_id != "mean_hosp_any" & me_id != "mean_prim_any")
# cairo_pdf(paste0(outdir, "four_panel_national", date, ".pdf"), width=16, height=8.5, onefile = TRUE)
# p = ggplot(data = nats,
#             mapping = aes(x = year_id,
#                           y = value,
#                           fill = factor(me_id, levels = rev(c("mean_pub_hosp", "mean_priv_hosp", "mean_pub_prim", "mean_priv_prim", "mean_non_facility"))))) +
#   geom_area(stat = "identity") +
#   theme_bw() +
#   facet_wrap(~location_name) +
#   theme(plot.title = element_text(size = 20),
#         axis.title = element_text(size = 15),
#         axis.title.y = element_text(angle = 0, vjust = 0.5),
#         axis.text = element_text(size = 12),
#         legend.title = element_text(size = 15),
#         legend.text = element_text(size = 15),
#         legend.position = "bottom", # Move legend to the bottom
#         panel.grid.major = element_blank(), # Remove major gridlines
#         panel.grid.minor = element_blank(), # Remove minor gridlines
#   ) +
#   labs(y = "Percent",
#        x = "Time (years)",
#        fill = "Delivery location") +
#   scale_fill_manual(values = c("mean_pub_hosp" = "darkblue", "mean_priv_hosp" = "lightblue", "mean_pub_prim" = "maroon", "mean_priv_prim" = "lightpink", "mean_non_facility" = "white"),
#                     labels = c("mean_pub_hosp" = "Public hospital",
#                                "mean_priv_hosp" = "Private hospital",
#                                "mean_pub_prim" = "Public Lower-level",
#                                "mean_priv_prim" = "Private Lower-level",
#                                "mean_non_facility" = "Non-facility")) +
#   guides(fill = guide_legend(nrow = 2)) # Arrange legend items into two rows
# print(p)
# dev.off()
# 