###########################################################################################################
### Author: Will Gardner (wgard@uw.edu)
###         Adapted from Simon Yadgir (syadgir)
### Date: 06/12/2019
### Project: ST-GPR
### Purpose: Explore Linear prior covariates from the test_prior() function
###########################################################################################################
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(yaml)
library(patchwork)

ROOT = file.path(h, 'repos', 'pcp')
settings = yaml.load_file(file.path(ROOT, 'config.yml'))

# if version_label is not defined, warn
# TODO: This version_label var needs to be global (i.e. in settings config) or else this script needs to be a function
if(!exists("version_label")){
  warning("No version_label for plot_prior_betas.R")
}

######################################
################ ARGS ################
######################################

if(!exists("me")){
  stop("No ME")
}
x_var <- "out_rmse" # in_rmse or out_rmse or aic
plot_pairs <- T

linear_oos_dir <- file.path("/ihme/scratch/projects/hssa/pcp/delivery_location_remapping/mcconrad_pre_gpr/", me, '02_linear_oos')
input_folder <- file.path(linear_oos_dir, 'rmse', version_label)
plot_output <- file.path(linear_oos_dir, version_label, paste0(me, "_cov_selection_beta_plots.pdf"))
pairs_plot_output <- file.path(linear_oos_dir, version_label, paste0(me, "_cov_selection_beta_pairs_plots.pdf"))

#############################################
################ GET RESULTS ################
#############################################

betas <- get_recent(input_folder)
betas[, mod_num:=1:.N]
# get covariates used
covs<-grep("*_fixd", names(betas), value=T)
covs<-covs[!grepl("_fixd_se", covs)]

covs<-unlist(lapply(covs, function(x){
  temp<-unlist(tstrsplit(x, "_"))
  temp<-temp[1:(length(temp)-1)]
  paste0(temp, collapse="_")
}))

message("Covariates tested: ", paste(covs, collapse=", "))

# reshape betas to show multiple metrics
betas_melt<-melt(betas, id.vars=setdiff(names(betas), c("aic", "in_rmse", "out_rmse")), 
                 measure_vars=c("aic", "in_rmse", "out_rmse"), variable.name = "Metric", value.name = "Fit")


###########################################################
################ GET WEIGHTS AND AGGREGATE ################
###########################################################

# get weights
scaler<-sum(betas[drop==0, c(x_var), with=F])

betas[drop==0, wt:=get(x_var)/scaler]
betas[drop==1, wt:=0]
betas[drop==0, draws:=wt*1000]

betas_out_path <- file.path(linear_oos_dir, paste0(me, "_cov_selection_betas_", 
                                                       Sys.Date(), ".csv"))
message("Writing out betas to ", betas_out_path)
fwrite(betas[drop==0,],
       file=betas_out_path)

# expand then aggregate weighted for first hist
expanded_full<-list()
for(cov in covs){
  expanded<-copy(betas)
  expanded[!is.na(get(paste0(cov,"_fixd"))), cov:=cov]
  expanded<-expanded[!is.na(cov)]
  expanded_full[[length(expanded_full)+1]]<-expanded
}
expanded_full<-rbindlist(expanded_full)
expanded_full<-expanded_full[cov!="(Intercept)"]

aggd<-expanded_full[, .(total_draws=sum(draws, na.rm=T)), by=.(sex, cov)]
aggd$cov_fact<-factor(aggd$cov, levels = unique(aggd[sex=="both_sexes",][order(total_draws, decreasing=T), cov]))


##############################################
################ PLOT RESULTS ################
##############################################
pdf(file=plot_output, width=15)

angle<-60
angle<-angle+length(unique(covs))*2
if(angle>90){angle<-90}

# plot summary weights of each model
# p<-ggplot(data=aggd, aes(x=cov_fact, y=total_draws))+
#   geom_bar(stat="identity")+
#   xlab("")+
#   ylab("Number of draws")+
#   scale_fill_brewer( palette = "Set1")+
#   guides(fill="none")+
#   theme_classic()+
#   theme(panel.border =  element_rect(color="black", fill=NA),
#         text = element_text(size=17),
#         axis.text.x = element_text(color="black", angle=angle, hjust=1))
# print(p)

me_labels <- c(
  pub_hosp = "Public hospital",
  priv_hosp = "Private hospital",
  pub_prim = "Public lower-level",
  priv_prim = "Private lower-level",
  hosp_any= "Hospital any sector",     # etc…
  prim_any = "Lower-level any sector"
)

cov_labels <- c(
  domestic_he_cap      = "Domestic health expenditure per capita",
  sdi = "Socio-demographic index",
  `(Intercept)` = "Intercept",
  SBA_coverage_prop = "Skilled birth attendant coverage",
  ANC1_coverage_prop   = "ANC1 coverage",
  ANC4_coverage_prop   = "ANC4 coverage",
  csection_coverage_prop  = "Caesarean coverage",
  haqi            = "Healthcare access and quality index",
  pct_births_in_over35s      = "Proportion of live births by mothers age 35 and older",
  universal_health_coverage = "Universal health coverage",
  TFR = "Total fertility rate",
  frac_oop_hexp = "Fraction of out-of-pocket health expenditure"
)
plot_list <- list()
for(cov in covs){
  var_name <- paste0(cov, "_fixd")
  me_lab  <- me_labels[me]
  cov_lab <- cov_labels[cov]
  #p<-ggplot(data=betas[drop == 0 & (sign_violation == 0)], aes(x=get(x_var), y=get(paste0(cov,"_fixd")), color=sex))+
  #   geom_point(size=2)+
  #   # dropped models are x's in the plots
  #   geom_point(data=betas[drop==1], aes(x=get(x_var), y=get(paste0(cov,"_fixd"))), shape=4, size=2, alpha=.5)+
  #   geom_errorbar(aes(ymin=get(paste0(cov,"_fixd"))-1.96*get(paste0(cov,"_fixd_se")),
  #                     ymax=get(paste0(cov,"_fixd"))+1.96*get(paste0(cov,"_fixd_se"))))+
  #   geom_abline(aes(intercept=0, slope=0))+
  #   geom_rug(data=betas[is.na(sign_violation)], aes(x=get(x_var)))+
  #   scale_color_brewer(palette="Set1")+
  #   xlab(x_var)+
  #   ylab("beta")+
  #   ggtitle(paste0("Covariates for ", me, " vs ", cov))+
  #   facet_wrap(~sex)+
  #   theme_bw()+
  #   theme(text=element_text(size=17),
  #         panel.border =  element_rect(color="black", fill=NA))
  # print(p)
  # 
  ##sy: plot hist
  p <- ggplot(betas[drop == 0,]) +
    geom_histogram(
      aes(x = .data[[var_name]]),   # <<–– here
      color = "black"
    ) +
    geom_vline(xintercept = 0) +
    labs(
      x     = paste0("Betas"), #for ", cov_lab
      y     = "Count",
      title = paste0(me_lab, " versus\n", cov_lab)
    ) +
    theme_classic() +
    theme(
      title = element_text(size = 11.5),
      text         = element_text(size = 10.5),
      panel.border = element_rect(color = "black", fill = NA)
    )
  
 # print(p)
  plot_list[[cov]] <- p
  
  # p<-ggplot(data=betas_melt, aes(x=Fit, y=get(paste0(cov,"_fixd")), color=sex))+
  #   geom_point(size=2)+
  #   geom_errorbar(aes(ymin=get(paste0(cov,"_fixd"))-1.96*get(paste0(cov,"_fixd_se")),
  #                     ymax=get(paste0(cov,"_fixd"))+1.96*get(paste0(cov,"_fixd_se"))))+
  #   geom_abline(aes(intercept=0, slope=0))+
  #   scale_color_brewer(palette="Set1")+
  #   facet_wrap(~Metric, scales="free")+
  #   #xlab()+
  #   ylab("beta")+
  #   ggtitle(paste0("Covariates for ", me, " vs ", cov))+
  #   theme_bw()+
  #   theme(text=element_text(size=17),
  #         panel.border =  element_rect(color="black", fill=NA))
  # print(p)
  
}
combined_plot <- wrap_plots(plot_list, ncol = 3)  # 3 columns, ~4 rows
ggsave("combined_betas_histograms.png", combined_plot, width = 16, height = 12, dpi = 300)
print(combined_plot)
#dev.off()


################### PLOT PAIRS #########################################
#####################################################
if(plot_pairs==T){
  message("Plotting pairs...")
  cov_pairs<-combn(covs, 2)
  
  pdf(file=pairs_plot_output)
  for(i in 1:ncol(cov_pairs)){
    pair<-cov_pairs[, i]
    
    p<-ggplot(data=betas, aes(x=get(paste0(pair[1], "_fixd")), y=get(paste0(pair[2], "_fixd"))))+
      geom_point(aes(color=get(x_var)))+
      xlab(pair[1])+
      ylab(pair[2])+
      ggtitle(paste0(me, " beta covariance: ", pair[1], " vs ", pair[2]))+
      facet_wrap(~sex)+
      scale_color_continuous(name = x_var)+
      theme_bw()
    print(p)
  }
  dev.off()
  message("Done")
}

message("Prior betas plots saved here: ", plot_output)