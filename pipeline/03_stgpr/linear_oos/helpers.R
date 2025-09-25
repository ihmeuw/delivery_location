###########################################################################################################
### Author: Will Gardner (wgard@uw.edu)
###         Adapted from Simon Yadgir (syadgir)
### Date: 06/12/2019
### Project: ST-GPR
### Purpose: Load helper functions used in ST-GPR custom linear ensemble code
###########################################################################################################

#########################
### OFFSET DATA 0s/1s ###
#########################

offset.data <- function(dt, data_transform, offset) {
  print(head(dt))
  # print(dt[, data])
  ## Offset 0's if logit or log
  # if ((nrow(df[data == 0 | data == 1]) > 0) & (data_transform %in% c("logit", "log"))) {
  if ((nrow(dt[data == 0,]) > 0) & (data_transform %in% c("logit", "log"))) {
    dt[data == 0, data := data + offset]
    ## Offset 1's if logit
    if (data_transform == "logit") {
      dt[data == 1, data := data - offset]
    }
  } else{
    print("No data need offsetting!")
  }
  return(dt)
}

#################################################
############ TRANSFORM_DATA FUNCTION ############
#################################################

transform_data <- function(var, space, reverse=F) {
  if (space == "logit" & reverse==F) {
    var <- logit(var)
  } else if (space == "logit" & reverse==T) {
    var <- inv.logit(var)
  } else if (space == "log" & reverse==F) {
    var <- log(var)
  } else if (space == "log" & reverse==T) {
    var <- exp(var)
  }
  
  return(var)
  
}

#############################################################
### FUNCTION FOR TAGGING MODELS THAT VIOLATE SIGNIFICANCE ###
#############################################################

restrict_violations<-function(rmses, covs, prior_sign=NULL, p_value=.05){
  rmses[, sign_violation:=0]
  if(!is.null(prior_sign)){
    message("Removing models that violate prior signs")
    signs<-data.table(sig=prior_sign, cov=covs)
    for(i in 1:nrow(signs)){
      cov<-signs[i, cov]
      sign<-signs[i, sig]
      #get models where cov doesn't violate sign
      if(sign==-1){
        message(cov, " must be negative")
        message("  Dropping ", nrow(rmses[get(paste0(cov, "_fixd"))>0,])," model(s) where ", cov, " is greater than 0")
        #rmses<-rmses[get(paste0(cov, "_fixd"))<=0  |  is.na(get(paste0(cov, "_fixd"))),]
        rmses[get(paste0(cov, "_fixd"))>0, sign_violation:=1]
      }
      if(sign==1){
        message(cov, " must be positive")
        message("  Dropping ", nrow(rmses[get(paste0(cov, "_fixd"))<0,])," model(s) where ", cov, " is less than 0")
        #rmses<-rmses[get(paste0(cov, "_fixd"))>=0  |  is.na(get(paste0(cov, "_fixd"))),]
        rmses[get(paste0(cov, "_fixd"))<0, sign_violation:=1]
      }
    }
  }
  
  ## REMOVE BASED ON SIGN VIOLATION
  z<-qnorm(p_value/2, mean=0, sd=1, lower.tail=F)
  rmses[, sig_violation:=0]
  invisible(
    lapply(covs, function(x){
      lowers<-rmses[, get(paste0(x, "_fixd"))-z*get(paste0(x, "_fixd_se"))]
      uppers<-rmses[, get(paste0(x, "_fixd"))+z*get(paste0(x, "_fixd_se"))]
      temp<-data.table(lower=lowers, upper=uppers)
      temp[, insig:=ifelse(data.table::between(0, lower, upper), 1, 0)]
      temp[is.na(lower) & is.na(upper), insig := 0]
      message(nrow(temp[insig==1, ]), " models have p>", p_value, " for ", x)
      #rmses[, sig_violation:=ifelse(temp$insig==1, 1, sig_violation)]
      rmses[temp$insig==1, sig_violation:=1]
      rmses[temp$insig==0, sig_violation:=0]
    })
  )
  rmses[sig_violation==1 | sign_violation==1, drop:=1]
  rmses[is.na(drop), drop:=0]
  return(rmses)
}

#################################
### CLUSTER JOB HOLD FUNCTION ###
#################################

job_hold <- function(job_name, file_list=NULL, obj=NULL, resub=0) {
  
  ## Give it a sec to launch
  Sys.sleep(5)
  
  ## Start timer
  start.time <- proc.time()
  
  ## Wait for job to finish
  flag <-  0
  while (flag == 0) {
    ## Check if job is done
    if (system(paste0("squeue -r | grep ", job_name, "|wc -l"), intern=T) == 0) {
      ## If so, set flag to 1
      flag <- 1
    } else {
      Sys.sleep(5)
    }
  }
  
  ## End Timer
  job.runtime <- proc.time() - start.time
  job.runtime <- job.runtime[3]
  
  ## Give it another sec
  Sys.sleep(10)
  
  
  ## Check for the file list
  if (!is.null(file_list)) {	
    missing_list <- NULL
    for (file in file_list) {
      ## Ensure that all files are there
      if (!file.exists(file)) {	
        missing_list <- rbind(missing_list, file)
        ## Check obj if hdf
      } else {
        if (grepl(".h5", file_list[1])) {
          if (!(obj %in% h5ls(file_list)$name)) {
            missing_list <- rbind(missing_list, file)
          }
        } 
      }
    }
    
    ## If missing_list > 0, break
    if (length(missing_list) > 0) {
      if (resub == 0) {
        stop(paste0("Job failed: ", job_name, 
                    "\nTime elapsed: ", job.runtime,
                    "\nYou are missing the following files: ", toString(missing_list)))
      } else {
        return(1)
      }
    } else {
      return(0)
    }
  }
  
  ## Complete
  print(paste0("Job ", job_name, " has completed. Time elapsed: ", job.runtime))
}

############################
### APPEND PDFS FUNCTION ###
############################

append_pdfs<-function(folder, pattern, output, rm=F){
  
  
  files<-list.files(folder, full.names=T)   ##sy:getting all of the files
  inputs <- grep(pattern, files, value=T)  ##sy: grepping for the pattern
  
  input<-gsub(",", "", toString(inputs))  ##sy: the ghostscript needs all of the files to look like this
  
  
  ##sy: the command itself is from Patty's append_pdf function, which is embedded in STGPR code
  cmd <- paste0("/usr/bin/ghostscript -dBATCH -dSAFER -dNOGC -DNOPAUSE -dNumRenderingThreads=4 -q -sDEVICE=pdfwrite -sOutputFile=", output, " ", input)
  system(cmd)	
  if (rm){
    invisible(lapply(inputs, unlink))
    #unlink(dirname(inputs[1]), recursive=T)
  } 
}

#################################
### GET RECENT FILES FUNCTION ###
#################################

get_recent<-function(folder, pattern=NULL, sheet=NULL, path=F){
  require(data.table)
  
  files<-list.files(folder, full.names=T, pattern=pattern)
  files<-files[!grepl("\\~\\$", files)]
  infoo<-file.info(files)
  most_recent_path<-row.names(infoo[infoo$mtime==max(infoo$mtime),])
  if(path==T){
    message(paste("Most recent file: ", most_recent_path))
    return(most_recent_path)
  }else{
    
    ##sy: get file type
    if(grepl(".csv", most_recent_path)){
      recent<-fread(most_recent_path)
    }
    if(grepl(".rds", most_recent_path)){
      recent<-readRDS(most_recent_path)
    }
    if(grepl(".xlsx", most_recent_path)){
      require(openxlsx)
      if(length(sheet)==0){
        message(" Reading an xlsx file, but no sheet name given, reading first sheet")
        sheet<-1
      }
      recent<-read.xlsx(most_recent_path, sheet=sheet)
      recent<-as.data.table(recent)
    }
    message(paste("Most recent file: ", most_recent_path))
    return(recent)
  }
}