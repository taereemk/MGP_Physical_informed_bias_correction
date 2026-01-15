rm(list=ls())
library(reshape2)
library(dplyr)
library(tibble)

path = "/.../"
path.inp <- paste0(path, "res/res093/")
path.out <- paste0(path, "res/res10/")
dir.create(path.out, showWarnings = F)

path.data = paste0(path,"data/")
yr <- c(1948:2014)

sub_region <- c("Northwest","Southwest","GreatPlainsNorth","GreatPlainsSouth","Midwest","Northeast","Southeast")

for(isub in 1:length(sub_region)){
  
  xy_coord <- readRDS(file=paste0(path.data, 'xy_',sub_region[isub],'_sel.rds'))
  ngrid <- nrow(xy_coord)
  
  
  df_median_all <- NULL
  
  for(imon in 1:length(month.name)){  
    
    df_median_ngrid <- NULL
    for(ii in 1:ngrid){
      df_all <- readRDS(paste0(path.inp, sub_region[isub],"/res09_skill_M", imon, "_ID", ii, "_",sub_region[isub],".rds"))
      
      df_median <- df_all %>% group_by(variable, group, method) %>%
        summarise(
          median_ss = median(ss, na.rm = TRUE),
          median_rho2 = median(rho2, na.rm = TRUE),
          median_cb = median(cb, na.rm = TRUE),
          median_ub = median(ub, na.rm = TRUE),
          median_rmse = median(rmse, na.rm = TRUE),
          .groups = "drop"
        )
      df_median$id <- ii
      df_median_ngrid <- rbind(df_median_ngrid, df_median)
    }
    
    df_median_ngrid$month <- imon
    df_median_all <- rbind(df_median_all, df_median_ngrid)
  }
  
  saveRDS(df_median_all, file=paste0(path.out, 'res10_skill_median_',sub_region[isub],'.rds'))
  
}

