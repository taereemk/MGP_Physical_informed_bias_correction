rm(list=ls())
library(doParallel) # for parallel computing
library(foreach) # for parallel computing
library(reshape2)
library(dplyr)
library(tibble)

get_skill_components <- function(obs, sim) {
  rho2 <- (cor(obs, sim, method = "pearson"))^2
  cb <- (cor(obs, sim, method = "pearson") - (sd(sim) / sd(obs)))^2
  ub <- ((mean(sim) - mean(obs)) / sd(obs))^2
  ss <- rho2 - cb - ub
  rmse <- sqrt(mean((sim - obs)^2))
  return(c(rho2 = rho2, cb = cb, ub = ub, ss = ss, rmse = rmse))
}

format_result <- function(res, variable, group, iter) {
  as.data.frame(res) %>%
    tibble::rownames_to_column("method") %>%
    mutate(variable = variable,
           group = group,
           iter = iter)
}

path = "/.../"
path.inp <- paste0(path, "res/res08_subregion/")
path.out <- paste0(path, "res/res09/")
dir.create(path.out, showWarnings = F)

path.data = paste0(path,"data/")
yr <- c(1948:2014)

sub_region <- c("Northwest","Southwest","GreatPlainsNorth","GreatPlainsSouth","Midwest","Northeast","Southeast")

#for(isub in c(1:length(sub_region)){
for(isub in c(7)){
  
  path.out.1 <- paste0(path.out, sub_region[isub],"/")
  dir.create(path.out.1, showWarnings = F)
  
  xy_coord <- readRDS(file=paste0(path.data, 'xy_',sub_region[isub],'_sel.rds'))
  ngrid <- nrow(xy_coord)
  
  for(imon in 1:length(month.name)){
    
    for(ii in 1:ngrid){
      
      if(ii %% 10 == 0) print(paste0(month.name[imon], "_ID",ii))
      
      #######################################
      ###### Parallel computing #############
      #detectCores()
      ncore = 102
      cl = makeCluster(ncore)
      registerDoParallel(cl)
      
      df_all <- foreach (ik = c(1:100), .combine = rbind, .packages = c("dplyr", "tibble")) %dopar% {
        
        res <- readRDS(paste0(path.inp, sub_region[isub],'/M',imon,'/ID',ii,'/res08_M',imon,'_ID',ii,'_kfold',ik,'.rds'))
        
        tas_train <- data.frame(year = res$yr.train,
                                obs = res$z.tas_obs,
                                model = res$tas.model.train,
                                mgp_emul = res$ypred.train.fm.tas,
                                mgp_bc = res$ypred.fin_tas_train,
                                eqm = res$s.bs.tas_train)
        
        tas_test <- data.frame(year=res$yr.test, 
                               obs = res$yobs.fin_tas,
                               model = res$tas.model.test,
                               mgp_emul = res$fm_xnew_tas,
                               mgp_bc = res$ypred.fin_tas,
                               eqm = res$s.bs.tas)
        
        res_tas_train <- rbind(
          model    = get_skill_components(tas_train$obs, tas_train$model),
          mgp_emul = get_skill_components(tas_train$obs, tas_train$mgp_emul),
          mgp_bc   = get_skill_components(tas_train$obs, tas_train$mgp_bc),
          eqm      = get_skill_components(tas_train$obs, tas_train$eqm)
        )
        
        res_tas_test <- rbind(
          model    = get_skill_components(tas_test$obs, tas_test$model),
          mgp_emul = get_skill_components(tas_test$obs, tas_test$mgp_emul),
          mgp_bc   = get_skill_components(tas_test$obs, tas_test$mgp_bc),
          eqm      = get_skill_components(tas_test$obs, tas_test$eqm)
        )
        
        pr_train <- data.frame(year = res$yr.train,
                               obs = res$z.pr_obs,
                               model = res$pr.model.train,
                               mgp_emul = res$ypred.train.fm.pr,
                               mgp_bc = res$ypred.fin_pr_train,
                               eqm = res$s.bs.pr_train)
        
        pr_test <- data.frame(year=res$yr.test, 
                              obs = res$yobs.fin_pr,
                              model = res$pr.model.test,
                              mgp_emul = res$fm_xnew_pr,
                              mgp_bc = res$ypred.fin_pr,
                              eqm = res$s.bs.pr)
        
        res_pr_train <- rbind(
          model    = get_skill_components(pr_train$obs, pr_train$model),
          mgp_emul = get_skill_components(pr_train$obs, pr_train$mgp_emul),
          mgp_bc   = get_skill_components(pr_train$obs, pr_train$mgp_bc),
          eqm      = get_skill_components(pr_train$obs, pr_train$eqm)
        )
        
        res_pr_test <- rbind(
          model    = get_skill_components(pr_test$obs, pr_test$model),
          mgp_emul = get_skill_components(pr_test$obs, pr_test$mgp_emul),
          mgp_bc   = get_skill_components(pr_test$obs, pr_test$mgp_bc),
          eqm      = get_skill_components(pr_test$obs, pr_test$eqm)
        )
        
        df_iter <- bind_rows(
          format_result(res_tas_train, variable = "tas", group = "train", iter = ik),
          format_result(res_tas_test,  variable = "tas", group = "test",  iter = ik),
          format_result(res_pr_train,  variable = "pr",  group = "train", iter = ik),
          format_result(res_pr_test,   variable = "pr",  group = "test",  iter = ik)
        )
        
        return(df_iter)
      }# end of iteration
      stopCluster(cl)
      saveRDS(df_all, paste0(path.out.1, "res09_skill_M", imon, "_ID", ii, "_",sub_region[isub],".rds"))
      
    } # end of ngrid
  } # end of month
} # end of sub region
