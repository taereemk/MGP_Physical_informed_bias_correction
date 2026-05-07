rm(list=ls(all=TRUE)) 
library(doParallel) # for parallel computing
library(foreach) # for parallel computing
library(Matrix)
library(nloptr)
library(minqa)
library(optimx)
library(rootSolve)
library(interp)
library(zoo)
library(evd)
library(MASS)
library(dplyr)
library(lubridate)
library(tidyr)

path = "/.../"

path.data = paste0(path,"data/")
path.out = paste0(path, "res")
path.out.1 = paste0(path.out, 'res08_subregion/')
dir.create(path.out, showWarnings = F)
dir.create(path.out.1, showWarnings = F)

source(paste0(path,"code/Function_MGP.R"))
source(paste0(path,"code/Function_biasCorrection_Month.R"))

sub_region <- c("Northwest","Southwest","GreatPlainsNorth","GreatPlainsSouth","Midwest","Northeast","Southeast")

#for(isub in c(1:length(sub_region)){
  isub=7
  path.out.2 = paste0(path.out.1, sub_region[isub],'/')
  dir.create(path.out.2, showWarnings = F)
  
  dt_raw = readRDS(file=paste0(path.data, 'dt_merge_',sub_region[isub],'.rds'))
  dt <- dt_raw %>% group_by(x, y) %>% filter(!any(is.na(value))) %>% ungroup()
  xy_coord <- dt %>% distinct(x, y)
  ngrid <- nrow(xy_coord)
  
  saveRDS(xy_coord, file=paste0(path.data, 'xy_',sub_region[isub],'_sel.rds'))
  
  yr = c(1948:2014)
  
  for(imon in 1:length(month.name)){
    
    path.out.3 = paste0(path.out.2, 'M',imon,'/')
    dir.create(path.out.3, showWarnings = F)
    
    dt_mon <- dt %>% mutate(month = month(ym(time_seq), label = TRUE, abbr = TRUE))
    dt_mon <- dt_mon %>% filter(month == month.abb[imon])
    
    for(ii in 1:ngrid){
      
      if(ii %% 10 == 0) print(paste0(month.name[imon], '_ID', ii , '/',ngrid))
      
      path.out.4 = paste0(path.out.3, 'ID',ii,'/')
      dir.create(path.out.4, showWarnings = F)
      
      xy_ids <- dt_mon %>% select(x, y) %>% distinct() %>% arrange(x, y) %>% mutate(ID = row_number())
      dt_id <- dt_mon %>% left_join(xy_ids, by = c("x", "y"))
      dt_sel <- dt_id %>% filter(ID == ii)
      
      ss_obs <- dt_sel %>% filter(variable == 'sshf' & group == 'observation')
      ss_model <- dt_sel %>% filter(variable == 'sshf' & group == 'model')
      
      sl_obs <- dt_sel %>% filter(variable == 'slhf' & group == 'observation')
      sl_model <- dt_sel %>% filter(variable == 'slhf' & group == 'model')
      
      pr_obs <- dt_sel %>% filter(variable == 'pr' & group == 'observation')
      pr_model <- dt_sel %>% filter(variable == 'pr' & group == 'model')
      
      tas_obs <- dt_sel %>% filter(variable == 'tas' & group == 'observation')
      tas_model <- dt_sel %>% filter(variable == 'tas' & group == 'model')
      
      
      #######################################
      ###### Parallel computing #############
      #detectCores()
      ncore = 102
      cl = makeCluster(ncore)
      registerDoParallel(cl)
      
      foreach (ik = c(1:100) ) %dopar% {
        
        library(Matrix);   library(nloptr);   library(minqa);   library(optimx)
        library(rootSolve);   library(interp);   library(zoo);  library(dplyr); 
        library(lubridate)
        
        
        selected_years = sample(yr, 47)
        remaining_years = setdiff(yr, selected_years)
        
        yr.train = selected_years #47 years
        yr.test = remaining_years #20 years
        
        pos.train = which(yr %in% yr.train)
        pos.test  = which(yr %in% yr.test)
        
        print(paste0('kfold',ik,'_',month.name[imon]))
        
        ss.obs.train <- ss_obs$value[pos.train];   sl.obs.train  <- sl_obs$value[pos.train]
        tas.obs.train <- tas_obs$value[pos.train]; pr.obs.train  <- pr_obs$value[pos.train]
        
        ss.model.train <- ss_model$value[pos.train]; sl.model.train  <- sl_model$value[pos.train]
        tas.model.train <- tas_model$value[pos.train]; pr.model.train  <- pr_model$value[pos.train]
        
        ss.obs.test <- ss_obs$value[pos.test]; sl.obs.test  <- sl_obs$value[pos.test]
        tas.obs.test <- tas_obs$value[pos.test]; pr.obs.test  <- pr_obs$value[pos.test]
        
        
        ##################################
        # Step1. Training: modeling CESM2 surface (f^M)
        ##################################
        # Call dataset
        n=2 #Number of outputs (temperature, precipitation)
        dp=length(yr.train) #Number of Design Points
        
        x_fm <- x2_fm <- ss.model.train
        y_fm <- y2_fm <- sl.model.train
        
        z.tas_fm <- tas.model.train
        z.pr_fm <- pr.model.train
        
        # Min-max normalization
        norm.x_fm <- norm.x2_fm <- minMax(x_fm)
        norm.y_fm <- norm.y2_fm <- minMax(y_fm)
        norm.z1_fm <- minMax(z.tas_fm)
        norm.z3_fm <- minMax(z.pr_fm)
        
        trains <- list()
        trains[[1]] = cbind(norm.x_fm, norm.y_fm)
        trains[[2]] = cbind(norm.x2_fm, norm.y2_fm)
        
        trainy=c(as.vector(norm.z1_fm),as.vector(norm.z3_fm))
        leny=length(trainy)
        
        # For records
        trains_fm <- trains; trainy_fm <- trainy; leny_fm <- leny
        
        # MGP training for f^M
        pf=index(n,dp)
        pfi=pf$pfi;pfj=pf$pfj 
        sparseMatrix(i=pfi,j=pfj) 
        
        # Set initial value of model parameters 
        x0_fm = c(runif(12, -1, 1), 0.1)
        opts_fm <- list("algorithm" = "NLOPT_LD_MMA", "maxeval" = 1000, print_level=0)
        lb_fm = c(rep(-10, 12), 0.001)
        ub_fm = c(rep(10, 12), 1)
        one_fm = tryCatch(nloptr(x0=x0_fm, eval_f= logL, eval_grad_f = logL_grad, opts= opts_fm, fn = logL,
                                 lb = lb_fm, ub = ub_fm), error = function(e) cat("error in nloptr"))
        H0_fm = one_fm$solution
        covM_fm = C(trains_fm,H0_fm) # covariance matrix for f^M surface
        
        rm(trains, trainy, leny)
        
        # Generate f^M surface 
        seq.x_fm <- seq(0,1, length.out=40)
        seq.y_fm <- seq(0,1, length.out=40)
        seq.xy.x_fm <- unlist(unname(expand.grid(x=seq.x_fm, y=seq.y_fm)$x))
        seq.xy.y_fm <- unlist(unname(expand.grid(x=seq.x_fm, y=seq.y_fm)$y))
        
        xsurf_fm <-list()
        xsurf_fm[[1]] <- cbind(c(seq.xy.x_fm), c(seq.xy.y_fm))
        xsurf_fm[[2]] <- cbind(c(seq.xy.x_fm), c(seq.xy.y_fm))
        
        pk_fm = etan(xs=xsurf_fm,x=trains_fm,L=H0_fm,measures=n)
        
        ypred_fm = as.matrix(t(pk_fm)%*%solve(covM_fm, trainy_fm))
        ypred.tas_fm = ypred_fm[1:(length(ypred_fm)/2)] * (max(z.tas_fm)-min(z.tas_fm)) + min(z.tas_fm)
        ypred.pr_fm   = ypred_fm[((length(ypred_fm)/2)+1):(length(ypred_fm))] * (max(z.pr_fm)-min(z.pr_fm)) + min(z.pr_fm)
        
        surf.z1_fm <- list(x=xsurf_fm[[1]][,1], y=xsurf_fm[[1]][,2], # x(GPP), y(ET) are normalized value 
                           z=ypred.tas_fm)
        surf.z3_fm <- list(x=xsurf_fm[[1]][,1], y=xsurf_fm[[1]][,2],
                           z=ypred.pr_fm)
        
        nx=40; ny=40
        sf.z1_fm <- with(surf.z1_fm, interp::interp(x, y, z, nx=nx, ny=ny, duplicate="mean", extrap = FALSE))
        sf.z3_fm <- with(surf.z3_fm, interp::interp(x, y, z, nx=nx, ny=ny, duplicate="mean", extrap = FALSE))
        
        sf.z1_fm$z <- rowMeans(simplify2array(list(na.approx(sf.z1_fm$z, rule = 2), t(na.approx(t(sf.z1_fm$z), rule = 2)))), TRUE, 2)
        sf.z3_fm$z <- rowMeans(simplify2array(list(na.approx(sf.z3_fm$z, rule = 2), t(na.approx(t(sf.z3_fm$z), rule = 2)))), TRUE, 2)
        
        
        # Get f^M(x^M)
        pk.train=etan(xs=trains_fm, x=trains_fm, L=H0_fm, measures=n)
        ypred.train = as.matrix(t(pk.train)%*%solve(covM_fm,trainy_fm))
        ypred.train.tas = ypred.train[1:dp] * (max(z.tas_fm)-min(z.tas_fm)) + min(z.tas_fm)
        ypred.train.pr   = ypred.train[(dp+1):(dp*2)] * (max(z.pr_fm)-min(z.pr_fm)) + min(z.pr_fm)
        
        
        ##################################
        # Step2. Getting bias 
        ##################################
        x_obs <- x2_obs <- ss.obs.train
        y_obs <- y2_obs <- sl.obs.train
        z.tas_obs <- tas.obs.train
        z.pr_obs <- pr.obs.train
        
        norm.x_obs <- norm.x2_obs <- minMax(x_obs)
        norm.y_obs <- norm.y2_obs <- minMax(y_obs)
        norm.z1_obs <- minMax(z.tas_obs)
        norm.z3_obs <- minMax(z.pr_obs)
        
        trains_obs <- list() # trains: observational pairs (GPP, ET) in training period
        trains_obs[[1]] = cbind(norm.x_obs, norm.y_obs)
        trains_obs[[2]] = cbind(norm.x2_obs, norm.y2_obs)
        
        trainy_obs=c(as.vector(norm.z1_obs),as.vector(norm.z3_obs))
        
        # Get f^M(x^o)
        pk.train_obs = etan(xs=trains_obs, x=trains_fm, L=H0_fm, measures=n)
        ypred.train_obs = as.matrix(t(pk.train_obs)%*%solve(covM_fm, trainy_fm))
        ypred.train.tas_obs = ypred.train_obs[1:dp] * (max(z.tas_fm)-min(z.tas_fm)) + min(z.tas_fm)
        ypred.train.pr_obs   = ypred.train_obs[(dp+1):(dp*2)] * (max(z.pr_fm)-min(z.pr_fm)) + min(z.pr_fm)
        
        # Get Bias set (temperature_bias and Precipitation_bias)
        tas_bias_train <- ypred.train.tas_obs - z.tas_obs
        pr_bias_train <- ypred.train.pr_obs - z.pr_obs
        
        
        ##################################
        # Step3. Training: modeling bias surface (f^E)
        ##################################
        n=2 #Number of outputs (temperature, precipitation)
        dp=length(yr.train) #Number of Design Points
        
        x_fe <- x2_fe <- ss.obs.train
        y_fe <- y2_fe <- sl.obs.train
        z.tas_fe <- tas_bias_train # tas_bias from f^M
        z.pr_fe <- pr_bias_train # Precip_bias from f^M
        
        norm.x_fe <- minMax(x_fe); norm.x2_fe <- minMax(x2_fe)
        norm.y_fe <- minMax(y_fe); norm.y2_fe <- minMax(y2_fe)
        norm.z1_fe <- minMax(z.tas_fe)
        norm.z3_fe <- minMax(z.pr_fe)
        
        trains <- list() # trains: observational pairs (GPP, ET) in training period
        trains[[1]] = cbind(norm.x_fe, norm.y_fe)
        trains[[2]] = cbind(norm.x2_fe, norm.y2_fe)
        
        trainy=c(as.vector(norm.z1_fe),as.vector(norm.z3_fe))
        leny=length(trainy)
        
        # For records
        trains_fe <- trains; trainy_fe <- trainy; leny_fe <- leny
        
        # Set initial value of model parameters 
        x0_fe = c(runif(12, -1, 1), 0.1)
        opts_fe <- list("algorithm" = "NLOPT_LD_MMA", "maxeval" = 1000, print_level=0)
        lb_fe = c(rep(-10, 12), 0.001)
        ub_fe = c(rep(10, 12), 1)
        one_fe = tryCatch(nloptr(x0=x0_fe, eval_f= logL, eval_grad_f = logL_grad, opts= opts_fe, fn = logL,
                                 lb = lb_fe, ub = ub_fe), error = function(e) cat("error in nloptr"))
        H0_fe = one_fe$solution
        covM_fe = C(trains_fe, H0_fe)
        
        rm(trains, trainy, leny)
        
        # Generate f^E surface 
        seq.x_fe <- seq(0,1, length.out=40)
        seq.y_fe <- seq(0,1, length.out=40)
        seq.xy.x_fe <- unlist(unname(expand.grid(x=seq.x_fe, y=seq.y_fe)$x))
        seq.xy.y_fe <- unlist(unname(expand.grid(x=seq.x_fe, y=seq.y_fe)$y))
        
        xsurf_fe <-list()
        xsurf_fe[[1]] <- cbind(c(seq.xy.x_fe), c(seq.xy.y_fe))
        xsurf_fe[[2]] <- cbind(c(seq.xy.x_fe), c(seq.xy.y_fe))
        
        pk_fe=etan(xs=xsurf_fe,x=trains_fe,L=H0_fe,measures=n)
        
        ypred_fe = as.matrix(t(pk_fe)%*%solve(covM_fe, trainy_fe))
        ypred.tas_fe = ypred_fe[1:(length(ypred_fe)/2)] * (max(z.tas_fe)-min(z.tas_fe)) + min(z.tas_fe)
        ypred.pr_fe   = ypred_fe[((length(ypred_fe)/2)+1):(length(ypred_fe))] * (max(z.pr_fe)-min(z.pr_fe)) + min(z.pr_fe)
        
        surf.z1_fe_inv <- list(x=xsurf_fe[[1]][,1] * (max(x_fe)-min(x_fe)) + min(x_fe), 
                               y=xsurf_fe[[1]][,2] * (max(y_fe)-min(y_fe)) + min(y_fe),
                               z=ypred.tas_fe)
        surf.z3_fe_inv <- list(x=xsurf_fe[[1]][,1] * (max(x_fe)-min(x_fe)) + min(x_fe), 
                               y=xsurf_fe[[1]][,2] * (max(y_fe)-min(y_fe)) + min(y_fe),
                               z=ypred.pr_fe)
        
        surf.z1_fe <- list(x=xsurf_fe[[1]][,1], y=xsurf_fe[[1]][,2],
                           z=ypred.tas_fe)
        surf.z3_fe <- list(x=xsurf_fe[[1]][,1], y=xsurf_fe[[1]][,2],
                           z=ypred.pr_fe)
        
        nx=40; ny=40
        sf.z1_fe <- with(surf.z1_fe, interp::interp(x, y, z, nx=nx, ny=ny, duplicate="mean", extrap = FALSE))
        sf.z3_fe <- with(surf.z3_fe, interp::interp(x, y, z, nx=nx, ny=ny, duplicate="mean", extrap = FALSE))
        
        sf.z1_fe$z <- rowMeans(simplify2array(list(na.approx(sf.z1_fe$z, rule = 2), t(na.approx(t(sf.z1_fe$z), rule = 2)))), TRUE, 2)
        sf.z3_fe$z <- rowMeans(simplify2array(list(na.approx(sf.z3_fe$z, rule = 2), t(na.approx(t(sf.z3_fe$z), rule = 2)))), TRUE, 2)
        
        
        # Get e(x^o)
        pk.train_fe=etan(xs=trains_fe, x=trains_fe, L=H0_fe, measures=n)
        ypred.train_fe=as.matrix(t(pk.train_fe)%*%solve(covM_fe, trainy_fe))
        ypred.train.tas_fe = ypred.train_fe[1:dp] * (max(z.tas_fe)-min(z.tas_fe)) + min(z.tas_fe)
        ypred.train.pr_fe   = ypred.train_fe[(dp+1):(dp*2)] * (max(z.pr_fe)-min(z.pr_fe)) + min(z.pr_fe)
        
        
        ##################################
        # Step4. Test (apply x^new to f^M and f^E)
        ##################################
        n=2 #Number of outputs (temperature, precipitation)
        dp.test=length(yr.test) #Number of Design Points
        
        tx_obs <- tx2_obs <- ss.obs.test
        ty_obs <- ty2_obs <- sl.obs.test
        
        norm.tx_obs <- norm.tx2_obs <- (tx_obs - min(x_obs)) / (max(x_obs) - min(x_obs))
        norm.ty_obs <- norm.ty2_obs <- (ty_obs - min(y_obs)) / (max(y_obs) - min(y_obs))
        
        test_obs <- list() 
        test_obs[[1]] = cbind(norm.tx_obs, norm.ty_obs)
        test_obs[[2]] = cbind(norm.tx2_obs, norm.ty2_obs)
        
        # Get f^M(x^new)
        pk.test_fm = etan(xs=test_obs, x=trains_fm, L=H0_fm, measures=n)
        ypred.test.fm = as.matrix(t(pk.test_fm)%*%solve(covM_fm, trainy_fm))
        ypred.test.fm.tas = ypred.test.fm[1:dp.test] * (max(z.tas_fm)-min(z.tas_fm)) + min(z.tas_fm)
        ypred.test.fm.pr   = ypred.test.fm[(dp.test+1):(dp.test*2)] * (max(z.pr_fm)-min(z.pr_fm)) + min(z.pr_fm)
        
        # Get f^E(x^new)
        pk.test_fe = etan(xs=test_obs, x=trains_fe, L=H0_fe, measures=n)
        ypred.test.fe = as.matrix(t(pk.test_fe)%*%solve(covM_fe, trainy_fe))
        ypred.test.fe.tas = ypred.test.fe[1:dp.test] * (max(z.tas_fe)-min(z.tas_fe)) + min(z.tas_fe)
        ypred.test.fe.pr   = ypred.test.fe[(dp.test+1):(dp.test*2)] * (max(z.pr_fe)-min(z.pr_fe)) + min(z.pr_fe)
        
        ypred.fin_tas <- ypred.test.fm.tas - ypred.test.fe.tas
        ypred.fin_pr <- ypred.test.fm.pr - ypred.test.fe.pr
        
        
        ##################################
        # Step5. Test but for training period (apply x^new of training period to f^M and f^E)
        ##################################
        n=2 #Number of outputs (temperature, precipitation)
        dp.train=length(yr.train) #Number of Design Points
        
        trx_obs <- trx2_obs <- ss.obs.train
        try_obs<- try2_obs <- sl.obs.train
        
        norm.trx_obs <- norm.trx2_obs <- (trx_obs - min(x_obs)) / (max(x_obs) - min(x_obs))
        norm.try_obs <- norm.try2_obs <- (try_obs - min(y_obs)) / (max(y_obs) - min(y_obs))
        
        train_obs <- list() 
        train_obs[[1]] = cbind(norm.trx_obs, norm.try_obs)
        train_obs[[2]] = cbind(norm.trx2_obs, norm.try2_obs)
        
        # Get f^M(x^new) but in training period
        pk.train_fm = etan(xs=train_obs, x=trains_fm, L=H0_fm, measures=n)
        ypred.train.fm = as.matrix(t(pk.train_fm)%*%solve(covM_fm, trainy_fm))
        ypred.train.fm.tas = ypred.train.fm[1:dp.train] * (max(z.tas_fm)-min(z.tas_fm)) + min(z.tas_fm)
        ypred.train.fm.pr   = ypred.train.fm[(dp.train+1):(dp.train*2)] * (max(z.pr_fm)-min(z.pr_fm)) + min(z.pr_fm)
        
        # Get f^E(x^new)
        pk.train_fe = etan(xs=train_obs, x=trains_fe, L=H0_fe, measures=n)
        ypred.train.fe = as.matrix(t(pk.train_fe)%*%solve(covM_fe, trainy_fe))
        ypred.train.fe.tas = ypred.train.fe[1:dp.train] * (max(z.tas_fe)-min(z.tas_fe)) + min(z.tas_fe)
        ypred.train.fe.pr   = ypred.train.fe[(dp.train+1):(dp.train*2)] * (max(z.pr_fe)-min(z.pr_fe)) + min(z.pr_fe)
        
        ypred.fin_tas_train <- ypred.train.fm.tas - ypred.train.fe.tas
        ypred.fin_pr_train <- ypred.train.fm.pr - ypred.train.fe.pr
        
        
        ##################################
        # Step 6. Bias correction with Eqm
        ##################################     
        tas.model.test <- tas_model$value[pos.test]
        pr.model.test <- pr_model$value[pos.test]
        
        o.pr    <- z.pr_obs
        p.pr    <- z.pr_fm
        s.pr_test    <- pr.model.test
        s.pr_train   <- pr.model.train
        s.bs.pr <- eqm(o.pr, p.pr, s.pr_test, precip=TRUE, pr.threshold = pr.threshold, n.quantiles=NULL, extrapolation = "constant")
        s.bs.pr_train <- eqm(o.pr, p.pr, s.pr_train, precip=TRUE, pr.threshold = pr.threshold, n.quantiles=NULL, extrapolation = "constant")
        
        o.tas    <- z.tas_obs
        p.tas    <- z.tas_fm
        s.tas_test    <- tas.model.test
        s.tas_train    <- tas.model.train
        s.bs.tas <- eqm(o.tas, p.tas, s.tas_test, precip=FALSE, n.quantiles=NULL, extrapolation = "constant")
        s.bs.tas_train <- eqm(o.tas, p.tas, s.tas_train, precip=FALSE, n.quantiles=NULL, extrapolation = "constant")
        
        ##################################
        # save dataset
        ##################################
        data.list <- list(fm_xnew_tas = ypred.test.fm.tas, #
                          fm_xnew_pr = ypred.test.fm.pr,  #
                          fe_xnew_tas = ypred.test.fe.tas, #
                          fe_xnew_pr = ypred.test.fe.pr, #
                          z.tas_obs = z.tas_obs, #
                          z.pr_obs = z.pr_obs, #
                          ypred.fin_tas = ypred.fin_tas, #
                          ypred.fin_pr = ypred.fin_pr, #
                          yobs.fin_tas = tas.obs.test, ##
                          yobs.fin_pr = pr.obs.test,
                          surf.z1_fe_inv = surf.z1_fe_inv, #
                          surf.z3_fe_inv = surf.z3_fe_inv, #
                          tas.model.train = tas.model.train, #
                          pr.model.train = pr.model.train, #
                          ypred.fin_tas_train = ypred.fin_tas_train, #
                          ypred.fin_pr_train = ypred.fin_pr_train, #
                          ypred.train.fm.tas = ypred.train.fm.tas, #
                          ypred.train.fm.pr = ypred.train.fm.pr, #
                          tas.model.test = tas.model.test, #
                          pr.model.test = pr.model.test, #
                          yr.train = yr.train,
                          yr.test = yr.test,
                          z.tas_fm = z.tas_fm, #
                          z.pr_fm = z.pr_fm,
                          sf.z1_fm = sf.z1_fm,
                          sf.z1_fe = sf.z1_fe,
                          sf.z3_fm = sf.z3_fm,
                          sf.z3_fe = sf.z3_fe,
                          s.bs.pr = s.bs.pr,
                          s.bs.tas = s.bs.tas,
                          s.bs.pr_train = s.bs.pr_train,
                          s.bs.tas_train = s.bs.tas_train
        )
        saveRDS(data.list, paste0(path.out.4, 'res08_M',imon,'_ID',ii,'_kfold',ik,'.rds'))
        rm(data.list)
      }# end of k-fold
      stopCluster(cl)
    } #end of grid
  } # end of month
#} # end of subregion
