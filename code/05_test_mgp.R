rm(list=ls())
library(doParallel) # for parallel computing
library(foreach) # for parallel computing
library(Matrix)
library(nloptr)
library(minqa)
library(optimx)
library(rootSolve)
library(interp)
library(zoo)
library(dplyr)
library(lubridate)

path = "/.../"

path.data = paste0(path,"data/")
path.out = paste0(path, "res/")
path.out.1 = paste0(path.out, 'res05/')
dir.create(path.out, showWarnings = F)
dir.create(path.out.1, showWarnings = F)
source(paste0(path,"Function_MGP.R"))

dt_raw = readRDS(file=paste0(path, 'dt_merge_Southeast_new.rds'))

dt <- dt_raw %>% group_by(x, y) %>% filter(!any(is.na(value))) %>% ungroup()
xy_coord <- dt %>% distinct(x, y)
ngrid <- nrow(xy_coord)

saveRDS(xy_coord, file=paste0(path, 'xy_Southeast_sel.rds'))

yr <- c(1948:2014)

for(imon in 1:length(month.name)){
  
  path.out.2 = paste0(path.out.1, 'M',imon,'/')
  dir.create(path.out.2, showWarnings = F)
  
  #######################################
  ###### Parallel computing #############
  #detectCores()
  ncore = 64
  cl = makeCluster(ncore)
  registerDoParallel(cl)
  
  foreach (ii = c(1:ngrid) ) %dopar% {
    
    library(Matrix);   library(nloptr);   library(minqa);   library(optimx)
    library(rootSolve);   library(interp);   library(zoo);  library(dplyr); 
    library(lubridate)
    
    dt_mon <- dt %>% mutate(month = month(ym(time_seq), label = TRUE, abbr = TRUE))
    dt_mon <- dt_mon %>% filter(month == month.abb[imon])
    
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
    
    
    ##################################
    # Step1. Emulator modeling with CESM2 (f^M)
    ##################################
    n=2 #Number of outputs (taserature, precipitation)
    dp=length(yr) #Number of Design Points
    
    x_fm <- x2_fm <- unname(unlist(ss_model$value))  #Surface from CESM2 (x for modeling tas, x2 for modeling precipitation)
    y_fm <- y2_fm <- unname(unlist(sl_model$value))  #Latent from CESM2  (y for modeling tas, y2 for modeling precipitation)
    z.tas_fm <- unname(unlist(tas_model$value)) # tas from CESM2
    z.pr_fm <- unname(unlist(pr_model$value)) # Precip from CESM2
    
    # Min-max normalization
    norm.x_fm <- norm.x2_fm <- minMax(x_fm)
    norm.y_fm <- norm.y2_fm <- minMax(y_fm)
    norm.z1_fm <- minMax(z.tas_fm)
    norm.z3_fm <- minMax(z.pr_fm)
    
    trains <- list() # trains: pairs (S, L) for training
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
    ub_fm = c(rep(10, 12), 0.5)
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
    ypred.train = as.matrix(t(pk.train)%*%solve(covM_fm, trainy_fm))
    ypred.train.tas = ypred.train[1:dp] * (max(z.tas_fm)-min(z.tas_fm)) + min(z.tas_fm)
    ypred.train.pr   = ypred.train[(dp+1):(dp*2)] * (max(z.pr_fm)-min(z.pr_fm)) + min(z.pr_fm)
    
    
    ##################################
    # Step2. Getting bias 
    ##################################
    # Apply observation set in training to get tas_bias and Pr_bias from f^M
    x_obs <- x2_obs <- ss_obs$value #S from OBS
    y_obs <- y2_obs <- sl_obs$value  #L from OBS 
    z.tas_obs <- tas_obs$value # tas from OBS
    z.pr_obs <- pr_obs$value # Precip from OBS
    
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
    
    # Get Bias set (taserature_bias and Precipitation_bias)
    tas_bias_train <- ypred.train.tas_obs - z.tas_obs
    pr_bias_train <- ypred.train.pr_obs - z.pr_obs
    
    
    ##################################
    # Step3. Training: modeling bias surface (f^E)
    ##################################
    n=2
    dp=length(yr) 
    
    x_fe <- x2_fe <- ss_obs$value #S from OBS
    y_fe <- y2_fe <- sl_obs$value #L from OBS 
    z.tas_fe <- tas_bias_train # tas_bias from f^M
    z.pr_fe <- pr_bias_train # Precip_bias from f^M
    
    norm.x_fe <- minMax(x_fe); norm.x2_fe <- minMax(x2_fe)
    norm.y_fe <- minMax(y_fe); norm.y2_fe <- minMax(y2_fe)
    norm.z1_fe <- minMax(z.tas_fe)
    norm.z3_fe <- minMax(z.pr_fe)
    
    trains <- list()
    trains[[1]] = cbind(norm.x_fe, norm.y_fe)
    trains[[2]] = cbind(norm.x2_fe, norm.y2_fe)
    
    trainy=c(as.vector(norm.z1_fe),as.vector(norm.z3_fe))
    leny=length(trainy)
    
    # For records
    trains_fe <- trains; trainy_fe <- trainy; leny_fe <- leny
    
    # MGP training
    pf=index(n,dp)
    pfi=pf$pfi;pfj=pf$pfj 
    sparseMatrix(i=pfi,j=pfj) 
    
    # Set initial value of model parameters 
    x0_fe = c(runif(12, -1, 1), 0.1)
    opts_fe <- list("algorithm" = "NLOPT_LD_MMA", "maxeval" = 1000, print_level=0)
    lb_fe = c(rep(-10, 12), 0.001)
    ub_fe = c(rep(10, 12), 0.5)
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
    pk.train_fe=etan(xs=trains_obs, x=trains_fe, L=H0_fe, measures=n)
    ypred.train_fe=as.matrix(t(pk.train_fe)%*%solve(covM_fe, trainy_fe))
    ypred.train.tas_fe = ypred.train_fe[1:dp] * (max(z.tas_fe)-min(z.tas_fe)) + min(z.tas_fe)
    ypred.train.pr_fe   = ypred.train_fe[(dp+1):(dp*2)] * (max(z.pr_fe)-min(z.pr_fe)) + min(z.pr_fe)
    
    
    ##################################
    # Step4. Test (apply x^new to f^M and f^E)
    ##################################
    n=2 
    dp.test=length(yr) 
    
    tx_obs <- tx2_obs <- ss_obs$value # THIS CAN BE CHANGED  
    ty_obs <- ty2_obs <- sl_obs$value # THIS CAN BE CHANGED  
    
    norm.tx_obs <- norm.tx2_obs <- minMax(tx_obs)
    norm.ty_obs <- norm.ty2_obs <- minMax(ty_obs)
    
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
                      surf.z1_fe_inv = surf.z1_fe_inv, #
                      surf.z3_fe_inv = surf.z3_fe_inv, #
                      z.tas_fm = z.tas_fm, #
                      z.pr_fm = z.pr_fm,
                      sf.z1_fm = sf.z1_fm,
                      norm.x_fm = norm.x_fm,
                      norm.y_fm = norm.y_fm,
                      ypred.train.tas = ypred.train.tas,
                      ypred.train.tas_obs = ypred.train.tas_obs,
                      norm.tx_obs = norm.tx_obs,
                      norm.ty_obs = norm.ty_obs,
                      sf.z1_fe = sf.z1_fe,
                      norm.x_obs = norm.x_obs, 
                      norm.y_obs = norm.y_obs,
                      ypred.train.tas_fe = ypred.train.tas_fe,
                      sf.z3_fm = sf.z3_fm,
                      ypred.train.pr = ypred.train.pr,
                      ypred.train.pr_obs = ypred.train.pr_obs,
                      sf.z3_fe = sf.z3_fe,
                      ypred.train.pr_fe = ypred.train.pr_fe,
                      z.tas_fe= z.tas_fe,
                      z.pr_fe= z.pr_fe
    )
    saveRDS(data.list, file=paste0(path.out.2, 'res05_ID',ii,'_M',imon,'_Southeast.rds'))
    rm(data.list)
  } # End of grid
 stopCluster(cl)
    
} # End of month

