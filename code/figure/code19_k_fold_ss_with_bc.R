rm(list=ls())
library(ggplot2)
library(reshape2)
library(ggh4x)
library(dplyr)
library(cowplot)


path.data = "C:/Users/tk6869/Princeton Dropbox/Taereem Kim/Projects/Pj04_MGP/data/"

path.inp <- "C:/Users/tk6869/Princeton Dropbox/Taereem Kim/Projects/Pj04_MGP/res18_ts_with_bc/"
path.out <- "C:/Users/tk6869/Princeton Dropbox/Taereem Kim/Projects/Pj04_MGP/res19_plot_with_bc/"
dir.create(path.out, showWarnings = F)

grids=c(62,63,64,79,80,81,94,95,96)
yrs = c(1981:2013)

list.pr.obs <- readRDS(paste0(path.data, 'data_pr_obs.rds'))
list.tas.obs <- readRDS(paste0(path.data, 'data_tas_obs.rds'))
list.pr.e3sm <- readRDS(paste0(path.data, 'data_pr_e3sm.rds'))
list.tas.e3sm <- readRDS(paste0(path.data, 'data_tas_e3sm.rds'))

#for(igr in 1:length(grids)){
igr=5

#Monthly time series of MGP outputs
ts <- readRDS(file=paste0("C:/Users/tk6869/Princeton Dropbox/Taereem Kim/Projects/Pj04_MGP/res09_med_timeseries/res09_mgp_ts_median_grid",grids[igr],".rds"))

# arrange observations
df_list.pr.obs <- NULL
df_list.pr.e3sm <- NULL
df_list.tas.obs <- NULL
df_list.tas.e3sm <- NULL

for(im in 1:length(month.name)){
  list.pr.obs.1 <- data.frame(year=yrs, value=as.numeric(do.call(rbind, list.pr.obs[[im]])[,grids[igr]]),
                              variable='Precipitation', legend='PRISM',month=im)
  list.pr.e3sm.1 <- data.frame(year=yrs, value=as.numeric(do.call(rbind, list.pr.e3sm[[im]])[,grids[igr]]),
                               variable='Precipitation', legend='E3SM',month=im)  
  list.tas.obs.1 <- data.frame(year=yrs, value=as.numeric(do.call(rbind, list.tas.obs[[im]])[,grids[igr]]),
                               variable='Temperature', legend='PRISM',month=im)  
  list.tas.e3sm.1 <- data.frame(year=yrs, value=as.numeric(do.call(rbind, list.tas.e3sm[[im]])[,grids[igr]]),
                                variable='Temperature', legend='E3SM',month=im)    
  
  df_list.pr.obs <- rbind(df_list.pr.obs, list.pr.obs.1)
  df_list.pr.e3sm <- rbind(df_list.pr.e3sm, list.pr.e3sm.1)  
  df_list.tas.obs <- rbind(df_list.tas.obs, list.tas.obs.1)  
  df_list.tas.e3sm <- rbind(df_list.tas.e3sm, list.tas.e3sm.1)  
}

df_obs <- rbind(df_list.pr.obs, df_list.pr.e3sm, df_list.tas.obs, df_list.tas.e3sm)


med_pr <- NULL
for(im in 1:length(month.name)){
  
  raw_mgp_emul <- readRDS(file=paste0(path.inp, 'pr_mgp_emul_mon',im,'_grid',grids[igr],'.rds'))
  raw_mgp_bc <- readRDS(file=paste0(path.inp, 'pr_mgp_bc_mon',im,'_grid',grids[igr],'.rds'))
  raw_eqm <- readRDS(file=paste0(path.inp, 'pr_eqm_mon',im,'_grid',grids[igr],'.rds'))
  
  raw_med_mgp_emul <- do.call(rbind, lapply(raw_mgp_emul, median))
  raw_med_mgp_bc <- do.call(rbind, lapply(raw_mgp_bc, median))
  raw_med_eqm <- do.call(rbind, lapply(raw_eqm, median))
  
  med_pr_1 <- rbind(data.frame(year = yrs, value=raw_med_mgp_emul[,1], variable = 'Precipitation', legend='mgp_emul', month=im),
                    data.frame(year = yrs, value=raw_med_mgp_bc[,1], variable = 'Precipitation', legend='mgp_bc', month=im),
                    data.frame(year = yrs, value=raw_med_eqm[,1], variable = 'Precipitation', legend='eqm', month=im))
  med_pr <- rbind(med_pr, med_pr_1)
}

med_tas <- NULL
for(im in 1:length(month.name)){
  
  raw_mgp_emul <- readRDS(file=paste0(path.inp, 'tas_mgp_emul_mon',im,'_grid',grids[igr],'.rds'))
  raw_mgp_bc <- readRDS(file=paste0(path.inp, 'tas_mgp_bc_mon',im,'_grid',grids[igr],'.rds'))
  raw_eqm <- readRDS(file=paste0(path.inp, 'tas_eqm_mon',im,'_grid',grids[igr],'.rds'))
  
  raw_med_mgp_emul <- do.call(rbind, lapply(raw_mgp_emul, median))
  raw_med_mgp_bc <- do.call(rbind, lapply(raw_mgp_bc, median))
  raw_med_eqm <- do.call(rbind, lapply(raw_eqm, median))
  
  med_tas_1 <- rbind(data.frame(year = yrs, value=raw_med_mgp_emul[,1], variable = 'Temperature', legend='mgp_emul', month=im),
                     data.frame(year = yrs, value=raw_med_mgp_bc[,1], variable = 'Temperature', legend='mgp_bc', month=im),
                     data.frame(year = yrs, value=raw_med_eqm[,1], variable = 'Temperature', legend='eqm', month=im))
  med_tas <- rbind(med_tas, med_tas_1)
}


# get ss for precipitation
ss_pr <- NULL
for(im in 1:length(month.name)){
  obs <- subset(df_list.pr.obs, month %in% im)
  e3sm <- subset(df_list.pr.e3sm, month %in% im)
  mgp_emul <- subset(med_pr, legend %in% c('mgp_emul') & month %in% im)
  mgp_bc <- subset(med_pr, legend %in% c('mgp_bc') & month %in% im)
  eqm <- subset(med_pr, legend %in% c('eqm') & month %in% im)
  
  prism.pr <- obs$value
  model.pr <- e3sm$value
  mgp_emul.pr <- mgp_emul$value
  mgp_bc.pr <- mgp_bc$value
  eqm.pr <- eqm$value
  
  ss_pr_component <- NULL
  for(ivar in 1:4){
    prism <- prism.pr
    if(ivar==1) model <- model.pr
    if(ivar==2) model <- eqm.pr
    if(ivar==3) model <- mgp_emul.pr
    if(ivar==4) model <- mgp_bc.pr
    
    rho2 <- (cor(prism, model, method = "pearson"))^2
    cb <- (cor(prism, model, method = "pearson") - (sd(model) / sd(prism)))^2
    ub <- ((mean(model) - mean(prism)) / sd(prism))^2
    ss <- ((cor(prism, model, method = "pearson"))^2) - ((cor(prism, model, method = "pearson") - (sd(model) / sd(prism)))^2) - (((mean(model) - mean(prism)) / sd(prism))^2)
    
    ss_pr_component <- rbind(ss_pr_component, data.frame(rho2 = round(rho2,2), cb = round(cb,2), ub = round(ub,2), ss = round(ss,2), month = month.name[im], lgd=ivar))
  }
  ss_pr <- rbind(ss_pr, ss_pr_component)
}

# get tas for temperature
ss_tas <- NULL
for(im in 1:length(month.name)){
  obs <- subset(df_list.tas.obs, month %in% im)
  e3sm <- subset(df_list.tas.e3sm, month %in% im)
  mgp_emul <- subset(med_tas, legend %in% c('mgp_emul') & month %in% im)
  mgp_bc <- subset(med_tas, legend %in% c('mgp_bc') & month %in% im)
  eqm <- subset(med_tas, legend %in% c('eqm') & month %in% im)
  
  tasism.tas <- obs$value
  model.tas <- e3sm$value
  mgp_emul.tas <- mgp_emul$value
  mgp_bc.tas <- mgp_bc$value
  eqm.tas <- eqm$value
  
  ss_tas_component <- NULL
  for(ivar in 1:4){
    tasism <- tasism.tas
    if(ivar==1) model <- model.tas
    if(ivar==2) model <- eqm.tas
    if(ivar==3) model <- mgp_emul.tas
    if(ivar==4) model <- mgp_bc.tas
    
    rho2 <- (cor(tasism, model, method = "pearson"))^2
    cb <- (cor(tasism, model, method = "pearson") - (sd(model) / sd(tasism)))^2
    ub <- ((mean(model) - mean(tasism)) / sd(tasism))^2
    ss <- ((cor(tasism, model, method = "pearson"))^2) - ((cor(tasism, model, method = "pearson") - (sd(model) / sd(tasism)))^2) - (((mean(model) - mean(tasism)) / sd(tasism))^2)
    
    ss_tas_component <- rbind(ss_tas_component, data.frame(rho2 = round(rho2,2), cb = round(cb,2), ub = round(ub,2), ss = round(ss,2), month = month.name[im], lgd=ivar))
  }
  ss_tas <- rbind(ss_tas, ss_tas_component)
}

#### Plot skill score
f <- function(x) {
  r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}
  
melt_ss_tas <- melt(ss_tas, id.vars = c('month','lgd'))
melt_ss_pr <- melt(ss_pr, id.vars = c('month','lgd'))

bind_ss <- rbind(data.frame(melt_ss_tas, group ='Temperature'), data.frame(melt_ss_pr, group='Precipitation'))
bind_ss$group <- factor(bind_ss$group, levels = c('Temperature','Precipitation'))
bind_ss$lgd <- factor(bind_ss$lgd, levels = c(1:4), labels = c('E3SM','EQM','MGP (Emulated)', 'MGP (Bias corrected)'))
bind_ss$month <- factor(bind_ss$month, levels = month.name, labels = substr(month.name, 1, 3))
bind_ss$variable <- factor(bind_ss$variable, levels = c('ss','rho2','cb','ub'), labels = c('Skill score','Potential skill','Conditional bias','Unconditional bias'))

# one plot
gg1 <- ggplot()
gg1 <- gg1 + geom_bar(data = subset(bind_ss, variable %in% c('Skill score')), aes(x=month, y=value, fill=lgd),
                      stat="identity", width=0.6, position="dodge", lwd=0.1, color="black")
gg1 <- gg1 + geom_hline(yintercept = 0, col="black", linetype="solid", linewidth=0.5)
gg1 <- gg1 + coord_cartesian(ylim = c(-2,1))
gg1 <- gg1 + scale_y_continuous(breaks = seq(-2, 1, 0.5))
gg1 <- gg1 + scale_fill_manual(name="", values = c("#6A00A8FF","#B12A90FF","#E16462FF","#FCA636FF"))
gg1 <- gg1 + facet_grid(variable~group, scales="free")
gg1 <- gg1 + xlab("") + ylab("")
gg1 <- gg1 + theme(panel.background = element_rect(fill = 'white', color="black"),
                   plot.background = element_rect(fill = 'NA',colour = 'transparent'),
                   panel.grid.major.y = element_line(color = "gray80", linewidth = 0.1, linetype="solid"),
                   panel.grid.major.x = element_blank(),
                   panel.grid.minor = element_blank(),
                   strip.text = element_text(size = 10),
                   strip.background = element_rect(color="black"),
                   axis.text.x=element_blank(),
                   axis.ticks.x=element_blank(),
                   legend.position="none")


gg2 <- ggplot()
gg2 <- gg2 + geom_bar(data = subset(bind_ss, variable %in% c('Potential skill')), aes(x=month, y=value, fill=lgd),
                      stat="identity", width=0.6, position="dodge", lwd=0.1, color="black")
gg2 <- gg2 + geom_hline(yintercept = 0, linetype="solid", linewidth=0.5)
gg2 <- gg2 + coord_cartesian(ylim = c(0,0.8))
gg2 <- gg2 + scale_y_continuous(breaks = seq(0,0.8,0.2))
gg2 <- gg2 + scale_fill_manual(name="", values = c("#6A00A8FF","#B12A90FF","#E16462FF","#FCA636FF"))
gg2 <- gg2 + facet_grid(variable~group, scales="free")
gg2 <- gg2 + xlab("") + ylab("")
gg2 <- gg2 + theme(panel.background = element_rect(fill = 'white', color="black"),
                   plot.background = element_rect(fill = 'NA',colour = 'transparent'),
                   panel.grid.major.y = element_line(color = "gray80", linewidth = 0.1, linetype="solid"),
                   panel.grid.major.x = element_blank(),
                   panel.grid.minor = element_blank(),
                   strip.text.x = element_blank(),
                   strip.text.y = element_text(size = 10),
                   strip.background = element_rect(color="black"),
                   axis.text.x=element_blank(),
                   axis.ticks.x=element_blank(),
                   legend.position="none")

gg3 <- ggplot()
gg3 <- gg3 + geom_bar(data = subset(bind_ss, variable %in% c('Conditional bias')), aes(x=month, y=value, fill=lgd),
                      stat="identity", width=0.6, position="dodge", lwd=0.1, color="black")
gg3 <- gg3 + geom_hline(yintercept = 0, linetype="solid", linewidth=0.5)
gg3 <- gg3 + coord_cartesian(ylim = c(0,4))
gg3 <- gg3 + scale_fill_manual(name="", values = c("#6A00A8FF","#B12A90FF","#E16462FF","#FCA636FF"))
gg3 <- gg3 + facet_grid(variable~group, scales="free")
gg3 <- gg3 + xlab("") + ylab("")
gg3 <- gg3 + theme(panel.background = element_rect(fill = 'white', color="black"),
                   plot.background = element_rect(fill = 'NA',colour = 'transparent'),
                   panel.grid.major.y = element_line(color = "gray80", linewidth = 0.1, linetype="solid"),
                   panel.grid.major.x = element_blank(),
                   panel.grid.minor = element_blank(),
                   strip.text.x = element_blank(),
                   strip.text.y = element_text(size = 10),
                   strip.background = element_rect(color="black"),
                   axis.text.x=element_blank(),
                   axis.ticks.x=element_blank(),
                   legend.position="none")  

gg4 <- ggplot()
gg4 <- gg4 + geom_bar(data = subset(bind_ss, variable %in% c('Unconditional bias')), aes(x=month, y=value, fill=lgd),
                      stat="identity", width=0.6, position="dodge", lwd=0.1, color="black")
gg4 <- gg4 + geom_hline(yintercept = 0, linetype="solid", linewidth=0.5)
gg4 <- gg4 + coord_cartesian(ylim = c(0,6))
gg4 <- gg4 + scale_y_continuous(breaks = seq(0,6,1))
gg4 <- gg4 + scale_fill_manual(name="", values = c("#6A00A8FF","#B12A90FF","#E16462FF","#FCA636FF"))
gg4 <- gg4 + facet_grid(variable~group, scales="free")
gg4 <- gg4 + xlab("") + ylab("")
gg4 <- gg4 + theme(panel.background = element_rect(fill = 'white', color="black"),
                   plot.background = element_rect(fill = 'NA',colour = 'transparent'),
                   panel.grid.major.y = element_line(color = "gray80", linewidth = 0.1, linetype="solid"),
                   panel.grid.major.x = element_blank(),
                   panel.grid.minor = element_blank(),
                   strip.text.x = element_blank(),
                   strip.text.y = element_text(size = 10),
                   strip.background = element_rect(color="black"),
                   legend.key.height = unit(0.2, "cm"),
                   legend.key.width = unit(0.15, "cm"),
                   legend.direction="horizontal",
                   legend.position= c(0.75, 0.84))    
gg4 <- gg4 + guides(fill = guide_legend(ncol=2, byrow=TRUE))

gg <-  ggdraw(plot = NULL, xlim = c(0, 4), ylim = c(0, 8.7), clip = "off")
gg <-  gg + draw_plot(gg1, x = -0.093, y = 6.2, width = 4.093, height = 2.5)
gg <-  gg + draw_plot(gg2, x = -0.051, y = 4.2, width = 4.051, height = 2.3)
gg <-  gg + draw_plot(gg3, x = 0, y = 2.2, width = 4, height = 2.3)
gg <-  gg + draw_plot(gg4, x = 0, y = -0.2, width = 4, height = 2.7)
gg <-  gg + theme(panel.background = element_rect(fill = 'NA',colour = 'transparent'),
                  panel.grid = element_blank())

png(filename=paste0(path.out, 'k_fold_ss_grid',grids[igr],'.png'), width = 7.5, height = 8, units = "in", res=300)
print(gg)
dev.off()


####Violin plot
df_merge <- rbind(df_obs, med_pr, med_tas)
df_merge$variable <- factor(df_merge$variable, levels = c('Temperature','Precipitation'))
df_merge$legend <- factor(df_merge$legend, levels = c('PRISM','E3SM','eqm','mgp_emul','mgp_bc'),
                          labels = c('PRISM','E3SM','EQM','MGP (Emulated)','MGP (Bias corrected)'))
df_merge$month <- factor(df_merge$month, levels = c(1:12), labels = month.name)   
df_merge$type <- ifelse(df_merge$legend %in% c('PRISM','E3SM'), 'boxplot', 'violin')



#plot

alpha.value = 0.6

gg <- ggplot(data=subset(df_merge, variable == 'Temperature'), aes(x=0, y=value, fill=legend), color="black")
gg <- gg + stat_summary(data = subset(df_merge, variable=='Temperature'& type == 'boxplot' & legend == 'PRISM'), 
                        fun.data = f, geom="boxplot", width=0.15, position=position_nudge(x=-0.5), lwd=0.2)
gg <- gg + stat_summary(data = subset(df_merge, variable=='Temperature'& type == 'boxplot' & legend == 'E3SM'), 
                        fun.data = f, geom="boxplot", width=0.15, position=position_nudge(x=-0.3), lwd=0.2)

gg <- gg + geom_violin(data = subset(df_merge, variable=='Temperature'& type == 'violin' & legend == 'EQM'),
                       position=position_nudge(x=0.1), alpha=alpha.value, lwd=0.2)
gg <- gg + stat_summary(data = subset(df_merge, variable=='Temperature'& type == 'violin' & legend == 'EQM'),
                        fun.data = function(x) {data.frame(y = median(x), ymin = quantile(x, 0.25), ymax = quantile(x, 0.75))},
                        geom = "pointrange", position=position_nudge(x=0.1), colour = "black", lwd=0.2, size=0.1)
gg <- gg + geom_violin(data = subset(df_merge, variable=='Temperature'& type == 'violin' & legend == 'MGP (Emulated)'),
                       position=position_nudge(x=0.4), alpha=alpha.value, lwd=0.2)
gg <- gg + stat_summary(data = subset(df_merge, variable=='Temperature'& type == 'violin' & legend == 'MGP (Emulated)'),
                        fun.data = function(x) {data.frame(y = median(x), ymin = quantile(x, 0.25), ymax = quantile(x, 0.75))},
                        geom = "pointrange", position=position_nudge(x=0.4), colour = "black", lwd=0.2, size=0.1)
gg <- gg + geom_violin(data = subset(df_merge, variable=='Temperature'& type == 'violin'& legend == 'MGP (Bias corrected)'),
                       position=position_nudge(x=0.7), alpha=alpha.value, lwd=0.2)
gg <- gg + stat_summary(data = subset(df_merge, variable=='Temperature'& type == 'violin'& legend == 'MGP (Bias corrected)'),
                        fun.data = function(x) {data.frame(y = median(x), ymin = quantile(x, 0.25), ymax = quantile(x, 0.75))},
                        geom = "pointrange", position=position_nudge(x=0.7), colour = "black", lwd=0.2, size=0.1)
gg <- gg + scale_fill_manual(name="", values = c("#0D0887FF","#6A00A8FF","#B12A90FF","#E16462FF","#FCA636FF"), drop=F)
#gg <- gg + scale_y_continuous(labels = label_number(accuracy = 0.1))
gg <- gg + facet_wrap(.~month, nrow=4, scales="free_y")
#gg <- gg + scale_y_continuous(breaks = seq(0,30,by=5), labels = seq(0,30,by=5))
#gg <- gg + coord_cartesian(ylim=c(0, 30))
gg <- gg + theme_bw() + labs(x = "", y = "Temperature (°C)")
gg <- gg + theme(legend.position = "bottom",
                 legend.background = element_rect(fill = 'NA',colour = 'transparent'),
                 panel.background = element_rect(fill = 'white'),
                 legend.box.margin = margin(0, 0, 0, 0), # Remove extra margin around the legend
                 legend.margin = margin(0.01, 0.01, 0.01, 0.01), # Remove margin inside the legend
                 legend.direction = "horizontal",
                 legend.key.width = unit(0.4, 'cm'), 
                 axis.ticks = element_blank(),
                 axis.text.x=element_blank(),
                 panel.grid.major.x = element_blank(),
                 panel.grid.minor.x = element_blank(),
                 strip.text.x = element_text(size = 10),
                 strip.text.y = element_text(size = 10),
                 plot.background = element_rect(fill = 'white',colour = 'transparent'))


png(filename=paste0(path.out, 'violin_temp.png'), width = 10, height = 11, units = "in", res=300)
print(gg)
dev.off()
  
  
  

alpha.value = 0.6

gg <- ggplot(data=subset(df_merge, variable == 'Precipitation'), aes(x=0, y=value, fill=legend), color="black")
gg <- gg + stat_summary(data = subset(df_merge, variable=='Precipitation'& type == 'boxplot' & legend == 'PRISM'), 
                        fun.data = f, geom="boxplot", width=0.15, position=position_nudge(x=-0.5), lwd=0.2)
gg <- gg + stat_summary(data = subset(df_merge, variable=='Precipitation'& type == 'boxplot' & legend == 'E3SM'), 
                        fun.data = f, geom="boxplot", width=0.15, position=position_nudge(x=-0.3), lwd=0.2)

gg <- gg + geom_violin(data = subset(df_merge, variable=='Precipitation'& type == 'violin' & legend == 'EQM'),
                       position=position_nudge(x=0.1), alpha=alpha.value, lwd=0.2)
gg <- gg + stat_summary(data = subset(df_merge, variable=='Precipitation'& type == 'violin' & legend == 'EQM'),
                        fun.data = function(x) {data.frame(y = median(x), ymin = quantile(x, 0.25), ymax = quantile(x, 0.75))},
                        geom = "pointrange", position=position_nudge(x=0.1), colour = "black", lwd=0.2, size=0.1)
gg <- gg + geom_violin(data = subset(df_merge, variable=='Precipitation'& type == 'violin' & legend == 'MGP (Emulated)'),
                       position=position_nudge(x=0.4), alpha=alpha.value, lwd=0.2)
gg <- gg + stat_summary(data = subset(df_merge, variable=='Precipitation'& type == 'violin' & legend == 'MGP (Emulated)'),
                        fun.data = function(x) {data.frame(y = median(x), ymin = quantile(x, 0.25), ymax = quantile(x, 0.75))},
                        geom = "pointrange", position=position_nudge(x=0.4), colour = "black", lwd=0.2, size=0.1)
gg <- gg + geom_violin(data = subset(df_merge, variable=='Precipitation'& type == 'violin'& legend == 'MGP (Bias corrected)'),
                       position=position_nudge(x=0.7), alpha=alpha.value, lwd=0.2)
gg <- gg + stat_summary(data = subset(df_merge, variable=='Precipitation'& type == 'violin'& legend == 'MGP (Bias corrected)'),
                        fun.data = function(x) {data.frame(y = median(x), ymin = quantile(x, 0.25), ymax = quantile(x, 0.75))},
                        geom = "pointrange", position=position_nudge(x=0.7), colour = "black", lwd=0.2, size=0.1)
gg <- gg + scale_fill_manual(name="", values = c("#0D0887FF","#6A00A8FF","#B12A90FF","#E16462FF","#FCA636FF"), drop=F)
#gg <- gg + scale_y_continuous(labels = label_number(accuracy = 0.1))
gg <- gg + facet_wrap(.~month, nrow=4)
gg <- gg + scale_y_continuous(breaks = seq(0,300,by=50), labels = seq(0,300,by=50))
gg <- gg + coord_cartesian(ylim=c(0, 300))
gg <- gg + theme_bw() + labs(x = "", y = "Precipitation (mm)")
gg <- gg + theme(legend.position = "bottom",
                 legend.background = element_rect(fill = 'NA',colour = 'transparent'),
                 panel.background = element_rect(fill = 'white'),
                 legend.box.margin = margin(0, 0, 0, 0), # Remove extra margin around the legend
                 legend.margin = margin(0.01, 0.01, 0.01, 0.01), # Remove margin inside the legend
                 legend.direction = "horizontal",
                 legend.key.width = unit(0.4, 'cm'), 
                 axis.ticks = element_blank(),
                 axis.text.x=element_blank(),
                 panel.grid.major.x = element_blank(),
                 panel.grid.minor.x = element_blank(),
                 strip.text.x = element_text(size = 10),
                 strip.text.y = element_text(size = 10),
                 plot.background = element_rect(fill = 'white',colour = 'transparent'))


png(filename=paste0(path.out, 'violin_pr.png'), width = 10, height = 11, units = "in", res=300)
print(gg)
dev.off()
