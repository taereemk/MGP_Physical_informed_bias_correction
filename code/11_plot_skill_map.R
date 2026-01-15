rm(list=ls())
library(reshape2)
library(dplyr)
library(tibble)
library(ggplot2)
library(ggh4x)
library(viridis)

path = ".../"
path.inp <- paste0(path, "res/res10/")
path.out <- paste0(path, "res/res11/")
dir.create(path.out, showWarnings = F)

path.data = paste0(path,"data/")

sub_region <- c("Northwest","Southwest","GreatPlainsNorth","GreatPlainsSouth","Midwest","Northeast","Southeast")


dt_all <- NULL
for(isub in 1:length(sub_region)){
  xy_coord <- readRDS(file=paste0(path.data, 'xy_',sub_region[isub],'_sel.rds'))
  ngrid <- nrow(xy_coord)
  
  dt <- readRDS(paste0(path.inp, 'res10_skill_median_',sub_region[isub],'.rds'))
  dt <- dt %>% left_join(xy_coord %>% mutate(id = row_number()), by = "id")
  
  dt_all <- rbind(dt_all, dt)
  rm(dt)
}

metrics <- c("median_ss","median_rho2","median_cb","median_ub","median_rmse")

for(imon in 1:12){
  
  dt_filter <- dt_all %>% filter(month == imon)
  dt_reshape <- melt(dt_filter, id.vars = c('id','x','y','variable','group','method'))
  colnames(dt_reshape) <- c("id","x","y","variable","group","method","metric","value")
  
  dt_sel <- dt_reshape %>% filter(metric == 'median_ss')
  dt_sel$variable <- factor(dt_sel$variable, levels = c('pr','tas'), labels = c('Precipitation','Temperature'))
  dt_sel$group <- factor(dt_sel$group, levels = c('train','test'), labels = c('Train','Test'))
  dt_sel$method <- factor(dt_sel$method, levels = c('model','eqm','mgp_emul','mgp_bc'), 
                          labels = c('CESM2','EQM','MGP (Emulated)','MGP (Bias corrected)'))
  
  gg <- ggplot()
  gg <- gg + ggtitle(paste0(month.name[imon],' Noise upper level: 1'))
  gg <- gg + geom_tile(dt_sel, mapping=aes(x=x, y=y, fill=value))
  gg <- gg + facet_nested(variable+group~method)
  gg <- gg + scale_fill_gradient2(name="Skill Score",
                                  low = "maroon", mid = "white", high = "blue", midpoint = 0, 
                                  limits = c(-2, 1), oob = scales::squish)
  gg <- gg + theme_bw()
  gg <- gg + labs(x = "", y="")
  gg <- gg + theme_bw()
  gg <- gg + theme(legend.position = "bottom",
                   legend.direction = "horizontal",
                   panel.background = element_rect(fill = 'aliceblue'),
                   panel.grid = element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_blank(),
                   axis.text.x=element_blank(),
                   axis.text.y=element_blank(),
                   axis.ticks.x=element_blank(),
                   axis.ticks.y=element_blank())
  
  png(paste0(path.out, "res11_ss_M",imon,".png"),  width=9, height=6, unit='in', res=150)
  plot(gg)
  dev.off()
}


