rm(list=ls())
library(dplyr)
library(plotly)
library(ggplot2)
library(viridis)
library(cowplot)
library(ggh4x)
library(scales)

path.inp <- "C:/Users/tk6869/Princeton Dropbox/Taereem Kim/Projects/Pj04_MGP/res11_mgp_all_data/res01_mgp/"
path.out <- "C:/Users/tk6869/Princeton Dropbox/Taereem Kim/Projects/Pj04_MGP/res11_mgp_all_data/res04_contour/"

grids=c(62,63,64,79,80,81,94,95,96)
yr = c(1981:2013)

for(im in 1:length(month.name)){
  
  mon.df.tas.list <- list()
  mon.df.pr.list <- list() 
  for(igr in 1:length(grids)){
    
    path.inp.1 <- paste0(path.inp,"grid",grids[igr],"/")
    
    res <- readRDS(paste0(path.inp.1, 'res_mon',im,'_grid',grids[igr],'_allyrs.rds'))
    
    df.surf.tas <- data.frame(x = res$surf.z1_fe_inv$x, 
                              y = res$surf.z1_fe_inv$y, 
                              z = res$surf.z1_fe_inv$z, 
                              cvar = 'Temperature',
                              grids = grids[igr])
    df.surf.pr <- data.frame(x = res$surf.z3_fe_inv$x, 
                             y = res$surf.z3_fe_inv$y, 
                             z = res$surf.z3_fe_inv$z, 
                             cvar = 'Precipitation',
                             grids = grids[igr])
    mon.df.tas.list[[igr]] <- df.surf.tas
    mon.df.pr.list[[igr]] <- df.surf.pr
  }
  df_mon_tas <- do.call(rbind, mon.df.tas.list)
  df_mon_pr <- do.call(rbind, mon.df.pr.list)
  
  
  ###############################
  # Temperature
  ###############################
  mybreaks.t <- c(-Inf, seq(-5, 5, by=1), Inf)
  n.interval <- length(mybreaks.t) - 1
  # Set palette
  mycolors.t <- function(x) {
    colors <- colorRampPalette(c("red4","white","blue4"))(n.interval)
    colors[1:x]
  }
  
  gg1 <- ggplot(data=df_mon_tas, aes(x = x, y = y, z = z))
  #gg1 <- gg1 + ggtitle(month.name[im])
  gg1 <- gg1 + geom_contour_filled(breaks= mybreaks.t, color = 'black', linewidth = 0.2)
  gg1 <- gg1 + facet_wrap(~grids, scales = "free", nrow = 3)
  gg1 <- gg1 + scale_fill_manual(name = "Temperature bias (°C)",
                                 values=mycolors.t(n.interval),
                                 drop=F,
                                 guide = guide_colorsteps())
  gg1 <- gg1 + scale_x_continuous(labels = number_format(accuracy = 1))
  gg1 <- gg1 + scale_y_continuous(labels = number_format(accuracy = 1))
  gg1 <- gg1 + xlab(expression("GPP (gC/m"^{2}~"/month)")) + ylab("ET (mm/month)")
  gg1 <- gg1 + theme_bw()
  gg1 <- gg1 + theme(strip.text = element_blank(),
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(), 
                     panel.border = element_blank(), 
                     legend.position = "bottom", 
                     legend.key.height = unit(0.2, "cm"), 
                     legend.key.width = unit(2.4, "cm")) 
  gg1 <- gg1 + guides(fill = guide_colorsteps(title.position = "bottom", title.hjust = 0.5, nrow=1,
                                              show.limits = F))
  gg1 <- gg1 + coord_cartesian(expand = FALSE) 
  
  png(filename=paste0(path.out, 'plot_contour_temperature_',month.name[im],'.png'), width = 8, height = 8.2, units = "in", res=300)
  print(gg1)
  dev.off()
  rm(gg1)
  
  
  ###############################
  # Precipitation
  ###############################
  mybreaks.p <- c(-Inf, seq(-100, 100, by=20), Inf)
  n.interval <- length(mybreaks.p) - 1 
  # Set palette
  mycolors.p <- function(x) {
    colors <- colorRampPalette(c("red4","white","blue4"))(n.interval)
    colors[1:x]
  }
  
  gg1 <- ggplot(data=df_mon_pr, aes(x = x, y = y, z = z))
  #gg1 <- gg1 + ggtitle(month.name[im])
  gg1 <- gg1 + geom_contour_filled(breaks= mybreaks.p, color = 'black', linewidth = 0.2) 
  gg1 <- gg1 + facet_wrap(~ grids, scales = "free", nrow=3) 
  gg1 <- gg1 + scale_fill_manual(name = "Precipitation bias (mm)",
                                 values=mycolors.t(n.interval), 
                                 drop=F,
                                 guide = guide_colorsteps())
  gg1 <- gg1 + scale_x_continuous(labels = number_format(accuracy = 1))
  gg1 <- gg1 + scale_y_continuous(labels = number_format(accuracy = 1))
  gg1 <- gg1 + xlab(expression("GPP (gC/m"^{2}~"/month)")) + ylab("ET (mm/month)")
  gg1 <- gg1 + theme_bw()
  gg1 <- gg1 + theme(strip.text = element_blank(),
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(), 
                     panel.border = element_blank(), 
                     legend.position = "bottom", 
                     legend.key.height = unit(0.2, "cm"), 
                     legend.key.width = unit(2.8, "cm")) 
  gg1 <- gg1 + guides(fill = guide_colorsteps(title.position = "bottom", title.hjust = 0.5, nrow=1,
                                              show.limits = FALSE))
  gg1 <- gg1 + coord_cartesian(expand = FALSE)
  
  png(filename=paste0(path.out, 'plot_contour_precipitation_',month.name[im],'.png'), width = 8, height = 8.2, units = "in", res=300)
  print(gg1)
  dev.off()
  rm(gg1)
}


