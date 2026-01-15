rm(list=ls())
library(dplyr)
library(plotly)
library(ggplot2)
library(viridis)
library(cowplot)
library(ggh4x)
library(scales)
library(plot3D)
library(patchwork)

path = ".../"

path.data = paste0(path,"data/")
path.inp = paste0(path, "res/res05/")
path.out = paste0(path,'res/res07/')
path.out.1 = paste0(path.out, 'Southeast/')
dir.create(path.out, showWarnings = F)
dir.create(path.out.1, showWarnings = F)

xy_coord <- readRDS(file=paste0(path.data, 'xy_Southeast_sel.rds'))
ngrid <- nrow(xy_coord)

## ggmap
usmap <- ggplot2::borders("state", colour="gray50", fill="white")
sps <- readRDS(file=paste0(path, "data/00_Basin_Southeast.RDS"))


for(ii in 1:ngrid){
  
  gg <- ggplot()
  gg <- gg + usmap
  gg <- gg + geom_polygon(data=sps, aes(x=long, y=lat, group=group), 
                          fill="pink",color="black", linewidth=0.1) 
  gg <- gg + geom_point(data = xy_coord, aes(x = x, y = y), color = 'gray', cex=0.5)
  gg <- gg + geom_point(data = xy_coord[ii,], aes(x = x, y = y), color = 'red', cex=2)
  gg <- gg + coord_cartesian(xlim = c(-96, -74), ylim = c(24, 40.5), expand=F)
  gg <- gg + labs(x = "", y="")
  gg <- gg + theme_bw()
  gg <- gg + theme(legend.position = c(0.3,0.05),
                   legend.direction = "horizontal",
                   panel.background = element_rect(fill = 'aliceblue'),
                   panel.grid = element_blank(),
                   axis.ticks.x=element_blank(),
                   axis.ticks.y=element_blank())
  gg_small <- gg + theme(plot.margin = margin(t = 10, b = 10)) + coord_fixed(ratio = 1.5)
  
  
  df.gg <- data.frame(x = numeric(),
                      y = numeric(),
                      z = numeric(),
                      cvar = character(),
                      month = character(),
                      stringsAsFactors = FALSE)
  
  
  for(imon in 1:length(month.name)){
    
    res <- readRDS(paste0(path.inp, 'M',imon,'/res05_ID',ii,'_M',imon,'_Southeast.rds'))
    
    #Get data for contour
    df.surf.tas <- data.frame(x = res$surf.z1_fe_inv$x,
                              y = res$surf.z1_fe_inv$y,
                              z = res$surf.z1_fe_inv$z,
                              cvar = 'Temperature',
                              month = month.name[imon])
    df.surf.pr <- data.frame(x = res$surf.z3_fe_inv$x,
                             y = res$surf.z3_fe_inv$y,
                             z = res$surf.z3_fe_inv$z,
                             cvar = 'Precipitation',
                             month = month.name[imon])
    df.merge <- rbind(df.surf.tas, df.surf.pr)
    df.gg <- rbind(df.gg, df.merge)
  }
  
  #3. Plot contour
  df.gg$month <- factor(df.gg$month, levels = month.name)
  range(df.gg %>% filter(cvar == "Temperature") %>% dplyr::select("z"))
  # Set bins
  mybreaks.t <- c(-Inf, seq(-5, 5, by=1), Inf)
  n.interval <- length(mybreaks.t) - 1
  
  # Set palette
  mycolors.t <- function(x) {
    colors <- colorRampPalette(c("red4","white","blue4"))(n.interval)
    colors[1:x]
  }
  
  gg1 <- ggplot(data=subset(df.gg, cvar %in% 'Temperature'), aes(x = x, y = y, z = z))
  gg1 <- gg1 + geom_contour_filled(breaks= mybreaks.t, color = 'black', linewidth = 0.2)
  gg1 <- gg1 + facet_wrap(~ month, scales = "free", nrow=4)
  gg1 <- gg1 + scale_fill_manual(name = "Temperature bias (°C)",
                                 values=mycolors.t(n.interval),
                                 drop=F,
                                 guide = guide_colorsteps())
  gg1 <- gg1 + scale_x_continuous(labels = number_format(accuracy = 0.1))
  gg1 <- gg1 + scale_y_continuous(labels = number_format(accuracy = 0.1))
  gg1 <- gg1 + xlab(expression("SH (W m-2)")) + ylab("LH (W m-2)")
  gg1 <- gg1 + theme_bw()
  gg1 <- gg1 + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.border = element_blank(),
                     legend.position = "bottom",
                     strip.text = element_text(size = 12),
                     legend.text = element_text(size = 12),
                     legend.title = element_text(size = 14),
                     axis.title = element_text(size = 14),
                     legend.key.height = unit(0.2, "cm"),
                     legend.key.width = unit(1.5, "cm"))
  gg1 <- gg1 + guides(fill = guide_colorsteps(title.position = "bottom", title.hjust = 0.5, nrow=1,
                                              show.limits = F))
  gg1 <- gg1 + coord_cartesian(expand = FALSE)
  
  
  ### Contour: Precipitation
  # # Check the range of z values
  df.gg$month <- factor(df.gg$month, levels = month.name)
  range(df.gg %>% filter(cvar == "Precipitation") %>% dplyr::select("z"))
  #Set bins
  mybreaks.p <- c(-Inf, seq(-100, 100, by=20), Inf)
  n.interval <- length(mybreaks.p) - 1
  # Set palette
  mycolors.p <- function(x) {
    colors <- colorRampPalette(c("red4","white","blue4"))(n.interval)
    colors[1:x]
  }
  
  gg2 <- ggplot(data=subset(df.gg, cvar %in% 'Precipitation'), aes(x = x, y = y, z = z))
  gg2 <- gg2 + geom_contour_filled(breaks= mybreaks.p, color = 'black', linewidth = 0.2)
  gg2 <- gg2 + facet_wrap(~ month, scales = "free", nrow=4)
  gg2 <- gg2 + scale_fill_manual(name = "Precipitation bias (mm/month)",
                                 values=mycolors.p(n.interval),
                                 drop=F,
                                 guide = guide_colorsteps())
  gg2 <- gg2 + scale_x_continuous(labels = number_format(accuracy = 0.1))
  gg2 <- gg2 + scale_y_continuous(labels = number_format(accuracy = 0.1))
  gg2 <- gg2 + xlab(expression("SH (W m-2)")) + ylab(NULL)
  gg2 <- gg2 + theme_bw()
  gg2 <- gg2 + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.border = element_blank(),
                     strip.text = element_text(size = 12),
                     legend.text = element_text(size = 12),
                     legend.title = element_text(size = 14),
                     axis.title = element_text(size = 14),
                     legend.position = "bottom",
                     legend.key.height = unit(0.2, "cm"),
                     legend.key.width = unit(1.8, "cm"))
  gg2 <- gg2 + guides(fill = guide_colorsteps(title.position = "bottom", title.hjust = 0.5, nrow=1,
                                              show.limits = F))
  gg2 <- gg2 + coord_cartesian(expand = FALSE)
  
  
  gg_fin <- gg_small + gg1 + gg2 + plot_layout(widths = c(0.5, 1, 1))
  
  
  png(paste0(path.out.1, "plot07_bias_surface_ID",ii,".png"), width=15, height=7.5, unit='in', res=150)
  plot(gg_fin)
  dev.off()
  
}# End of Grid

