rm(list=ls())
library(dplyr)
library(plotly)
library(ggplot2)
library(viridis)
library(cowplot)
library(ggh4x)
library(scales)
library(plot3D)

path = ".../"

path.data = paste0(path,"data/")
path.inp = paste0(path, "res/res05/")
path.out = paste0(path,'res/res06/')
path.out.1 = paste0(path.out, 'Southeast/')
dir.create(path.out, showWarnings = F)
dir.create(path.out.1, showWarnings = F)

xy_coord <- readRDS(file=paste0(path.data, 'xy_Southeast_sel.rds'))

#for(ii in 1:ngrid){
ii=1
  path.out.2 = paste0(path.out.1, "ID",ii,"/")
  dir.create(path.out.2, showWarnings = F)
  
for(imon in 1:length(month.name)){

    res <- readRDS(paste0(path.inp, 'M',imon,'/res05_ID',ii,'_M',imon,'_Southeast.rds'))
    
    # #1. Surface plot
    tas.surf <- plot_ly(y = res$sf.z1_fm$x, x = res$sf.z1_fm$y, z = res$sf.z1_fm$z, type = "surface", opacity = 0.5) %>%
      add_markers(name="CESM2", y = res$norm.x_fm, x = res$norm.y_fm, z = res$z.tas_fm, color = I("black"), size = 2, marker = list(symbol = "x")) %>%
      add_markers(name="PRISM", y = res$norm.x_obs, x = res$norm.y_obs, z = res$z.tas_obs, color = I("red"), size = 2, marker = list(symbol = "square")) %>%
      add_markers(name="MGP (Emulated)", y = res$norm.x_obs, x = res$norm.y_obs, z = res$ypred.train.tas_obs, color = I("red"), size = 2, marker = list(symbol = "x")) %>%
      add_markers(name="MGP (Bias corrected)", y = res$norm.tx_obs, x = res$norm.ty_obs, z = res$ypred.fin_tas, color = I("blue"), size = 2, marker = list(symbol = "square")) %>%
      layout(scene = list(xaxis = list(title = "Latent heat flux", range = range(res$sf.z1_fm$y)),
                          yaxis = list(title = "Sensible heat flux"),
                          zaxis = list(title = "taserature")))
    
    tas.surf.bias <- plot_ly(y = res$sf.z1_fe$x, x = res$sf.z1_fe$y, z = res$sf.z1_fe$z, type = "surface", opacity = 0.5, colorscale="plasma") %>%
      add_markers(name="Bias",y = res$norm.x_obs, x = res$norm.y_obs, z = res$z.tas_fe, color = I("black"), size = 2.5, marker = list(symbol = "x")) %>%
      add_markers(name="MGP (Emulated bias)",y = res$norm.tx_obs, x = res$norm.ty_obs, z = res$fe_xnew_tas, color = I("blue"), size = 2.5, marker = list(symbol = "circle")) %>%
      layout(scene = list(xaxis = list(title = "Latent heat flux", range = rev(range(res$sf.z1_fe$y))),
                          yaxis = list(title = "Sensible heat flux"),
                          zaxis = list(title = "taserature")))
    
    combined_plot_tas <- subplot(tas.surf, tas.surf.bias, nrows = 2, shareX = FALSE, shareY = FALSE)
    htmlwidgets::saveWidget(combined_plot_tas, paste0(path.out.2, "ID",ii,"_Surface_tas_M",imon,"_Southeast.html"),
                            selfcontained = TRUE)

  }
  
  
#}
