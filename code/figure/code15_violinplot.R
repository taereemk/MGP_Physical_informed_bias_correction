rm(list=ls())
library(dplyr)
library(reshape2)
library(ggplot2)
library(cowplot)

path.data = "C:/Users/tk6869/Princeton Dropbox/Taereem Kim/Projects/Pj04_MGP/data/"


path.inp <- "C:/Users/tk6869/Princeton Dropbox/Taereem Kim/Projects/Pj04_MGP/res09_med_timeseries/"
path.out <- "C:/Users/tk6869/Princeton Dropbox/Taereem Kim/Projects/Pj04_MGP/res13_violinplot/"
dir.create(path.out, showWarnings = F)


grids=c(62,63,64,79,80,81,94,95,96)
yr = c(1981:2013)

list.pr.obs <- readRDS(paste0(path.data, 'data_pr_obs.rds'))
list.tas.obs <- readRDS(paste0(path.data, 'data_tas_obs.rds'))
list.pr.e3sm <- readRDS(paste0(path.data, 'data_pr_e3sm.rds'))
list.tas.e3sm <- readRDS(paste0(path.data, 'data_tas_e3sm.rds'))

f <- function(x) {
  r <- quantile(x, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  return(r)
}


#for(igr in 1:length(grids)){
igr=5
# arrange observations
df_list.pr.obs <- NULL
df_list.pr.e3sm <- NULL
df_list.tas.obs <- NULL
df_list.tas.e3sm <- NULL

for(im in 1:length(month.name)){
  list.pr.obs.1 <- data.frame(year=yr, value=as.numeric(do.call(rbind, list.pr.obs[[im]])[,grids[igr]]),
                              variable='Precipitation', legend='PRISM',month=im)
  list.pr.e3sm.1 <- data.frame(year=yr, value=as.numeric(do.call(rbind, list.pr.e3sm[[im]])[,grids[igr]]),
                               variable='Precipitation', legend='E3SM',month=im)  
  list.tas.obs.1 <- data.frame(year=yr, value=as.numeric(do.call(rbind, list.tas.obs[[im]])[,grids[igr]]),
                               variable='Temperature', legend='PRISM',month=im)  
  list.tas.e3sm.1 <- data.frame(year=yr, value=as.numeric(do.call(rbind, list.tas.e3sm[[im]])[,grids[igr]]),
                                variable='Temperature', legend='E3SM',month=im)    
  
  df_list.pr.obs <- rbind(df_list.pr.obs, list.pr.obs.1)
  df_list.pr.e3sm <- rbind(df_list.pr.e3sm, list.pr.e3sm.1)  
  df_list.tas.obs <- rbind(df_list.tas.obs, list.tas.obs.1)  
  df_list.tas.e3sm <- rbind(df_list.tas.e3sm, list.tas.e3sm.1)  
}

df_obs <- rbind(df_list.pr.obs, df_list.pr.e3sm, df_list.tas.obs, df_list.tas.e3sm)


ts <- readRDS('C:/Users/tk6869/Princeton Dropbox/Taereem Kim/Projects/Pj04_MGP/res08_mgp_ts_grid80.rds')
ts.test <- ts %>% filter(group %in% c('Test'))
df_mgp <- ts.test[,-which(colnames(ts.test) == 'group')]
df_mgp <- df_mgp[, c("L1","value", "variable", "legend", "month")]
colnames(df_mgp) <- c("year","value", "variable", "legend", "month")



df_merge <- rbind(df_obs, df_mgp)
df_merge$variable <- factor(df_merge$variable, levels = c('Temperature','Precipitation'))
df_merge$legend <- factor(df_merge$legend, levels = c('E3SM','PRISM','MGP_Emul','MGP_BC'),
                          labels = c('E3SM','PRISM','MGP (Emulated)','MGP (Bias corrected)'))
df_merge$month <- factor(df_merge$month, levels = c(1:12), labels = month.name)   
df_merge$type <- ifelse(df_merge$legend %in% c('E3SM', 'PRISM'), 'boxplot', 'violin')



#plot
gg <- ggplot(data=subset(df_merge, variable == 'Temperature'), aes(x=0, y=value, color=legend, fill=legend))
gg <- gg + stat_summary(data = subset(df_merge, variable=='Temperature'& type == 'boxplot' & legend == 'E3SM'), 
                        fun.data = f, geom="boxplot", width=0.15, position=position_nudge(x=-0.5), lwd=0.2)
gg <- gg + stat_summary(data = subset(df_merge, variable=='Temperature'& type == 'boxplot' & legend == 'PRISM'), 
                        fun.data = f, geom="boxplot", width=0.15, position=position_nudge(x=-0.3), lwd=0.2)

gg <- gg + geom_violin(data = subset(df_merge, variable=='Temperature'& type == 'violin' & legend == 'MGP (Emulated)'),
                       position=position_nudge(x=0.2), alpha=0.3, lwd=0.2)
gg <- gg + stat_summary(data = subset(df_merge, variable=='Temperature'& type == 'violin' & legend == 'MGP (Emulated)'),
                        fun.data = function(x) {data.frame(y = median(x), ymin = quantile(x, 0.25), ymax = quantile(x, 0.75))},
                        geom = "pointrange", position=position_nudge(x=0.2), colour = "red", lwd=0.2, size=0.1)
gg <- gg + geom_violin(data = subset(df_merge, variable=='Temperature'& type == 'violin'& legend == 'MGP (Bias corrected)'),
                       position=position_nudge(x=0.8), alpha=0.3, lwd=0.2)
gg <- gg + stat_summary(data = subset(df_merge, variable=='Temperature'& type == 'violin'& legend == 'MGP (Bias corrected)'),
                        fun.data = function(x) {data.frame(y = median(x), ymin = quantile(x, 0.25), ymax = quantile(x, 0.75))},
                        geom = "pointrange", position=position_nudge(x=0.8), colour = "blue", lwd=0.2, size=0.1)
gg <- gg + scale_color_manual(name="", values = c("magenta","black","red","blue"), drop=F)
gg <- gg + scale_fill_manual(name="", values = c("#ff80ff","#808080","#ff8080","#8080ff"), drop=F)
#gg <- gg + scale_y_continuous(labels = label_number(accuracy = 0.1))
gg <- gg + facet_wrap(.~month, nrow=2)
gg <- gg + scale_y_continuous(breaks = seq(0,30,by=5), labels = seq(0,30,by=5))
gg <- gg + coord_cartesian(ylim=c(0, 30))
gg <- gg + theme_bw() + labs(x = "", y = "Temperature (°C)")
gg <- gg + theme(legend.position = c(0.082, 0.93),
                 legend.background = element_rect(fill = 'NA',colour = 'transparent'),
                 panel.background = element_rect(fill = 'white'),
                 legend.box.margin = margin(0, 0, 0, 0), # Remove extra margin around the legend
                 legend.margin = margin(0.01, 0.01, 0.01, 0.01), # Remove margin inside the legend
                 legend.direction = "vertical",
                 legend.key.width = unit(0.4, 'cm'), 
                 axis.ticks = element_blank(),
                 axis.text.x=element_blank(),
                 panel.grid.major.x = element_blank(),
                 panel.grid.minor.x = element_blank(),
                 strip.text.x = element_text(size = 10),
                 strip.text.y = element_text(size = 10),
                 plot.background = element_rect(fill = 'white',colour = 'transparent'))


png(filename=paste0(path.out, 'violin_temp.png'), width = 10, height = 7, units = "in", res=300)
print(gg)
dev.off()

#precipitation
gg <- ggplot(data=subset(df_merge, variable == 'Precipitation'), aes(x=0, y=value, color=legend, fill=legend))
gg <- gg + stat_summary(data = subset(df_merge, variable=='Precipitation'& type == 'boxplot' & legend == 'E3SM'), 
                        fun.data = f, geom="boxplot", width=0.15, position=position_nudge(x=-0.5), lwd=0.2)
gg <- gg + stat_summary(data = subset(df_merge, variable=='Precipitation'& type == 'boxplot' & legend == 'PRISM'), 
                        fun.data = f, geom="boxplot", width=0.15, position=position_nudge(x=-0.3), lwd=0.2)

gg <- gg + geom_violin(data = subset(df_merge, variable=='Precipitation'& type == 'violin' & legend == 'MGP (Emulated)'),
                       position=position_nudge(x=0.2), alpha=0.3, lwd=0.2)
gg <- gg + stat_summary(data = subset(df_merge, variable=='Precipitation'& type == 'violin' & legend == 'MGP (Emulated)'),
                        fun.data = function(x) {data.frame(y = median(x), ymin = quantile(x, 0.25), ymax = quantile(x, 0.75))},
                        geom = "pointrange", position=position_nudge(x=0.2), colour = "red", lwd=0.2, size=0.1)
gg <- gg + geom_violin(data = subset(df_merge, variable=='Precipitation'& type == 'violin'& legend == 'MGP (Bias corrected)'),
                       position=position_nudge(x=0.8), alpha=0.3, lwd=0.2)
gg <- gg + stat_summary(data = subset(df_merge, variable=='Precipitation'& type == 'violin'& legend == 'MGP (Bias corrected)'),
                        fun.data = function(x) {data.frame(y = median(x), ymin = quantile(x, 0.25), ymax = quantile(x, 0.75))},
                        geom = "pointrange", position=position_nudge(x=0.8), colour = "blue", lwd=0.2, size=0.1)
gg <- gg + scale_color_manual(name="", values = c("magenta","black","red","blue"), drop=F)
gg <- gg + scale_fill_manual(name="", values = c("#ff80ff","#808080","#ff8080","#8080ff"), drop=F)
#gg <- gg + scale_y_continuous(labels = label_number(accuracy = 0.1))
gg <- gg + facet_wrap(.~month, nrow=2)
gg <- gg + scale_y_continuous(breaks = seq(0,300,by=50), labels = seq(0,300,by=50))
gg <- gg + coord_cartesian(ylim=c(0, 300))
gg <- gg + theme_bw() + labs(x = "", y = "Precipitation (mm/month)")
gg <- gg + theme(legend.position = c(0.082, 0.93),
                 legend.background = element_rect(fill = 'NA',colour = 'transparent'),
                 panel.background = element_rect(fill = 'white'),
                 legend.box.margin = margin(0, 0, 0, 0), # Remove extra margin around the legend
                 legend.margin = margin(0.01, 0.01, 0.01, 0.01), # Remove margin inside the legend
                 legend.direction = "vertical",
                 legend.key.width = unit(0.4, 'cm'), 
                 axis.ticks = element_blank(),
                 axis.text.x=element_blank(),
                 panel.grid.major.x = element_blank(),
                 panel.grid.minor.x = element_blank(),
                 strip.text.x = element_text(size = 10),
                 strip.text.y = element_text(size = 10),
                 plot.background = element_rect(fill = 'white',colour = 'transparent'))

png(filename=paste0(path.out, 'violin_precip.png'), width = 10, height = 7, units = "in", res=300)
print(gg)
dev.off()



#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################
#######################################################################################






#plot
gg1 <- ggplot(data=subset(df_merge, variable == 'Precipitation'), aes(x=month, y=value, color=legend, fill=legend))
gg1 <- gg1 + stat_summary(data = subset(df_merge, variable=='Precipitation'& type == 'boxplot' & legend == 'E3SM'), 
                        fun.data = f, geom="boxplot", width=0.18, position=position_nudge(x=-0.3), lwd=0.1)
gg1 <- gg1 + stat_summary(data = subset(df_merge, variable=='Precipitation'& type == 'boxplot' & legend == 'PRISM'), 
                        fun.data = f, geom="boxplot", width=0.18, position=position_nudge(x=-0.1), lwd=0.1)

gg1 <- gg1 + geom_violin(data = subset(df_merge, variable=='Precipitation'& type == 'violin' & legend == 'MGP (Emulated)'),
                       position=position_nudge(x=0.1), alpha=0.3, lwd=0.1)
gg1 <- gg1 + stat_summary(data = subset(df_merge, variable=='Precipitation'& type == 'violin' & legend == 'MGP (Emulated)'),
                        fun = "median",  geom = "pointrange", position=position_nudge(x=0.1), colour = "red", lwd=0.1, size=0.1)
gg1 <- gg1 + geom_violin(data = subset(df_merge, variable=='Precipitation'& type == 'violin'& legend == 'MGP (Bias corrected)'),
                       position=position_nudge(x=0.3), alpha=0.3, lwd=0.1)
gg1 <- gg1 + stat_summary(data = subset(df_merge, variable=='Precipitation'& type == 'violin'& legend == 'MGP (Bias corrected)'),
                        fun = "median",  geom = "pointrange", position=position_nudge(x=0.3), colour = "blue", lwd=0.1, size=0.1)
gg1 <- gg1 + scale_color_manual(name="", values = c("magenta","black","red","blue"), drop=F)
gg1 <- gg1 + scale_fill_manual(name="", values = c("#ff80ff","#808080","#ff8080","#8080ff"), drop=F)
gg1 <- gg1 + facet_grid(variable~.)
gg1 <- gg1 + coord_cartesian(ylim=c(0, 250))
gg1 <- gg1 + theme_bw() + labs(x = "", y = "")
gg1 <- gg1 + theme(legend.position = "none",
                 panel.background = element_rect(fill = 'white'),
                 legend.box.margin = margin(0, 0, 0, 0), # Remove extra margin around the legend
                 legend.margin = margin(0, 0, 0, 0), # Remove margin inside the legend
                 legend.direction = "horizontal",
                 strip.text.x = element_text(size = 9),
                 strip.text.y = element_text(size = 9),
                 plot.background = element_rect(fill = 'NA',colour = 'transparent'))


gg2 <-  ggdraw(plot = NULL, xlim = c(0, 8), ylim = c(0, 7), clip = "off")
gg2 <-  gg2 + draw_plot(gg, x = 0, y = 3.5, width = 8, height = 3.5)
gg2 <-  gg2 + draw_plot(gg1, x = 0, y = 0, width = 8, height = 3.5)



png(filename=paste0(path.out, 'plot1.png'), width = 8, height = 7, units = "in", res=300)
print(gg2)
dev.off()



#######################################################
#######################################################
gg <- ggplot(df_merge, aes(x=month, y=value, color=legend, fill=legend, group = interaction(month, legend,type)))
gg <- gg + stat_summary(fun.data = f, geom="boxplot", width=0.6, lwd=0.1, position="dodge")
gg <- gg + geom_point(data = subset(df_merge, type == 'violin'), alpha=0.3, col="grey", size=0.1)
gg <- gg + scale_color_manual(name="", values = c("magenta","black","red","blue"), drop=F)
gg <- gg + scale_fill_manual(name="", values = c("#ff80ff","#808080","#ff8080","#8080ff"), drop=F)
#gg <- gg + scale_y_continuous(labels = label_number(accuracy = 0.1))
gg <- gg + facet_grid(variable ~., scales="free")
gg <- gg + theme_bw() + labs(x = "", y = "")
gg <- gg + theme(legend.position = c(0.5, 0.55),
                 panel.background = element_rect(fill = 'white'),
                 legend.box.margin = margin(0, 0, 0, 0), # Remove extra margin around the legend
                 legend.margin = margin(0, 0, 0, 0), # Remove margin inside the legend
                 legend.direction = "horizontal",
                 strip.text.x = element_text(size = 9),
                 strip.text.y = element_text(size = 9),
                 plot.background = element_rect(fill = 'NA',colour = 'transparent'))


gg







#######################################################
#######################################################
density_data <- df_merge %>%
  filter(variable == 'Temperature') %>%
  group_by(legend, month) %>%
  do({
    # Compute the kernel density estimate for 'value' in each group
    density_values <- density(.$value, na.rm = TRUE)
    data.frame(x = density_values$x, y = density_values$y)
  })

# Calculate the mean of the 'value' for each combination of 'legend' and 'month'
mean_values <- df_merge %>%
  filter(variable == 'Temperature') %>%
  group_by(legend, month) %>%
  summarise(mean_value = median(value, na.rm = TRUE))

# Find the corresponding density y-value for each mean_value by matching it with the density curve
mean_values_with_density <- mean_values %>%
  left_join(density_data, by = c("legend", "month")) %>%
  group_by(legend, month) %>%
  mutate(
    mean_density = y[which.min(abs(x - mean_value))]  # Find the y value for the closest x to mean_value
  )


gg <- ggplot(data = subset(df_merge, variable %in% 'Temperature' & month %in% 'Jan'), aes(x = value, color = legend, fill = legend, group = interaction(month, legend)))
gg <- gg + geom_density(alpha = 0.3)
gg <- gg + geom_segment(data = mean_values_with_density, 
                        aes(x = mean_value, xend = mean_value, 
                            y = 0, yend = mean_density,
                            color= legend),linetype = "dashed", linewidth = 0.1)
gg <- gg + scale_color_manual(name = "", values = c("magenta", "black", "red", "blue"), drop = F)
gg <- gg + scale_fill_manual(name = "", values = c("#ff80ff", "#808080", "#ff8080", "#8080ff"), drop = F)
gg <- gg + facet_grid(variable ~ month, scales = "free")
gg <- gg + coord_cartesian(ylim = c(0, 0.5))
print(gg)



gg <- gg + scale_color_manual(name="", values = c("magenta","black","red","blue"), drop=F)
gg <- gg + scale_fill_manual(name="", values = c("#ff80ff","#808080","#ff8080","#8080ff"), drop=F)
#gg <- gg + scale_y_continuous(labels = label_number(accuracy = 0.1))
gg <- gg + facet_grid(variable ~ month, scales="free")
gg <- gg + coord_cartesian(ylim=c(0,1))

gg <- gg + theme_bw() + labs(x = "", y = "")
gg <- gg + theme(legend.position = c(0.5, 0.55),
                 panel.background = element_rect(fill = 'white'),
                 legend.box.margin = margin(0, 0, 0, 0), # Remove extra margin around the legend
                 legend.margin = margin(0, 0, 0, 0), # Remove margin inside the legend
                 legend.direction = "horizontal",
                 strip.text.x = element_text(size = 9),
                 strip.text.y = element_text(size = 9),
                 plot.background = element_rect(fill = 'NA',colour = 'transparent'))


#######################################################
#######################################################


gg <- ggplot(data = subset(df_merge, legend %in% 'E3SM'), aes(x=month, y=value, color = legend, fill = legend, group = interaction(month, legend)))
gg <- gg + stat_summary(fun.data = f, geom="boxplot", width=0.15, position="dodge", lwd=0.1)

gg <- gg + scale_color_manual(name="", values = c("magenta","black","red","blue"), drop=F)
gg <- gg + scale_fill_manual(name="", values = c("#ff80ff","#808080","#ff8080","#8080ff"), drop=F)
gg <- gg + facet_grid(variable ~., scales="free")
gg <- gg + theme_bw() + labs(x = "", y = "")
gg <- gg + theme(legend.position = c(0.5, 0.55),
                 panel.background = element_rect(fill = 'white'),
                 legend.box.margin = margin(0, 0, 0, 0), # Remove extra margin around the legend
                 legend.margin = margin(0, 0, 0, 0), # Remove margin inside the legend
                 legend.direction = "horizontal",
                 strip.text.x = element_text(size = 9),
                 strip.text.y = element_text(size = 9),
                 plot.background = element_rect(fill = 'NA',colour = 'transparent'))

png(filename=paste0(path.out, 'boxplot_ppt1.png'), width = 7, height = 4.5, units = "in", res=200)
print(gg)
dev.off()

gg <- ggplot(data = subset(df_merge, legend %in% c('E3SM', 'PRISM')), aes(x=month, y=value, color = legend, fill = legend, group = interaction(month, legend)))
gg <- gg + stat_summary(fun.data = f, geom="boxplot", width=0.3, position="dodge", lwd=0.1)

gg <- gg + scale_color_manual(name="", values = c("magenta","black","red","blue"), drop=F)
gg <- gg + scale_fill_manual(name="", values = c("#ff80ff","#808080","#ff8080","#8080ff"), drop=F)
gg <- gg + facet_grid(variable ~., scales="free")
gg <- gg + theme_bw() + labs(x = "", y = "")
gg <- gg + theme(legend.position = c(0.5, 0.55),
                 panel.background = element_rect(fill = 'white'),
                 legend.box.margin = margin(0, 0, 0, 0), # Remove extra margin around the legend
                 legend.margin = margin(0, 0, 0, 0), # Remove margin inside the legend
                 legend.direction = "horizontal",
                 strip.text.x = element_text(size = 9),
                 strip.text.y = element_text(size = 9),
                 plot.background = element_rect(fill = 'NA',colour = 'transparent'))

png(filename=paste0(path.out, 'boxplot_ppt2.png'), width = 7, height = 4.5, units = "in", res=200)
print(gg)
dev.off()

gg <- ggplot(data = subset(df_merge, legend %in% c('E3SM','PRISM', 'MGP (Emulated)')), aes(x=month, y=value, color = legend, fill = legend, group = interaction(month, legend)))
gg <- gg + stat_summary(fun.data = f, geom="boxplot", width=0.45, position="dodge", lwd=0.1)

gg <- gg + scale_color_manual(name="", values = c("magenta","black","red","blue"), drop=F)
gg <- gg + scale_fill_manual(name="", values = c("#ff80ff","#808080","#ff8080","#8080ff"), drop=F)
gg <- gg + facet_grid(variable ~., scales="free")
gg <- gg + theme_bw() + labs(x = "", y = "")
gg <- gg + theme(legend.position = c(0.5, 0.55),
                 panel.background = element_rect(fill = 'white'),
                 legend.box.margin = margin(0, 0, 0, 0), # Remove extra margin around the legend
                 legend.margin = margin(0, 0, 0, 0), # Remove margin inside the legend
                 legend.direction = "horizontal",
                 strip.text.x = element_text(size = 9),
                 strip.text.y = element_text(size = 9),
                 plot.background = element_rect(fill = 'NA',colour = 'transparent'))

png(filename=paste0(path.out, 'boxplot_ppt3.png'), width = 7, height = 4.5, units = "in", res=200)
print(gg)
dev.off()



#}