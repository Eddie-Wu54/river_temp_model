
{
  library(gridExtra)
  library(ggplot2)
  library(ggrepel)
  library(maps)
  library(dplyr)
  library(tidyr)
}




#### Performance metrics inputs and cleaning ####
rmse <- read.csv("RMSE for plot.csv")
bias <- read.csv("bias for plot.csv")
nsc <- read.csv("NSC for plot.csv")

names(rmse) = c("location", "linear", "seasonal", "nonlinear", "futureStreams")
names(bias) = c("location", "linear", "seasonal", "nonlinear", "futureStreams")
names(nsc) = c("location", "linear", "seasonal", "nonlinear", "futureStreams")


## Cleaning
rmse.piv <- rmse %>% pivot_longer(cols = -c(location), names_to = "spec", values_to = "value")
rmse.piv$spec <- factor(rmse.piv$spec, c("linear","seasonal","nonlinear","futureStreams"))

bias.piv <- bias %>% pivot_longer(cols = -c(location), names_to = "spec", values_to = "value")
bias.piv$spec <- factor(bias.piv$spec, c("linear","seasonal","nonlinear","futureStreams"))

nsc.piv <- nsc %>% pivot_longer(cols = -c(location), names_to = "spec", values_to = "value")
nsc.piv$spec <- factor(nsc.piv$spec, c("linear","seasonal","nonlinear","futureStreams"))




#### Plot 1: Great Lakes tributary map ####
triloc <- read.csv("tributary locations.csv")
triloc <- triloc[-c(13,14),]
triloc$tributary.name <- gsub(paste0("\\b", "River", "\\b"), "", triloc$tributary.name)
triloc$tributary.name <- trimws(triloc$tributary.name)


## Specify text locations
nudge.x = c(0.2,-0.4,-0.2,0,0,0.2,0.4,-1.3,0.3,0.5,0,-0.4)
nudge.y = c(-0.3,-0.3,-0.3,-0.3,-0.3,-0.3,0.4,0.1,0.4,0.4,0.4,0.4)


## Get Great Lakes map
great_lakes <- map_data("lakes") %>% subset(region == "Great Lakes")


## Plot
png("figure 1_tributary location.png", width= 2600, height= 1800, units="px", res = 300)


loc_color <- c(northern = "#E69F00", southern = "#56B4E9")

ggplot()+
  geom_polygon(data=great_lakes, aes(x=long, y=lat, group=group), fill="grey", color="black")+
  geom_point(aes(x=longitude, y=latitude, color=loc), data=triloc, size=4)+
  geom_text(aes(x=longitude, y=latitude, label = tributary.name), data=triloc,
            nudge_x = nudge.x, nudge_y = nudge.y, size=4, color="black")+
  coord_fixed(1.3) +  # Fix aspect ratio
  ylim(41,50)+
  theme_bw()+
  labs(x="longitude", y="latitude")+
  scale_color_manual(name="", values=loc_color,
                     labels=c("northern","southern"))+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.justification=c(0,0), legend.position=c(0.8,0.8),
        legend.text = element_text(size = 14),
        legend.background = element_blank(),  #make background transparent
        plot.margin = margin(b=12,l=5,t=5))


dev.off()




#### Plot 2: RMSE plot ####

## Small tributaries
small <- c("bigotter","bigcreek","vermilion","humber")
big <- c("nipigon","mississagi","still","stlouis","fox","saginaw","genesee","portage")

modlab <- c("linear","seasonal","nonlinear","futureStreams")
bcontrol <- c("seasonal","futureStreams")


## Plot
png("figure 2_rmse out.png", width= 3200, height= 1600, units="px", res = 300)

smallrmse <- ggplot(aes(x=spec, y=value), data=rmse.piv[rmse.piv$location %in% small,])+
  stat_summary(fun.y=mean, geom="crossbar", width=0.35, size=0.3, color="red")+
  stat_summary(fun.data=mean_sdl, geom="errorbar", fun.args=list(mult=1), width=0.2, size=0.6, color="red", alpha=0.4)+
  geom_jitter(color="black", size=2.5, alpha=0.7, width=0.05)+
  ggtitle(paste("(A)", "Small Rivers"))+
  theme_classic()+
  labs(x = "model", y = "test RMSE (°C)")+
  scale_x_discrete(labels = modlab)+ #x-axis label
  scale_y_continuous(limits = c(0.5,6.5), breaks = seq(1,6,1))+
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        plot.margin = margin(l=10,b=12,r=10,t=10)) # make background transparent

bigrmse <- ggplot(aes(x=spec, y=value), data=rmse.piv[(rmse.piv$location %in% big) & (rmse.piv$spec %in% bcontrol),])+
  stat_summary(fun.y=mean, geom="crossbar", width=0.35, size=0.3, color="red")+
  stat_summary(fun.data=mean_sdl, geom="errorbar", fun.args=list(mult=1), width=0.2, size=0.6, color="red", alpha=0.4)+
  geom_jitter(color="black", size=2.5, alpha=0.7, width=0.05)+
  ggtitle(paste("(B)", "Large Rivers"))+
  theme_classic()+
  labs(x = "model", y="")+
  scale_x_discrete(labels = c("seasonal","futureStreams"))+ #x-axis label
  scale_y_continuous(limits = c(0.5,6.5), breaks = seq(1,6,1))+
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        plot.margin = margin(b=12,r=10,t=10)) # make background transparent


grid.arrange(smallrmse, bigrmse, ncol=2)

dev.off()




#### Plot 3: bias out ####

## Plot
png("figure 3_bias out.png", width= 3200, height= 1600, units="px", res = 300)

smallbias <- ggplot(aes(x=spec, y=value), data=bias.piv[bias.piv$location %in% small,])+
  stat_summary(fun.y=mean, geom="crossbar", width=0.35, size=0.3, color="#E69F00")+
  stat_summary(fun.data=mean_sdl, geom="errorbar", fun.args=list(mult=1), width=0.2, size=0.6, color="#E69F00", alpha=0.4)+
  geom_jitter(color="black", size=2.5, alpha=0.7, width=0.05)+
  ggtitle(paste("(A)", "Small Rivers"))+
  theme_classic()+
  labs(x = "model", y = "test bias (°C)")+
  scale_x_discrete(labels = modlab)+ #x-axis label
  scale_y_continuous(limits = c(-5,3), breaks = seq(-5,3,1))+
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        plot.margin = margin(l=10,b=12,r=10,t=10)) # make background transparent

bigbias <- ggplot(aes(x=spec, y=value), data=bias.piv[(bias.piv$location %in% big) & (rmse.piv$spec %in% bcontrol),])+
  stat_summary(fun.y=mean, geom="crossbar", width=0.35, size=0.3, color="#E69F00")+
  stat_summary(fun.data=mean_sdl, geom="errorbar", fun.args=list(mult=1), width=0.2, size=0.6, color="#E69F00", alpha=0.4)+
  geom_jitter(color="black", size=2.5, alpha=0.7, width=0.05)+
  ggtitle(paste("(B)", "Large Rivers"))+
  theme_classic()+
  labs(x = "model", y="")+
  scale_x_discrete(labels = c("seasonal","futureStreams"))+ #x-axis label
  scale_y_continuous(limits = c(-5,3), breaks = seq(-5,3,1))+
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        plot.margin = margin(b=12,r=10,t=10)) # make background transparent


grid.arrange(smallbias, bigbias, ncol=2)

dev.off()




#### Plot 4: nsc out ####

## Plot
png("figure 4_nsc out.png", width= 3200, height= 1600, units="px", res = 300)

smallnsc <- ggplot(aes(x=spec, y=value), data=nsc.piv[nsc.piv$location %in% small,])+
  stat_summary(fun.y=mean, geom="crossbar", width=0.35, size=0.3, color="#009E73")+
  stat_summary(fun.data=mean_sdl, geom="errorbar", fun.args=list(mult=1), width=0.2, size=0.6, color="#009E73", alpha=0.4)+
  geom_jitter(color="black", size=2.5, alpha=0.7, width=0.05)+
  ggtitle(paste("(A)", "Small Rivers"))+
  theme_classic()+
  labs(x = "model", y = "test NSC (°C)")+
  scale_x_discrete(labels = modlab)+ #x-axis label
  coord_cartesian(ylim = c(-0.25, 1))+ #zoom in the y-axis
  scale_y_continuous(breaks = seq(-0.2,1,0.2))+
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        plot.margin = margin(l=10,b=12,r=10,t=10)) # make background transparent

bignsc <- ggplot(aes(x=spec, y=value), data=nsc.piv[(nsc.piv$location %in% big) & (rmse.piv$spec %in% bcontrol),])+
  stat_summary(fun.y=mean, geom="crossbar", width=0.35, size=0.3, color="#009E73")+
  stat_summary(fun.data=mean_sdl, geom="errorbar", fun.args=list(mult=1), width=0.2, size=0.6, color="#009E73", alpha=0.4)+
  geom_jitter(color="black", size=2.5, alpha=0.7, width=0.05)+
  ggtitle(paste("(B)", "Large Rivers"))+
  theme_classic()+
  labs(x = "model", y="")+
  scale_x_discrete(labels = c("seasonal","futureStreams"))+ #x-axis label
  scale_y_continuous(limits = c(-0.2,1), breaks = seq(-0.2,1,0.2))+
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        plot.margin = margin(b=12,r=10,t=10)) # make background transparent


grid.arrange(smallnsc, bignsc, ncol=2)

dev.off()





#### Plot 5: Example plot for regional and global temp ####

#' Data used to plot this example plot:
#' Bigcreek: year 2004; Mississagi: year 2011

png("figure 5_example compare.png", width= 3200, height= 1500, units="px", res = 300)


## Legends
color_types <- c("seasonal"="orange", "futureStreams"="#009E73", "empirical"="black")


## Get data
plot.df <- read.csv("data for example plot.csv")
plot.df1 <- plot.df[plot.df$location == "vermilion",]
plot.df2 <- plot.df[plot.df$location == "stlouis",]


## Plots
ver <- ggplot(data=plot.df1, aes(x=week))+
  geom_line(aes(x=week, y=water, color = "empirical"),linewidth=0.8)+
  geom_line(aes(x=week, y=preds.ar, color = "seasonal"),linewidth=0.8)+
  geom_line(aes(x=week, y=preds.futureS, color = "futureStreams"),linewidth=0.8)+
  ggtitle("(A) Vermilion River")+
  labs(x = "weeks", y = "water temperature (°C)")+
  scale_x_continuous(breaks=seq(14,29,3))+
  theme_bw()+
  scale_color_manual(name="", values=color_types,
                     labels=c("empirical","futureStreams","seasonal"))+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.justification=c(0,0), legend.position=c(0.58,0.08),
        legend.text = element_text(size = 14),
        legend.background = element_blank(),
        plot.margin = margin(l=10,b=12,r=10,t=10)) # make background transparent

ver

stl <- ggplot(data=plot.df2, aes(x=week))+
  geom_line(aes(x=week, y=water, color = "empirical"),linewidth=0.8)+
  geom_line(aes(x=week, y=preds.ar, color = "seasonal"),linewidth=0.8)+
  geom_line(aes(x=week, y=preds.futureS, color = "futureStreams"),linewidth=0.8)+
  ggtitle("(B) St.Louis River")+
  labs(x = "weeks", y = "")+
  scale_x_continuous(breaks=seq(14,29,3))+
  theme_bw()+
  scale_color_manual(name="", values=color_types,
                     labels=c("empirical","futureStreams","seasonal"))+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none",
        plot.margin = margin(l=10,b=12,r=10,t=10)) # http://127.0.0.1:9891/graphics/f3abbc77-4fff-4ca6-937a-adc8d72c342a.pngnd transparent

stl

grid.arrange(ver, stl, ncol=2)

dev.off()


## RMSE calculation
diff1.r <- round(sqrt(mean((plot.df1$preds.ar-plot.df1$water)^2)),2)
diff1.g <- round(sqrt(mean((plot.df1$preds.futureS-plot.df1$water)^2)),2)

diff2.r <- round(sqrt(mean((plot.df2$preds.ar-plot.df2$water)^2)),2)
diff2.g <- round(sqrt(mean((plot.df2$preds.futureS-plot.df2$water)^2)),2)

diff1.r
diff1.g

diff2.r
diff2.g




#### Plot X ####

## Plot
png("figure 2_small out.png", width= 3200, height= 2800, units="px", res = 300)

smallrmse <- ggplot(aes(x=spec, y=value), data=rmse.piv[rmse.piv$location %in% small,])+
  geom_boxplot(width=0.3, outlier.shape = NA)+
  geom_jitter(color="black", size=2, alpha=0.5, width=0.05)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red")+
  ggtitle(paste("(A)", "RMSE"))+
  theme_classic()+
  labs(x = "model", y = "test RMSE (°C)")+
  scale_x_discrete(labels = modlab)+ #x-axis label
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        plot.margin = margin(b=12,r=10,t=10)) # make background transparent


smallbias <- ggplot(aes(x=spec, y=value), data=bias.piv[bias.piv$location %in% small,])+
  geom_boxplot(width=0.3, outlier.shape = NA)+
  geom_jitter(color="black", size=2, alpha=0.5, width=0.05)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red")+
  ggtitle(paste("(B)", "bias"))+
  theme_classic()+
  labs(x = "model", y = "test bias (°C)")+
  scale_x_discrete(labels = modlab)+ #x-axis label
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        plot.margin = margin(b=12,r=10,t=10)) # make background transparent


smallnsc <- ggplot(aes(x=spec, y=value), data=nsc.piv[nsc.piv$location %in% small,])+
  geom_boxplot(width=0.3, outlier.shape = NA)+
  geom_jitter(color="black", size=2, alpha=0.5, width=0.05)+
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="red", fill="red")+
  ggtitle(paste("(C)", "NSC"))+
  theme_classic()+
  labs(x = "model", y = "test NSC")+
  scale_x_discrete(labels = modlab)+ #x-axis label
  theme(axis.title = element_text(size = 18),
        axis.text = element_text(size = 14),
        plot.margin = margin(b=12,r=10,t=10)) # make background transparent


grid.arrange(smallrmse, smallbias, smallnsc, ncol=2, nrow=2)

dev.off()