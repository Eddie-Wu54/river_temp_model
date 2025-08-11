
{
  library(gridExtra)
  library(ggplot2)
  library(ggrepel)
  library(patchwork)
  library(maps)
  library(dplyr)
  library(tidyr)
}




#### Performance metrics inputs and cleaning ####
rmse.r <- read.csv("RMSE for plot.csv")[,-1]
bias.r <- read.csv("BIAS for plot.csv")[,-1]
nsc.r <- read.csv("NSC for plot.csv")[,-1]
aic.r <- read.csv("AIC for plot.csv")[,-1]
triloc <- read.csv("tributary locations.csv")



## Cleaning
# --- RMSE ---
rmse.piv <- rmse.r %>%
  dplyr::select(-test_year) %>% 
  pivot_longer(cols=-c(location,fold), names_to="spec", values_to="value")
rmse.piv$spec <- factor(rmse.piv$spec,
                        c("linear","nonlinear","seasonal","air2stream","futureS"))

rmse.piv <- rmse.piv %>% 
  group_by(location, spec) %>% 
  summarize(value_avg = mean(value, na.rm = TRUE))
rmse.piv <- inner_join(rmse.piv, triloc[,c(2,9)], by="location")
rmse.piv$cat <- as.factor(rmse.piv$cat)


# --- Bias ---
bias.piv <- bias.r %>% 
  dplyr::select(-test_year) %>% 
  pivot_longer(cols=-c(location,fold), names_to="spec", values_to="value")
bias.piv$spec <- factor(bias.piv$spec,
                        c("linear","nonlinear","seasonal","air2stream","futureS"))

bias.piv <- bias.piv %>% 
  group_by(location, spec) %>% 
  summarize(value_avg = mean(value, na.rm = TRUE))
bias.piv <- inner_join(bias.piv, triloc[,c(2,9)], by="location")
bias.piv$cat <- as.factor(bias.piv$cat)


# --- NSC ---
nsc.piv <- nsc.r %>% 
  dplyr::select(-test_year) %>% 
  pivot_longer(cols=-c(location,fold), names_to="spec", values_to="value")
nsc.piv$spec <- factor(nsc.piv$spec,
                       c("linear","nonlinear","seasonal","air2stream","futureS"))

nsc.piv <- nsc.piv %>% 
  group_by(location, spec) %>% 
  summarize(value_avg = mean(value, na.rm = TRUE))
nsc.piv <- merge(nsc.piv, triloc[,c(2,9)], by="location")
nsc.piv$cat <- as.factor(nsc.piv$cat)


# --- AIC ---
aic.piv <- aic.r %>%
  dplyr::select(-test_year) %>% 
  pivot_longer(cols=-c(location,fold), names_to="spec", values_to="value")
aic.piv$spec <- factor(aic.piv$spec,
                       c("linear","nonlinear","seasonal","air2stream","futureS"))

aic.piv <- aic.piv %>% 
  group_by(location, spec) %>% 
  summarize(value_avg = mean(value, na.rm = TRUE))
aic.piv <- inner_join(aic.piv, triloc[,c(2,9)], by="location")
aic.piv$cat <- as.factor(aic.piv$cat)




#### Plot 1: Great Lakes tributary map ####
triloc <- read.csv("tributary locations.csv")
triloc$tributary.name <- gsub(paste0("\\b", "River", "\\b"), "", triloc$tributary.name)
triloc$tributary.name <- trimws(triloc$tributary.name)


## Specify text locations
nudge.x = c(0.2,-0.5,-0.2,0,1.2,-0.8,0.2,-1,0.2,-0.1)
nudge.y = c(-0.4,-0.4,-0.4,0.4,0,0,-0.4,0.1,-0.4,0.4)


## Get Great Lakes map and world map
great_lakes <- map_data("lakes") %>% subset(region == "Great Lakes")
world <- map_data("world")


## Big Plot
png("figure 1_tributary location.png", width= 2400, height= 1600, units="px", res = 300)

loc_color <- c(large = "#E69F00", small = "#56B4E9")

gl_map <- ggplot()+
  geom_polygon(data=great_lakes, aes(x=long, y=lat, group=group), fill="grey", color="black")+
  geom_point(aes(x=longitude, y=latitude, color=cat), data=triloc, size=4)+
  geom_text(aes(x=longitude, y=latitude, label = tributary.name), data=triloc,
            nudge_x = nudge.x, nudge_y = nudge.y, size=4, color="black")+
  coord_fixed(1.3) +  # Fix aspect ratio
  ylim(41,49.5)+
  theme_bw()+
  labs(x="longitude", y="latitude")+
  scale_color_manual(name="", values=loc_color,
                     labels=c("large","small"))+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.justification=c(0,0), legend.position=c(0.05,0.1),
        legend.text = element_text(size = 14),
        legend.background = element_blank(),  #make background transparent
        plot.margin = margin(b=12,t=5,l=5,r=5))

gl_map


## Inset map
world_map <- ggplot(world, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group), fill = "grey", color = "white", size=0.1) +
  geom_rect(aes(xmin = -92, xmax = -75, ymin = 41.5, ymax = 48), 
            fill = NA, color = "red", size = 0.5) +  # Highlight region
  coord_cartesian(xlim = c(-170, 180), ylim = c(-60, 90), expand = F) +
  theme_void() +
  theme(panel.border = element_rect(color="black", fill=NA))

world_map


## Combine
final_plot <- gl_map + 
  inset_element(world_map, left = 0.65, bottom = 0.7, right = 0.98, top = 0.95)


final_plot


dev.off()




#### Plot 2: Seasonal residual model representation ####


## Creating dataset first
julian_day1 <- seq(92, 203, length.out = 100)  # Julian days
julian_day2 <- seq(120, 203, length.out = 100)  # Second curve: 120-203

mean_temp <- 8  # Mean river water temperature (°C)
max_temp <- 15  # Seasonal fluctuation amplitude


temperature1 <- mean_temp+max_temp*sin(2*pi/365*(julian_day1-120))
temperature2 <- mean_temp+max_temp*sin(2*pi/365*(julian_day2-150))

data1 <- data.frame(Julian_Day = julian_day1, Temperature = temperature1, Curve = "Early Start")
data2 <- data.frame(Julian_Day = julian_day2, Temperature = temperature2, Curve = "Late Start")

data <- rbind(data1, data2)


## Plotting starts here
png("figure 2_sin surve representation.png", width= 1800, height= 1200, units="px", res = 300)


ggplot(data, aes(x = Julian_Day, y = Temperature, color = Curve)) +
  geom_line(size = 1) +
  labs(x = "julian day",y = "river water temperature (°C)") +
  theme_classic() +
  scale_color_manual(name="", values = c("Early Start"="#ff7f00", "Late Start"="#377eb8")) +
  scale_x_continuous(limits = c(90,210), breaks = seq(90,210,30))+
  scale_y_continuous(limits = c(0,25), breaks = seq(0,25,5))+
  theme(
    panel.grid.major = element_line(color = "gray", linetype = "dashed"), #gridded background
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    legend.justification=c(0,0), legend.position=c(0.7,0.1),
    legend.text = element_text(size = 14),
    legend.background = element_blank(),  #make background transparent
    plot.margin = margin(l=10,b=12,r=10,t=10),
  )


dev.off()




#### Plot 3: RMSE plot ####

modlab <- c("linear","nonlinear","seasonal","air2stream","futureStreams")


## Plot
png("figure 3_rmse out.png", width= 2200, height= 1400, units="px", res = 300)


plot.rmse <- ggplot(aes(x=spec, y=value_avg), data=rmse.piv)+
  stat_summary(fun.y=mean, geom="crossbar", width=0.35, size=0.3, color="#CC0000")+
  stat_summary(fun.data=mean_sdl, geom="errorbar", fun.args=list(mult=1), width=0.2, size=0.6, color="#CC0000", alpha=0.6)+
  geom_jitter(color="black", size=2.5, alpha=0.5, width=0.08,
              data=rmse.piv[rmse.piv$location!="allegheny",])+
  geom_point(color="#E69F00", size=2.5, alpha=1, shape=17,#hightlight the Allegheny point
             data=rmse.piv[rmse.piv$location=="allegheny",])+
  theme_classic()+
  labs(x = "model", y = "test RMSE (°C)")+
  scale_x_discrete(labels = modlab)+ #x-axis labelhttp://127.0.0.1:46337/graphics/plot_zoom_png?width=370&height=369
  scale_y_continuous(limits = c(0.5,8.5), breaks = seq(1,8,1))+
  #annotate("text", x=c(1,2,3), y=c(3,3.8,3), label="Allegheny", size=2)+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        plot.margin = margin(l=10,b=12,r=10,t=10)) # make background transparent

plot.rmse


dev.off()




#### Plot 4: bias out ####

## Plot
png("figure 4_bias out.png", width= 2200, height= 1400, units="px", res = 300)


plot.bias <- ggplot(aes(x=spec, y=value_avg), data=bias.piv)+
  stat_summary(fun.y=mean, geom="crossbar", width=0.35, size=0.3, color="#56B4E9")+
  stat_summary(fun.data=mean_sdl, geom="errorbar", fun.args=list(mult=1), width=0.2, size=0.6, color="#56B4E9", alpha=0.6)+
  geom_jitter(color="black", size=2.5, alpha=0.5, width=0.08)+
  theme_classic()+
  labs(x = "model", y = "test bias (°C)")+
  scale_x_discrete(labels = modlab)+ #x-axis label
  scale_y_continuous(limits = c(-6.5,7), breaks = seq(-7,7,1))+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        plot.margin = margin(l=10,b=12,r=10,t=10)) # make background transparent

plot.bias


dev.off()




#### Plot 5: AIC out ####

## Plot
png("figure 5_aic out.png", width= 2200, height= 1400, units="px", res = 300)


plot.aic <- ggplot(aes(x=spec, y=value_avg), data=aic.piv)+
  stat_summary(fun.y=mean, geom="crossbar", width=0.35, size=0.3, color="#E69F00")+
  stat_summary(fun.data=mean_sdl, geom="errorbar", fun.args=list(mult=1), width=0.2, size=0.6, color="#E69F00", alpha=0.6)+
  geom_jitter(color="black", size=2.5, alpha=0.5, width=0.08)+
  theme_classic()+
  labs(x = "model", y = "test AIC")+
  scale_x_discrete(labels = modlab)+ #x-axis label
  scale_y_continuous(limits = c(0,65), breaks = seq(0,60,10))+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        plot.margin = margin(l=10,b=12,r=10,t=10)) # make background transparent

plot.aic


dev.off()



      
#### Plot 6: NSC out (GAPPED) ####


## Plot
png("figure 6_nsc out.png", width= 2200, height= 1800, units="px", res = 300)


plot.nsc <- ggplot(aes(x=spec, y=value_avg), data=nsc.piv)+
  stat_summary(fun.y=mean, geom="crossbar", width=0.35, size=0.3, color="#009E73")+
  stat_summary(fun.data=mean_sdl, geom="errorbar", fun.args=list(mult=1), width=0.2, size=0.6, color="#009E73", alpha=0.6)+
  geom_jitter(color="black", size=2.5, alpha=0.5, width=0.08)+
  theme_classic()+
  labs(x = "model", y = "test NSC (°C)")+
  scale_x_discrete(labels = modlab)+ #x-axis label
  coord_cartesian(ylim = c(-2.5,1))+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.line = element_blank(), #remove axis lines
        plot.margin = margin(l=10,b=12,r=10,t=10)) # make background transparent


plot.nsc


## Breaks
gg.gap(plot = plot.nsc,
       segments = c(-1.2,-0.75),
       ylim = c(-2.7,1.25),
       tick_width = c(0.5,0.5),
       rel_heights = c(0.25,0,0.75),
       margin = c(0,0,0,0.5),
       segment_color = NA)


dev.off()




#### Plot 7: Example plot for local and global temp ####

#' Data used to plot this example plot:
#' Genesee River: year 2017
#' Allegheny River: year 2015


## Legends
color_types <- c("nonlinear"="orange", "futureStreams"="#009E73", "data"="black")
line_types <- c("nonlinear"=1, "futureStreams"=1, "data"=2)


## Get data
plot.df <- read.csv("data for example plot.csv")
plot.df1 <- plot.df[plot.df$location == "stjoseph",]
plot.df2 <- plot.df[plot.df$location == "allegheny",]


png("figure 7_example compare.png", width= 3200, height= 1500, units="px", res = 300)


## Plot 1
joseph <- ggplot(data=plot.df1, aes(x=week))+
  geom_line(aes(x=week, y=obs, color = "data", linetype="data"),linewidth=0.8)+
  geom_line(aes(x=week, y=preds, color = "nonlinear", linetype="nonlinear"),linewidth=0.8)+
  geom_line(aes(x=week, y=preds.futureS, color = "futureStreams", linetype="futureStreams"),linewidth=0.8)+
  ggtitle("(A) St.Joseph River")+
  labs(x = "weeks", y = "water temperature (°C)")+
  scale_x_continuous(breaks=seq(14,29,3))+
  scale_y_continuous(breaks=seq(0,30,5))+
  theme_bw()+
  scale_color_manual(name="", values=color_types,
                     labels=c("data","futureStreams","nonlinear"))+
  scale_linetype_manual(name="", values=line_types,
                        labels=c("data","futureStreams","nonlinear"))+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.justification=c(0,0), legend.position=c(0.58,0.08),
        legend.text = element_text(size = 14),
        legend.background = element_blank(),
        plot.margin = margin(l=10,b=12,r=10,t=10))
 # make background transparent


## Plot 2
allg <- ggplot(data=plot.df2, aes(x=week))+
  geom_line(aes(x=week, y=obs, color = "data", linetype="data"),linewidth=0.8)+
  geom_line(aes(x=week, y=preds, color = "nonlinear", linetype="nonlinear"),linewidth=0.8)+
  geom_line(aes(x=week, y=preds.futureS, color = "futureStreams", linetype="futureStreams"),linewidth=0.8)+
  ggtitle("(B) Allegheny River")+
  labs(x = "weeks", y = "water temperature (°C)")+
  scale_x_continuous(breaks=seq(14,29,3))+
  scale_y_continuous(breaks=seq(0,20,5))+
  theme_bw()+
  scale_color_manual(name="", values=color_types,
                     labels=c("data","futureStreams","nonlinear"))+
  scale_linetype_manual(name="", values=line_types,
                        labels=c("data","futureStreams","nonlinear"))+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none",
        plot.margin = margin(l=10,b=12,r=10,t=10)) # make background transparent


grid.arrange(joseph, allg, ncol=2)


dev.off()





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




#### Plot DEMO for Kim: Example plot for Salmontrout and Humber River ####

#' Data used to plot this example plot:
#' Salmontrout River: year 2018
#' Humber River: year 


## Legends
color_types <- c("nonlinear"="red", "data"="black")
line_types <- c("nonlinear"=1, "data"=2)


## Get data
plot.df <- read.csv("data for example plot for kim.csv")
plot.df1 <- plot.df[plot.df$location == "salmontrout",]
plot.df2 <- plot.df[plot.df$location == "humber",]


png("figure 5_example compare for kim.png", width= 3200, height= 1500, units="px", res = 300)


## Plot 1
salmontrout <- ggplot(data=plot.df1, aes(x=week))+
  geom_line(aes(x=week, y=mean_water, color = "data", linetype = "data"),linewidth=0.8)+
  geom_line(aes(x=week, y=preds.new, color = "nonlinear", linetype = "nonlinear"),linewidth=0.8)+
  ggtitle("(A) Salmontrout River")+
  labs(x = "weeks", y = "water temperature (°C)")+
  scale_x_continuous(breaks=seq(18,29,3))+
  scale_y_continuous(breaks=seq(0,15,5))+
  theme_bw()+
  scale_color_manual(name="", values=color_types,
                     labels=c("data","nonlinear"))+
  scale_linetype_manual(name="", values=line_types,
                        labels=c("data","nonlinear"))+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.justification=c(0,0), legend.position=c(0.68,0.08),
        legend.text = element_text(size = 14),
        legend.background = element_blank(),
        plot.margin = margin(l=10,b=12,r=10,t=10)) # make background transparent


## Plot 2
humber <- ggplot(data=plot.df2, aes(x=week))+
  geom_line(aes(x=week, y=mean_water, color = "data", linetype = "data"),linewidth=0.8)+
  geom_line(aes(x=week, y=preds.new, color = "nonlinear", linetype = "nonlinear"),linewidth=0.8)+
  ggtitle("(B) Humber River")+
  labs(x = "weeks", y = "water temperature (°C)")+
  scale_x_continuous(breaks=seq(14,29,3))+
  scale_y_continuous(breaks=seq(0,25,5))+
  theme_bw()+
  scale_color_manual(name="", values=color_types,
                     labels=c("data","nonlinear"))+
  scale_linetype_manual(name="", values=line_types,
                        labels=c("data","nonlinear"))+
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none",
        plot.margin = margin(l=10,b=12,r=10,t=10)) # make background transparent


grid.arrange(salmontrout, humber, ncol=2)


dev.off()
