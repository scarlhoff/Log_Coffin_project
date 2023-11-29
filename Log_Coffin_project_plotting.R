# Genomic portrait and relatedness patterns of the Iron Age Log Coffin culture in northwestern Thailand
# Figure plotting in R

## by: Selina Carlhoff

## load required libraries
library(ggplot2)
library(tidyverse)
library(ggmap)

## Figure 1

### load Data from Source Data

### create map of Thailand
coordinates =  c(97,5.5,106,20.5)
map <- get_stadiamap(coordinates, maptype = "stamen_terrain_background", crop = TRUE, zoom = 7)
ancientThailand <- ggmap(map) + geom_point(data = subset(Data,ancientpop=="Ban Chiang"), aes(x = Longitude, y = Latitude), colour = c("black"), size = 3, shape = 21, fill = c("orange")) + 
  geom_point(data = Data, aes(x = Longitude, y = Latitude), colour = c("black"), size = 0.5, shape = 22, fill = c("grey")) +
  geom_text(data = subset(Data,ancientpop=="Ban Chiang"), aes(x = Longitude, y = Latitude, label = ancientpop), nudge_y = -0.4) + 
  geom_rect(xmin = 98.1, xmax = 98.4, ymin= 19.5, ymax = 19.65, color = "black", alpha = 0, size =0.4) +
  theme_bw() + labs(x="Longitude", y="Latitude") + theme(axis.text = element_text(size = 12, colour = "black"), axis.title = element_text(size=12))
ancientThailand

### create map of northwestern Thailand
zoom_coordinates = c(98.1,19.5,98.4,19.65)
zoom <- get_stadiamap(zoom_coordinates, maptype = "stamen_terrain_background", crop = TRUE, zoom = 13)
ancientMapha <- ggmap(zoom) + geom_point(data = Data, aes(x = Longitude, y = Latitude), colour = c("black"), size = 4, shape = 22, fill = c("grey")) + 
  geom_point(data = Data, aes(x = Longitude, y = Latitude), colour = c("black"), size = 4, shape = 25, fill = c("orange")) + 
  geom_text(data = subset(Data,ancientpop=="Yappa Nhae 1"|ancientpop=="Long Long Rak"), aes(x = Longitude, y = Latitude, label = ancientpop), size = 4, nudge_x = -0.03, color ="white") +
  geom_text(data = subset(Data,ancientpop=="Yappa Nhae 2"|ancientpop=="Tham Lod cave"), aes(x = Longitude, y = Latitude, label = ancientpop), size = 4, nudge_x = 0.03, color ="white") +
  geom_text(data = subset(Data,ancientpop=="Lahu Pot"),aes(x = Longitude, y = Latitude, label = ancientpop), size = 4, nudge_x = 0.02, color ="white") +
  geom_text(data = subset(Data,ancientpop=="Ban Rai"),aes(x = Longitude, y = Latitude, label = ancientpop), size = 4, nudge_y = -0.008, color ="white") +
  theme_bw() + labs(x="Longitude", y="Latitude") + theme(axis.text = element_text(size = 12, colour = "black"), axis.title = element_text(size=12))
ancientMapha

## Figure 2c: IBD network

### load IBD from Supplementary Data 4

### add site identifiers
IBD$Site1 <- IBD$Individual.1 %>% substr(start = 1, stop = 3)
IBD$Site2 <- IBD$Individual.2 %>% substr(start = 1, stop = 3)

### remove 0 length blocks
IBD <- IBD %>% filter(IBD$Total.length.of.IBD.blocks..20cM!=0)

### load info from Source Data

### add site identifiers
info$Site <- info$ID %>% substr(start = 1, stop = 3)
info <- info %>% mutate(SiteName = case_when(Site =="YPN" ~ "Yappa Nhae 2",
                                            Site == "LHP" ~ "Lahu Pot",
                                            Site == "THL" ~ "Tham Lod cave"))

### combine coordinates and IBD
IBD$from.long <- info$lon_new[match(IBD$Individual.1, info$ID)]
IBD$from.lat <- info$lat_new[match(IBD$Individual.1, info$ID)]
IBD$to.long <- info$lon_new[match(IBD$Individual.2, info$ID)]
IBD$to.lat <- info$lat_new[match(IBD$Individual.2, info$ID)]

### separate by site
IBD_THL <- IBD %>% filter(Site1 == "THL" | Site2 == "THL")
IBD_LHP <- IBD %>% filter(Site1 == "LHP"| Site2 == "LHP")
IBD_YPN_low <- IBD %>% filter(Site1 == "YPN" & Site2 == "YPN") %>% filter(Total.length.of.IBD.blocks..20cM<150)
IBD_YPN_high <- IBD %>% filter(Site1 == "YPN" & Site2 == "YPN") %>% filter(Total.length.of.IBD.blocks..20cM>150)

### create map
IBD_coordinates = c(98.22,19.565,98.29,19.615)
IBDmap <- get_stadiamap(IBD_coordinates, maptype = "stamen_terrain_background", crop = TRUE, zoom = 13)

### plot IBD on map
IBD_map <- ggmap(IBDmap) + 
  geom_curve(data=IBD_LHP, aes(x=from.long, xend=to.long, y=from.lat, yend=to.lat, color=Total.length.of.IBD.blocks..20cM), alpha=1, size=0.8, curvature = 0.2) + 
  geom_curve(data=IBD_THL, aes(x=from.long, xend=to.long, y=from.lat, yend=to.lat, color=Total.length.of.IBD.blocks..20cM), alpha=1, size=0.8, curvature = 0.2) + 
  geom_segment(data=IBD_YPN_low, aes(x=from.long, xend=to.long, y=from.lat, yend=to.lat, color=Total.length.of.IBD.blocks..20cM), alpha=1, linewidth=0.8) + 
  geom_segment(data=IBD_YPN_high, aes(x=from.long, xend=to.long, y=from.lat, yend=to.lat, color=Total.length.of.IBD.blocks..20cM), alpha=1, linewidth=0.8) + 
  coord_quickmap() + scale_colour_gradientn(colors = c("lightblue", "yellow", "orange", "red", "black"), limits = c(0,2000), name="total IBD length") + 
  geom_point(data = info, aes(x = lon_new, y = lat_new), colour = c("black"), size = 2, shape = 22, fill = c("grey")) + 
  geom_text(data = subset(info,Site=="THL"|Site=="LHP"), aes(x = lon_new, y = lat_new, label = ID), size = 3.5, nudge_x = 0.004, color = "grey20") + 
  geom_text(data = subset(info,ID=="YPN003"|ID=="YPN007"|ID=="YPN012"|ID=="YPN011"|ID=="YPN019"|ID=="YPN017"|ID=="YPN021"|ID=="YPN023"|ID=="YPN027"), aes(x = lon_new, y = lat_new, label = ID), size = 3.5, nudge_x = -0.004, color = "grey20") +
  geom_text(data = subset(info,ID=="YPN030"|ID=="YPN022"|ID=="YPN020"|ID=="YPN018"|ID=="YPN016"|ID=="YPN013"|ID=="YPN009"|ID=="YPN010"|ID=="YPN008"|ID=="YPN006"), aes(x = lon_new, y = lat_new, label = ID), size = 3.5, nudge_x = 0.004, color = "grey20") + 
  geom_text(data = subset(info, ID=="YPN025"), aes(x = lon_new, y = lat_new, label = ID), size = 3.5, nudge_y = 0.0015, color = "grey20") + 
  geom_text(data = subset(info, ID=="YPN001"), aes(x = lon_new, y = lat_new, label = ID), size = 3.5, nudge_y = -0.0015, color = "grey20") +
  geom_text(data = subset(info,Site=="THL"|Site=="LHP"), aes(x = lon_new, y = lat_new, label = SiteName), size = 4, nudge_x = 0.003, nudge_y = -0.0015) + 
  geom_text(data = subset(info,ID=="YPN025"), aes(x = lon_new, y = lat_new, label = SiteName), size = 4, nudge_x = -0.012, nudge_y = 0.0013) +
  theme_bw() + labs(x="Longitude", y="Latitude") + 
  theme(legend.position =c(0.89,0.835), legend.text = element_text(size = 10), legend.title = element_text(size = 12), axis.text = element_text(size = 12, colour = "black"), axis.title = element_text(size =12),axis.line = element_line(colour = "black"), legend.background = element_rect(fill = F))
IBD_map

## Figure 3a: Yellow River-related ancestry

### read in data from Source Data

### create map of continental Southeast Asia
boundaries = c(94,7.5,109,29.5)
map <- get_stadiamap(boundaries, maptype = "stamen_terrain_background", crop = TRUE, zoom = 7)

### plot ancient genomes and information if they can be modelled with or without Yellow River-related ancestry
YR_presence$Era <- factor(YR_presence$Era, levels = c('Hunter-Gatherer', 'Neolithic', 'Bronze Age','Iron Age' , 'Historic'))
presentmap <- ggmap(map) + geom_point(data = YR_presence, aes(x = Longitude , y = Latitude, fill = factor(Yellow.River.ancestry), shape = Era), size = 3.5) + 
  scale_shape_manual(values = c('Historic'=25, 'Iron Age'=24, 'Bronze Age'=23, 'Neolithic'=22, 'Hunter-Gatherer'=21), name = "Age", guide=guide_legend(override.aes = list(size=2.5), order = 1)) + 
  scale_fill_manual(values = c("1"="black", "0"="white"), labels = c("absent","present"), name = "Yellow River-related ancestry", guide = guide_legend(override.aes = list(shape = c(21,21), size=2.5), order = 2)) + theme_bw() + 
  theme(legend.position = c(0.286,0.206), legend.spacing.y = unit(2,"pt"),legend.margin = margin(t = 1, b = 1), legend.background = element_rect(fill=F), legend.text = element_text(size= 10), legend.title = element_text(size=10), axis.text = element_text(size = 12, colour = "black"), axis.title = element_text(size=12)) + 
  labs(x="Longitude",y="Latitude")                
presentmap

## Supplementary Figure 15: ADMIXTURE CV errors

### read in cverrors from Source Data
cverrors$K <- as.factor(cverrors$K)
cverrors$replicate <- as.factor(cverrors$replicate)

### plot boxplot and individual data points
ggplot(cverrors, aes(K,cv_error)) + theme_bw() + ylab("CV Error") + 
  geom_boxplot(fill="grey90") +
  geom_jitter(width= 0.4, fill = "white", shape=21, size=2)
