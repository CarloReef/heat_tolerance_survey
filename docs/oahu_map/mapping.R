library(tidyverse);library(janitor);library(cowplot);library(readxl);library(lubridate)
library(ade4)
library(broom)
library(geodist)
library(ggdendro)
library(R.matlab)
library(zoo)
library(scales)
library(tools)
library(vegan)
library(ggrepel)
library(pdftools)
library(magick)
library(forcats)
library(factoextra)
options(scipen=999)
setwd("~/omics/mcap_clonality")

###################### MAP ########################################################### #####
library(sf);library(ggrepel);library(ggsn);library(cowplot);library(tidyverse);library(patchwork);library(jpeg);library(ggnewscale)

oahu<-st_transform(st_read("~/projects/oahu_map/Layers/coast_n83.shp"),crs=4386)
cropped<-st_crop(oahu,xmin=-158.8,xmax=-157.4,ymin=20.8,ymax=21.8)
fringe<-st_read("~/projects/oahu_map/Layers/Fringing Reef.shp")%>%select(id,geometry)%>%mutate(zone='Fringing Reef')
habitat<-st_read("~/projects/oahu_map/Layers/haw_benthic_habitat.shp")%>%clean_names()%>%filter(zone=='Backreef'|zone=="Reef Flat")%>%select(id,zone,geometry)
patch<-st_read("~/projects/oahu_map/Layers/Patches2.shp")%>%mutate(type="Reef")%>%clean_names()%>%select(id,zone,block,geometry)%>%mutate(zone='Patch Reef')
points<-read_tsv("./metadata/Site_depth_lat_long_numColonies.txt")%>%clean_names()%>%select(site,block,lat,lon)%>%mutate(block=paste("Block",as.factor(block)))

out<-bind_rows(st_transform(fringe,crs=4386),st_transform(habitat,crs=4386))%>%
  mutate(zone=case_when(zone=="Reef Flat"~ "Fringing Reef",
                        zone=="Reef Crest"~"Fringing Reef",
                        zone=="Backreef"|zone=="Fringing Reef"~"Back/Fringing Reef",
                        TRUE~as.character(zone)))
inset<-ggplotGrob(ggplot()+
                    geom_sf(data=cropped)+
                    theme_minimal(base_size=8)+
                    theme(axis.text.x=element_blank(),
                          axis.title.y=element_blank(),
                          axis.title.x=element_blank(),
                          axis.text.y=element_blank(),
                          panel.grid.minor=element_blank(),
                          panel.grid.major=element_blank(),
                          panel.border = element_rect(colour = "gray35", fill=NA, size=1),
                          panel.background = element_rect(color="white"))+
                    annotate("rect",xmin=-157.85,xmax=-157.76,ymin=21.413,ymax=21.507,color="blue",alpha=0.25,size=0.25)+
                    annotate("text",x=-158.05,y=21.52,label="Oʻahu",size=2.5))

map<-ggplot()+
  geom_sf(data=out,fill="lightgrey",color='lightgrey')+
  geom_sf(data=oahu,fill="darkgrey")+
  geom_sf(aes(fill=block,color=block),dat=patch)+
  #geom_label_repel(aes(lon,lat,label=site),data=points,size=2,min.segment.length = 0,nudge_x=0,segment.color = "grey50")+
  coord_sf(xlim=c(-157.85,-157.76),ylim=c(21.413,21.507))+
  theme_classic(base_size=8)+
  theme(legend.position=c(0.16,0.22),
        legend.key.size=unit(0.4,"cm"),
        axis.title=element_blank(),
        axis.text.y=element_text(angle=90,hjust=0.5))+
  scale_fill_viridis_d(direction=1,name="Block")+
  scale_color_viridis_d(direction=1,name="Block",guide=FALSE)+
  scalebar(x.min=-157.78,x.max=-157.82,y.min=21.415,y.max=21.50,transform=TRUE,dist=1,dist_unit="km",
           height = .02, st.dist = 0.04,
           box.fill = c("black", "white"),
           box.color = "black", border.size = .1,st.size=2,
           location="bottomleft")+
  annotation_custom(inset,xmin=-157.8,xmax=-157.75,ymin=21.479,ymax=21.512)+
  #new_scale_color()+
  geom_point(aes(lon,lat,fill=block),pch=21,data=points)+
  scale_fill_viridis_d(name="Block");map


quartz(w=6,h=2.9) 
map

#supplemental
quartz(w=6,h=7)
sf1<-ggplot()+
  geom_sf(data=out,fill="lightgrey",color='lightgrey')+
  geom_sf(data=oahu,fill="darkgrey")+
  geom_sf(aes(fill=block,color=block),dat=patch)+
  geom_label_repel(aes(lon,lat,label=site),data=points,size=3,min.segment.length = 0,nudge_x=0,segment.color = "grey50",max.overlaps = Inf)+
  coord_sf(xlim=c(-157.85,-157.76),ylim=c(21.413,21.507))+
  theme_classic(base_size=10)+
  theme(legend.position=c(0.075,0.1),
        legend.key.size=unit(0.4,"cm"),
        axis.title=element_blank(),
        axis.text.y=element_text(angle=90,hjust=0.5))+
  scale_fill_viridis_d(direction=1,name="Block")+
  scale_color_viridis_d(direction=1,name="Block")+
  geom_point(aes(lon,lat),pch=21,data=points);sf1
