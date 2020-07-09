setwd('C:/Users/noemi/Documents/GitHub/master-thesis/R project')
source('functions.R')
if (!require(rgeos)) {
  install.packages("rgeos", repos = "http://cran.us.r-project.org")
  require(rgeos)
}

if (!require(rgdal)) {
  install.packages("rgdal", repos = "http://cran.us.r-project.org")
  require(rgdal)
}
if (!require(raster)) {
  install.packages("raster", repos = "http://cran.us.r-project.org")
  require(raster)
}
  if(!require(ggplot2)) {
  install.packages("ggplot2", repos="http://cloud.r-project.org")
  require(ggplot2)
}
if(!require(viridis)) {
  install.packages("viridis", repos="http://cloud.r-project.org")
  require(viridis)
}
if(!require(dplyr)) {
  install.packages("dplyr", repos = "https://cloud.r-project.org/")
  require(dplyr)
}
if(!require(gtable)) {
  install.packages("gtable", repos = "https://cloud.r-project.org/")
  require(gtable)
}
if(!require(grid)) {
  install.packages("grid", repos = "https://cloud.r-project.org/")
  require(grid)
}
if(!require(readxl)) {
  install.packages("readxl", repos = "https://cloud.r-project.org/")
  require(readxl)
}
if(!require(magrittr)) {
  install.packages("magrittr", repos = "https://cloud.r-project.org/")
  require(magrittr)
} 
if(!require(ggsn)) {
  install.packages("ggsn", repos = "https://cloud.r-project.org/")
  require(ggsn)
}
if(!require(ggpubr)) {
  install.packages("ggpubr", repos = "https://cloud.r-project.org/")
  require(ggpubr)
}


#Libraries 
libraries <- c('ggplot2','rgdal','rgeos','raster','ggsn','ggpubr','magrittr','readxl', 'viridis','gtable','grid','dplyr','pophelper')
lapply(libraries, require, character.only = TRUE)

theme_map <- function(...) {
  theme_minimal() +
    theme(
      #text = element_text(family = "Computer Modern", color = "#22211d"),
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}


#import dtm
relief<-raster('../shapefiles/02-relief-georef-clipped-resampled_WGS84.tif')
relief_spdf <- as(relief, "SpatialPixelsDataFrame")
relief <- as.data.frame(relief_spdf) 

# %>% 
  # rename(value = `X02.relief.georef.clipped.resampled_WGS84`)

#import coordinates
coord<-read.table('coordinates_wgs84.txt',header = T)
K5_trend<-read.table('../Results/CLUMPP_output/pop_K5-combined-merged.txt')[,-c(1,7)]
K6_nospatial<-read.table('../Results/CLUMPP_output/pop_K6-combined-merged.txt')[,-c(1,8)]
K5_lowest_dic<-readQ('../Results/CLUMPP_output/pop_K6-combined-merged.txt',filetype="clumpp")
#K6_structure<-read.table('../Results/STRUCTURE/par_1/Results/pop_K6/pop_K6-combined-merged.txt')[,-c(1,8)]


K5_lowest_dic_fulltrend<-read.table('../Results/CLUMPP_output/pop_K5-combined-merged_fulltrend.txt')[,-c(1,7)]
K5_lowest_dic<-qlist
K5_lowest_dic<-K5_lowest_dic$`k5-adm-8`
K5_lowest_dic<-cbind(coord,K5_lowest_dic)

colnames(K5_trend)<-c('p1','p2','p3','p4','p5')
colnames(K5_lowest_dic_fulltrend)<-c('p1','p2','p3','p4','p5')
colnames(K6_nospatial)<-c('p1','p2','p3','p4','p5','p6')
#colnames(K6_structure)<-c('p1','p2','p3','p4','p5','p6')
k5_tess<-cbind(coord,K5_trend)
k5_tess.fulltrend<-cbind(coord,K5_lowest_dic_fulltrend)
k6_tess<-cbind(coord,K6_nospatial)
#k6_structure<-cbind(coord,K6_structure)
write.table(k5_tess,'k5_tess.txt',row.names = F)
write.table(k6_tess,'k6_tess.txt',row.names = F)
#write.table(k6_structure,'k6_structure.txt',row.names = F)
write.table(k5_tess.fulltrend,'k5_fulltrend.txt',row.names = F)
write.table(K5_lowest_dic,'k5_fulltrend_lowest_dic.txt',row.names = F)


#Read shapefile
cantons <- readOGR(dsn = "../shapefiles", layer = "cantons_abbrv")
lakes <-readOGR(dsn = "../shapefiles", layer = "lakes")
rivers <-readOGR(dsn = "../shapefiles", layer = "rivers")
cultural<-readOGR(dsn="../shapefiles",layer="brunig_napf_reuss")
centroids.df <- as.data.frame(coordinates(cantons))
names(centroids.df) <- c("long_c", "lat_c")  #more sensible column names


#Fortify cantons shp
cantons@data$long_c<-centroids.df$long_c
cantons@data$lat_c<-centroids.df$lat_c
cantons@data$ID<-seq.int(nrow(cantons@data))
cantons.fortified <- fortify(cantons,region='ID')%>% mutate(id = as.numeric(id))
cantons.fortified <- cantons.fortified %>% left_join(cantons@data, by = c("id" = "ID"))
cantons.fortified<-cantons.fortified[,c(1:7,31:33)]
cnames<-cantons@data[,c(24:26)]
cnames$labels_abb<-as.character(cnames$labels_abb)

#Fortify lakes and rivers
lakes.fortified<-fortify(lakes)
cultural.fortified<-fortify(cultural)

#Read shapefile
voronoi_tess_k5 <- readOGR(dsn = "../shapefiles", layer = "k5_tess")
#Add ID to shp data
voronoi_tess_k5@data$ID<-seq.int(nrow(voronoi_tess_k5@data))
#Fortify and left join
map.df_tess<- fortify(voronoi_tess_k5,region = 'ID') %>% mutate(id = as.numeric(id))
map.df_tess <- map.df_tess %>% left_join(voronoi_tess_k5@data, by = c("id" = "ID"))


#Read shapefile
voronoi_tess_k6_nospatial <- readOGR(dsn = "../shapefiles", layer = "k6_tess")
#Add ID to shp data
voronoi_tess_k6_nospatial@data$ID<-seq.int(nrow(voronoi_tess_k6_nospatial@data))
#Fortify and left join
map.df_tess_nospatial<- fortify(voronoi_tess_k6_nospatial,region = 'ID') %>% mutate(id = as.numeric(id))
map.df_tess_nospatial <- map.df_tess_nospatial %>% left_join(voronoi_tess_k6_nospatial@data, by = c("id" = "ID"))

#Read shapefile
voronoi_tess_k5_full_trend <- readOGR(dsn = "../shapefiles", layer = "k5_fulltrend")
#Add ID to shp data
voronoi_tess_k5_full_trend@data$ID<-seq.int(nrow(voronoi_tess_k5_full_trend@data))
#Fortify and left join
map.df_tess_fulltrend<- fortify(voronoi_tess_k5_full_trend,region = 'ID') %>% mutate(id = as.numeric(id))
map.df_tess_fulltrend <- map.df_tess_fulltrend %>% left_join(voronoi_tess_k5_full_trend@data, by = c("id" = "ID"))

#Read shapefile
voronoi_tess_k6_structure <- readOGR(dsn = "../shapefiles", layer = "k6_structure")
#Add ID to shp data
voronoi_tess_k6_structure@data$ID<-seq.int(nrow(voronoi_tess_k6_structure@data))
#Fortify and left join
map.df_tess_structure<- fortify(voronoi_tess_k6_structure,region = 'ID') %>% mutate(id = as.numeric(id))
map.df_tess_structure <- map.df_tess_structure %>% left_join(voronoi_tess_k6_structure@data, by = c("id" = "ID"))





mypaletTESS<-c("#1F78B4","#33A02C","#E31A1C","#FF7F00","#6A3D9A",'deeppink1')
mypalet<-c("#1F78B4",
           "#FF7F00",
           "#33A02C",
           "#E31A1C",
           "#6A3D9A",
           "#B2DF8A",
           "#A6CEE3",
           "#FDBF6F","#CAB2D6","#6A3D9A","#FFFF99","#B15928")

ext <- extent(relief)
#ext <- extent(cantons.fortified)
x_scale_loc <- ext@xmax
y_scale_loc <- ext@ymin
y_scale_loc_max <- ext@ymax

colnames(map.df_tess)[8:12]<-c('p1','p2','p3','p4','p5')
colnames(map.df_tess_nospatial)[8:13]<-c('p1','p2','p3','p4','p5','p6')
colnames(map.df_tess_fulltrend)[8:12]<-c('p1','p2','p3','p4','p5')
#Shannon index
# library(vegan)
# attach(map.df)
# diversity(map.df[,10:13],index='shannon')
# map.df$div<-diversity(map.df[,10:13],index='shannon')

source('functions.R')
prova<-pop_dominants(map.df_tess)
map.df_tess<-names_dom(prova)
prova2<-assign_cat(map.df_tess)

#Transitions colors
p1_p2 <- colorRampPalette(c("deeppink1", "#F9E640"))(10)
p1_p3 <- colorRampPalette(c("deeppink1", "#E31A1C"))(10)
p1_p4 <- colorRampPalette(c("deeppink1", "#198E8B"))(10)
p1_p5 <- colorRampPalette(c("deeppink1", "#33A02C"))(10)
p2_p3 <- colorRampPalette(c("#F9E640", "#E31A1C"))(10)
p2_p4 <- colorRampPalette(c("#F9E640", "#198E8B"))(10)
p2_p5 <- colorRampPalette(c("#F9E640", "#33A02C"))(10)
p3_p4 <- colorRampPalette(c("#E31A1C", "#198E8B"))(10)
p3_p5 <- colorRampPalette(c("#E31A1C", "#33A02C"))(10)
p4_p5 <- colorRampPalette(c("#198E8B","#33A02C"))(10)

color.list<-list(p1_p2,p1_p3,p1_p4,p1_p5,p2_p3,p2_p4,p2_p5,p3_p4,p3_p5,p4_p5)


colnames(map.df_tess)[8:12]<-c('p1','p2','p3','p4','p5')
map.df_tess<-pop_dominants(map.df_tess)
map.df_tess<-assign_cat(map.df_tess)
map.df_tess<-names_dom(map.df_tess)
#Assign color to each element
for (i in 1:nrow(map.df_tess)){
map.df_tess$col_code[i]<-color.list[[map.df_tess$color[i]]][map.df_tess$col_pos[i]]
}


dev.off()

colnames(map.df_tess_nospatial)[8:13]<-c('p1','p2','p3','p4','p5','p6')
mypaletTESS<-c('deeppink1',
               "#FF7F00",
               "#6A3D9A",
               '#233755',
               "#33A02C",
               "#1F78B4")

#Plot non-spatial results
p<-list()
for(i in 1:6){
  p[[i]]<-ggplot(map.df_tess_nospatial) +
    geom_polygon(aes_string(x='long', y='lat',group='group',fill=paste0('p',i)))+
    scale_fill_gradient(low = 'white',high = mypaletTESS[i],limits=c(0,1))+
    geom_raster(data = relief, aes(x = x,
                                   y = y,
                                   alpha = `X02.relief.georef.clipped.resampled_WGS84`),inherit.aes = FALSE) +
    # use the "alpha hack"
    scale_alpha(name = "", range = c(0.5, 0), guide = F)+
    geom_polygon(data=lakes.fortified,aes(x=long,y=lat,group=group),fill='#AFBDC0',color='#AFBDC0')+
    geom_polygon(data=cantons.fortified,aes(x=long, y=lat,group=group),fill=NA,color='grey',inherit.aes = FALSE)+
    
    geom_text(data = cnames, aes(x = long_c, y = lat_c, label = labels_abb), size = 4,inherit.aes = FALSE)+
    #coord_equal() +
    # add the previously defined basic theme
    scalebar(map.df_tess_nospatial, location = "bottomright",dist = 25,st.size = 3.7, height=0.01,dist_unit = "km",transform = TRUE, model = 'WGS84',anchor = c(x = x_scale_loc, y = (y_scale_loc - 0.05)))+
    north(data = map.df_tess_nospatial, location = "topright", scale = 0.1, symbol = 3,anchor = c(x = x_scale_loc, y = (y_scale_loc_max + 0.05)))+
    theme_map()+
    theme(plot.title = element_text(hjust = 0.5),plot.subtitle =element_text(hjust = 0.5) )+
    ggtitle(paste0('Municipality admixture levels of population ',i))+
    theme(legend.position = c(0.5, 0),
          legend.justification = c("center", "bottom"),legend.direction='horizontal')+
    labs(fill = "Municipality Admixture proportions",subtitle='Non-spatial model')+
    theme(plot.title = element_text(hjust = 0.5,size= 14),plot.subtitle =element_text(hjust = 0.5) )
  
  
  p[[i]]<-annotate_figure(p[[i]],
                          bottom = text_grob("Data sources | Canton borders, Digital Height Model 200m : Swiss Federal Office of Topography (swisstopo), Lakes : OpenStreetMap", color = "#a0a0a0",
     
                                                                                     hjust = 1, x = 1, face = "italic", size = 10))
  
  
  }




p[[1]]
p[[2]]
p[[3]]
p[[4]]
p[[5]]
p[[6]]


#Plot sample sites
p<-ggplot(coord) +
  geom_polygon(data=lakes.fortified,aes(x=long,y=lat,group=group),fill='#AFBDC0',color='#AFBDC0')+
  geom_point(aes(x=X_1, y=Y_1),color='#86113B')+
  geom_raster(data = relief, aes(x = x,
                                 y = y,
                                 alpha = `X02.relief.georef.clipped.resampled_WGS84`),inherit.aes = FALSE) +
  # use the "alpha hack"
  scale_alpha(name = "", range = c(0.5, 0), guide = F)+
  geom_polygon(data=cantons.fortified,aes(x=long, y=lat,group=group),fill=NA,color='grey',inherit.aes = FALSE)+
  geom_text(data = cnames, aes(x = long_c, y = lat_c, label = labels_abb), size = 4,inherit.aes = FALSE)+
  #coord_equal() +
  # add the previously defined basic theme
  scalebar(map.df_tess, location = "bottomright",dist = 25,st.size = 3.7, height=0.01,dd2km = TRUE, model = 'WGS84',anchor = c(x = x_scale_loc, y = (y_scale_loc - 0.05)))+
  north(map.df_tess, location = "topright", scale = 0.1, symbol = 3,anchor = c(x = x_scale_loc, y = (y_scale_loc_max + 0.05)))+
  theme_map()+
  theme(plot.title = element_text(hjust = 0.5),plot.subtitle =element_text(hjust = 0.5) )+
  ggtitle('SADS municipalities')+
  theme(legend.position = c(0.5, 0),
        legend.justification = c("center", "bottom"),legend.direction='horizontal')+
  theme(plot.title = element_text(hjust = 0.5,size= 14),plot.subtitle =element_text(hjust = 0.5) )


p<-annotate_figure(p,bottom = text_grob("Data sources | Canton borders, Digital Height Model 200m : Swiss Federal Office of Topography (swisstopo), Lakes : OpenStreetMap", color = "#a0a0a0",      hjust = 1, x = 1, face = "italic", size = 10))
p                                           



#Plot trend results
p<-list()

mypaletTESS<-c("#6A3D9A",
               "#FF7F00",
               "#33A02C",
               "#1F78B4",
               "#E31A1C")
for(i in 1:5){
  p[[i]]<-ggplot(map.df_tess) +
    geom_polygon(aes_string(x='long', y='lat',group='group',fill=paste0('p',i)))+
    scale_fill_gradient(low = 'white',high = mypaletTESS[i],limits=c(0,1))+
    geom_raster(data = relief, aes(x = x, y = y, alpha = `X02.relief.georef.clipped.resampled_WGS84`),inherit.aes = FALSE) +
    # use the "alpha hack"
    scale_alpha(name = "", range = c(0.5, 0), guide = F)+
    geom_polygon(data=lakes.fortified,aes(x=long,y=lat,group=group),fill='#AFBDC0',color='#AFBDC0')+
    geom_polygon(data=cantons.fortified,aes(x=long, y=lat,group=group),fill=NA,color='grey',inherit.aes = FALSE)+
    geom_path(data=cultural,aes(x=long,y=lat,group=group),color='black')+
    #geom_line(data=rivers.fortified,aes(x=long,y=lat,group=group),fill='#EBF9FA',color='#EBF9FA')+
    geom_text(data = cnames, aes(x = long_c, y = lat_c, label = labels_abb), size = 4,inherit.aes = FALSE)+
    #coord_equal() +
    # add the previously defined basic theme
    scalebar(map.df_tess, location = "bottomright",dist = 25,st.size = 3.7, height=0.01,dd2km = TRUE, model = 'WGS84',anchor = c(x = x_scale_loc, y = (y_scale_loc - 0.05)))+
    north(data = map.df_tess, location = "topright", scale = 0.1, symbol = 3,anchor = c(x = x_scale_loc, y = (y_scale_loc_max + 0.05)))+
    #theme_map()+
    theme_classic()+
    theme(plot.title = element_text(hjust = 0.5),plot.subtitle =element_text(hjust = 0.5) )+
    ggtitle(paste0('Municipality admixture proportions of population ',i))+
    theme(legend.position = c(0.5, 0),
          legend.justification = c("center", "bottom"),legend.direction='horizontal')+
    labs(fill = "Municipality admixture proportions",subtitle='Full-trend model')+
    theme(plot.title = element_text(hjust = 0.5,size= 18),plot.subtitle =element_text(hjust = 0.5) )
  
  
  p[[i]]<-annotate_figure(p[[i]],
                          bottom = text_grob("Data sources | Canton borders, Digital Height Model 200m : Swiss Federal Office of Topography (swisstopo), Lakes: OpenStreetMap", color = "#a0a0a0",
                                             hjust = 1, x = 1, face = "italic", size = 10))
}






#Plot STRUCTURE results
p<-list()

mypaletTESS<-c("#6A3D9A",
               "#33A02C",
               "#E31A1C",
               "#1F78B4",
               "#FF7F00")

mypaletTESS<-c('deeppink1',"#33A02C","#1F78B4","#6A3D9A",'#233755',"#FF7F00")
colnames(map.df_tess_structure)[8:13]<-c('p1','p2','p3','p4','p5','p6')

for(i in 1:6){
  p[[i]]<-ggplot(map.df_tess_structure) +
    geom_polygon(aes_string(x='long', y='lat',group='group',fill=paste0('p',i)))+
    scale_fill_gradient(low = 'white',high = mypaletTESS[i],limits=c(0,1))+
    geom_raster(data = relief, aes(x = x, y = y, alpha = `X02.relief.georef.clipped.resampled_WGS84`),inherit.aes = FALSE) +
    # use the "alpha hack"
    scale_alpha(name = "", range = c(0.5, 0), guide = F)+
    geom_polygon(data=lakes.fortified,aes(x=long,y=lat,group=group),fill='#AFBDC0',color='#AFBDC0')+
    geom_polygon(data=cantons.fortified,aes(x=long, y=lat,group=group),fill=NA,color='grey',inherit.aes = FALSE)+
    geom_path(data=cultural,aes(x=long,y=lat,group=group),color='black')+
    geom_text(data = cnames, aes(x = long_c, y = lat_c, label = labels_abb), size = 4,inherit.aes = FALSE)+
    #coord_equal() +
    # add the previously defined basic theme
    scalebar(map.df_tess_structure, location = "bottomright",dist = 25,st.size = 3.7, height=0.01,dd2km = TRUE, model = 'WGS84',anchor = c(x = x_scale_loc, y = (y_scale_loc - 0.05)))+
    north(data = map.df_tess_structure, location = "topright", scale = 0.1, symbol = 3,anchor = c(x = x_scale_loc, y = (y_scale_loc_max + 0.05)))+
    theme_map()+
    theme(plot.title = element_text(hjust = 0.5),plot.subtitle =element_text(hjust = 0.5) )+
    ggtitle(paste0('Municipality admixture proportions of population ',i))+
    theme(legend.position = c(0.5, 0),
          legend.justification = c("center", "bottom"),legend.direction='horizontal')+
    labs(fill = "Municipality admixture proportions",subtitle='STRUCTURE')+
    theme(plot.title = element_text(hjust = 0.5,size= 18),plot.subtitle =element_text(hjust = 0.5) )
  
  
  p[[i]]<-annotate_figure(p[[i]],
                          bottom = text_grob("Data sources | Canton borders, Digital Height Model 200m : Swiss Federal Office of Topography (swisstopo), Lakes: OpenStreetMap", color = "#a0a0a0",
                                             hjust = 1, x = 1, face = "italic", size = 10))
}



