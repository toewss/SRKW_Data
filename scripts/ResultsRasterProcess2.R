#----Clear R environment----

rm(list = ls(all=TRUE)) ; ls()
#.libPaths('C:/Users/Acoustic 1/Documents/SRKW_Effort/Library')
#.libPaths('media/stoews/T7/1-DFO/1-Analyses/SRKW_Effort/Library')
#.libPaths('D:/1-DFO/1-Analyses/SRKW_Effort/Library')
#.libPaths('F:/SRKW_Effort/Library')
#----Load required packages----
suppressMessages(library(tidyr))
suppressMessages(library(dplyr))
suppressMessages(library(lubridate))
suppressMessages(library(maptools))
suppressMessages(library(rgeos))
suppressMessages(library(rgdal))
suppressMessages(library(spatial))
suppressMessages(library(here))
suppressMessages(library(sf))
suppressMessages(library(tibble))
suppressMessages(library(readxl))
suppressMessages(library(ggplot2))
suppressMessages(library(raster))
suppressMessages(library(sp))
suppressMessages(library(inlabru))
suppressMessages(library(ggpubr))


memory.size() ### Checking your memory size
memory.limit() ## Checking the set limit
memory.limit(size=5600000)

setwd(here("Data","Coast shapefile"))
COAST=readOGR(".",layer="coast_ALBERS", verbose = FALSE)

setwd(here("Data","Original"))

# load the COAST shapefiles
# filled in and clipped shapefile
COAST_simp = readRDS('COAST_mesh.rds')
COAST_simp = spTransform(COAST_simp, CRS(proj4string(COAST)))
COAST_plotting = readRDS('COAST_simp.rds')
COAST_plotting = spTransform(COAST_plotting, CRS(proj4string(COAST_simp)))#ST added to tranform CRS
COAST_plotting = CHS_crop_simp
EPSGcode = 3005    # NAD83 / BC Albers projection (3005). Or if I want to use UTM zone 10, use EPSG code 32610; OR WGS84 unprojected = 4326
setwd(here("Data","BASEMAP FILES"))

CHS_crop_simp = st_read("CHS_crop_simp_for_mappingClip.shp") %>% st_transform(EPSGcode)
CHS_crop_simp<- sf:::as_Spatial(CHS_crop_simp$geometry)

setwd(here("Data","Coast shapefiles"))
CHS_crop_simp = st_read("CHS_crop_simp_for_mapping.shp") %>% st_transform(EPSGcode)
CHS_crop_simp<- sf:::as_Spatial(CHS_crop_simp$geometry)

#attributes(COAST_simp)
#attributes(COAST_plotting)

# Get polygon outlining areas of exceedance above a particular proportion
exceedance_raster_to_polygon_outline <- function(raster, prop_value){
  raster <- raster::rasterToPolygons(raster, fun = function(x) {x>=prop_value}) %>%
    aggregate() %>%
    disaggregate()
  return(raster)
}

#FLAG: setwd!!!
create.raster <- function(results, month){ #results=spatialpixelsdataframe, month=as.numeric
  r <- raster(results, layer = month, values = TRUE)
  name <- paste0(month.abb[month+4], "_D", substr(deparse(quote(results)), nchar(deparse(quote(results))),nchar(deparse(quote(results))))) #May_D9
  writeRaster(r, paste0(name, ".tif"),options=c('TFW=YES'),overwrite=T)
  return(r)
}
 
# Export polygons for examination in QGIS
save_poly <- function(sp_poly) {
  poly.df <- data.frame(ID=1:length(sp_poly))
  poly <- SpatialPolygonsDataFrame(sp_poly, poly.df)
  #setwd("C:/Users/stredulinskye/Documents/PROJECTS/CSAS SRKW - Feb 2021/Behavioural modelling/Spatial files/FF-GBS comparison polygons")
  obj.name <- deparse(substitute(sp_poly))
  poly.name <- paste(name, "NAD83_BCAlbers", sep = "_")
  writeOGR(poly, dsn = getwd(), layer = poly.name, driver = "ESRI Shapefile")
}

 
#Clipping polygons
gClip <- function(shp, bb){
  #MAKE SURE THE SHAPEFILES ARE IN THE SAME PROJECTION BEFORE USING THIS FUNCTION!
  bb_ext <- matrix(st_bbox(bb), 2, byrow = TRUE)
  b_poly <- st_polygon(list(rbind(c(bb_ext[1,1],bb_ext[1,2]),
                                  c(bb_ext[1,1],bb_ext[2,2]),
                                  c(bb_ext[2,1],bb_ext[2,2]),
                                  c(bb_ext[2,1],bb_ext[1,2]),
                                  c(bb_ext[1,1],bb_ext[1,2]))))
  b_poly <- st_sfc(b_poly, crs = 3005)
  st_intersection(st_geometry(shp), st_geometry(b_poly))
}




setwd(here("Data","Results"))
results<-readRDS("results_simple.rds")# This is the original model with spatial-temporal terms
#results<-readRDS("results_space_MCMC_podSPDE_monthly_dup_removed.rds")## This is from the model without spatial-temporal term
results<-results_simple
setwd(here("Data","Results","SRKW_Occurrence","AcrossMonth"))


# create the SpatialPixelsDataFrame object
tmp9 <- SpatialPixelsDataFrame(results$combined_results$pixels_file,
                              data.frame(Exc9 = results$combined_results$Post_Bigger_D9))

tmp8 <- SpatialPixelsDataFrame(results$combined_results$pixels_file,
                               data.frame(Exc9 = results$combined_results$Post_Bigger_D8))

tmp7 <- SpatialPixelsDataFrame(results$combined_results$pixels_file,
                               data.frame(Exc9 = results$combined_results$Post_Bigger_D7))

###Skip this if not recreating rasters from model results
for(i in 1:6){
  r <- create.raster(tmp9, i)
  name <- paste0(month.abb[i+4], "_D", substr(deparse(quote(tmp9)), nchar(deparse(quote(tmp9))),nchar(deparse(quote(tmp9))))) #May_D9
  assign(name, r)
  poly <- exceedance_raster_to_polygon_outline(r, 0.9)
  save_poly(poly)
}

for(i in 1:6){
  r <- create.raster(tmp8, i)
  name <- paste0(month.abb[i+4], "_D", substr(deparse(quote(tmp8)), nchar(deparse(quote(tmp8))),nchar(deparse(quote(tmp8))))) #May_D9
  assign(name, r)
  poly <- exceedance_raster_to_polygon_outline(r, 0.9)
  save_poly(poly)
}
for(i in 1:6){
  r <- create.raster(tmp7, i)
  name <- paste0(month.abb[i+4], "_D", substr(deparse(quote(tmp7)), nchar(deparse(quote(tmp7))),nchar(deparse(quote(tmp7))))) #May_D9
  assign(name, r)
  poly <- exceedance_raster_to_polygon_outline(r, 0.9)
  save_poly(poly)
}

# plot using breaks.
plot(AM1, 
     breaks = c(0.7, 0.8,0.9), 
     col = terrain.colors(3),
     main="Digital Surface Model (DSM)\n NEON Harvard Forest Field Site")
AM1r <- reclassify(AM1, c(0, 0.7, 1,
                     0.7,0.8, 2,
                     0.8,0.9,3,
                     0.9,1,4))



# get all files with the .shp extension from working directory
#setwd(here("Data","Results","SRKW_Occurrence","AcrossMonth","Regional SRKW Polygons_AM"))
# get all files with the .shp extension from working directory
setwd(here("Data","Results","SRKW_Occurrence","AcrossMonth","Shapefiles"))

shps <- dir(getwd(), "*.shp")
# the assign function will take the string representing shp and turn it into a variable
# which holds the spatial points data
for (shp in shps) {
assign(shp, st_read(shp))#,proj4string=CRS('+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0'))
}
plot(get(shp[1])) # i.e.
# ...done
#####Updated to map Active Pass

pl1_May <- ggplot() +
     geom_sf(data=May_D7_NAD83_BCAlbers.shp, aes(fill = "70%"), color=NA)+ geom_sf(data=May_D8_NAD83_BCAlbers.shp, aes(fill = "80%"), colour ="orange")+
  geom_sf(data=May_D9_NAD83_BCAlbers.shp,aes(fill = "90%"), colour ="red")+
  geom_sf(data = CHS_crop_simp, fill = "#c2c2c2", show.legend = FALSE) + 
  scale_fill_manual(values = c("70%" = "yellow", "80%"= "orange", "90%"="red"), 
                    guide = guide_legend(override.aes = list(linetype = "blank",size=5, shape = NA))) +  
  ggtitle("May") + labs( fill="Exceedance")+#, y="Latitude", x="Longitude")+ 
  theme(axis.text = element_text(size=8), axis.title = element_text(size=9),legend.title =element_text(size =10), legend.text=element_text(size = 10),plot.title = element_text( size=8, face="bold.italic"), plot.subtitle = element_text(size=8))+ 
  coord_sf(crs=st_crs(3005),xlim = c(1160000 , 1230000), ylim = c(380000, 440000), expand = FALSE)#  xlim = c(1036778.046114 , 1225730.937177), ylim = c(331246.297141, 446991.579926), expand = FALSE)
  pl1_May

pl1_Jun <- ggplot() + 
  geom_sf(data=Jun_D7_NAD83_BCAlbers.shp, aes(fill = "70%"), color=NA)+ geom_sf(data=Jun_D8_NAD83_BCAlbers.shp, aes(fill = "80%"), colour ="orange")+
  geom_sf(data=Jun_D9_NAD83_BCAlbers.shp,aes(fill = "90%"), colour ="red")+
  geom_sf(data = CHS_crop_simp, fill = "#c2c2c2", show.legend = FALSE) + 
  scale_fill_manual(values = c("70%" = "yellow", "80%"= "orange", "90%"="red"), 
                    guide = guide_legend(override.aes = list(linetype = "blank",size=5, shape = NA))) +  
  ggtitle("June") + labs( fill="Exceedance")+#, y="Latitude", x="Longitude")+
  theme(axis.text = element_text(size=8), axis.title = element_text(size=9),legend.title =element_text(size = 10), legend.text=element_text(size = 10),plot.title = element_text( size=8, face="bold.italic"), plot.subtitle = element_text(size=8))+ 
  coord_sf(crs=st_crs(3005),xlim = c(1160000 , 1230000), ylim = c(380000, 440000), expand = FALSE) #xlim = c(1036778.046114 , 1225730.937177), ylim = c(331246.297141, 446991.579926), expand = FALSE)

pl1_Jul <- ggplot() + 
  geom_sf(data=Jul_D7_NAD83_BCAlbers.shp, aes(fill = "70%"), color=NA)+ geom_sf(data=Jul_D8_NAD83_BCAlbers.shp, aes(fill = "80%"), colour ="orange")+
  geom_sf(data=Jul_D9_NAD83_BCAlbers.shp,aes(fill = "90%"), colour ="red")+
  geom_sf(data = CHS_crop_simp, fill = "#c2c2c2", show.legend = FALSE) + 
  scale_fill_manual(values = c("70%" = "yellow", "80%"= "orange", "90%"="red"), 
                    guide = guide_legend(override.aes = list(linetype = "blank",size=5, shape = NA))) +  
  ggtitle("July") + labs( fill="Exceedance")+#, y="Latitude", x="Longitude")+
  theme(axis.text = element_text(size=8), axis.title = element_text(size=9),legend.title =element_text(size = 10), legend.text=element_text(size = 10),plot.title = element_text( size=8, face="bold.italic"), plot.subtitle = element_text(size=8))+ 
  coord_sf(crs=st_crs(3005),xlim = c(1160000 , 1230000), ylim = c(380000, 440000), expand = FALSE) #xlim = c(1036778.046114 , 1225730.937177), ylim = c(331246.297141, 446991.579926), expand = FALSE)

pl1_Aug <- ggplot() + 
  geom_sf(data=Aug_D7_NAD83_BCAlbers.shp, aes(fill = "70%"), color=NA)+ geom_sf(data=Aug_D8_NAD83_BCAlbers.shp, aes(fill = "80%"), colour ="orange")+
  geom_sf(data=Aug_D9_NAD83_BCAlbers.shp,aes(fill = "90%"), colour ="red")+
  geom_sf(data = CHS_crop_simp, fill = "#c2c2c2", show.legend = FALSE) + 
  scale_fill_manual(values = c("70%" = "yellow", "80%"= "orange", "90%"="red"), 
                    guide = guide_legend(override.aes = list(linetype = "blank",size=5, shape = NA))) +  
 ggtitle( "August") + labs( fill="Exceedance")+#, y="Latitude", x="Longitude")+
  theme(axis.text = element_text(size=8), axis.title = element_text(size=9),legend.title =element_text(size = 10), legend.text=element_text(size = 10),plot.title = element_text( size=8, face="bold.italic"), plot.subtitle = element_text(size=8))+ 
  coord_sf(crs=st_crs(3005),xlim = c(1160000 , 1230000), ylim = c(380000, 440000), expand = FALSE) #xlim = c(1036778.046114 , 1225730.937177), ylim = c(331246.297141, 446991.579926), expand = FALSE)

pl1_Sep <- ggplot() + 
  geom_sf(data=Sep_D7_NAD83_BCAlbers.shp, aes(fill = "70%"), color=NA)+ geom_sf(data=Sep_D8_NAD83_BCAlbers.shp, aes(fill = "80%"), colour ="orange")+
  geom_sf(data=Sep_D9_NAD83_BCAlbers.shp,aes(fill = "90%"), colour ="red")+
  geom_sf(data = CHS_crop_simp, fill = "#c2c2c2", show.legend = FALSE) + 
  scale_fill_manual(values = c("70%" = "yellow", "80%"= "orange", "90%"="red"), 
                    guide = guide_legend(override.aes = list(linetype = "blank",size=5, shape = NA))) +  
  ggtitle("September") + labs( fill="Exceedance")+#, y="Latitude", x="Longitude")+
  theme(axis.text = element_text(size=8), axis.title = element_text(size=9),legend.title =element_text(size = 10), legend.text=element_text(size = 10),plot.title = element_text( size=8, face="bold.italic"), plot.subtitle = element_text(size=8))+ 
  coord_sf(crs=st_crs(3005),xlim = c(1160000 , 1230000), ylim = c(380000, 440000), expand = FALSE)#+annotation_scale(location = "bl", width_hint = 0.2) #xlim = c(1036778.046114 , 1225730.937177), ylim = c(331246.297141, 446991.579926), expand = FALSE)

pl1_Oct <- ggplot() + 
  geom_sf(data=Oct_D7_NAD83_BCAlbers.shp, aes(fill = "70%"), color=NA)+ geom_sf(data=Oct_D8_NAD83_BCAlbers.shp, aes(fill = "80%"), colour ="orange")+
  geom_sf(data=Oct_D9_NAD83_BCAlbers.shp,aes(fill = "90%"), colour ="red")+
  geom_sf(data = CHS_crop_simp, fill = "#c2c2c2", show.legend = FALSE) + 
  scale_fill_manual(values = c("70%" = "yellow", "80%"= "orange", "90%"="red"), 
                    guide = guide_legend(override.aes = list(linetype = "blank", size=5,shape = NA))) +  
  ggtitle( "October") +labs( fill="Exceedance")+#, y="Latitude", x="Longitude")+
  theme(axis.text = element_text(size=8), axis.title = element_text(size=9),legend.title =element_text(size = 10), legend.text=element_text(size = 10),plot.title = element_text( size=8, face="bold.italic"), plot.subtitle = element_text(size=8))+ 
  coord_sf(crs=st_crs(3005),xlim = c(1160000 , 1230000), ylim = c(380000, 440000), expand = FALSE) #xlim = c(1036778.046114 , 1225730.937177), ylim = c(331246.297141, 446991.579926), expand = FALSE)

setwd(here("Outputs","Coocurrence"))
multiplot(pl1_May,pl1_Jun,pl1_Jul,pl1_Aug,pl1_Sep, pl1_Oct, layout = matrix(c(1,2,3,4,5,6),nrow = 3, byrow = T))
a<-ggarrange(pl1_May,pl1_Jun,pl1_Jul,pl1_Aug,pl1_Sep, pl1_Oct, common.legend=T, ncol = 2, nrow = 3)
a<-annotate_figure(a,bottom = text_grob("Longitude",),left = text_grob("Lattitude", rot = 90))#,top = text_grob("SRKW 90% Probability of Occurrence", color = "black", face = "bold", size = 14), )
a
ggsave(a,file="AP_SRKW_OccuranceAM2.png",width = 28, height = 22, units = "cm")
#, labels=c("May","June","July","August",'September', 'October'))

#####################################################################               

par(mfrow=c(2,3))
plot(pl1_May)
library(cowplot)
ggdraw() +
  draw_plot(pl1_May, 0, 2/3, 4/11, 4/11) +
  draw_plot(pl1_Jun, 0.5, 2/3, 4/11, 4/11) +
  draw_plot(pl1_Jul, 0, 1/3, 4/11, 4/11) +
  draw_plot(pl1_Aug, 0.5, 1/3, 4/11, 4/11) +
  draw_plot(pl1_Sep, 0, 0, 4/11, 4/11) +
  draw_plot(pl1_Oct, 0.5, 0, 4/11, 4/11) 

png(filename = "Myplot.png", width = 28, height = 20, units = "cm", pointsize =12, bg = "white", res = 360, restoreConsole = TRUE)
multiplot(pl1_May,pl1_Jun,pl1_Jul,pl1_Aug,pl1_Sep, pl1_Oct, cols=2)
dev.off()

# Save plots to tiff. Makes a separate file for each plot.
l<-list(pl1_May,pl1_Jun,pl1_Jul,pl1_Aug,pl1_Sep, pl1_Oct)
names(l)<- c("May SRKW Occurrence", "June SRKW Occurrence","July SRKW Occurrence",
             "August SRKW Occurrence","September SRKW Occurrence","October SRKW Occurrence")
for (i in 1:6){
  ggsave(plot = l[[i]], file = paste(names (l)[i],".png", sep=""))
}



ggsave(mpl,file="SRKW_OccuranceAM.png",width = 28, height = 20, units = "cm")
plot_list<-list(pl1_May,pl1_Jun,pl1_Jul,pl1_Aug,pl1_Sep, pl1_Oct)
multiplot(plotlist = plot_list, cols = 2, layout = matrix(c(1:6), nrow=3, ncol =2, byrow = T))
ggsave("SRKW_OccuranceAM.pdf")    #Only saves p2
pdf("test2.pdf")
multiplot(plotlist = plot_list, cols = 2, layout = matrix(c(1:6), nrow=3, ncol =2, byrow = T))
dev.off()                




#############
#Repeat for the by month data


setwd(here("Data","Results","SRKW_Occurrence", "ByMonth","Seventy"))
tmp7 <- SpatialPixelsDataFrame(results$combined_results$pixels_file,
                                data.frame(results$combined_results$Post_Bigger_D7_Monthly))

setwd(here("Data","Results","SRKW_Occurrence", "ByMonth","Eighty"))
tmp8 <- SpatialPixelsDataFrame(results$combined_results$pixels_file,
                               data.frame(results$combined_results$Post_Bigger_D8_Monthly))

setwd(here("Data","Results","SRKW_Occurrence", "ByMonth","Ninety"))
tmp9 <- SpatialPixelsDataFrame(results$combined_results$pixels_file,
                               data.frame(results$combined_results$Post_Bigger_D9_Monthly))

###Skip this if not recreating rasters from model results

for(i in 1:6){
  r <- create.raster(tmp7, i)
  name <- paste0(month.abb[i+4], "_D", substr(deparse(quote(tmp7)), nchar(deparse(quote(tmp7))),nchar(deparse(quote(tmp7))))) #May_D9
  assign(name, r)
  poly <- exceedance_raster_to_polygon_outline(r, 0.9)
  save_poly(poly)
}



library(maptools)

# get all files with the .shp extension from working directory
setwd(here("Data","Results","SRKW_Occurrence","ByMonth","Shapefiles_BM"))
r<-st_read("May_D7_NAD83_BCAlbers.shp")

shps <- dir(getwd(), "*.shp")
# the assign function will take the string representing shp and turn it into a variable
# which holds the spatial points data
for (shp in shps) {
  assign(shp, st_read(shp))#,proj4string=CRS('+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0' )))
}


pl1_Oct <- ggplot() + 
  geom_sf(data=Oct_D7_NAD83_BCAlbers.shp, aes(fill = "70%"), color=NA)+ geom_sf(data=Oct_D8_NAD83_BCAlbers.shp, aes(fill = "80%"), colour ="orange")+
  geom_sf(data=Oct_D9_NAD83_BCAlbers.shp,aes(fill = "90%"), colour ="red")+
  geom_sf(data = CHS_crop_simp, fill = "#c2c2c2", show.legend = FALSE) + 
  scale_fill_manual(values = c("70%" = "yellow", "80%"= "orange", "90%"="red"), 
                    guide = guide_legend(override.aes = list(linetype = "blank", size=5,shape = NA))) +  
  ggtitle( "October") +labs( fill="Exceedance")+#, y="Latitude", x="Longitude")+
  theme(axis.text = element_text(size=8), axis.title = element_text(size=9),legend.title =element_text(size = 10), legend.text=element_text(size = 10),plot.title = element_text( size=8, face="bold.italic"), plot.subtitle = element_text(size=8))+ 
  coord_sf(crs=st_crs(3005),xlim = c(990000 , 1270000), ylim = c(350000, 495000), expand = FALSE) #xlim = c(1036778.046114 , 1225730.937177), ylim = c(331246.297141, 446991.579926), expand = FALSE)


pl1_May <- ggplot() + 
  geom_sf(data=May_D7_NAD83_BCAlbers.shp, aes(fill = "70%"), color=NA)+ geom_sf(data=May_D8_NAD83_BCAlbers.shp, aes(fill = "80%"), colour ="orange")+
  geom_sf(data=May_D9_NAD83_BCAlbers.shp,aes(fill = "90%"), colour ="red")+
  geom_sf(data = CHS_crop_simp, fill = "#c2c2c2", show.legend = FALSE) + 
  scale_fill_manual(values = c("70%" = "yellow", "80%"= "orange", "90%"="red"), 
  guide = guide_legend(override.aes = list(linetype = "blank",size=5, shape = NA))) +  
  ggtitle("May") + labs( fill="Exceedance")+#, y="Latitude", x="Longitude")+
  theme(axis.text = element_text(size=8), axis.title = element_text(size=9),legend.title =element_text(size =10), legend.text=element_text(size = 10),plot.title = element_text( size=8, face="bold.italic"), plot.subtitle = element_text(size=8))+ 
  coord_sf(crs=st_crs(3005),xlim = c(990000 , 1270000), ylim = c(350000, 495000), expand = FALSE) #xlim = c(1036778.046114 , 1225730.937177), ylim = c(331246.297141, 446991.579926), expand = FALSE)

pl1_Jun <- ggplot() + 
  geom_sf(data=Jun_D7_NAD83_BCAlbers.shp, aes(fill = "70%"), color=NA)+ geom_sf(data=Jun_D8_NAD83_BCAlbers.shp, aes(fill = "80%"), colour ="orange")+
  geom_sf(data=Jun_D9_NAD83_BCAlbers.shp,aes(fill = "90%"), colour ="red")+
  geom_sf(data = CHS_crop_simp, fill = "#c2c2c2", show.legend = FALSE) + 
  scale_fill_manual(values = c("70%" = "yellow", "80%"= "orange", "90%"="red"), 
  guide = guide_legend(override.aes = list(linetype = "blank",size=5, shape = NA))) +  
  ggtitle("June") +  labs( fill="Exceedance")+#, y="Latitude", x="Longitude")+
  theme(axis.text = element_text(size=8), axis.title = element_text(size=9),legend.title =element_text(size =10), legend.text=element_text(size = 10),plot.title = element_text( size=8, face="bold.italic"), plot.subtitle = element_text(size=8))+ 
  coord_sf(crs=st_crs(3005),xlim = c(990000 , 1270000), ylim = c(350000, 495000), expand = FALSE) #xlim = c(1036778.046114 , 1225730.937177), ylim = c(331246.297141, 446991.579926), expand = FALSE)

pl1_Jul <- ggplot() + 
  geom_sf(data=Jul_D7_NAD83_BCAlbers.shp, aes(fill = "70%"), color=NA)+ geom_sf(data=Jul_D8_NAD83_BCAlbers.shp, aes(fill = "80%"), colour ="orange")+
  geom_sf(data=Jul_D9_NAD83_BCAlbers.shp,aes(fill = "90%"), colour ="red")+
  geom_sf(data = CHS_crop_simp, fill = "#c2c2c2", show.legend = FALSE) + 
  scale_fill_manual(values = c("70%" = "yellow", "80%"= "orange", "90%"="red"), 
  guide = guide_legend(override.aes = list(linetype = "blank",size=5, shape = NA))) +  
  ggtitle("July") +   labs( fill="Exceedance")+#, y="Latitude", x="Longitude")+
  theme(axis.text = element_text(size=8), axis.title = element_text(size=9),legend.title =element_text(size =10), legend.text=element_text(size = 10),plot.title = element_text( size=8, face="bold.italic"), plot.subtitle = element_text(size=8))+ 
  coord_sf(crs=st_crs(3005),xlim = c(990000 , 1270000), ylim = c(350000, 495000), expand = FALSE) #xlim = c(1036778.046114 , 1225730.937177), ylim = c(331246.297141, 446991.579926), expand = FALSE)

pl1_Aug <- ggplot() + 
  geom_sf(data=Aug_D7_NAD83_BCAlbers.shp, aes(fill = "70%"), color=NA)+ geom_sf(data=Aug_D8_NAD83_BCAlbers.shp, aes(fill = "80%"), colour ="orange")+
  geom_sf(data=Aug_D9_NAD83_BCAlbers.shp,aes(fill = "90%"), colour ="red")+
  geom_sf(data = CHS_crop_simp, fill = "#c2c2c2", show.legend = FALSE) + 
  scale_fill_manual(values = c("70%" = "yellow", "80%"= "orange", "90%"="red"), 
  guide = guide_legend(override.aes = list(linetype = "blank",size=5, shape = NA))) +  
  ggtitle("August") +  labs( fill="Exceedance")+#, y="Latitude", x="Longitude")+
  theme(axis.text = element_text(size=8), axis.title = element_text(size=9),legend.title =element_text(size =10), legend.text=element_text(size = 10),plot.title = element_text( size=8, face="bold.italic"), plot.subtitle = element_text(size=8))+ 
  coord_sf(crs=st_crs(3005),xlim = c(990000 , 1270000), ylim = c(350000, 495000), expand = FALSE) #xlim = c(1036778.046114 , 1225730.937177), ylim = c(331246.297141, 446991.579926), expand = FALSE)

pl1_Sep <- ggplot() + 
  geom_sf(data=Sep_D7_NAD83_BCAlbers.shp, aes(fill = "70%"), color=NA)+ geom_sf(data=Sep_D8_NAD83_BCAlbers.shp, aes(fill = "80%"), colour ="orange")+
  geom_sf(data=Sep_D9_NAD83_BCAlbers.shp,aes(fill = "90%"), colour ="red")+
  geom_sf(data = CHS_crop_simp, fill = "#c2c2c2", show.legend = FALSE) + 
  scale_fill_manual(values = c("70%" = "yellow", "80%"= "orange", "90%"="red"), 
  guide = guide_legend(override.aes = list(linetype = "blank",size=5, shape = NA))) +  
  ggtitle("September") + labs( fill="Exceedance")+#, y="Latitude", x="Longitude")+
  theme(axis.text = element_text(size=8), axis.title = element_text(size=9),legend.title =element_text(size =10), legend.text=element_text(size = 10),plot.title = element_text( size=8, face="bold.italic"), plot.subtitle = element_text(size=8))+ 
  coord_sf(crs=st_crs(3005),xlim = c(990000 , 1270000), ylim = c(350000, 495000), expand = FALSE)+annotation_scale(location = "bl", width_hint = 0.2) #xlim = c(1036778.046114 , 1225730.937177), ylim = c(331246.297141, 446991.579926), expand = FALSE)

pl1_Oct <- ggplot() + 
  geom_sf(data=Oct_D7_NAD83_BCAlbers.shp, aes(fill = "70%"), color=NA)+ geom_sf(data=Oct_D8_NAD83_BCAlbers.shp, aes(fill = "80%"), colour ="orange")+
  geom_sf(data=Oct_D9_NAD83_BCAlbers.shp,aes(fill = "90%"), colour ="red")+
  #gg(COAST_plotting)
  geom_sf(data = CHS_crop_simp, fill = "#c2c2c2", show.legend = FALSE) +  
  scale_fill_manual(values = c("70%" = "yellow", "80%"= "orange", "90%"="red"), 
  guide = guide_legend(override.aes = list(linetype = "blank",size=5, shape = NA))) +  
  ggtitle("October") +   labs( fill="Exceedance")+#, y="Latitude", x="Longitude")+
  theme(axis.text = element_text(size=8), axis.title = element_text(size=9),legend.title =element_text(size =10), legend.text=element_text(size = 10),plot.title = element_text( size=8, face="bold.italic"), plot.subtitle = element_text(size=8))+ 
  coord_sf(crs=st_crs(3005),xlim = c(990000 , 1270000), ylim = c(350000, 495000), expand = FALSE) #xlim = c(1036778.046114 , 1225730.937177), ylim = c(331246.297141, 446991.579926), expand = FALSE)

  
setwd(here("Outputs","Coocurrence"))
a<-ggarrange(pl1_May,pl1_Jun,pl1_Jul,pl1_Aug,pl1_Sep, pl1_Oct, common.legend=T, ncol = 2, nrow = 3)
a<-annotate_figure(a,bottom = text_grob("Longitude",),left = text_grob("Lattitude", rot = 90))#,top = text_grob("SRKW 90% Probability of Occurrence", color = "black", face = "bold", size = 14), )
a
ggsave(a,file="SRKW_OccuranceBM2.png",width = 28, height = 22, units = "cm")

#save as a multiplot 
png(filename = "Myplot.png", width = 28, height = 20, units = "cm", pointsize =12, bg = "white", res = 300, restoreConsole = TRUE)
multiplot(pl1_May,pl1_Jun,pl1_Jul,pl1_Aug,pl1_Sep, pl1_Oct, cols=2)
dev.off()

# Save plots to tiff. Makes a separate file for each plot.
l<-list(pl1_May,pl1_Jun,pl1_Jul,pl1_Aug,pl1_Sep, pl1_Oct)
names(l)<- c("May SRKW Occurrence by Month", "June SRKW Occurrence by Month","July SRKW Occurrence by Month",
             "August SRKW Occurrence by Month","September SRKW Occurrence by Month","October SRKW Occurrence by Month")
for (i in 1:6){
      ggsave(plot = l[[i]], file = paste(names (l)[i],".png", sep=""))
  }




setwd(here("Data","Results","SRKW_Occurrence","AcrossMonth","REgional SRKW Polygons_AM"))
setwd(here("Data","Results","SRKW_Occurrence","ByMonth",  "Regional SRKW Polygons_BM"))
shps <- dir(getwd(), "*.shp")
# the assign function will take the string representing shp and turn it into a variable
# which holds the spatial points data
for (shp in shps) {
  assign(shp, st_read(shp))#,proj4string=CRS('+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0' )))
}


#################################################################



#Clip the polygons by regional shapefiles
EPSGcode<-'3005'

setwd(here("Data","Results","RegionalBoundaries"))
Boundary<-st_read("Boundary.shp")#%>% st_transform(EPSGcode) %>% as('Spatial')
crs(Boundary)
#Boundary<-st_transform(Boundary, crs = '+proj=aea +lat_0=45 +lon_0=-126 +lat_1=50 +lat_2=58.5 +x_0=1000000 +y_0=0 +datum=NAD83 +units=km +no_defs +ellps=GRS80 +towgs84=0,0,0')
Oct_D7_NAD83_BCAlbers.shp<-st_transform(Oct_D7_NAD83_BCAlbers.shp,EPSGcode)


# get all files with the .shp extension from working directory
setwd(here("Data","Results","SRKW_Occurrence","AcrossMonth","Regional SRKW Polygons_AM"))
setwd(here("Data","Results","SRKW_Occurrence","ByMonth","Shapefiles_BM"))

shps <- dir(getwd(), "*.shp")
# the assign function will take the string representing shp and turn it into a variable
# which holds the spatial points data

for (shp in shps) {
  s<-assign(shp, st_read(shp)%>%st_transform(3005))
  c <- st_intersection( Boundary, s)
  file.name=paste0("Clip_",shp)
  st_write(c, dsn = getwd(), layer = file.name, driver = "ESRI Shapefile", quite=T,delete_dsn=F,delete_layer=T,append=FALSE)
        }

#COAST_simp = readRDS('COAST_simp.rds') %>% st_as_sf() %>% st_transform(3005) %>% st_transform(EPSGcode) %>% as('Spatial')
#st_write(dat_xy, dsn = FILEPATH, layer = SHP_FILE_NAME, driver = "ESRI Shapefile", quiet = TRUE, delete_dsn = FALSE, delete_layer = TRUE, append = FALSE)

# ...done


#ECHOLOCATION Load NetCDF files and save as raster files 
setwd(here("Data","Sound","Existing","Echolocation"))
rlist=list.files(getwd(), pattern="*.nc", full.names=FALSE)
for(i in rlist) { 
  c<-assign(unlist(strsplit(i, "[.]"))[1], raster(i))
  outputFile <- paste(i,'.tif',sep='')
  writeRaster(c,  filename=outputFile, bylayer=TRUE, driver='GTiff')
}

#COMMUNICATION Load NetCDF files and save as raster files 
setwd(here("Data","Sound","Existing","Communication"))
rlist=list.files(getwd(), pattern="*.nc", full.names=FALSE)
for(i in rlist) { 
  c<-assign(unlist(strsplit(i, "[.]"))[1], raster(i))
  outputFile <- paste(i,'.tif',sep='')
  writeRaster(c,  filename=outputFile, bylayer=TRUE, driver='GTiff')
}

#WIND_ECHOLOCATION Load NetCDF files and save as raster files 
setwd(here("Data","Sound","Wind","EcholocationrangeFromWind_netcdf"))
rlist=list.files(getwd(), pattern="*.nc", full.names=FALSE)
for(i in rlist) { 
  c<-assign(unlist(strsplit(i, "[.]"))[1], raster(i))
  outputFile <- paste(i,'.tif',sep='')
    writeRaster(c,  filename=outputFile, bylayer=TRUE, driver='GTiff')
}


#WIND_COMMUNICATION Load NetCDF files and save as raster files 
setwd(here("Data","Sound","Wind","CommunicationrangeFromWind_netcdf"))
rlist=list.files(getwd(), pattern="*.nc", full.names=FALSE)
for(i in rlist) { 
  c<-assign(unlist(strsplit(i, "[.]"))[1], raster(i))
  outputFile <- paste(i,'.tif',sep='')
  writeRaster(c,  filename=outputFile, bylayer=TRUE, driver='GTiff')
}



#setwd(here("Data","Results","SRKW_Occurence", "AcrossMonth","Regional SRKW Polygons_AM"))
#plist=list.files(getwd(), pattern="*.shp$")
#setwd(here("Data","Sound","WorkingFiles","Echolocation", "Raster"))
#rlist=list.files(getwd(), pattern="tif")

#Load clipped Raster for 
setwd(here("Data","Sound","Existing","Echolocation","Raster","Clip"))
rlist=list.files(getwd(), pattern="*.tif", full.names=F)
for(i in rlist) { 
  assign(i, raster(i))
}

p.months <- c("May","Jun","Jul","Aug","Sep","Oct")
p.exc <- c("D7","D8","D9")
r.months <- c("May","June",'July',"August","September","October")
r.depths <- c("Depth10","Depth100")


for(m in 1:length(p.months)){
  dat = data.frame(Depth=sort(rep(r.depths,6)), Decile = c(seq(0,1.0,0.2)), D7.Swiftsure=rep(NA,6), D8.Swiftsure=rep(NA,6), D9.Swiftsure=rep(NA,6), D7.Haro=rep(NA,6), D8.Haro=rep(NA,6), D9.Haro=rep(NA,6))
  for(d in r.depths){
    rast <- mget(paste0("Q50_", r.months[m],' 2018_', d,"Clip.tif"))[[1]] %>%
      projectRaster(crs = CRS("+proj=aea +lat_0=45 +lon_0=-126 +lat_1=50 +lat_2=58.5 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
    for(ex in p.exc){
      shp <- mget(ls(pattern = paste0("Clip_",p.months[m],"_",ex,"_NAD83_BCAlbers.shp")))[[1]]
      sub.dat <- extract(rast, shp) #vectors of raster values corresponding to each polygon
      names(sub.dat) <- shp$Region # name each vector according to REGION
      for(reg in unique(shp$Region)){
        vals <- as.data.frame(c(unlist(sub.dat[which(names(sub.dat)==reg)])))
        names(vals) <- "values"
        vals$bins <- cut(vals$values, breaks = c(-0.1,  20, 40, 60, 80,100))
        vals$bins<-as.character(vals$bins)
        conv.df = data.frame(fbin = c("(-0.1,20]","(20,40]","(40,60]","(60,80]","(80,100]"), cbin = c("20","40","60","80","100"))
        vals = merge(vals, conv.df, by.x = "bins", by.y = "fbin", all.x = TRUE, all.y = FALSE)
        summ <- as.data.frame(table(vals$cbin))
        test <- data.frame(Var1 = as.character(c(0,20,40,60,80,100))) %>% left_join(., summ, by = "Var1") 
        if(any(is.na(test$Freq))){ test[which(is.na(test$Freq)),]$Freq <- 0 }
        test$prop <- test$Freq/sum(test$Freq)
        col <- grep(paste0(ex, ".",reg), names(dat))
        dat[which(dat$Depth==d),col] <- test$prop
        print(paste("Region", reg, "complete"))
      }
      print(paste("Exceedance", ex, "complete"))  
    }
    print(paste("Depth", d, "complete"))
  }
 dat$MONTH <- r.months[m]
 if(m==1){
   Echo_region_summary = dat
 } else {
   Echo_region_summary = rbind(Echo_region_summary,dat)
 }
 print(paste("Month", r.months[m], "complete"))
}

setwd(here("Outputs","Coocurrence")) 
tail(Echo_region_summary)
saveRDS(Echo_region_summary,"Echo_region_summaryBM.rds")
Echo_region_summary<-readRDS('Echo_region_summary.rds')


Echo_region_summary$MONTH <- factor(Echo_region_summary$MONTH, levels = c('May', 'June', 'July','August','September','October'))  
# Stacked Bar Plots
D9SE<-ggplot(Echo_region_summary, aes(fill=factor(Decile), y=D9.Swiftsure, x=MONTH)) + facet_wrap(~Depth) +
  geom_bar(position="stack", stat="identity")+scale_fill_manual(values=c("#e31a1c","#e31a1c","#fdbf6f",'#ffffb3', "#a6cee3", "#1f78b4"  ))
D8SE<-ggplot(Echo_region_summary, aes(fill=factor(Decile), y=D8.Swiftsure, x=MONTH)) + facet_wrap(~Depth) +
  geom_bar(position="stack", stat="identity")+scale_fill_manual(values=c("#e31a1c","#e31a1c","#fdbf6f",'#ffffb3', "#a6cee3", "#1f78b4"  ))
D7SE<-ggplot(Echo_region_summary, aes(fill=factor(Decile), y=D7.Swiftsure, x=MONTH)) + facet_wrap(~Depth) +
  geom_bar(position="stack", stat="identity")+scale_fill_manual(values=c("#e31a1c","#e31a1c","#fdbf6f",'#ffffb3', "#a6cee3", "#1f78b4"  ))
multiplot(D9E,D9Eb)#,D7E)

D9HE<-ggplot(Echo_region_summary, aes(fill=factor(Decile), y=D9.Haro, x=MONTH)) +  facet_wrap(~Depth) +
  geom_bar(position="stack", stat="identity")+scale_fill_manual(values=c("#e31a1c","#e31a1c","#fdbf6f",'#ffffb3', "#a6cee3", "#1f78b4" ))
D8HE<-ggplot(Echo_region_summary, aes(fill=factor(Decile), y=D8.Haro, x=MONTH)) +  facet_wrap(~Depth) +
  geom_bar(position="stack", stat="identity")+scale_fill_manual(values=c("#e31a1c","#e31a1c","#fdbf6f",'#ffffb3', "#a6cee3", "#1f78b4" ))
D7HE<-ggplot(Echo_region_summary, aes(fill=factor(Decile), y=D8.Haro, x=MONTH)) +  facet_wrap(~Depth) +
  geom_bar(position="stack", stat="identity")+scale_fill_manual(values=c("#e31a1c","#e31a1c","#fdbf6f",'#ffffb3', "#a6cee3", "#1f78b4" ))
ggarrange(D7SE,D8SE,D9SE, D7HE,D8HE,D9HE, common.legend = T, ncol=2,nrow=4)
ggarrange(D7SE, D7SC, common.legend = T, ncol=2,nrow=1)
# PieCharts

D9E<-ggplot(Echo_region_summary, aes(fill=factor(Decile), y=value, x=MONTH)) + facet_wrap(~Exceedance) +
  geom_bar(stat="identity")+coord_polar("y", start=0)+
  scale_fill_manual(values=c("#e31a1c","#e31a1c","#fdbf6f","#33a02c", "#a6cee3", "#1f78b4"  ))
D8E<-ggplot(Echo_region_summary, aes(fill=factor(Decile), y=D8.Swiftsure, x=MONTH)) + facet_wrap(~Depth) +
  geom_bar( stat="identity")+coord_polar("y", start=0)+scale_fill_manual(values=c("#e31a1c","#e31a1c","#fdbf6f","#33a02c", "#a6cee3", "#1f78b4"  ))
D7E<-ggplot(Echo_region_summary, aes(fill=factor(Decile), y=D7.Swiftsure, x=MONTH)) + facet_wrap(~Depth) +
  geom_bar( stat="identity")+coord_polar("y", start=0)+scale_fill_manual(values=c("#e31a1c","#e31a1c","#fdbf6f","#33a02c", "#a6cee3", "#1f78b4"  ))
multiplot(D7E,D8E,D9E)
plot(D7E,D8E,D9E,D7H,D8H,D9H)

D9H<-ggplot(Com_region_summary, aes(fill=factor(Decile), y=D9.Haro, x=MONTH)) +  facet_wrap(~Depth) +
  geom_bar(stat="identity")+coord_polar("y", start=0)
D8H<-ggplot(Com_region_summary, aes(fill=factor(Decile), y=D8.Haro, x=MONTH)) +  facet_wrap(~Depth) +
  geom_bar(stat="identity")+coord_polar("y", start=0)
D7H<-ggplot(Com_region_summary, aes(fill=factor(Decile), y=D8.Haro, x=MONTH)) +  facet_wrap(~Depth) +
  geom_bar(stat="identity")+coord_polar("y", start=0)
multiplot(D7H,D8H,D9H)

#COMMUNICATION Load NetCDF files and save as raster files 
setwd(here("Data","Sound","Existing","Communication","Raster","Clip"))
rlist=list.files(getwd(), pattern="*.tif", full.names=F)
for(i in rlist) { 
  assign(i, raster(i))
}


#Load NetCDF files and save as raster files 
setwd(here("Data","Sound","Existing","Communication","Raster","Clip"))
rlist=list.files(getwd(), pattern="*.nc", full.names=FALSE)
for(i in rlist) { 
  c<-assign(unlist(strsplit(i, "[.]"))[1], raster(i))
  outputFile <- paste(i,'.tif',sep='')
#  writeRaster(c,  filename=outputFile, bylayer=TRUE, driver='GTiff')
}

p.months <- c("May","Jun","Jul","Aug","Sep","Oct")
p.exc <- c("D7","D8","D9")
r.months <- c("May","June",'July',"August","September","October")
r.depths <- c("Depth10","Depth100")

for(m in 1:length(p.months)){
  dat = data.frame(Depth=sort(rep(r.depths,6)), Decile = c(seq(0,1.0,0.2)), D7.Swiftsure=rep(NA,6), D8.Swiftsure=rep(NA,6), D9.Swiftsure=rep(NA,6), D7.Haro=rep(NA,6), D8.Haro=rep(NA,6), D9.Haro=rep(NA,6))
  for(d in r.depths){
    rast <- mget(paste0("Q50_", r.months[m],' 2018_', d,"Clip.tif"))[[1]] %>%
      projectRaster(crs = CRS("+proj=aea +lat_0=45 +lon_0=-126 +lat_1=50 +lat_2=58.5 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
    for(ex in p.exc){
      shp <- mget(ls(pattern = paste0("Clip_",p.months[m],"_",ex,"_NAD83_BCAlbers.shp")))[[1]]
      sub.dat <- extract(rast, shp) #vectors of raster values corresponding to each polygon
      names(sub.dat) <- shp$Region # name each vector according to REGION
      for(reg in unique(shp$Region)){
        vals <- as.data.frame(c(unlist(sub.dat[which(names(sub.dat)==reg)])))
        names(vals) <- "values"
        vals$bins <- cut(vals$values, breaks = c(-0.1,  20, 40, 60, 80,100))
        vals$bins<-as.character(vals$bins)
        conv.df = data.frame(fbin = c("(-0.1,20]","(20,40]","(40,60]","(60,80]","(80,100]"), cbin = c("20","40","60","80","100"))
        vals = merge(vals, conv.df, by.x = "bins", by.y = "fbin", all.x = TRUE, all.y = FALSE)
        summ <- as.data.frame(table(vals$cbin))
        test <- data.frame(Var1 = as.character(c(0,20,40,60,80,100))) %>% left_join(., summ, by = "Var1") 
        if(any(is.na(test$Freq))){ test[which(is.na(test$Freq)),]$Freq <- 0 }
        test$prop <- test$Freq/sum(test$Freq)
        col <- grep(paste0(ex, ".",reg), names(dat))
        dat[which(dat$Depth==d),col] <- test$prop
        print(paste("Region", reg, "complete"))
      }
      print(paste("Exceedance", ex, "complete"))  
    }
    print(paste("Depth", d, "complete"))
  }
  dat$MONTH <- r.months[m]
  if(m==1){
    Com_region_summary = dat
  } else {
    Com_region_summary = rbind(Com_region_summary,dat)
  }
  print(paste("Month", r.months[m], "complete"))
}


comm = reshape2::melt(comm, id = c("MONTH","Depth","Decile"))
comm$Exceedance <- unlist(purrr::map(strsplit(as.character(comm$variable),"\\."), 1))
comm$Region <- unlist(purrr::map(strsplit(as.character(comm$variable),"\\."), 2))
comm$variable <- NULL

tail(Com_region_summary)
setwd(here("Outputs")) 
saveRDS(Com_region_summary,"Com_region_summaryBM.rds")
Com_region_summary$MONTH <- factor(Com_region_summary$MONTH, levels = c('May', 'June', 'July','August','September','October'))  
# Stacked Bar Plots
D9SC<-ggplot(Com_region_summary, aes(fill=factor(Decile), y=D9.Swiftsure, x=MONTH)) + facet_wrap(~Depth) +
  geom_bar(position="stack", stat="identity")+scale_fill_manual(values=c("#e31a1c","#e31a1c","#fdbf6f",'#ffffb3', "#a6cee3", "#1f78b4"  ))
D8SC<-ggplot(Com_region_summary, aes(fill=factor(Decile), y=D8.Swiftsure, x=MONTH)) + facet_wrap(~Depth) +
  geom_bar(position="stack", stat="identity")+scale_fill_manual(values=c("#e31a1c","#e31a1c","#fdbf6f",'#ffffb3', "#a6cee3", "#1f78b4"  ))
D7SC<-ggplot(Com_region_summary, aes(fill=factor(Decile), y=D7.Swiftsure, x=MONTH)) + facet_wrap(~Depth) +
  geom_bar(position="stack", stat="identity")+scale_fill_manual(values=c("#e31a1c","#e31a1c","#fdbf6f",'#ffffb3', "#a6cee3", "#1f78b4" ))
ggarrange(D7E,D8E,D9E, common.legend = T)

D9HC<-ggplot(Com_region_summary, aes(fill=factor(Decile), y=D9.Haro, x=MONTH)) +  facet_wrap(~Depth) +
  geom_bar(position="stack", stat="identity")+scale_fill_manual(values=c("#e31a1c","#e31a1c","#fdbf6f",'#ffffb3', "#a6cee3", "#1f78b4" ))
D8HC<-ggplot(Com_region_summary, aes(fill=factor(Decile), y=D8.Haro, x=MONTH)) +  facet_wrap(~Depth) +
  geom_bar(position="stack", stat="identity")+scale_fill_manual(values=c("#e31a1c","#e31a1c","#fdbf6f",'#ffffb3', "#a6cee3", "#1f78b4" ))
D7HC<-ggplot(Com_region_summary, aes(fill=factor(Decile), y=D8.Haro, x=MONTH)) +  facet_wrap(~Depth) +
  geom_bar(position="stack", stat="identity")+scale_fill_manual(values=c("#e31a1c","#e31a1c","#fdbf6f",'#ffffb3', "#a6cee3", "#1f78b4" ))
ggarrange(D7E,D8E,D9E, D7H,D8H,D9H, common.legend = T, ncol=2,nrow=4)

# Grouped


D9E<-ggplot(Com_region_summary, aes(fill=factor(Decile), y=D9.Swiftsure, x=MONTH)) + facet_wrap(~Depth) +
  geom_bar(stat="identity")+coord_polar("y", start=0)+
  scale_fill_manual(values=c("#e31a1c","#e31a1c","#fdbf6f","#33a02c", "#a6cee3", "#1f78b4"  ))
  geom_text(aes(y = ypos, label = Decile), color = "white", size=6) 
D8E<-ggplot(Com_region_summary, aes(fill=factor(Decile), y=D8.Swiftsure, x=MONTH)) + facet_wrap(~Depth) +
  geom_bar( stat="identity")+coord_polar("y", start=0)+scale_fill_manual(values=c("#e31a1c","#e31a1c","#fdbf6f","#33a02c", "#a6cee3", "#1f78b4"  ))
D7E<-ggplot(Com_region_summary, aes(fill=factor(Decile), y=D7.Swiftsure, x=MONTH)) + facet_wrap(~Depth) +
  geom_bar( stat="identity")+coord_polar("y", start=0)+scale_fill_manual(values=c("#e31a1c","#e31a1c","#fdbf6f","#33a02c", "#a6cee3", "#1f78b4"  ))
ggarrange(D7E,D8E,D9E, common.legend = T,nrow=3)


D9H<-ggplot(Com_region_summary, aes(fill=factor(Decile), y=D9.Haro, x=MONTH)) +  facet_wrap(~Depth) +
  geom_bar(stat="identity")+coord_polar("y", start=0)
D8H<-ggplot(Com_region_summary, aes(fill=factor(Decile), y=D8.Haro, x=MONTH)) +  facet_wrap(~Depth) +
  geom_bar(stat="identity")+coord_polar("y", start=0)
D7H<-ggplot(Com_region_summary, aes(fill=factor(Decile), y=D8.Haro, x=MONTH)) +  facet_wrap(~Depth) +
  geom_bar(stat="identity")+coord_polar("y", start=0)
multiplot(D9H,D8H,D7H)
ggarrange(D7E,D8E,D9E, D7H,D8H,D9H, common.legend = T, ncol=2,nrow=4)


#Load clipped Raster for Wind Communication
setwd(here("Data","Sound","Wind","CommunicationrangeFromWind_netcdf","Raster","Clip"))
rlist=list.files(getwd(), pattern="*.tif", full.names=F)
for(i in rlist) { 
  assign(i, raster(i))
}

p.months <- c("May","Jun","Jul","Aug","Sep","Oct")
p.exc <- c("D7","D8","D9")
r.months <- c("May","June",'July',"August","September","October")
r.years <- c("2018","2019","2020")#,"Depth100")
y="2018"
m = 3
ex="D9"

for(m in 1:length(p.months)){
  dat = data.frame(Year=sort(rep(r.years,6)), Decile = c(seq(0,1.0,0.2)), D7.Swiftsure=rep(NA,6), D8.Swiftsure=rep(NA,6), D9.Swiftsure=rep(NA,6), D7.Haro=rep(NA,6), D8.Haro=rep(NA,6), D9.Haro=rep(NA,6))
  for(y in r.years){
    rast <- mget(paste0("ClipCommunicationFromMeanWinds_",y,"_", r.months[m],".nc.tif"))[[1]] %>%
      projectRaster(crs = CRS("+proj=aea +lat_0=45 +lon_0=-126 +lat_1=50 +lat_2=58.5 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"))
    for(ex in p.exc){
      shp <- mget(ls(pattern = paste0("Clip_",p.months[m],"_",ex,"_NAD83_BCAlbers.shp")))[[1]]
      sub.dat <- extract(rast, shp) #vectors of raster values corresponding to each polygon
      names(sub.dat) <- shp$Region # name each vector according to REGION
      for(reg in unique(shp$Region)){
        vals <- as.data.frame(c(unlist(sub.dat[which(names(sub.dat)==reg)])))
        names(vals) <- "values"
        vals$bins <- cut(vals$values, breaks = c(-0.1, 20, 40, 60, 80,100))
        vals$bins<-as.character(vals$bins)
        conv.df = data.frame(fbin = c("(-0.1,20]","(20,40]","(40,60]","(60,80]","(80,100]"), cbin = c("20","40","60","80","100"))
        vals = merge(vals, conv.df, by.x = "bins", by.y = "fbin", all.x = TRUE, all.y = FALSE)
        summ <- as.data.frame(table(vals$cbin))
        test <- data.frame(Var1 = as.character(c(0,20,40,60,80,100))) %>% left_join(., summ, by = "Var1") 
        if(any(is.na(test$Freq))){ test[which(is.na(test$Freq)),]$Freq <- 0 }
        test$prop <- test$Freq/sum(test$Freq)
        col <- grep(paste0(ex, ".",reg), names(dat))
        dat[which(dat$Year==y),col] <- test$prop
        print(paste("Region", reg, "complete"))
      }
      print(paste("Exceedance", ex, "complete"))  
    }
    print(paste("Depth", y, "complete"))
  }
  dat$MONTH <- r.months[m]
  if(m==1){
    Comwind_region_summary = dat
  } else {
    Comwind_region_summary = rbind(Comwind_region_summary,dat)
  }
  print(paste("Month", r.months[m], "complete"))
}

setwd(here("Outputs")) 
tail(Echo_region_summary)
saveRDS(Comwind_region_summary,"Comwind_region_summaryBM.rds")

Figure1base

Comwind_region_summary$MONTH <- factor(Comwind_region_summary$MONTH, levels = c('May', 'June', 'July','August','September','October'))  
# Stacked Bar Plots
D9E<-ggplot(Comwind_region_summary, aes(fill=factor(Decile), y=D9.Swiftsure, x=MONTH)) + facet_wrap(~Year) +
  geom_bar(position="stack", stat="identity")+scale_fill_manual(values=c("#e31a1c","#fdbf6f","#33a02c", "#a6cee3", "#1f78b4"  ))
D8E<-ggplot(Comwind_region_summary, aes(fill=factor(Decile), y=D8.Swiftsure, x=MONTH)) + facet_wrap(~Year) +
  geom_bar(position="stack", stat="identity")+scale_fill_manual(values=c("#e31a1c","#fdbf6f","#33a02c", "#a6cee3", "#1f78b4"  ))
D7E<-ggplot(Comwind_region_summary, aes(fill=factor(Decile), y=D7.Swiftsure, x=MONTH)) + facet_wrap(~Year) +
  geom_bar(position="stack", stat="identity")+scale_fill_manual(values=c("#e31a1c","#fdbf6f","#33a02c", "#a6cee3", "#1f78b4"  ))
ggarrange(D7E,D8E,D9E, common.legend = T,nrow=3)

D9H<-ggplot(Comwind_region_summary, aes(fill=factor(Decile), y=D9.Haro, x=MONTH)) +  facet_wrap(~Year) +
  geom_bar(position="stack", stat="identity")+scale_fill_manual(values=c("#e31a1c","#fdbf6f","#33a02c", "#a6cee3", "#1f78b4"  ))
D8H<-ggplot(Comwind_region_summary, aes(fill=factor(Decile), y=D8.Haro, x=MONTH)) +  facet_wrap(~Year) +
  geom_bar(position="stack", stat="identity")+scale_fill_manual(values=c("#e31a1c","#fdbf6f","#33a02c", "#a6cee3", "#1f78b4"  ))
D7H<-ggplot(Comwind_region_summary, aes(fill=factor(Decile), y=D8.Haro, x=MONTH)) +  facet_wrap(~Year) +
  geom_bar(position="stack", stat="identity")+scale_fill_manual(values=c("#e31a1c","#fdbf6f","#33a02c", "#a6cee3", "#1f78b4"  ))
multiplot(D9H,D8H,D7H)


D9E<-ggplot(Comwind_region_summary, aes(fill=factor(Decile), y=D9.Swiftsure, x=MONTH)) + facet_wrap(~Year) +
  geom_bar( stat="identity")+coord_polar("y", start=0)+scale_fill_manual(values=c("#e31a1c","#fdbf6f","#33a02c", "#a6cee3", "#1f78b4"  ))
geom_text(aes(y = ypos, label = Decile), color = "white", size=6) 
D8E<-ggplot(Comwind_region_summary, aes(fill=factor(Decile), y=D8.Swiftsure, x=MONTH)) + facet_wrap(~Year) +
  geom_bar( stat="identity")+coord_polar("y", start=0)+scale_fill_manual(values=c("#e31a1c","#fdbf6f","#33a02c", "#a6cee3", "#1f78b4"  ))
D7E<-ggplot(Comwind_region_summary, aes(fill=factor(Decile), y=D7.Swiftsure, x=MONTH)) + facet_wrap(~Year) +
  geom_bar( stat="identity")+coord_polar("y", start=0)+scale_fill_manual(values=c("#e31a1c","#fdbf6f","#33a02c", "#a6cee3", "#1f78b4"  ))
multiplot(D9E,D8E,D7E)
plot(D9E,D8E,D7E,D9H,D8H,D7H)

D9H<-ggplot(Com_region_summary, aes(fill=factor(Decile), y=D9.Haro, x=MONTH)) +  facet_wrap(~Depth) +
  geom_bar(stat="identity")+coord_polar("y", start=0)
D8H<-ggplot(Com_region_summary, aes(fill=factor(Decile), y=D8.Haro, x=MONTH)) +  facet_wrap(~Depth) +
  geom_bar(stat="identity")+coord_polar("y", start=0)
D7H<-ggplot(Com_region_summary, aes(fill=factor(Decile), y=D8.Haro, x=MONTH)) +  facet_wrap(~Depth) +
  geom_bar(stat="identity")+coord_polar("y", start=0)
multiplot(D9H,D8H,D7H)


#Load clipped Raster for 
setwd(here("Data","Sound","Existing","Echolocation","Raster","Clip"))
rlist=list.files(getwd(), pattern="*.tif", full.names=F)
for(i in rlist) { 
  assign(i, raster(i))
}





pol.list<-list()
shps <- dir(getwd(), "*.shp")
# the assign function will take the string representing shp and turn it into a variable
# which holds the spatial points data

for (shp in shps) {
  s<-assign(shp, st_read(shp)%>%st_transform(3005))
  c <- st_intersection( Boundary, s)
  #c<-gClip(shp = s, bb=Boundary)
  file.name=paste0("Clip_",shp)
}
  





# now use the mask function
rr <- mask(`AIS-Mandatory_05_2018-2020`, pol)
for(i in rlist) { assign(unlist(strsplit(i, "[.]"))[1], raster(i)) } 
# plot, and overlay:
plot(rr);plot(pol,add=TRUE)
plot(rr)
summary(rr)
quantile(rr)      
summary(`AIS-Mandatory_05_2018-2020`)
decile<-quantile(`AIS-Mandatory_05_2018-2020`,probs=c(0.90))
a<-cellStats(rr,'mean')
b<-cellStats(`AIS-Mandatory_05_2018-2020`,'sum')
a/b

####

###Clip the raster data for AIS total km's
setwd(here("Data","VESSELS","AIS"))
#dir<-file.path("Data/VESSELS/AIS/knots.x.hours/Clips")
# for loop with (in this case) 60 runs
files <- list.files(path="knots.x.hours",full.names=TRUE, pattern = "\\.tif$")
ind=(1:length(files))
for (i in ind) {
      # Reading the raster to crop
  files <- list.files(path="knots.x.hours",full.names=TRUE, pattern = "\\.tif$")
  Env_raster <- stack(files[i])
  # Save the filename
  filename <- (paste("Clip_", basename(files[i]), sep=""))
  # Putting NA values in all the raster cells outside the shapefile boundaries
  Maskshp.masked <- mask(x=Env_raster, mask=pol) 
  plot(Maskshp.masked)
  #Export file to working directory with orginal name as new name
  writeRaster(Maskshp.masked, filename,overwrite=T)
}

library(raster)

setwd(here())
#get the path
path <- (here("Data","VESSELS","AIS","knots.x.hours")) 
# Create list of NDVI file paths
files <- list.files(path, pattern = "tif$", full.names = TRUE)

# Create a time series raster stack
files_stack <- stack(files)
# calculate mean NDVI for each raster
avg_total_hours <- as.data.frame(cellStats(files_stack, mean))
names(avg_total_hours)[1]<-"Mean"
sum_total_hours <- as.data.frame(cellStats(files_stack, sum))
names(sum_total_hours)[1]<-"Sum"

total_hours<-cbind(avg_total_hours,sum_total_hours)


# To be more efficient we could do the above two steps with one line of code
# avg_NDVI_HARV <- as.data.frame(cellStats(NDVI_stack_HARV, mean))

# view data
total_hours
write.csv(total_hours, file="Data/VESSELS/AIS/knots.x.hours/SRKW.knots.x.hours.csv")

##Repeat for VesselPresence


setwd(here("Data","VESSELS","AIS","VesselPresence"))
rlist=list.files(getwd(), pattern="tif$", full.names=FALSE)
for(i in rlist) { assign(unlist(strsplit(i, "[.]"))[1], raster(i)) } 
c<-stack(rlist)

# now use the mask function
rr <- mask(`AIS-Mandatory_05_2018-2020`, pol)
for(i in rlist) { assign(unlist(strsplit(i, "[.]"))[1], raster(i)) } 
# plot, and overlay:
plot(rr);plot(pol,add=TRUE)
plot(rr)
summary(rr)
quantile(rr)      
summary(`AIS-Mandatory_05_2018-2020`)
decile<-quantile(`AIS-Mandatory_05_2018-2020`,probs=c(0.90))
a<-cellStats(rr,'mean')
b<-cellStats(`AIS-Mandatory_05_2018-2020`,'sum')
a/b

####

###Clip the raster data for AIS total km's
setwd(here("Data","VESSELS","AIS"))
#dir<-file.path("Data/VESSELS/AIS/VesselPresence/Clips")
# for loop with (in this case) 60 runs
files <- list.files(path="VesselPresence",full.names=TRUE, pattern = "\\.tif$")
ind=(1:length(files))
for (i in ind) {
  # Reading the raster to crop
  files <- list.files(path="VesselPresence",full.names=TRUE, pattern = "\\.tif$")
  Env_raster <- stack(files[i])
  # Save the filename
  filename <- (paste("Clip_", basename(files[i]), sep=""))
  # Putting NA values in all the raster cells outside the shapefile boundaries
  Maskshp.masked <- mask(x=Env_raster, mask=pol) 
  plot(Maskshp.masked)
  #Export file to working directory with orginal name as new name
  writeRaster(Maskshp.masked, filename,overwrite=T)
}

library(raster)

setwd(here())
#get the path
setwd(here("Data","VESSELS","AIS","VesselPresence")) 
# Create list of NDVI file paths
files <- list.files(getwd(), pattern = "tif$", full.names = TRUE)

# Create a time series raster stack
files_stack <- stack(files)
# calculate mean NDVI for each raster
avg_total_hours <- as.data.frame(cellStats(files_stack, mean))
names(avg_total_hours)[1]<-"Mean"
sum_total_hours <- as.data.frame(cellStats(files_stack, sum))
names(sum_total_hours)[1]<-"Sum"

total_hours<-cbind(avg_total_hours,sum_total_hours)


# To be more efficient we could do the above two steps with one line of code
# avg_NDVI_HARV <- as.data.frame(cellStats(NDVI_stack_HARV, mean))

# view data
total_hours
write.csv(total_hours, file="SRKW.VesselPresence.csv")

#Repeat for Creel data

###Clip the raster data for AIS total km's
setwd(here("Data","VESSELS"))
#dir<-file.path("Data/VESSELS/AIS/VesselPresence/Clips")
# for loop with (in this case) 60 runs
files <- list.files(path="CREEL",full.names=TRUE, pattern = "\\.tif$")
ind=(1:length(files))
for (i in ind) {
  # Reading the raster to crop
  files <- list.files(path="CREEL",full.names=TRUE, pattern = "\\.tif$")
  Env_raster <- stack(files[i])
  # Save the filename
  filename <- (paste("Clip_", basename(files[i]), sep=""))
  # Putting NA values in all the raster cells outside the shapefile boundaries
  Maskshp.masked <- mask(x=Env_raster, mask=pol) 
  plot(Maskshp.masked)
  #Export file to working directory with orginal name as new name
  writeRaster(Maskshp.masked, filename,overwrite=T)
}

#get the path

setwd(here("Data","VESSELS","CREEL")) 
# Create list of NDVI file paths
files <- list.files(getwd(), pattern = "tif$", full.names = TRUE)

# Create a time series raster stack
files_stack <- stack(files)
# calculate mean NDVI for each raster
avg_total_hours <- as.data.frame(cellStats(files_stack, mean))
names(avg_total_hours)[1]<-"Mean"
sum_total_hours <- as.data.frame(cellStats(files_stack, sum))
names(sum_total_hours)[1]<-"Sum"

total_hours<-cbind(avg_total_hours,sum_total_hours)


# To be more efficient we could do the above two steps with one line of code
# avg_NDVI_HARV <- as.data.frame(cellStats(NDVI_stack_HARV, mean))

# view data
total_hours
write.csv(total_hours, file="SRKW.CREEL.csv")



#Repeat for NASP data
newproj<-"+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs"
###Clip the raster data for AIS total km's
setwd(here("Data","VESSELS"))
#dir<-file.path("Data/VESSELS/AIS/VesselPresence/Clips")
# for loop with (in this case) 60 runs
files <- list.files(path="NASP",full.names=TRUE, pattern = "\\.tif$")
ind=(1:length(files))
for (i in ind) {
  # Reading the raster to crop
  files <- list.files(path="NASP",full.names=TRUE, pattern = "\\.tif$")
   Env_raster <- stack(files[i])
  # Save the filename
  filename <- (paste("Clip_", basename(files[i]), sep=""))
  filename2<- (paste("Proj_", basename(files[i]), sep=""))
  Env_raster<-projectRaster(Env_raster, crs=newproj)
  # Putting NA values in all the raster cells outside the shapefile boundaries
  Maskshp.masked <- mask(x=Env_raster, mask=pol) 
  plot(Maskshp.masked)
  #Export file to working directory with orginal name as new name
  writeRaster(Env_raster, filename2, overwrite=T)
  writeRaster(Maskshp.masked, filename,overwrite=T)
}

#get the path

setwd(here("Data","VESSELS","NASP")) 
# Create list of NDVI file paths
files <- list.files(getwd(), pattern = "tif$", full.names = TRUE)

# Create a time series raster stack
files_stack <- stack(files)
# calculate mean NDVI for each raster
avg_total_hours <- as.data.frame(cellStats(files_stack, mean))
names(avg_total_hours)[1]<-"Mean"
sum_total_hours <- as.data.frame(cellStats(files_stack, sum))
names(sum_total_hours)[1]<-"Sum"

total_hours<-cbind(avg_total_hours,sum_total_hours)


# To be more efficient we could do the above two steps with one line of code
# avg_NDVI_HARV <- as.data.frame(cellStats(NDVI_stack_HARV, mean))

# view data
total_hours
write.csv(total_hours, file="SRKW.NASP.csv")
