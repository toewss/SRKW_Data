################################### 
##Intersection between Occurrence## 
##Polygons and Critical Habitat  ##
###################################
##TAKEN FROM FIGURE PRODUCTION SCRIPT##

#----IMPORT PACKAGES----

library(ggplot2)
library(dplyr)
library(tmap)
library(sf)
library(raster)
library(smoothr) # for 'fill_holes' function
library(ggsn)    # for scale bars and north arrows
library(scales)  # for setting axis labels decimal places
library(here)
########################################################################

##                   Template 1: Study Area                           ##

########################################################################
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

setwd(here("Data","BASEMAP FILES"))
# Read in files ########################################################

EPSGcode = 3005    # NAD83 / BC Albers projection. Or if I want to use UTM zone 10, use EPSG code 26910

AOI = st_read("Data/BASEMAP FILES/AIScoverage_NAD83_Albers.shp")%>% st_transform(EPSGcode)   # Study AOI
setwd(here())
CHS_crop_simp = st_read("Data/Coast shapefile/coast_ALBERScliped.shp")%>% st_transform(EPSGcode)    # land polygons for context (cropped and simplied)
lines<-st_read('Data/DFO_Data/Master_Tracks2009_20.shp') %>% st_transform(EPSGcode) # transect lines
# shipping lanes
Shipping_lanes = st_read("Data/BASEMAP FILES/SalishSeaShippingLanes_NAD83_Albers.shp")%>% st_transform(EPSGcode)

# critical habitat
CH_New = st_read("Data/BASEMAP FILES/Critical habitat/CSAS_2016_CHProposed_SWVI_NAD83_Albers.shp")%>% st_transform(EPSGcode)
CH_Orig = st_read("DATA/BASEMAP FILES/Critical habitat/southern resident kw ch april 07_NAD83_Albers.shp")%>% st_transform(EPSGcode)

area_thresh <- units::set_units(0.1, km^2)

CH_US = st_read("DATA/BASEMAP FILES/Critical habitat/SRKW_FCH_11_14_06_NAD83_Albers.shp")%>%
  st_transform(EPSGcode) %>%
  summarise() %>%                    # to dissolve internal boundaries
  fill_holes(threshold = area_thresh)

st_precision(CH_New)=1
st_precision(CH_Orig)=1
CH_New = st_make_valid(CH_New)
CH_Orig = st_make_valid(CH_Orig)

CH_CA = st_union(CH_New,CH_Orig)
Boundary<-CH_CA
# bathymetry
bathy = raster("Data/BASEMAP FILES/BathymetryCropped.tif")
res(bathy)
bathyLowRes = aggregate(bathy, fact = 2, fun = mean)
bathyDF = as(bathyLowRes, "SpatialPixelsDataFrame") %>%
  as.data.frame()


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

for (shp in shps) {
  s<-(sum(st_area(shp))/1000000)
    file.name=paste0("Clip_",shp)
}
