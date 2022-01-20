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


# recorder locations
Recorders = st_read("DATA/BASEMAP FILES/RecorderLocations.csv", stringsAsFactors = F,
                    options=c("X_POSSIBLE_NAMES=LONG","Y_POSSIBLE_NAMES=LAT")) %>%
  st_set_crs(4326) %>%  st_transform(EPSGcode) %>%
  filter(UsedForModelling == "Yes") %>%
  mutate(Shape = case_when(UsedForDetections == "Yes" ~ 23,
                           UsedForDetections == "No" ~ 22),
         Colour = case_when(UsedForDetections == "Yes" ~ "#2d9c5d",
                            UsedForDetections == "No" ~ "#ffb900"))

# AIS locations
AIS = st_read("DATA/BASEMAP FILES/CCG AIS-towers 2017-pacific.csv", stringsAsFactors = F,
              options=c("X_POSSIBLE_NAMES=LONG","Y_POSSIBLE_NAMES=LAT")) %>%
  st_set_crs(4326) %>%  st_transform(EPSGcode)


# bathymetry
bathy = raster("Data/BASEMAP FILES/BathymetryCropped.tif")
res(bathy)
bathyLowRes = aggregate(bathy, fact = 2, fun = mean)
bathyDF = as(bathyLowRes, "SpatialPixelsDataFrame") %>%
  as.data.frame()

# Labels for landmarks
Landmarks = st_read("Data/BASEMAP FILES/Labels.csv", stringsAsFactors = F,
                    options=c("X_POSSIBLE_NAMES=LONG","Y_POSSIBLE_NAMES=LAT")) %>%
  st_set_crs(4326) %>%  st_transform(EPSGcode) %>%
  filter(Label != "San Juan Isl") %>%
  mutate(Label = case_when(Label == "La Perouse Bank" ~ "La P\u00E9rouse Bank",
                           Label != "La Perouse Bank" ~ Label),
         FontFace = case_when(Subtype == "Waterway" ~ "italic",
                              T ~ "plain"),
         nudgeY = case_when(Label == "Vancouver" ~ -1500,
                            Label == "Nitinat" ~ 0,
                            Label == "Port Renfrew" ~ -1000,
                            Label == "Jordan River" ~ 3500,
                            Label == "Sooke" ~ 3500,
                            Label == "Victoria" ~ 5000,
                            Label == "Port Angeles" ~ -3500,
                            Label == "Neah Bay" ~ -7000,
                            Label == "La P\u00E9rouse Bank" ~ -5000,
                            TRUE ~ 0),
         nudgeX = case_when(Label =="Vancouver" ~ 11000,
                            Label == "Nitinat" ~ 9000,
                            Label == "Port Renfrew" ~ 13000,
                            Label == "Jordan River" ~ 5000,
                            Label == "Sooke" ~ -4000,
                            Label == "Victoria" ~ -3000,
                            Label == "Haro Strait" ~ 2000,
                            Label == "Neah Bay" ~ 10000,
                            TRUE ~ 0),
         rot = case_when(Label == "Strait of Juan de Fuca" ~ -21,
                         Label == "Neah Bay" ~ -27,
                         Label == "Haro Strait" ~ -70,
                         Label == "Boundary Pass" ~ 20,
                         TRUE ~ 0))



# create polygon of general area of interest

top = 48.9
bottom = 48.7
left = -123.8
right = -122.8

extentCoords = matrix(c(left,bottom, right,bottom, right,top, left,top, left,bottom),ncol=2, byrow=TRUE) %>%
  list() %>% st_polygon() %>% st_sfc(crs = 4326) %>% st_transform(EPSGcode) %>%
  st_bbox()


# map all layers ----------------------------------------------------------------

# base layers for Figure 1 on all maps

Figure1base = ggplot() +
  
  geom_raster(data = bathyDF, aes(x=x, y=y, fill=BathymetryCropped), show.legend = F)+
  scale_fill_gradientn(colours=c("#081d3d","#08306b","#2171b5","#deebf7","#faf0e6"),
                      limits = c(-750,NA), na.value = "#081d3d",
                     breaks = c(-750,-600,-400,-300,-200,-100,-50),
                      values = c(0,0.47,0.73,0.95,1),
                      name = "Depth")+
  
  #geom_sf(CH_CA, fill = "#cccf99", alpha = 0.4, col = NA, mapping = aes())+
  #geom_sf(CH_US, fill = "#cf99c9", alpha = 0.4, col = NA, mapping = aes())+
  geom_sf(data = CHS_crop_simp, fill = "#c2c2c2") +
  #geom_sf(Shipping_lanes, fill = "#5e5e5e", alpha = 0.7, col = NA, mapping = aes(), show.legend = F) +
  
  ggsn::scalebar(x.min = extentCoords$xmin, x.max =  extentCoords$xmax - 15000,
                 y.min = extentCoords$ymin, y.max = extentCoords$ymax,
                 dist = 20, transform = F, dist_unit = "km", location = "bottomright",
                 height = 0.017, st.size = 3, st.dist = 0.02, border.size = 0.5)+
  
  north(x.min = extentCoords$xmin, x.max = extentCoords$xmax + 10000,
        y.min = extentCoords$ymin, y.max = extentCoords$ymax + 6000,
        location = "topright", symbol = 12, scale = 0.06)+
  
  coord_sf(xlim = c(extentCoords$xmin, extentCoords$xmax), ylim = c(extentCoords$ymin, extentCoords$ymax))+
  
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1,suffix = "\u00b0N"))+
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1,suffix = "\u00b0W"))+
  
  theme(panel.background = element_rect(fill = "#d7ecfc"),
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", fill = NA),
        axis.text.x = element_text(color="black",size = 10),
        axis.text.y = element_text(color="black",size = 10),
        axis.title = element_blank())


# add layers needed for Threats paper

Figure1Threats = Figure1base+
  geom_sf(lines, lwd =0.2, mapping=aes(), col ="#4E4E4D", alpha=0.8 )+
  geom_sf(st_as_sf(WW,cex=1.2,pch=15),mapping=aes(),col=rgb(0,0,0,.01),alpha=0.3)+
  #geom_sf(AOI, col = "#383838", fill = NA, lwd = 1, mapping = aes()) +
  geom_sf(st_as_sf(BG_subset,cex=1.2,pch=16),alpha=0.4,mapping=aes(),col=rgb(0.5,0,0,.05))+
  coord_sf(xlim = c(extentCoords$xmin, extentCoords$xmax), ylim = c(extentCoords$ymin, extentCoords$ymax))
  Figure1Threats
  geom_sf(Landmarks, size = 2, mapping = aes(col = Type), show.legend = F)+
  scale_colour_manual(values = c(NA,"black"))+
  
 #  geom_sf_text(data = Landmarks,  mapping = aes(label = Label),
#             fontface=Landmarks$FontFace,
 #              angle = Landmarks$rot, nudge_y = Landmarks$nudgeY, nudge_x = Landmarks$nudgeX)+
  
ggsave("Outputs/Sightings-Effort-Whales-paper.png",plot = Figure1Threats, units = "in", width = 12, height = 9,
       type = "cairo") # this is required to make sure polygons extending beyond the frame are properly coloured (due to an issue with Windows default graphics)


# add layers needed for Whale paper

Figure1Whale = Figure1base +
  geom_sf(st_as_sf(WW,col=rgb(.8,0,0,.01),cex=1.2,pch=16),alpha=0.3,mapping=aes())+
  #geom_sf(Landmarks, size = 2, mapping = aes(col = Type), show.legend = F)+
  #scale_colour_manual(values = c(NA,"black"))+
coord_sf(xlim = c(extentCoords$xmin, extentCoords$xmax), ylim = c(extentCoords$ymin, extentCoords$ymax))
  #geom_sf(Recorders, col = "black", fill = Recorders$Colour, pch = Recorders$Shape, size = 3,
  #        mapping = aes())+
  
  #geom_sf_text(data = Landmarks,  mapping = aes(label = Label),
  #             fontface=Landmarks$FontFace,
  #             angle = Landmarks$rot, nudge_y = Landmarks$nudgeY, nudge_x = Landmarks$nudgeX)+
  
  #geom_text()
  
  #coord_sf(xlim = c(extentCoords$xmin, extentCoords$xmax), ylim = c(extentCoords$ymin, extentCoords$ymax))
Figure1Whale
ggsave("Outputs/Figure4-WWSightings.png",plot = Figure1Whale, units = "in", width = 12, height = 9,
       type = "cairo") # this is required to make sure polygons extending beyond the frame are properly coloured (due to an issue with Windows default graphics)



# Study area in the Salish Sea. Passive acoustic monitoring recorders were used
# for killer whale detections and underwater noise modelling (orange diamonds) or
# for underwater noise modelling only (orange squares).
# Southern resident killer whale critical habitat in Canadian and American waters
# is indicated by yellow shading. The study area is indicated by the thick black
# line.

ggsave("CK Results/Figure1-whale-paper-v2.png",plot = Figure1Whale , units = "in", width = 12, height = 8,
       type = "cairo") # this is required to make sure polygons extending beyond the frame are properly coloured (due to an issue with Windows default graphics)



########################################################################

##                      Map DFO Creel Context                         ##

########################################################################

EPSGcode = 4326    # WGS

AOI = st_read("DATA/BASEMAP FILES/AIScoverage_NAD83_Albers.shp")%>% st_transform(EPSGcode)   # Study AOI

CHS_crop_simp = st_read("DATA/BASEMAP FILES/CHS_crop_simp_for_mapping.shp")%>% st_transform(EPSGcode)    # land polygons for context (cropped and simplied)

# create polygon of general area of interest
top = 49
bottom = 48
left = -125.5
right = -123
extentCoords = matrix(c(left,bottom, right,bottom, right,top, left,top, left,bottom),
                      ncol=2, byrow=TRUE) %>%
  list() %>% st_polygon() %>% st_sfc(crs = 4326) %>% st_transform(EPSGcode) %>%
  st_bbox()

# read in creel sub areas and generalized flight paths

CreelFlightAreas = st_read("DATA/BASEMAP FILES/Creel-Flight-Areas-in-AOI.gpkg", stringsAsFactors = F)%>% st_transform(EPSGcode) %>%
  filter(is.na(FlightArea)==F) %>%
  mutate(CreelSubArea = case_when(FlightArea %in% c("Area 19 (GS)", "Area 19 (JDF)") ~ "Area 19",
                                  FlightArea %in% c("Area 20 (East)", "Area 20 (West)") ~ "Area 20",
                                  FlightArea %in% c("Area 23 (Alberni Canal)", "Area 23 (Barkley)") ~ "Area 23",
                                  T ~ FlightArea)) %>%
  mutate(CreelSubArea = ordered(CreelSubArea,
                                levels = c("Area 17","Area 18","Area 19", "Area 20","Area 21" ,"Area 23", "Area 29", "Area 121", "Area 123" )))

CreelFlightPaths = st_read("DATA/BASEMAP FILES/Creel-Flight-Paths-in-AOI.gpkg")%>% st_transform(EPSGcode)

# make map

CreelContextMap = ggplot() +
  
  geom_sf(data = CreelFlightAreas, aes(fill = CreelSubArea), colour = NA) +
  
  scale_fill_manual(values = c("#332288","#117733","#44AA99","#88CCEE","#DDCC77","#CC6677","#AA4499","#882255","#000000"),
                    name = "Creel Subarea")+
  
  geom_sf(data = CHS_crop_simp, col = "#4f4f4f", fill = "#c2c2c2", lwd = 0.3) +
  
  geom_sf(data = CreelFlightPaths, aes(),colour = "#ffffff", lwd = 0.75) +
  
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1,suffix = "\u00b0N"))+
  
  theme(panel.background = element_rect(fill = "#ffffff"),
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", fill = NA),
        axis.text.x = element_text(color="black",size = 10),
        axis.text.y = element_text(color="black",size = 10),
        axis.title = element_blank())+
  coord_sf(xlim = c(extentCoords$xmin, extentCoords$xmax), ylim = c(extentCoords$ymin, extentCoords$ymax),
           expand = F)

CreelContextMap

ggsave("CK Results/Figure4_creel-context.png",plot = CreelContextMap , units = "in", width = 6.5, height = 3.75,
       type = "cairo") # this is required to make sure polygons extending beyond the frame are properly coloured (due to an issue with Windows default graphics)


########################################################################

##           Map CCG AIS Receiver Coverage Data                       ##

########################################################################

# read in from top of the script:
# EPSGcode
# AOI
# CHS_crop_simp
# AIS
# extentCoords


# AIS coverage data
# this data was provided by Emmanuel Rosetti from CCG
# from a 2017 analysis to evaluate the empirical coverage of their AIS stations

## The AIS Class B coverage layer needs to be displayed:
# with the signal strength only
# gradient scale from -121 and below for Poor or no coverage to -103 and above for Good coverage

# To remove erratic points (which might be due to errors/issues/etc.)
# filter the datasets with a # of expected messages > 100

classB_coverage = st_read("DATA/VESSELS/AIS/AISCoverage.gdb", layer = 'classB_20171019')%>%
  st_transform(EPSGcode) %>%
  st_crop(extentCoords) %>%
  filter(NumberOfExpectedMessages > 100)%>%
  mutate(AvgSigStr_scaled = replace(AverageSignalStrength, AverageSignalStrength < -121, -121)) %>%
  mutate(AvgSigStr_scaled = replace(AvgSigStr_scaled, AvgSigStr_scaled > -103, -103))

classA_coverage = st_read("DATA/VESSELS/AIS/AISCoverage.gdb", layer = 'classA_20171019')%>%
  st_transform(EPSGcode) %>%
  st_crop(extentCoords) %>%
  filter(NumberOfExpectedMessages > 100)%>%
  mutate(AvgSigStr_scaled = replace(AverageSignalStrength, AverageSignalStrength < -121, -121)) %>%
  mutate(AvgSigStr_scaled = replace(AvgSigStr_scaled, AvgSigStr_scaled > -103, -103))


tm_shape(classA_coverage)+
  tm_fill("PercentageOfMessagesReceived", breaks = c(0,0.1,0.3,1), palette = 'RdYlBu',
          labels = c("Poor (<10%)", "Medium (10-30%)","Good (>30%)"),
          title = "Messages received")+
  tm_shape(CHS_crop_simp)+
  tm_polygons()+
  tm_legend(legend.outside = T)


AISCoverageMap = ggplot() +
  
  geom_sf(data = classB_coverage, aes(fill = AvgSigStr_scaled), colour = NA) +
  
  scale_fill_gradientn(colours=c("#d7191c","#ffffbf","#2c7bb6"),
                       limits = c(-121,-103), na.value = "#081d3d",
                       breaks = c(-121,-103),
                       labels = c("Poor","Good"),
                       values = c(0,0.5,1),
                       name = "Signal Strength")+
  
  geom_sf(data = CHS_crop_simp, fill = "#c2c2c2") +
  
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1,suffix = "\u00b0N"))+
  
  theme(panel.background = element_rect(fill = "#ffffff"),
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", fill = NA),
        axis.text.x = element_text(color="black",size = 10),
        axis.text.y = element_text(color="black",size = 10),
        axis.title = element_blank())+
  geom_sf(AOI, col = "#000000", fill = NA, lwd = 2, mapping = aes()) +
  
  geom_sf(AIS, col = "black", fill = "#e80000", size = 3, pch = 24, mapping = aes())+
  
  coord_sf(xlim = c(extentCoords$xmin, extentCoords$xmax), ylim = c(extentCoords$ymin, extentCoords$ymax),
           expand = F)

AISCoverageMap
