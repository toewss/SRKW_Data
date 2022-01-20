#----IMPORT PACKAGES----

library(ggplot2)
library(dplyr)
library(tmap)
library(sf)
library(raster)
library(smoothr) # for 'fill_holes' function
library(ggsn)    # for scale bars and north arrows
library(scales)  # for setting axis labels decimal places
library(ggpubr)  # for 'ggarrange' (to put multiple ggplots together)

########################################################################

##                   Co-occurrence maps                               ##

########################################################################

source("CK Results/sc/mapMakingFunctions.r")

# prep whale polygons -------------------------------------------

whalePolygonFiles = list.files("DATA/SRKW_Occurence_Shapefiles/Archive/Shapefiles_AM/", full.names = T) %>%
  as.data.frame(stringsAsFactors=F) %>%
  filter(.,grepl(("*.shp$"),.))

whalePolygonList = list()

for(i in c(1:nrow(whalePolygonFiles))){
  fileName = substr(whalePolygonFiles[i,1],nchar(whalePolygonFiles[i,1])-24,nchar(whalePolygonFiles[i,1])-19)
  whalePolygonList[[fileName]] = st_read(whalePolygonFiles[i,1],stringsAsFactors=F)%>% st_transform(EPSGcode) %>%
    mutate(MONTH = case_when(substr(fileName,1,3) == "May" ~ 5,
                             substr(fileName,1,3) == "Jun" ~ 6,
                             substr(fileName,1,3) == "Jul" ~ 7,
                             substr(fileName,1,3) == "Aug" ~ 8,
                             substr(fileName,1,3) == "Sep" ~ 9,
                             substr(fileName,1,3) == "Oct" ~ 10),
           Threshold = case_when(substr(fileName,5,6) == "D7" ~ 70,
                                 substr(fileName,5,6) == "D8" ~ 80,
                                 substr(fileName,5,6) == "D9" ~ 90))
}

whalePolygons = bind_rows(whalePolygonList)  # combine whale polygons

whalePolygons %>%
  st_write(paste0("DATA/SRKW_Occurence_Shapefiles/Whale-Polygons_full-extent.gpkg"), append = F)




#----IMPORT DATA----

EPSGcode = 4326    # WGS

CHS_crop_simp = st_read("DATA/BASEMAP FILES/CHS_crop_simp_for_mapping.shp")%>% st_transform(EPSGcode)    # land polygons for context (cropped and simplied)

#AIS vessel presence (all vessel categories together)
rasStackAisVessHrs = raster::stack(list.files("DATA/VESSELS/AIS/VesselPresence/", full.names = T)) %>%
  projectRaster(crs = st_crs(CHS_crop_simp)$proj4string, method = 'ngb')   # default is 'bilinear' but this creates non-sense negative numbers. So using nearest neighbour instead

# full whale ploygons
whalePolygons = st_read("DATA/SRKW_Occurence_Shapefiles/Whale-Polygons_full-extent.gpkg") %>%
  filter(Threshold == 70) %>% st_transform(EPSGcode)

# clip areas
#region_poly = st_read("DATA/SRKW_Occurence_Shapefiles/RegionalBoundaries/Boundary.shp") %>%
#  st_transform(EPSGcode)

# clipped whale ploygons
whaleClips = st_read("DATA/SRKW_Occurence_Shapefiles/Whale-Polygons.gpkg") %>%
  filter(Threshold == 70) %>% st_transform(EPSGcode)

#NASP data
rasStackNASP = raster::stack(list.files("DATA/VESSELS/NASP/", full.names = T)) %>%
  projectRaster(crs = st_crs(CHS_crop_simp)$proj4string, method = 'ngb')

#Creel data
rasStackCreel = raster::stack(list.files("DATA/VESSELS/CREEL/", full.names = T)) %>%
  projectRaster(crs = st_crs(CHS_crop_simp)$proj4string, method = 'ngb')


# create polygon of general area of interest
top = 49
bottom = 48
left = -125.5
right = -123
extentCoords = matrix(c(left,bottom, right,bottom, right,top, left,top, left,bottom),
                      ncol=2, byrow=TRUE) %>%
  list() %>% st_polygon() %>% st_sfc(crs = 4326) %>% st_transform(EPSGcode) %>%
  st_bbox()




### AIS DATA CO-OCCURRENCE MAP ###################################################################


AISmap_CoOccur = function(rasStackAisVessHrs, targetClass, MonthChar,
                          whaleClips, whalePolygons, region_poly, extentCoords, CHS_crop_simp){

  targetLayers = grep(paste0("ship.hours.",targetClass,"_",MonthChar,"_Mean18.20"),names(rasStackAisVessHrs))

  rasLayers = rasStackAisVessHrs[[targetLayers]]

  whaleMonthClips = whaleClips %>% filter(MONTH == as.numeric(MonthChar))

  whaleMonthPolygons = whalePolygons %>% filter(MONTH == as.numeric(MonthChar))

  # convert raster stack to dataframe:
  coords = xyFromCell(rasLayers, seq_len(ncell(rasLayers)))
  rasLayersDF = stack(as.data.frame(getValues(rasLayers))) %>%
    cbind(coords, .) %>%
    mutate(ValuesMod = case_when(values > 10 ~ 10,
                                 values <= 10 ~ values))

  # plot data
  rasterMap = ggplot() +

    scale_y_continuous(labels = scales::number_format(accuracy = 0.1,suffix = "\u00b0N"))+
    scale_x_continuous(breaks = c(-125,-124,-123))+

    theme(panel.background = element_rect(fill = "#262a73"),
          panel.grid.major = element_blank(),
          panel.border = element_rect(linetype = "solid", fill = NA),
          axis.text.x = element_text(color="black",size = 8, hjust=0.7),
          axis.text.y = element_text(color="black",size = 8),
          axis.title = element_blank())+

    geom_raster(data = rasLayersDF, aes(x=x, y=y, fill=log10(ValuesMod)), show.legend = T)+

    scale_fill_gradientn(colours=c("#262a73","#313695","#74add1","#ffffbf","#f46d43","#a50026"),
                         na.value = "#262a73",
                         limits = c(-4,NA),
                         breaks = c(-4,-3,-2,-1,0,1),
                         values = c(0,0.2,0.4,0.6,0.8,1),
                         labels = c("0","0.001","0.01","0.1","1","\u2265 10"),
                         guide = guide_colourbar())+

    #geom_sf(data = region_poly, col = "grey", fill = "grey", alpha = 0.5, lty = 2)+

    geom_sf(data = CHS_crop_simp, col = "#4f4f4f", fill = "#c2c2c2", lwd = 0.3) +

    geom_sf(data = whaleMonthClips, col = "#000000", fill = NA, lwd = 0.6) +

    geom_sf(data = whaleMonthPolygons, col = "#000000", fill = NA, lwd = 0.4) +

    guides(fill = guide_colourbar(barwidth = 27.5, barheight = 1,
                                  ticks.colour = "black", frame.colour = "black",
                                  title.theme = element_text(size = 11, angle = 0),
                                  label.theme = element_text(size = 8),
                                  draw.ulim = FALSE, draw.llim = FALSE,
                                  title = "Average Daily Vessel Hours",
                                  title.position = "bottom",
                                  title.hjust = 0.55))+

    coord_sf(xlim = c(extentCoords$xmin, extentCoords$xmax),
             ylim = c(extentCoords$ymin, extentCoords$ymax),
             expand = F)

  return(rasterMap)
}


targetClass = "All.AIS.Vessels"

rasterMap5 = AISmap_CoOccur(rasStackAisVessHrs, targetClass, "05",
                            whaleClips, whalePolygons, region_poly, extentCoords, CHS_crop_simp)+
  theme(axis.text.x=element_blank())

rasterMap6 = AISmap_CoOccur(rasStackAisVessHrs, targetClass, "06",
                              whaleClips, whalePolygons, region_poly, extentCoords, CHS_crop_simp)+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

rasterMap7 = AISmap_CoOccur(rasStackAisVessHrs, targetClass, "07",
                              whaleClips, whalePolygons, region_poly, extentCoords, CHS_crop_simp)+
  theme(axis.text.x=element_blank())

rasterMap8 = AISmap_CoOccur(rasStackAisVessHrs, targetClass, "08",
                            whaleClips, whalePolygons, region_poly, extentCoords, CHS_crop_simp)+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

rasterMap9 = AISmap_CoOccur(rasStackAisVessHrs, targetClass, "09",
                            whaleClips, whalePolygons, region_poly, extentCoords, CHS_crop_simp)

rasterMap10 = AISmap_CoOccur(rasStackAisVessHrs, targetClass, "10",
                            whaleClips, whalePolygons, region_poly, extentCoords, CHS_crop_simp)+
  theme(axis.text.y=element_blank())

rasterMapCoOccur = ggarrange(rasterMap5,rasterMap6,rasterMap7,rasterMap8,rasterMap9,rasterMap10, ncol = 2, nrow = 3,
                             common.legend = T, legend = "bottom",align = c("hv"),
                             labels = c("      May","      June","     July","  August","September","  October"),
                             label.x = 0.4, label.y = 0.95,
                             font.label = list(size = 8, color = "black", face = "bold"))
rasterMapCoOccur


ggsave("CK Results/CoOccurrence_VesselRaster_AllAIS.png",plot = rasterMapCoOccur,
       units = "in", width = 6.5, height = 6.5)

## Extra AIS Co-Occurrance Maps for Sheila and SAR -------------------------------------------------------

#AIS vessel presence Aug 2018 for comparison to vessel noise model outputs
rasStackAisVessHrsExample = raster::stack(list.files("DATA/VESSELS/AIS/VesselPresence/Aug2018", full.names = T)) %>%
  projectRaster(crs = st_crs(CHS_crop_simp)$proj4string, method = 'ngb')   # default is 'bilinear' but this creates non-sense negative numbers. So using nearest neighbour instead


# Class A and B comparison

rasterMapAugA= AISmap_CoOccur_AB_Comparison(rasStackAisVessHrsExample,"A", "08",
                            whaleClips, whalePolygons, region_poly, extentCoords, CHS_crop_simp)

rasterMapAugB= AISmap_CoOccur_AB_Comparison(rasStackAisVessHrsExample,"B", "08",
                                            whaleClips, whalePolygons, region_poly, extentCoords, CHS_crop_simp)+
  theme(axis.text.y=element_blank())

rasterMapCoOccur = ggarrange(rasterMapAugA,rasterMapAugB, ncol = 2, nrow = 1,
                             common.legend = T, legend = "bottom",align = c("hv"),
                             labels = c("Class A","Class B"),
                             label.x = 0.4, label.y = 0.95,
                             font.label = list(size = 8, color = "black", face = "bold"))
rasterMapCoOccur

ggsave("CK Results/CoOccurrence_VesselRaster_AvsB_Aug2018.png",plot = rasterMapCoOccur,
       units = "in", width = 6.75, height = 2.75)


# Class A only

rasterMapAugA= AISmap_CoOccur_AB_Comparison(rasStackAisVessHrsExample,"A", "08",
                                            whaleClips, whalePolygons, region_poly, extentCoords, CHS_crop_simp)+
  guides(fill = guide_colourbar(barwidth = 22, barheight = 2,
                                ticks.colour = "white", frame.colour = "white",
                                draw.ulim = FALSE, draw.llim = FALSE,
                                title = "Average Daily Vessel Hours",
                                title.position = "bottom",
                                title.hjust = 0.55))+
  theme(axis.text.x = element_text(color="black",size = 10, hjust=0.5),
        axis.text.y = element_text(color="black",size = 10))+
  scale_x_continuous(breaks = c(-125.5,-125,-124.5,-124,-123.5,-123))

rasterMapCoOccur = ggarrange(rasterMapAugA, ncol = 1, nrow = 1,
                             common.legend = T, legend = "bottom",align = c("hv"),
                             label.x = 0.4, label.y = 0.95,
                             font.label = list(size = 8, color = "black", face = "bold"))
rasterMapCoOccur

ggsave("CK Results/CoOccurrence_VesselRaster_ClassA_Aug2018.png",plot = rasterMapCoOccur,
       units = "in", width = 6.5, height = 4.5)



### CREEL DATA CO-OCCURRENCE MAP ###################################################################


Creelmap_CoOccur = function(rasStackCreel, MonthChar, whalePolygons, extentCoords, CHS_crop_simp){

  targetLayers = grep(paste0("Vessels.per.flight_available.years_",MonthChar),names(rasStackCreel))

  rasLayers = rasStackCreel[[targetLayers]]

  whaleMonthPolygons = whalePolygons %>% filter(MONTH == as.numeric(MonthChar))

  # convert raster stack to dataframe:
  coords = xyFromCell(rasLayers, seq_len(ncell(rasLayers)))
  rasLayersDF = stack(as.data.frame(getValues(rasLayers))) %>%
    cbind(coords, .)%>%
    mutate(ValuesMod = case_when(values > 5 ~ 5,
                                 T ~ values))

  # plot data
  rasterMap = ggplot() +

    scale_y_continuous(labels = scales::number_format(accuracy = 0.1,suffix = "\u00b0N"))+
    scale_x_continuous(breaks = c(-125,-124,-123))+

    theme(panel.background = element_rect(fill = "#ffffff"),
          panel.grid.major = element_blank(),
          panel.border = element_rect(linetype = "solid", fill = NA),
          axis.text.x = element_text(color="black",size = 8,hjust=0.7),
          axis.text.y = element_text(color="black",size = 8),
          axis.title = element_blank())+

    geom_raster(data = rasLayersDF, aes(x=x, y=y, fill=ValuesMod), show.legend = T)+

    scale_fill_gradientn(colours=c("#4575b4","#91bfdb","#e0f3f8","#ffffbf","#fee090","#fc8d59","#d73027"),
                         na.value = "#ffffff",
                         breaks = c(0,1,2,3,4,5),
                         values = c(0,0.01,0.2,0.4,0.6,0.8,1),
                         labels = c("0","1","2","3","4","\u2265 5"),
                         guide = guide_colourbar())+

    geom_sf(data = CHS_crop_simp, col = "#4f4f4f", fill = "#c2c2c2", lwd = 0.3) +

    geom_sf(data = whaleMonthPolygons, col = "#000000", fill = NA, lwd = 0.5) +

    guides(fill = guide_colourbar(barwidth = 27.5, barheight = 1,
                                  ticks.colour = "black", frame.colour = "black",
                                  title.theme = element_text(size = 11, angle = 0),
                                  label.theme = element_text(size = 8),
                                  draw.ulim = FALSE, draw.llim = FALSE,
                                  title = "Vessels observed per flight",
                                  title.position = "bottom",
                                  title.hjust = 0.55))+

    coord_sf(xlim = c(extentCoords$xmin, extentCoords$xmax),
             ylim = c(extentCoords$ymin, extentCoords$ymax),
             expand = F)

  return(rasterMap)
}



rasterMap5 = Creelmap_CoOccur(rasStackCreel, "05", whalePolygons, extentCoords, CHS_crop_simp)+
  theme(axis.text.x=element_blank())

rasterMap6 = Creelmap_CoOccur(rasStackCreel, "06", whalePolygons, extentCoords, CHS_crop_simp)+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

rasterMap7 = Creelmap_CoOccur(rasStackCreel, "07", whalePolygons, extentCoords, CHS_crop_simp)+
  theme(axis.text.x=element_blank())

rasterMap8 = Creelmap_CoOccur(rasStackCreel, "08", whalePolygons, extentCoords, CHS_crop_simp)+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank())

rasterMap9 = Creelmap_CoOccur(rasStackCreel, "09", whalePolygons, extentCoords, CHS_crop_simp)

rasterMap10 = Creelmap_CoOccur(rasStackCreel, "10", whalePolygons, extentCoords, CHS_crop_simp)+
  theme(axis.text.y=element_blank())

rasterMapCoOccurCreel = ggarrange(rasterMap5,rasterMap6,rasterMap7,rasterMap8,rasterMap9,rasterMap10, ncol = 2, nrow = 3,
                             common.legend = T, legend = "bottom",
                             legend.grob = get_legend(rasterMap7, position = "bottom"),
                             align = c("hv"),
                             labels = c("      May","      June","     July","  August","September","  October"),
                             label.x = 0.4, label.y = 0.95,
                             font.label = list(size = 8, color = "black", face = "bold"))
rasterMapCoOccurCreel


ggsave("CK Results/CoOccurrence_VesselRaster_Creel.png",plot = rasterMapCoOccurCreel,
       units = "in", width = 6.5, height = 6.5)


## Extra Creel Co-Occurrance Maps for Sheila and SAR -------------------------------------------------------

Recorders = st_read("DATA/BASEMAP FILES/RecorderLocations.csv", stringsAsFactors = F,
                    options=c("X_POSSIBLE_NAMES=LONG","Y_POSSIBLE_NAMES=LAT")) %>%
  st_set_crs(4326) %>%  st_transform(EPSGcode) %>%
  filter(UsedForModelling == "Yes")

Shipping_lanes = st_read("DATA/BASEMAP FILES/SalishSeaShippingLanes_NAD83_Albers.shp")%>% st_transform(EPSGcode)

Creelmap_CoOccur = function(rasStackCreel, MonthChar, whalePolygons, extentCoords, CHS_crop_simp){

  targetLayers = grep(paste0("Vessels.per.flight_available.years_",MonthChar),names(rasStackCreel))

  rasLayers = rasStackCreel[[targetLayers]]

  whaleMonthPolygons = whalePolygons %>% filter(MONTH == as.numeric(MonthChar))

  # convert raster stack to dataframe:
  coords = xyFromCell(rasLayers, seq_len(ncell(rasLayers)))
  rasLayersDF = stack(as.data.frame(getValues(rasLayers))) %>%
    cbind(coords, .)%>%
    mutate(ValuesMod = case_when(values > 5 ~ 5,
                                 T ~ values))

  # plot data
  rasterMap = ggplot() +

    scale_y_continuous(labels = scales::number_format(accuracy = 0.1,suffix = "\u00b0N"))+
    scale_x_continuous(breaks = c(-125,-124,-123))+

    theme(panel.background = element_rect(fill = "#ffffff"),
          panel.grid.major = element_blank(),
          panel.border = element_rect(linetype = "solid", fill = NA),
          axis.text.x = element_text(color="black",size = 8,hjust=0.7),
          axis.text.y = element_text(color="black",size = 8),
          axis.title = element_blank())+

    geom_raster(data = rasLayersDF, aes(x=x, y=y, fill=ValuesMod), show.legend = T)+

    scale_fill_gradientn(colours=c("#4575b4","#91bfdb","#e0f3f8","#ffffbf","#fee090","#fc8d59","#d73027"),
                         na.value = "#ffffff",
                         breaks = c(0,1,2,3,4,5),
                         values = c(0,0.01,0.2,0.4,0.6,0.8,1),
                         labels = c("0","1","2","3","4","\u2265 5"),
                         guide = guide_colourbar())+

    geom_sf(data = CHS_crop_simp, col = "#4f4f4f", fill = "#c2c2c2", lwd = 0.3) +

    geom_sf(Shipping_lanes, fill = "#5e5e5e", alpha = 0.5, col = NA, mapping = aes(), show.legend = F) +

    geom_sf(Recorders, col = "white", fill = "black", pch = 21, size = 3,
            mapping = aes())+

    guides(fill = guide_colourbar(barwidth = 1, barheight = 15,
                                  ticks.colour = "black", frame.colour = "black",
                                  title.theme = element_text(size = 11, angle = 90),
                                  label.theme = element_text(size = 8),
                                  draw.ulim = FALSE, draw.llim = FALSE,
                                  title = "Vessels observed per flight",
                                  title.position = "right",
                                  title.hjust = 0.55))+

    coord_sf(xlim = c(extentCoords$xmin, extentCoords$xmax),
             ylim = c(extentCoords$ymin, extentCoords$ymax),
             expand = F)

  return(rasterMap)
}


rasterMapSept = Creelmap_CoOccur(rasStackCreel, "09", whalePolygons, extentCoords, CHS_crop_simp)

rasterMapSept

ggsave("CK Results/CoOccurrence_VesselRaster_Creel_Sept.png",plot = rasterMapSept,
       units = "in", width = 6.5, height = 4)




### NASP DATA CO-OCCURRENCE MAP ###################################################################


NASPmap_CoOccur = function(rasStackNASP, whalePolygons, extentCoords, CHS_crop_simp){

 targetLayers = grep("NASP_Summer_SPUE.*",names(rasStackNASP))

    rasLayers = rasStackNASP[[targetLayers]]

    names(rasLayers) = c("AIS Vessels","Non-AIS Vessels")

    whaleMonthPolygons = whalePolygons %>% filter(MONTH == 8)

    # convert raster stack to dataframe:
    coords = xyFromCell(rasLayers, seq_len(ncell(rasLayers)))
    rasLayersDF = stack(as.data.frame(getValues(rasLayers))) %>%
      cbind(coords, .)%>%
      mutate(ValuesMod = case_when(values > 20 ~ 20,
                                   T ~ values),
             ind = case_when(ind == "AIS.Vessels" ~ "AIS Vessels",
                             ind == "Non.AIS.Vessels" ~ "Non-AIS Vessels"))

    # plot data
    rasterMap = ggplot() +

      scale_y_continuous(labels = scales::number_format(accuracy = 0.1,suffix = "\u00b0N"))+
      scale_x_continuous(breaks = c(-125,-124,-123))+

      theme(panel.background = element_rect(fill = "#ffffff"),
            panel.grid.major = element_blank(),
            panel.border = element_rect(linetype = "solid", fill = NA),
            axis.text.x = element_text(color="black",size = 8),
            axis.text.y = element_text(color="black",size = 8),
            axis.title = element_blank(),
            legend.position = "bottom")+

      geom_raster(data = rasLayersDF, aes(x=x, y=y, fill=ValuesMod), show.legend = T)+

      facet_wrap(~ind, nrow = 1) +

      scale_fill_gradientn(colours=c("#4575b4","#91bfdb","#e0f3f8","#ffffbf","#fee090","#fc8d59","#d73027"),
                           na.value = "#ffffff",
                           limits = c(0,20),
                           breaks = c(0,1,5,10,15,20),
                           values = c(0,0.05,0.25,0.5,0.75,1),
                           labels = c("0","1","5","10","15","\u2265 20"),
                           guide = guide_colourbar())+

      geom_sf(data = CHS_crop_simp, col = "#4f4f4f", fill = "#c2c2c2", lwd = 0.3) +

      geom_sf(data = whaleMonthPolygons, col = "#000000", fill = NA, lwd = 0.5) +

      guides(fill = guide_colourbar(barwidth = 27.5, barheight = 1,
                                    ticks.colour = "black", frame.colour = "black",
                                    title.theme = element_text(size = 11, angle = 0),
                                    label.theme = element_text(size = 8),
                                    draw.ulim = FALSE, draw.llim = FALSE,
                                    title = "Vessels Per Unit Effort",
                                    title.position = "bottom",
                                    title.hjust = 0.55))+

      coord_sf(xlim = c(extentCoords$xmin, extentCoords$xmax),
               ylim = c(extentCoords$ymin, extentCoords$ymax),
               expand = F)

  return(rasterMap)
}


rasterMapCoOccurNASP = NASPmap_CoOccur(rasStackNASP, whalePolygons, extentCoords, CHS_crop_simp)


ggsave("CK Results/CoOccurrence_NASP_AugPoly.png",plot = rasterMapCoOccurNASP,
       units = "in", width = 6.5, height = 3)
