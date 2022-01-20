#####################################################################
## SRKW behaviour figure generation for Feb 2021 CSAS documents
##    Plotting regional stats of available echolocation and communication space
#####################################################################



#----Clear R environment----

rm(list = ls(all=TRUE)) ; ls()


#----Install required packages----

# remotes::install_github("coolbutuseless/ggpattern")
# install.packages("remotes")
#remotes::install_github("coolbutuseless/ggpattern")

library(ggplot2)
library(dplyr)
library(tmap)
library(sf)
library(raster)
library(smoothr) # for 'fill_holes' function
library(ggsn)    # for scale bars and north arrows
library(scales)  # for setting axis labels decimal places
library(cowplot)
library(gridExtra)
library(ggpattern)
library(ggpubr)
library(here)

#----Set working directory----

setwd("Data/Results/SRKW_Occurrence/AcrossMonth/Regional SRKW Polygons_AM")


#----Load functions----

'%ni%' <- Negate('%in%')

colsc <- function(...) {
  scale_fill_gradientn(colours = rev(RColorBrewer::brewer.pal(11,"RdYlBu")),
                       # limits = range(..., na.rm=TRUE)
                       )
}

exc_palette_green <- RColorBrewer::brewer.pal(name="Greens",n=4)[2:4]

exc_palette_yor <- RColorBrewer::brewer.pal(name="YlOrRd",n=3)

exc_palette_bw <- rev(RColorBrewer::brewer.pal(name="RdGy",n=11)[c(7,9,11)])

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    plot.title = element_text(size = 14, face="bold")
  )


setwd(here())
source("RCode/CK Results/sc/mapMakingFunctions.r")



#----Load basemap files----

EPSGcode = 4326    # NAD83 / BC Albers projection (3005). Or if I want to use UTM zone 10, use EPSG code 32610; OR WGS84 unprojected = 4326
CHS_crop_simp = st_read("Data/BASEMAP FILES/CHS_crop_simp_for_mapping.shp") %>% st_transform(EPSGcode)


# Labels for landmarks
Landmarks = st_read("Labels.csv", stringsAsFactors = F,
                    options=c("X_POSSIBLE_NAMES=LONG","Y_POSSIBLE_NAMES=LAT")) %>%
  st_set_crs(4326) %>%  st_transform(EPSGcode) %>%
  mutate(Label = case_when(Label == "La Perouse Bank" ~ "La P\u00E9rouse Bank",
                           Label == "San Juan Isl" ~ "San\nJuan\nIsl",
                           Label == "Swiftsure Bank" ~ "Swiftsure\nBank",
                           Label %ni% c("La Perouse Bank","San Juan Isl", "Swiftsure Bank") ~ Label),
         FontFace = case_when(Subtype == "Waterway" ~ "italic",
                              T ~ "plain"),
         FontCol = case_when(Subtype == "Waterway" ~ "dodgerblue3",
                             T ~ "black"),
         nudgeY = case_when(Label == "VANCOUVER ISLAND" ~ -5000,
                            Label == "Vancouver" ~ -1500,
                            Label == "Nitinat" ~ 0,
                            Label == "Port Renfrew" ~ -1000,
                            Label == "Jordan River" ~ 3500,
                            Label == "Sooke" ~ 3500,
                            Label == "Victoria" ~ 5000,
                            Label == "Port Angeles" ~ -3500,
                            Label == "Neah Bay" ~ -7000,
                            Label == "La P\u00E9rouse Bank" ~ -5000,
                            Label == "Haro Strait" ~ -5000,
                            Label == "San\nJuan\nIsl" ~ -1000,
                            Label == "Swiftsure\nBank" ~ -2500,
                            Label == "Strait of Juan de Fuca" ~ -4000,
                            TRUE ~ 0),
         nudgeX = case_when(Label =="Vancouver" ~ 11000,
                            Label == "Nitinat" ~ 9000,
                            Label == "Port Renfrew" ~ 13000,
                            Label == "Jordan River" ~ 5000,
                            Label == "Sooke" ~ -4000,
                            Label == "Victoria" ~ -3000,
                            Label == "Haro Strait" ~ 2000,
                            Label == "Neah Bay" ~ 10000,
                            Label == "San\nJuan\nIsl" ~ 1300,
                            Label == "Swiftsure\nBank" ~ 1000,
                            TRUE ~ 0),
         rot = case_when(Label == "Strait of Juan de Fuca" ~ -21,
                         Label == "Neah Bay" ~ -27,
                         Label == "Haro Strait" ~ -70,
                         Label == "Boundary Pass" ~ 20,
                         TRUE ~ 0))

depth_contours = st_read("SDE_ELEVATION.shp") %>% st_transform(EPSGcode) %>%
  mutate(Colour = case_when(DEPTH %in% c(seq(-250, -50, 50)) ~ "darkgrey",
                            DEPTH %ni% c(seq(-250, -50, 50)) ~ "lightgrey"
                            ))
doi = seq(-250, -50, 50) #depths 'of interest'
depth_contours_ltd = depth_contours[which(depth_contours$DEPTH %in% doi),]


#----Load data----

# Echolocation space rasters
echo <- list.files("Data/Sound/Existing/Echolocation/Raster/Clip", full.names = TRUE)
echo <- echo[grep(".tif",echo)]
echo.rasts <- as.list(echo)
echo <- list.files("Data/Sound/Existing/Echolocation/Raster/Clip", full.names = FALSE)
echo <- echo[grep(".tif",echo)]
for(i in 1:length(echo)){
  echo.rasts[[i]] = raster(echo.rasts[[i]]) %>%
    as("SpatialPixelsDataFrame") %>%
    as.data.frame()
}
names(echo.rasts) <- echo


# Communication space rasters
comm <- list.files("Data/Sound/Existing/Communication/Raster/Clip", full.names = TRUE)
comm <- comm[grep(".tif",comm)]
comm.rasts <- as.list(comm)
comm <- list.files("Data/Sound/Existing/Communication/Raster/Clip", full.names = FALSE)
comm <- comm[grep(".tif",comm)]
for(i in 1:length(comm)){
  comm.rasts[[i]] = raster(comm.rasts[[i]]) %>%
    as("SpatialPixelsDataFrame") %>%
    as.data.frame()
}
names(comm.rasts) <- comm
# We will only plot the rasters from 10m depth
ind <- grep("Depth10Clip.tif",names(comm.rasts))
comm.rasts = comm.rasts[ind]

# Communication-wind space rasters
wind <- list.files("Data/Sound/Wind/CommunicationrangeFromWind_netcdf/Raster/Clip", full.names = TRUE)
wind <- wind[grep(".tif",wind)]
wind.rasts <- as.list(wind)
wind <- list.files("Data/Sound/Wind/CommunicationrangeFromWind_netcdf/Raster/Clip", full.names = FALSE)
wind <- wind[grep(".tif",wind)]
for(i in 1:length(wind)){
  wind.rasts[[i]] = raster(wind.rasts[[i]]) %>%
    as("SpatialPixelsDataFrame") %>%
    as.data.frame()
}
names(wind.rasts) <- wind


# Load region polygons
regions <- st_read("Data/Results/RegionalBoundaries/Boundary.shp") %>% st_transform(EPSGcode)

# Load exceedance polygons
exc <- list.files("Data/Results/SRKW_Occurrence/AcrossMonth/Regional SRKW POlygons_AM", full.names = TRUE)
exc <- exc[grep(".shp", exc)]
exc.polys <- as.list(exc)
exc <- list.files("Data/Results/SRKW_Occurrence/AcrossMonth/Regional SRKW POlygons_AM", full.names = FALSE)
exc <- exc[grep(".shp", exc)]
for(i in 1:length(exc)){
  exc.polys[[i]] = st_read(exc.polys[[i]]) %>%
    st_transform(EPSGcode)
}
names(exc.polys) <- exc
setwd(here())
# Load chart data
echo = readRDS('Outputs/Coocurrence/Echo_region_summaryAM.rds')
comm = readRDS('Outputs/Coocurrence/Com_region_summaryAM.rds')
wind = readRDS('Outputs/Coocurrence/Comwind_region_summaryAM.rds')


#----SET MAP EXTENT----

#Extent limits
xmin = -125.5
xmax = -123
ymin = 48
ymax =49

bb <- as(extent(c(xmin, xmax, ymin, ymax)), 'SpatialPolygons') %>%
  st_as_sf() %>%
  st_set_crs(4326) %>%
  st_transform(EPSGcode) %>%
  st_bbox


#----MAPPING----

#----generate inset plots----

# Reformat chart data to long format
  # For some reason there is a 0 decile...all zeros, so remove
ind = which(echo$Decile==0.0)
echo = echo[-ind,]

rownames(echo) <- c(1:nrow(echo))

echo = reshape2::melt(echo, id = c("MONTH","Depth","Decile"))
echo$Exceedance <- unlist(purrr::map(strsplit(as.character(echo$variable),"\\."), 1))
echo$Region <- unlist(purrr::map(strsplit(as.character(echo$variable),"\\."), 2))
echo$variable <- NULL
echo$Region <- factor(echo$Region, levels = c("Swiftsure","Haro"))

ind = which(comm$Decile==0.0)
comm = comm[-ind,]
rownames(comm) <- c(1:nrow(comm))

comm = reshape2::melt(comm, id = c("MONTH","Depth","Decile"))
comm$Exceedance <- unlist(purrr::map(strsplit(as.character(comm$variable),"\\."), 1))
comm$Region <- unlist(purrr::map(strsplit(as.character(comm$variable),"\\."), 2))
comm$variable <- NULL
comm$Region <- factor(comm$Region, levels = c("Swiftsure","Haro"))

#ind = which(wind$Decile==0.0)
#wind = wind[-ind,]
rownames(wind) <- c(1:nrow(wind))

wind = reshape2::melt(wind, id = c("Year","MONTH","Decile"))
wind$Exceedance <- unlist(purrr::map(strsplit(as.character(wind$variable),"\\."), 1))
wind$Region <- unlist(purrr::map(strsplit(as.character(wind$variable),"\\."), 2))
wind$variable <- NULL
wind$Region <- factor(wind$Region, levels = c("Swiftsure","Haro"))

wind[which(wind$Decile==0.2),]

# Facet labels
Region.labs = c("Swiftsure Area", "Haro Strait")
names(Region.labs) = c("Swiftsure", "Haro")


# For echolocation space:
  # Looking at both 10m and 100m depth but in separate maps
for(m in unique(echo$MONTH)){
  for(d in unique(echo$Depth)){
    if(d=="Depth10"){
      dname = "10m depth"
    } else {
      dname = "100m depth"
    }
    # Create stacked bar charts
    p <- ggplot(data = echo[which(echo$Depth==d & echo$MONTH==m),], aes(x = factor(Exceedance, levels = c("D7","D8","D9")), y=value, fill = factor(Decile))) +
      geom_bar(width = 0.8, stat="identity") +
      scale_fill_brewer(type = "seq", palette = "RdYlBu", direction = 1, labels = c("0 - 20%", "21 - 40%", "41 - 60%", "61 - 80%", "81 - 100%"), name = "Echolocation\nrange available") +
      scale_x_discrete(name = "SRKW high intensity area",
                       labels = c("70%","80%","90%")) +
      scale_y_continuous(name = paste0("Proportion of area\nat ", dname),
                         breaks = c(0,0.5,1),
                         labels = c("0","0.5","1")) +
      theme_classic() +
      theme(axis.text.y = element_text(colour = "black"),
            axis.text.x = element_text(colour = "black"),
            plot.title = element_text(hjust = 0.5)) +
      facet_grid(cols = vars(Region),
                 labeller = labeller(Region = Region.labs))
    assign(paste(m,d,"echo","bars", sep = "_"), p)
  }
}





echo$MONTH <- factor(echo$MONTH, levels = c('May', 'June', 'July','August','September','October'))

echo$Exceedence<-recode(echo$Exceedance,D7="70", D8="80",D9="90")


# Pie charts for For echolocation space:
# Looking at both 10m and 100m depth but in separate maps
for(e in unique(echo$Exceedence)){
  for(d in unique(echo$Depth)){
    if(d=="Depth10"){
      dname = "10m depth"
    } else {
      dname = "100m depth"
    }
    # Create stacked bar charts
    p <- ggplot(data = echo[which(echo$Depth==d & echo$Exceedence==e),], aes(x = MONTH, y=value, fill = factor(Decile))) +
      geom_bar(width = 0.6, stat="identity") +# coord_polar("y", start=0)+
      scale_fill_brewer(type = "seq", palette = "RdYlBu", direction = 1, labels = c("0 - 20%", "21 - 40%", "41 - 60%", "61 - 80%", "81 - 100%"), name = "Echolocation\nrange available") +
      
      scale_x_discrete(name = paste0(e," %","high intensity area"),
                       labels = c("May","June","July","August","Sept","Oct")) +
             scale_y_continuous(name = paste0("Proportion of area at ", dname),
                         breaks = c(0,0.5,1),
                         labels = c("0","0.5","1")) +
      theme_classic() +
      theme(axis.text.y = element_text(colour = "black"),
            axis.text.x = element_text(colour = "black"),
            plot.title = element_text(hjust = 0.5)) +
      facet_grid(cols = vars(Region),
                 labeller = labeller(Region = Region.labs))
    assign(paste(d,e,"echo","bars", sep = "_"), p)
  }
}
scale_y_continuous(expand = c(0,0))
setwd(here("Outputs"))
EchoPie<-ggarrange(Depth10_70_echo_bars, Depth100_70_echo_bars, Depth10_80_echo_bars,
                  Depth100_80_echo_bars,Depth10_90_echo_bars, Depth100_90_echo_bars, common.legend = T,ncol = 2, nrow = 3 )
EchoPie<-annotate_figure(EchoPie,top = text_grob("SRKW 90% Probability Co-Occurrence of Echolocation Range", color = "black", face = "bold", size = 14))
EchoPie
ggsave(EchoPie,file="EchoBarAM.png",width = 28, height = 22, units = "cm")


# Pie charts for For Communication space:
# Looking at both 10m and 100m depth but in separate maps
comm$MONTH <- factor(comm$MONTH, levels = c('May', 'June', 'July','August','September','October'))

comm$Exceedence<-recode(comm$Exceedance,D7="70", D8="80",D9="90")

for(e in unique(comm$Exceedence)){
  for(d in unique(comm$Depth)){
    if(d=="Depth10"){
      dname = "10m depth"
    } else {
      dname = "100m depth"
    }
    # Create stacked bar charts
    p <- ggplot(data = comm[which(comm$Depth==d & comm$Exceedence==e),], aes(x = MONTH, y=value, fill = factor(Decile))) +
      geom_bar(width = 0.6, stat="identity") + #coord_polar("y", start=0)+
      scale_fill_brewer(type = "seq", palette = "RdYlBu", direction = 1, labels = c("0 - 20%", "21 - 40%", "41 - 60%", "61 - 80%", "81 - 100%"), name = "Communication\nrange available") +
      scale_x_discrete(name = paste0(e,"%","high intensity area"),
                       labels = c("May","June","July","August","September","October")) +
      scale_y_continuous(name = paste0("Proportion of area at ", dname),
                         breaks = c(0,0.5,1),
                         labels = c("0","0.5","1")) +
      theme_classic() +
      theme(axis.text.y = element_text(colour = "black"),
            axis.text.x = element_text(colour = "black"),
            plot.title = element_text(hjust = 0.5)) +
      facet_grid(cols = vars(Region),
                 labeller = labeller(Region = Region.labs))
    assign(paste(d,e,"comm","bars", sep = "_"), p)
  }
}

setwd(here("Outputs"))
CommPie<-ggarrange(Depth10_70_comm_bars,  Depth10_80_comm_bars,Depth10_90_comm_bars, common.legend = T,ncol = 1, nrow = 3 )
CommPie<-annotate_figure(CommPie,top = text_grob("SRKW 90% Probability Co-Occurrence of Communication Range", color = "black", face = "bold", size = 14))
CommPie
ggsave(CommPie,file="CommPieAM2.png",width = 22, height = 28, units = "cm")


# Pie charts for For Communication space:
# Looking at both 10m and 100m depth but in separate maps
wind$MONTH <- factor(wind$MONTH, levels = c('May', 'June', 'July','August','September','October'))

wind$Exceedence<-recode(wind$Exceedance,D7="70", D8="80",D9="90")
wind$Exceedence<-factor(wind$Exceedence)
wind$Year<-factor(wind$Year)


for(y in unique(wind$Year)){
  for(e in unique(wind$Exceedence)){
    if(e=="D90"){
      dname = "90"
      }else{
          if (e=="D80") {
      dname = "80"
    } else {
      dname ="70"
    }
      }
    # Create stacked bar charts
    p <- ggplot(data = wind[which(wind$Year==y & wind$Exceedence==e),], aes(x = MONTH, y=value, fill = factor(Decile))) +
      geom_bar(width = 0.8, stat="identity") +# coord_polar("y", start=0)+
      scale_fill_brewer(type = "seq", palette = "RdYlBu", direction = 1, labels = c("0 - 20%", "21 - 40%", "41 - 60%", "61 - 80%", "81 - 100%"), name = "Communication\nrange available") +
      scale_x_discrete(name = paste0(e,"%","high intensity area "),
                       labels = c("May","June","July","August","September","October")) +
      scale_y_continuous(name = paste0("Proportion of area at 25 m depth"),
                         breaks = c(0,0.5,1),
                         labels = c("0","0.5","1")) +
      theme_classic() +
      theme(axis.text.y = element_text(colour = "black"),
            axis.text.x = element_text(colour = "black"),
            plot.title = element_text(hjust = 0.5)) +
      facet_grid(cols = vars(Region),
                 labeller = labeller(Region = Region.labs))
    assign(paste("y",y,e,"wind","bars", sep = "_"), p)
  }
}

setwd(here("Outputs"))
windPie<-ggarrange(y_2018_70_wind_bars,y_2019_70_wind_bars,y_2020_70_wind_bars,y_2018_80_wind_bars,y_2019_80_wind_bars,y_2020_80_wind_bars,
                 y_2018_90_wind_bars,y_2019_90_wind_bars ,y_2020_90_wind_bars,common.legend = T,ncol = 3, nrow = 3, labels= c("2018","2019","2020"))
windPie<-annotate_figure(windPie,top = text_grob("SRKW 90% Probability Co-Occurrence of Communication Range", color = "black", face = "bold", size = 14))
windPie
ggsave(windPie,file="WindPieAM.png",width = 28, height = 22, units = "cm")



# For communication space:
  #Only looking at 10m depth
for(m in unique(comm$MONTH)){
  for(d in "Depth10"){
    if(d=="Depth10"){
      dname = "10m depth"
    } else {
      dname = "100m depth"
    }
    # Create stacked bar charts
    p <- ggplot(data = comm[which(comm$Depth==d & comm$MONTH==m),], aes(x = factor(Exceedance, levels = c("D7","D8","D9")), y=value, fill = factor(Decile))) +
      geom_bar(width = 0.8, stat="identity") +
      scale_fill_brewer(type = "seq", palette = "RdYlBu", direction = 1, labels = c("0 - 20%", "21 - 40%", "41 - 60%", "61 - 80%", "81 - 100%"), name = "Communication\nrange available") +
      scale_x_discrete(name = "SRKW high intensity area",
                       labels = c("70%","80%","90%")) +
      scale_y_continuous(name = paste0("Proportion of area\nat ", dname),
                         breaks = c(0,0.5,1),
                       labels = c("0","0.5","1")) +
      theme_classic() +
      theme(axis.text.y = element_text(colour = "black"),
            axis.text.x = element_text(colour = "black"),
            plot.title = element_text(hjust = 0.5)) +
      facet_grid(cols = vars(Region),
                 labeller = labeller(Region = Region.labs))
    assign(paste(m,d,"comm","bars", sep = "_"), p)
  }
}


# For communication-wind space:
# Looking at all year-months but in separate maps
for(m in unique(wind$MONTH)){
  for(y in unique(wind$Year)){
    # Create stacked bar charts
    p <- ggplot(data = wind[which(wind$Year==y & wind$MONTH==m),], aes(x = factor(Exceedance, levels = c("D7","D8","D9")), y=value, fill = factor(Decile))) +
      geom_bar(width = 0.8, stat="identity") +
      scale_fill_brewer(type = "seq", palette = "RdYlBu", direction = 1, labels = c("0 - 20%", "21 - 40%", "41 - 60%", "61 - 80%", "81 - 100%"), name = "Communication\nrange available") +
      scale_x_discrete(name = "SRKW high intensity area",
                       labels = c("70%","80%","90%")) +
      scale_y_continuous(name = paste0("Proportion of area\nat 25m depth"),
                         breaks = c(0,0.5,1),
                         labels = c("0","0.5","1")) +
      theme_classic() +
      theme(axis.text.y = element_text(colour = "black"),
            axis.text.x = element_text(colour = "black"),
            plot.title = element_text(hjust = 0.5)) +
      facet_grid(cols = vars(Region),
                 labeller = labeller(Region = Region.labs))
    assign(paste(m,y,"comm-wind","bars", sep = "_"), p)
  }
}


#----Generate raster maps----

# For echolocation rasters:
  # We will plot the rasters from 10m and 100m depth

for(m in unique(echo$MONTH)){
  for(d in unique(echo$Depth)){
    ind <- grep(paste0(m," 2018_",d,"Clip.tif"), names(echo.rasts))
    df <- echo.rasts[[ind]]
    ind <- grep(substr(m,1,3),names(exc.polys))
    polys <- exc.polys[ind]
    poly_D7 <- polys[grep("_D7_",names(polys))][[1]]
    poly_D8 <- polys[grep("_D8_",names(polys))][[1]]
    poly_D9 <- polys[grep("_D9_",names(polys))][[1]]
    comm_map <- ggplot() +
      geom_raster(data = df, aes_string(x="x", y="y", fill=names(df)[1]), 
                  show.legend = TRUE) +
      scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11,"RdYlBu"),
                           limits = c(0,100)) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.1, suffix = "\u00b0N")) +
      geom_sf(data = regions, col = "grey", fill = "grey", alpha = 0.1, lty = 2) +
      theme(panel.background = element_rect(fill = "darkgrey"),
            panel.grid.major = element_blank(),
            panel.border = element_rect(linetype = "solid", fill = NA),
            axis.text.x = element_text(color="black",size = 10),
            axis.text.y = element_text(color="black",size = 10),
            axis.title = element_blank()) +
      geom_sf(data = poly_D7, aes(colour = "70%"), fill = NA) +
      geom_sf(data = poly_D8, aes(colour = "80%"), fill = NA) +
      geom_sf(data = poly_D9, aes(colour = "90%"), fill = NA) +
      scale_colour_manual(values = exc_palette_green) +
      geom_sf(data = CHS_crop_simp, fill = "#c2c2c2", show.legend = FALSE) +
      coord_sf(xlim = c(bb$xmin, bb$xmax), ylim = c(bb$ymin, bb$ymax), expand = FALSE) +
      guides(fill = guide_colourbar(title = "% echolocation\nrange available"),
             colour = guide_legend(title = "Areas of high\nSRKW intensity", override.aes = list(fill = NA, lwd = 1)))
    assign(paste(m,d,"echo_map", sep = "_"), comm_map)
  }
}



# For communication rasters:
  # We will only plot the rasters from 10m depth
for(m in unique(comm$MONTH)){
  # plot raster with exceedance polygons (and regional boundary)
  ind <- grep(m, names(comm.rasts))
  df <- comm.rasts[[ind]]
  ind <- grep(substr(m,1,3),names(exc.polys))
  polys <- exc.polys[ind]
  poly_D7 <- polys[grep("_D7_",names(polys))][[1]]
  poly_D8 <- polys[grep("_D8_",names(polys))][[1]]
  poly_D9 <- polys[grep("_D9_",names(polys))][[1]]
  comm_map <- ggplot() +
  geom_raster(data = df, aes_string(x="x", y="y", fill=names(df)[1]), 
            show.legend = TRUE) +
  scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11,"RdYlBu"),
                       limits = c(0,100)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.1, suffix = "\u00b0N")) +
  geom_sf(data = regions, col = "grey", fill = "grey", alpha = 0.1, lty = 2) +
  theme(panel.background = element_rect(fill = "darkgrey"),
        panel.grid.major = element_blank(),
        panel.border = element_rect(linetype = "solid", fill = NA),
        axis.text.x = element_text(color="black",size = 10),
        axis.text.y = element_text(color="black",size = 10),
        axis.title = element_blank()) +
  geom_sf(data = poly_D7, aes(colour = "70%"), fill = NA) +
  geom_sf(data = poly_D8, aes(colour = "80%"), fill = NA) +
  geom_sf(data = poly_D9, aes(colour = "90%"), fill = NA) +
  scale_colour_manual(values = exc_palette_green) +
  geom_sf(data = CHS_crop_simp, fill = "#c2c2c2", show.legend = FALSE) +
  coord_sf(xlim = c(bb$xmin, bb$xmax), ylim = c(bb$ymin, bb$ymax), expand = FALSE) +
  guides(fill = guide_colourbar(title = "% communication\nrange available"),
         colour = guide_legend(title = "Areas of high\nSRKW intensity", override.aes = list(fill = NA, lwd = 1)))
  assign(paste(m,"Depth10","comm_map", sep = "_"), comm_map)
}


# For wind-communication rasters:
# We will plot the rasters from each year

for(m in unique(wind$MONTH)){
  for(y in unique(wind$Year)){
    ind <- grep(paste0(y,"_",m,".nc.tif"), names(wind.rasts))
    df <- wind.rasts[[ind]]
    ind <- grep(substr(m,1,3),names(exc.polys))
    polys <- exc.polys[ind]
    poly_D7 <- polys[grep("_D7_",names(polys))][[1]]
    poly_D8 <- polys[grep("_D8_",names(polys))][[1]]
    poly_D9 <- polys[grep("_D9_",names(polys))][[1]]
    comm_map <- ggplot() +
      geom_raster(data = df, aes_string(x="x", y="y", fill=names(df)[1]), 
                  show.legend = TRUE) +
      scale_fill_gradientn(colours = RColorBrewer::brewer.pal(11,"RdYlBu"),
                           limits = c(0,100)) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.1, suffix = "\u00b0N")) +
      geom_sf(data = regions, col = "grey", fill = "grey", alpha = 0.1, lty = 2) +
      theme(panel.background = element_rect(fill = "darkgrey"),
            panel.grid.major = element_blank(),
            panel.border = element_rect(linetype = "solid", fill = NA),
            axis.text.x = element_text(color="black",size = 10),
            axis.text.y = element_text(color="black",size = 10),
            axis.title = element_blank()) +
      geom_sf(data = poly_D7, aes(colour = "70%"), fill = NA) +
      geom_sf(data = poly_D8, aes(colour = "80%"), fill = NA) +
      geom_sf(data = poly_D9, aes(colour = "90%"), fill = NA) +
      scale_colour_manual(values = exc_palette_green) +
      geom_sf(data = CHS_crop_simp, fill = "#c2c2c2", show.legend = FALSE) +
      coord_sf(xlim = c(bb$xmin, bb$xmax), ylim = c(bb$ymin, bb$ymax), expand = FALSE) +
      guides(fill = guide_colourbar(title = "% communication\nrange available"),
             colour = guide_legend(title = "Areas of high\nSRKW intensity", override.aes = list(fill = NA, lwd = 1)))
    assign(paste(m,y,"wind-comm_map", sep = "_"), comm_map)
  }
}


#----Save plots----
setwd(here("Outputs"))
setwd("C:/Users/stredulinskye/Documents/PROJECTS/CSAS SRKW - Feb 2021/Co-occurrence mapping/OUTPUTS")

# Individual months
# make all monthly plots and pannelled plot with all months
  # For echolocation, depths paired
for(m in unique(echo$MONTH)){
  mp <- ggarrange(ggarrange(mget(paste0(m,"_Depth10_echo_map"))[[1]],
                            mget(paste0(m,"_Depth100_echo_map"))[[1]],
                            nrow = 1,
                            common.legend = TRUE,
                            legend = "right",
                            labels = c("10m depth", "100m depth"),
                            hjust = -2),
                  ggarrange(mget(paste0(m,"_Depth10_echo_bars"))[[1]],
                            mget(paste0(m,"_Depth100_echo_bars"))[[1]],
                            nrow = 1,
                            common.legend = TRUE,
                            legend = "right"),
                  nrow = 2,
                  heights = c(2,1))
  mpf <- annotate_figure(mp,
                         top = text_grob(paste0(m),
                                         hjust = 1,
                                         face = "bold",
                                         size = 20))
  assign(paste0("SRKW echolocation co-occurrence_",m), mpf)
  ggsave(paste0("SRKW echolocation co-occurrence_",m,".png"), plot = mpf, units = "in", width = 10, height = 6, type = "cairo")
}

# For communication:
for(m in unique(comm$MONTH)){
  mp <- ggarrange(mget(paste0(m,"_Depth10_comm_map"))[[1]],
                  mget(paste0(m,"_Depth10_comm_bars"))[[1]],
                  ncol = 1,
                  heights = c(2,1))
  mpf <- annotate_figure(mp,
                         top = text_grob(paste0(m),
                                         face = "bold",
                                         size = 20,
                                         hjust = 1))
  assign(paste0("SRKW communication co-occurrence_",m), mpf)
  ggsave(paste0("SRKW communication co-occurrence_",m,".png"), plot = mpf, units = "in", width = 8, height = 5.5, type = "cairo")
}


# For wind-communication, years set
for(m in unique(wind$MONTH)){
  mp <- ggarrange(ggarrange(mget(paste0(m,"_2018_wind-comm_map"))[[1]],
                            mget(paste0(m,"_2019_wind-comm_map"))[[1]],
                            mget(paste0(m,"_2020_wind-comm_map"))[[1]],
                            nrow = 1,
                            common.legend = TRUE,
                            legend = "right",
                            labels = c("2018", "2019", "2020"),
                            hjust = -2,
                            vjust = 3),
                  ggarrange(mget(paste0(m,"_2018_comm-wind_bars"))[[1]],
                            mget(paste0(m,"_2019_comm-wind_bars"))[[1]],
                            mget(paste0(m,"_2020_comm-wind_bars"))[[1]],
                            nrow = 1,
                            common.legend = TRUE,
                            legend = "right"),
                  nrow = 2,
                  heights = c(2.2,1.4))
  mpf <- annotate_figure(mp,
                         top = text_grob(paste0(m),
                                         hjust = 1,
                                         face = "bold",
                                         size = 20))
  assign(paste0("SRKW wind-communication co-occurrence_",m), mpf)
  ggsave(paste0("SRKW wind-communication co-occurrence_",m,".png"), plot = mpf, units = "in", width = 12, height = 5.5, type = "cairo")
}


# All months panelled
# ----figure out how to suppress legends and only have one per multiplot---
  # For echolocation 2 months per page
mp <- ggarrange(`SRKW echolocation co-occurrence_May`,
          `SRKW echolocation co-occurrence_June`,
          nrow = 2)
ggsave(paste0("SRKW echolocation co-occurrence_monthspanelled1.png"), plot = mp, units = "in", width = 10, height = 11, type = "cairo")

mp <- ggarrange(`SRKW echolocation co-occurrence_July`,
                `SRKW echolocation co-occurrence_August`,
                nrow = 2)
ggsave(paste0("SRKW echolocation co-occurrence_monthspanelled2.png"), plot = mp, units = "in", width = 10, height = 11, type = "cairo")

mp <- ggarrange(`SRKW echolocation co-occurrence_September`,
                `SRKW echolocation co-occurrence_October`,
                nrow = 2)
ggsave(paste0("SRKW echolocation co-occurrence_monthspanelled3.png"), plot = mp, units = "in", width = 10, height = 11, type = "cairo")


  # For communication all 6 months in the page
mp <- ggarrange(`SRKW communication co-occurrence_May` + 
                  theme(legend.position = "none"),
                `SRKW communication co-occurrence_June` + 
                  theme(legend.position = "none"),
                `SRKW communication co-occurrence_July` + 
                  theme(legend.position = "none"),
                `SRKW communication co-occurrence_August`,
                `SRKW communication co-occurrence_September`+ 
                  theme(legend.position = "none"),
                `SRKW communication co-occurrence_October` + 
                  theme(legend.position = "none"),
                ncol = 2,
                nrow = 3)
ggsave(paste0("SRKW echolocation co-occurrence_allmonths.png"), plot = mp, units = "in", width = 12, height = 16, type = "cairo")
