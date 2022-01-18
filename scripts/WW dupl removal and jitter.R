#########################################################################################
#                                                                                       #
#                         Non-DFO Sightings Data Prep                                   #
#                                                                                       #
#########################################################################################

# Eva Stredulinsky (12-12-2020)

# Once raw data from TWM and BCCSN was recieved, datasets were modified to match the formatting of the 2009-2016 "WW" data and records without time were removed. This resulted in: "BCCSN South Salish Sea 2018-05 to 2020-09_Time_MOD.csv" & "DFO_SRKW2017-18_Sources_MOD.csv" found in this directory.
# In this script, these two datasets will be appended and we will run duplicate removal and jittering according to SMRU's protocol descriptions from the 2009-2016 analysis.


#----Clear R environment----

rm(list = ls(all=TRUE)) ; ls()


#----Load packages----

suppressMessages(library('here'))
suppressMessages(library('sf'))
suppressMessages(library('dplyr'))
suppressMessages(library('ggplot2'))
suppressMessages(library('viridis'))
library(hrbrthemes)
#----Set working directory----

setwd(here("data","NGO data"))


#----Load required functions----

'%ni%' <- Negate('%in%')

#Convert decimal degrees to radians
radians <- function(x){pi*x/180}

#Calculate the distance between two positions in nautical miles
PosDist <- function(Lat1,Lon1,Lat2,Lon2){
  if(Lat1==Lat2 & Lon1==Lon2){
    Posdist = 0
  } else {
    Rlat1 = radians(Lat1)
    Rlat2 = radians(Lat2)
    Rlon = radians(Lon2 - Lon1)
    Posdist = 60 * (180 / pi) * acos(sin(Rlat1) * sin(Rlat2) + cos(Rlat1) * cos(Rlat2) * cos(Rlon))
  }
  return(Posdist)
}

equivalent <- function(v1, v2){
  if(v1==v2){
    sh <- 1
  } else {
    sh <- 0
  }
  return(sh)
}


# Duplicate removal
  #This provides the row indices for all duplicates requiring removal (lower ranked of sightings within 1 nm and 1 hr of one another)

duplicates <- function(df, type){
  # Calculate pairwise time differences
  # Set time as POSIXct
  df$DT <- as.POSIXct(df$DateTime *3600*24, origin="1900-01-01", tz="GMT")
  tout <- outer(df$DT, df$DT, units = "mins", FUN = Vectorize(difftime))
  tout[upper.tri(tout, diag = TRUE)] <- NA
  tout <- reshape2::melt(tout)
  names(tout) <- c("Row1","Row2","TimeDiff")
  tout_dup <- tout[which(tout$TimeDiff<=60),]
  rownames(tout_dup) <- c(1:nrow(tout_dup))
  #are we looking for duplicates "within" a dataset (1nm/1hr) or "between" datasets (US/CAN)?
  if(type=="within"){
    # Filter time 'duplicates' for only those from same source family
    tout_dup$Fam1 <- df[tout_dup$Row1,]$Source.family
    tout_dup$Fam2 <- df[tout_dup$Row2,]$Source.family
    tout_dup <- tout_dup[which(tout_dup$Fam1==tout_dup$Fam2),c(1:4)]
    rownames(tout_dup) <- c(1:nrow(tout_dup))
    # Calculate pairwise distances for all pairs in tout_dup
    ww_xy <- st_as_sf(df, coords = c('lon', 'lat'), crs = 3005, stringsAsFactors = FALSE, remove = TRUE)
    ww_xy <- st_transform(ww_xy, 4326)
    xy <- st_coordinates(ww_xy)
    tout_dup$Dist_nm <- mapply(PosDist, xy[tout_dup$Row1,2], xy[tout_dup$Row1,1], xy[tout_dup$Row2,2], xy[tout_dup$Row2,1])
    dupl <- tout_dup[which(tout_dup$Dist_nm<=1),]
    rownames(dupl) <- c(1:nrow(dupl))
    # Get rankings for duplicates
    dupl$Rank1 <- NA
    dupl$Rank2 <- NA
    dupl[which(dupl$Fam1=="OM"),]$Rank1 <- df[dupl[which(dupl$Fam1=="OM"),]$Row1,]$OM.RANK
    dupl[which(dupl$Fam1=="OM"),]$Rank2 <- df[dupl[which(dupl$Fam1=="OM"),]$Row2,]$OM.RANK
    dupl[which(dupl$Fam1=="BCCSN"),]$Rank1 <- df[dupl[which(dupl$Fam1=="BCCSN"),]$Row1,]$Pod.rank
    dupl[which(dupl$Fam1=="BCCSN"),]$Rank2 <- df[dupl[which(dupl$Fam1=="BCCSN"),]$Row2,]$Pod.rank
    #Compare rankings to select records that need to be removed
    dupl$rem <- as.numeric(dupl$Rank1>dupl$Rank2)
    dupl[which(dupl$rem==0),]$rem <- 2
    dupl$rem.rec <- NA
    dupl[which(dupl$rem==1),]$rem.rec <- dupl[which(dupl$rem==1),]$Row1
    dupl[which(dupl$rem==2),]$rem.rec <- dupl[which(dupl$rem==2),]$Row2
    duplicate.records <- sort(unique(dupl$rem.rec))
  } else {
    # Filter time 'duplicates' for only those from different source families
    tout_dup$Fam1 <- df[tout_dup$Row1,]$Source.family
    tout_dup$Fam2 <- df[tout_dup$Row2,]$Source.family
    tout_dup <- tout_dup[which(tout_dup$Fam1!=tout_dup$Fam2),]
    rownames(tout_dup) <- c(1:nrow(tout_dup))
    # Calculate pairwise distances for all pairs in tout_dup
    ww_xy <- st_as_sf(df, coords = c('lon', 'lat'), crs = 3005, stringsAsFactors = FALSE, remove = TRUE)
    ww_xy <- st_transform(ww_xy, 4326)
    xy <- st_coordinates(ww_xy)
    tout_dup$Dist_nm <- mapply(PosDist, xy[tout_dup$Row1,2], xy[tout_dup$Row1,1], xy[tout_dup$Row2,2], xy[tout_dup$Row2,1])
    dupl <- tout_dup[which(tout_dup$Dist_nm<=1),]
    rownames(dupl) <- c(1:nrow(dupl))
    # Select record to keep depending on whether sighting falls in Canada/US
          #Load in EEZ shapefile
    eez <- st_read(here("Data","Basemap shapefiles","SDE_Boundaries_patched_NAD83_Albers.shp"), quiet = TRUE)
    st_crs(eez) <- 3005
    ww_xy <- st_as_sf(df, coords = c('lon', 'lat'), crs = 3005, stringsAsFactors = FALSE, remove = TRUE)
    #Find points that fall within Canadian EEZ
    can <- as.numeric(as.character(rownames(st_drop_geometry(ww_xy[eez,]))))
    dupl$WATERS <- "OM"
    dupl[which(dupl$Row1 %in% can | dupl$Row2 %in% can),]$WATERS <- "BCCSN"
    dupl$rem <- NA
    for(i in 1:nrow(dupl)){
      dupl[i,]$rem <- as.numeric(as.character(dupl[i,1:2][which(dupl[i,4:5]!=dupl[i,]$WATERS)]))
    }
    duplicate.records <- sort(unique(dupl$rem))
  }
  return(duplicate.records)
}


#----Load data----

bccsn1 <- read.csv("BCCSN Salish Sea Sightings May-Oct 2009-2020_Stredulinsky DFO.csv", header=TRUE, stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA","na","n/a","N/A",""))
nrow(bccsn1)
#29296
str(bccsn1)
bccsn1$Date<-as.POSIXct(bccsn1$SightingDate, format ="%m/%d/%Y")
bccsn1$Month<-as.numeric(format(bccsn1$Date,"%m"))
bccsn1$Day<-as.numeric(format(bccsn1$Date,"%d"))
bccsn1$Year <- as.numeric(format(bccsn1$Date, "%Y"))
bccsn1 <- bccsn1[which(bccsn1$Month > 1),]
bccsn1 <- bccsn1[which(bccsn1$Month < 11),] 
bccsn1$Year[bccsn1$Year=="3012"]<-"2012"
bccsn1$Year[bccsn1$Year=="2047"]<-"2017"
bccsn1$Year[bccsn1$Year=="3041"]<-"2011"
bccsn1$Year[bccsn1$Year=="2041"]<-"2011"
unique(bccsn1$Year)
bccsn1 <- bccsn1[which(bccsn1$Year > 2016),]  
unique(bccsn1$SpeciesCommonName)
bccsn1<-filter(bccsn1,SpeciesType %in% c("southern resident", "possible southern resident"))
str(bccsn1)
csn<-subset(bccsn1, select=c(15:18))
str(csn)


#bccsn2 <- read.csv("BCCSN Sightings South Salish Sea May-Oct17&Oct20_MOD.csv", header=TRUE, stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA","na","n/a","N/A",""))
#nrow(bccsn2)

bccsn0<-read.csv("SmrCombinedduplicatesRemoved_BCCSNAlbers.csv", header=T, stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA","na","n/a","N/A",""))
str(bccsn0)
nrow(bccsn0)
om <- read.csv("DFO_SRKW2017-18_Sources_MOD.csv", header=TRUE, stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA","na","n/a","N/A",""))
nrow(om)


#----Append data----

ww <- rbind(bccsn0,om)
ww <- rbind(ww,csn)
str(ww)
ww <- ww[order(ww$DateTime),]
nrow(bccsn1)+nrow(bccsn2)+nrow(om)
ww$Date <- as.Date(with(ww, paste(Year, Month, Day,sep="-")), "%Y-%m-%d")
ww$Date
str(ww)
ww<-subset(ww, select=c(4,5,6,25))

#----Limit time series---
  #We are only including 2017 & 2018 sightings data
unique(ww$Year)
ww <- ww[which(ww$Year < 2019),]
rownames(ww) <- c(1:nrow(ww))

nrow(ww)




s<-ww%>%
  group_by(Year,Month)%>%
  summarise(id = n_distinct(Date))
str(s)
s$Year<-as.numeric(s$Year)
s$Month<-as.numeric(s$Month)
#s$Date<-paste(s$Year, s$Month)
s$Date<-as.Date(with(s, paste(Year, Month,"1", sep="-")), "%Y-%m-%d")
s <- s[order(s$Date),]

ggplot(data=s, aes(x=Year, y=id, group=Year)) +
  geom_boxplot()+
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot with jitter") +
  xlab("")


ggplot(s, aes(x=Date, y=id,group=1)) +
  geom_line()+
scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    plot.title = element_text(size=11)
  ) +
  ggtitle("A boxplot with jitter") +
  xlab("")
#----Populate projected coordinates----

  # lon and lat are already in metres (NAD83 Albers)
  # need to transform to NAD83 UTM 10N to populate PlotUTMn and PlotUTMe
ww_xy <- st_as_sf(ww, coords = c('lon', 'lat'), crs = 3005, stringsAsFactors = FALSE, remove = TRUE)
ww_xy <- st_transform(ww_xy, 26910)
xy <- st_coordinates(ww_xy)
head(xy)
ww$PlotUTMn <- xy[,2]
ww$PlotUTMe <- xy[,1]

plot(ww$lat~ww$lon)
plot(ww$PlotUTMn~ww$PlotUTMe)


#----Duplicate removal----

# See Jason's protocol doc: "Notes on steps taken by JW... .doc"

# Assign rankings by Pod
    #Actual Pod ID ranked 1
    #Pod ID with ? is ranked 2
    #SRKW is ranked 3
    #Orcas is ranked 4
unique(ww$Pod)
  #Remove "L87" from pod field
ww[which(ww$Pod=="L87?"),]$Pod <- "SRKW"
ww$Pod <- gsub("L87","", ww$Pod)
unique(ww$Pod)

ww$Pod.rank <- 1
ww[grep("\\?",ww$Pod),]$Pod.rank <- 2
ww[which(ww$Pod=="SRKW"),]$Pod.rank <- 3
table(ww$Pod, ww$Pod.rank)

# Assign rankings by Source (only relevant for OM)
    # Soundwatch and Otis are ranked 1
    # Whale watched is ranked 2
    # Spot is ranked 3
    # Pager is ranked 4
    # Reliable is ranked 5
    # Hydrophone is ranked 6
    # Public is ranked 7
ww$Source.rank <- NA
ww[which(ww$Source %in% c("TWM-SW","TWM-Otis")),]$Source.rank <- 1
ww[which(ww$Source %in% c("TWM-SA-WW")),]$Source.rank <- 2
ww[which(ww$Source %in% c("SPOT")),]$Source.rank <- 3
# ww[which(ww$Source %in% c("TWM-Pager")),]$Source.rank <- 4  #No pager data exists for 2017-2018
ww[which(ww$Source %in% c("TWM-SA-Rel")),]$Source.rank <- 5
ww[which(ww$Source %in% c("TWM-HYD-Rel","TWM-HYD-Pub")),]$Source.rank <- 6
ww[which(ww$Source %in% c("TWM-SA-Pub")),]$Source.rank <- 7

ww$OM.RANK <- ww$Pod.rank + ww$Source.rank


# Finding duplicates
    # All data within 1 hour and 1 nautical mile of each other were considered duplicates
# ----ISSUE: PAPER SAYS WITHIN 2 nmi but JASON'S NOTES SAY WITHIN 1 nmi...----
    # OM data treated independently from BCCSN data
    # Duplicates among OM data picks highest ranked by OM.RANK
    # Duplicates among BCCSN data picks highest ranked by Pod.rank
    # Duplicates between OM and BCCSN removed based on Country of the sighting

  # Distinguish OM from BCCSN data (by Source)
ww$Source.family <- "OM"
ww[grep("BCCSN", ww$Source),]$Source.family <- "BCCSN"

  #Remove duplicates (within each data source first, based on ranking)
d <- duplicates(ww,"within")
if(length(d)!=0){
  ww <- ww[-d,]
  rownames(ww) <- c(1:nrow(ww))
}

nrow(ww)
# [1] 1368

  #Remove duplicates (between the data sources next, based on US/CAN waters)
d <- duplicates(ww,"between")
if(length(d)!=0){
  ww <- ww[-d,]
  rownames(ww) <- c(1:nrow(ww))
}
nrow(ww)
# [1] 1268

ww$Pod.rank <- NULL
ww$Source.rank <- NULL
ww$OM.RANK <- NULL
ww$Source.family <- NULL


#----Jitter locations----

# From a JW email subject: "jitterbug" 20 December 2017 at 15:34
# The OrcaMaster data to use is here.
# https://smrumarine.app.box.com/file/247110492024
# and the shapefile of the quadrants are here.
# https://smrumarine.app.box.com/folder/43557762037

library(sp)
library(rgdal)
library(here)

setwd(here("Data","NGO data","orcasfq"))
GRIDs <- readOGR(".",'orcasfq')

OM <- ww

temp <- OM[OM$GPS==0,]
rownames(temp) <- 1:nrow(temp)
dFirstTroid <- sapply(split(temp, temp$Quadrant), 
                      function(x) min(as.numeric(rownames(x))))

par(mfrow=c(1,1), oma=c(2,2,1,1), mar=c(0,0,0,0))
plot(NA, xlim=c(410000,560000),
     ylim=c(5220000,5480000), axes=F, ylab="UTM Northing", xlab="UTM Easting")
for (i in 1:length(GRIDs)) {
  lines(GRIDs@polygons[[i]]@Polygons[[1]]@coords[,1],GRIDs@polygons[[i]]@Polygons[[1]]@coords[,2])
}
points(OM[OM$GPS==1,'PlotUTMe'],OM[OM$GPS==1,'PlotUTMn'],col=rgb(0,0,1,.5), cex=.2,pch=16)
points(OM[dFirstTroid,'PlotUTMe'],OM[dFirstTroid,'PlotUTMn'],col=rgb(1,0,1,1), cex=.4,pch=16)
mtext(side=1,line=0,'UTM Easting')
mtext(side=2,line=0,'UTM Northing')
mtext(side=3,line=-1,'Salish Sea GPS Sightings\nand Centroids',cex=1.2)

length(table(OM$Quadrant))
length(table(OM$Quadrant[OM$GPS==1]))
# 106 quadrants wherein GPS data is found
length(table(OM$Quadrant[OM$GPS==0]))
# 77 quadrants wherein centroid data is found
sum(unique(OM[which(OM$GPS==0),]$Quadrant) %ni% unique(OM[which(OM$GPS==1),]$Quadrant))
unique(OM[which(OM$GPS==0),]$Quadrant)[unique(OM[which(OM$GPS==0),]$Quadrant) %ni% unique(OM[which(OM$GPS==1),]$Quadrant)]
# 30 quadrants with centroid data only - which means they won't be jittered...

library(rgdal)
library(mgcv)
library(sp)

set.seed=0; jitter.centroids=NULL
for (i in 1:length(GRIDs)) {
  # for (i in 295:305) {
  idx1 <- which(in.out(GRIDs@polygons[[i]]@Polygons[[1]]@coords,cbind(OM[OM$GPS==1,'PlotUTMe'],OM[OM$GPS==1,'PlotUTMn'])   ))
  idx0 <- which(in.out(GRIDs@polygons[[i]]@Polygons[[1]]@coords,cbind(OM[OM$GPS==0,'PlotUTMe'],OM[OM$GPS==0,'PlotUTMn'])   ))
  # get the row number from OM where GPS==0
  idx2 <- cbind(1:nrow(OM),OM$GPS,OM[,'PlotUTMe'],OM[,'PlotUTMn'])[(in.out(GRIDs@polygons[[i]]@Polygons[[1]]@coords,cbind(OM[,'PlotUTMe'],OM[,'PlotUTMn'])   )),]
  
  if(length(idx1)>0) {
    bnd <- GRIDs@polygons[[i]]@Polygons[[1]]@coords # grid boundary
    pts <- cbind(OM[OM$GPS==1,'PlotUTMe'],OM[OM$GPS==1,'PlotUTMn'])[idx1,] # GPS points
    exp.grid <- expand.grid(bnd[,1],bnd[,2])
    # get the closest exp.grid to 'pts' and add the weight to that exp.grid and add runif noise
    if (length(pts)== 2) {closest <- rep(NA, 1)
    closest <- which.min(spDists(as.matrix(cbind(pts[1], pts[2])),as.matrix(exp.grid)))
    sampe <- rep(closest, length(idx0), replace=T); 
    if (length(idx0)==0) rowidx <- NULL
    if (length(idx0) > 0 & length(idx2)>4)  rowidx <- idx2[idx2[,2]==0,1]
    if (length(idx0) > 0 & length(idx2)==4) rowidx <- idx2[1]
    if (length(idx2)>4) {print(paste(i,"dim = ", nrow(idx2)))}
    sampe.picked=TRUE}
    if (length(pts) > 2) {closest <- rep(NA, nrow(pts))
    for (row.num in 1:nrow(pts)) {closest[row.num] <- which.min(spDists(as.matrix(cbind(pts[row.num,1], pts[row.num,2])),as.matrix(exp.grid)))}
    rowidx <- idx2[idx2[,2]==0,1]
    sampe.picked=FALSE}
    
    if (sampe.picked==FALSE) { sampe <- sample(closest, length(idx0), replace=T) }
    # troids <- cbind(OM[OM$GPS==0,'PlotUTMe'],OM[OM$GPS==0,'PlotUTMn'])[idx0,] # GPS points
    troids.new <- cbind(exp.grid[sampe,1]+runif(length(sampe),-0.5*min(diff(sort(unique(exp.grid[,1])))),0.5*min(diff(sort(unique(exp.grid[,1]))))),
                        exp.grid[sampe,2]+runif(length(sampe),-0.5*min(diff(sort(unique(exp.grid[,2])))),0.5*min(diff(sort(unique(exp.grid[,2]))))),
                        rowidx)
    jitter.centroids <- rbind(jitter.centroids,troids.new)
  }
  if (i%%5==0) print(i)
}


# merge two datasets
jitter.centroids <- as.data.frame(jitter.centroids)
dim(jitter.centroids)
names(jitter.centroids)[c(1,2)] = c("jitter.UTMe","jitter.UTMn")
OM$row.idx <- 1:nrow(OM)
temp.merge <- merge(OM, jitter.centroids,by.x="row.idx", by.y="rowidx", all.x=T)
tail(temp.merge[,c('row.idx','Quadrant','lat','PlotUTMe','jitter.UTMe','GPS')],100)
dim(OM)

temp.merge$PlotUTMMerge.e <- ifelse(temp.merge$GPS==0, temp.merge$jitter.UTMe, temp.merge$PlotUTMe) 
temp.merge$PlotUTMMerge.n <- ifelse(temp.merge$GPS==0, temp.merge$jitter.UTMn, temp.merge$PlotUTMn) 
dim(temp.merge)
c(names(temp.merge)[!is.element(names(temp.merge) , names(OM))])

OM.final <- temp.merge

length(which(is.na(OM.final$PlotUTMMerge.e)))
  #50 points that couldn't be jittered (because they were in one of the 30 quadrats that did not contain any GPS data)

dev.off()
# quartz(height=6,width=3.5)
par(mfrow=c(1,1), oma=c(2,2,1,1), mar=c(0,0,0,0))
plot(NA, xlim=c(410000,560000),
     ylim=c(5220000,5480000), axes=F, ylab="UTM Northing", xlab="UTM Easting")
for (i in 1:length(GRIDs)) {
  lines(GRIDs@polygons[[i]]@Polygons[[1]]@coords[,1],GRIDs@polygons[[i]]@Polygons[[1]]@coords[,2])
}
points(OM[OM$GPS==1,'PlotUTMe'],OM[OM$GPS==1,'PlotUTMn'],col=rgb(0,0,1,.5), cex=.2,pch=16)
points(jitter.centroids[,1],jitter.centroids[,2],col=rgb(1,.2,1,.5), cex=.2,pch=16)
head(jitter.centroids)
#points(OM[dFirstTroid,'PlotUTMe'],OM[dFirstTroid,'PlotUTMn'],col=rgb(1,0,1,1), cex=.4,pch=16)
mtext(side=1,line=0,'UTM Easting')
mtext(side=2,line=0,'UTM Northing')
#mtext(side=3,line=-1,'Salish Sea GPS Sightings',cex=1.2)
mtext(side=3,line=-1.5,'Salish Sea GPS Sightings\nand Jittered Centroids',cex=1.2)


# According to Jason's methods write-up, points that could not be jittered just use the grid centroid value
OM.final[which(is.na(OM.final$PlotUTMMerge.e)),]$PlotUTMMerge.e <- OM.final[which(is.na(OM.final$PlotUTMMerge.e)),]$PlotUTMe
OM.final[which(is.na(OM.final$PlotUTMMerge.n)),]$PlotUTMMerge.n <- OM.final[which(is.na(OM.final$PlotUTMMerge.n)),]$PlotUTMn


#----Write out data----

setwd(here("Data","NGO data"))
getwd()
write.csv(OM.final,'OM-BCCSN_2017-2018_duplrem_jittered.csv')
