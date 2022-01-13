#################################################################################
#################################################################################
##                    EFFORT-CORRECTED PRESENCE MODELLING                      ##
##                              Initial Analysis                               ##
#################################################################################
#################################################################################

#----SCRIPT INFORMATION----
#Author: Scott Toews and Eva Stredulinsky


#----Clear R environment----

rm(list = ls(all=TRUE)) ; ls()
#.libPaths('C:/Users/Acoustic 1/Documents/SRKW_Effort/Library')
#.libPaths('media/stoews/T7/1-DFO/1-Analyses/SRKW_Effort/Library')
#.libPaths('D:/1-DFO/1-Analyses/SRKW_Effort/Library')
#----Load required packages----

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
library(hrbrthemes)
library(viridis)
memory.size() ### Checking your memory size
memory.limit() ## Checking the set limit
memory.limit(size=56000)

#----Load functions----

'%ni%' <- Negate('%in%')

#Get TRUE/FALSE whether a time falls within a set of time intervals
within_check <- function(time, list.of.intervals){
  any(time %within% list.of.intervals)
}

setwd(here("Data","Coast shapefile"))
COAST=readOGR(".",layer="coast_ALBERS", verbose = FALSE)


BCCSNAlbers <- read.csv("data/BCCSN Salish Sea Sightings May-Oct 2009-2020_Stredulinsky DFO.csv")
Combined <- read.csv("data/SmrCombinedduplicatesRemoved_BCCSNAlbers.csv")
OM2018<-read.csv("data/OM_Data.csv")
summary(BCCSNAlbers)
summary(Combined)
summary(OM2018)

df<-BCCSNAlbers
#For Combined Only
df<-Combined
df$DateT <- as.POSIXct(df$DateTime *3600*24, origin="1900-01-01", tz="GMT")
df$Date<-as.Date(df$DateT)



str(df)

df$Date<-as.POSIXct(df$SightingDate, format ="%m/%d/%Y")
df$Date


df %>% count(Date)

df$Year <- as.numeric(format(df$Date, "%Y"))
df$Month<-as.numeric(format(df$Date,"%m"))
df <- df[which(df$month > 1),]
df <- df[which(df$month < 11),]  
#df$year[df$year=="3012"]<-"2012"
str(df)
df<-df[which(df$SpeciesType =="southern resident")]
df<-filter(df, SpeciesType %in% c("southern resident", "possible southern resident"))




s<-df%>%
group_by(Year,Month)%>%
  summarise(id = n_distinct(Date))
str(s)

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

