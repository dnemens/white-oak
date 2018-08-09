oaks <- read.csv("C:/Users/dnemens/Dropbox/OWO/white-oak/data sheets/post_merge_forR.csv")

library (plyr)
library (dplyr)
require (fitdistrplus)
require (logspline)
library (vegan)

oaks = oaks[!oaks$tag==469,] #removes tree that has no spatial coordinates

#imports coordinates for spatial autocorrelation test (Mantel)
#lat2 <- oaks$Lat
#lat1 <- na.omit(lat2)
lat <- oaks$Lat-(min(oaks$Lat))
lat.r <- decostand(x=lat, method = "range")

#long2 <- oaks$Long
#long1 <- na.omit(long2)
long <- oaks$Long-(min(oaks$Long))
long.r <- decostand(x=long, method = "range")

# imports tree characteristics (first set of explanatory variables)
site <- oaks$site
height <- oaks$height.m
cbh <- oaks$crown.base.ht.m
dbh <- oaks$DBH
scar <- oaks$scar
scar[is.na(scar)] <- 0
scar  <- as.factor(if_else(scar>0, 1, 0)) 
#turns photo numbers (or lack of) into 1's(for scarred) and 0's (for unscarred trees)
levels(scar) <- c('unscarred', 'scarred')

#imports first-order fire effects (first set of response variables)
CVS <- oaks$CVS/100
charht <- oaks$bole.char.ht.cm
charht.stan <- decostand(x=charht, method = "max")#relativizes char height by max
charbase <- oaks$bole.ch.base.perc
chardbh <- oaks$bole.ch.DBH.perc/100

#imports second-order fire effects (second set of response variables)
sprout.vol <- oaks$Cvsprouting.percent/100
duff.per.cons <- oaks$percent.duff.cons

#########################################################################
#set up for Mantel test for spacial autocorrelation####
#separate holden and upper weir sites
hold <- subset(oaks, oaks$site=="HLD01"|oaks$site=="HLD02")
uw <- subset(oaks, oaks$site=="UWTT"|oaks$site=="UWTB")

#Holden###
lat.h <- hold$Lat-(min(hold$Lat))
lat.h.r <- decostand(x=lat.h, method = "range")
long.h <- hold$Long-(min(hold$Long))
long.h.r <- decostand(x=long.h, method = "range")
#data frame of locations
gps.h <- data.frame(lat.h.r, long.h.r)

#UW###
lat.u <- uw$Lat-(min(uw$Lat))
lat.u.r <- decostand(x=lat.u, method = "range")
long.u <- uw$Long-(min(uw$Long))
long.u.r <- decostand(x=long.u, method = "range")
#data frame of locations
gps.u <- data.frame(lat.u.r, long.u.r)

# data frame of response variables
all.response <- data.frame(site, CVS, charht.stan, charbase, chardbh, sprout.vol, duff.per.cons)

all.resp.h <- subset(all.response, site=="HLD01"|site=="HLD02")
all.resp.h <- all.resp.h[,2:7]

all.resp.u <- subset(all.response, site=="UWTT"|site=="UWTB")
all.resp.u <- all.resp.u[,2:7]

#create matrices for predictor, response variables
#Holden
gps.dist.h <- vegdist(gps.h, method = "euclidean") #creates distance matrix of relativized lat/long using euclidean distance measure
all.resp.h.dist <- vegdist(all.resp.h, method="euclidean") #creates a distance matrix of relativized response variables using eucl distance

#Upper Weir
gps.dist.u <- vegdist(gps.u, method = "euclidean") #creates distance matrix of relativized lat/long using euclidean distance measure
all.resp.u.dist <- vegdist(all.resp.u, method="euclidean") #creates a distance matrix of relativized response variables using eucl distance

#Mantel test
latlong.man.h <- mantel(xdis = all.resp.h.dist, ydis = gps.dist.h, method = "pearson", permutations = 999, na.rm = T)
latlong.man.u <- mantel(xdis = all.resp.u.dist, ydis = gps.dist.u, method = "pearson", permutations = 999, na.rm = T)

str(latlong.man)

latlong.man.h$statistic #-.117 -- indicated very little correlation between the matrices
latlong.man.h$signif #.995

latlong.man.u$statistic #-.0418 -- indicated very little correlation between the matrices
latlong.man.u$signif #.861 
############################################################################


#read in shapefile to extract coodinates

library(rgdal)
library(rgeos)
library(raster)
library(tidyverse)
library(DescTools)

setwd("C:/Users/dnemens/Dropbox/OWO")

### read in shapefile ###
trees  <- readOGR("maps/GIS", "all_oaks")

coodinates <- trees@data$Northing
