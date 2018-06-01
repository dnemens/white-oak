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

#data frame of above-ground predictor variables
pre <- data.frame(CVS, charht.stan, charbase, chardbh)

#####PCA###############
#creates PCA of predictor variables
pre1.PCA <- prcomp(pre, scale.=TRUE, center=TRUE)
summary (pre1.PCA)
biplot(pre1.PCA)
