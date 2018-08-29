oaks <- read.csv("C:/Users/dnemens/Dropbox/OWO/white-oak/data sheets/post_merge_forR.csv")

library (tidyverse)
require (fitdistrplus)
require (logspline)
library (vegan)

# imports tree characteristics (first set of explanatory variables)
height <- oaks$height
dbh <- oaks$DBH
cbh <- oaks$crown.base.ht.m

#imports fire effects 
cvs <- oaks$CVS
charht <- oaks$bole.char.ht.cm
charbase <- oaks$bole.ch.base.perc*100
chardbh <- oaks$bole.ch.DBH.perc

#imports response variables
sprout.vol <- oaks$Cvsprouting.percent
duff.per.cons <- oaks$percent.duff.cons
dieback <- oaks$dieback

#data frame of tree characteristics 
size <- data.frame(height, dbh, cbh)

#####PCA###############
#creates PCA of predictor variables
size1.PCA <- prcomp(size, scale.=TRUE, center=TRUE)
summary (size1.PCA)
biplot(size1.PCA)

scores <- size1.PCA$x[,1]

hrm <- lm(dieback~scores)
summary(hrm)
