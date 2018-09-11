## using PCA, we reduce the tree size variables to one for analsis of effect on response variables

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

#extract PCA scores for PC1, which accounts for 70% of variation
scores <- size1.PCA$x[,1]

hrm <- lm(dieback~scores)
summary(hrm)

#plot it
plot(dieback~scores)
curve(predict(hrm, data.frame(scores=x),type="resp"), lwd=2, add=T, col="blue")

#second PCA including DBH + height only
size2 <- size[,1:2]
size2.pca <- prcomp(size2, scale. = T, center = T)
summary(size2.pca)
biplot(size2.pca)

#extract PCA scores for PC1, which accounts for 86% of variation
scores2 <- size2.pca$x[,1]

###################
#pca of fire effects variables
#create data.frame
effs <- data.frame(cvs, charht, chardbh, charbase, duff.per.cons, sprout.vol)
effs.pc <- princomp(effs, scale = T, center = T)
summary(effs.pc)
biplot(effs.pc)
effs.scores <- effs.pc$x[,1]

ef <- vegdist(effs, method = "bray")
effs.perm <- adonis(effs~site, permutations=999, method="bray")
summary(effs.perm)
effs.perm
