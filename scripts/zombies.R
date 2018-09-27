#2 year mortality

oaks <- read.csv("C:/Users/dnemens/Dropbox/OWO/white-oak/data sheets/post_merge_forR.csv")
#subsets out trees that appeared dead 9 months post burn
zombies <- subset(oaks, oaks$dieback==100)

#loads necessary libraries
library(MASS)
library(tidyverse)
library(GGally)

site <- zombies$site

#import size variables
height <- zombies$height
dbh <- zombies$DBH
cbh <- zombies$crown.base.ht.m

#imports fire effects variables
#above-ground
cvs <- zombies$CVS
charht <- zombies$bole.char.ht
charbase <- zombies$bole.ch.base.perc
chardbh <- zombies$bole.ch.DBH.perc
#below-ground
duff.per.cons <- zombies$percent.duff.cons

#scar stuff
scar <- as.numeric(zombies$scar)
scar[is.na(scar)] <- 0
scar  <- as.factor(if_else(scar>0, 1, 0)) 
#turns photo numbers (or lack of) into 1's(for scarred) and 0's (for unscarred trees)
levels(scar) <- c('Unscarred', 'Scarred')

#fall effects
sprout.vol <- zombies$Cvsprouting.percent[which(site!="HLD02")]
#spring response
dieback <- zombies$dieback
#2nd year
secyr.status <- zombies$X2yr.status
stilldead <- which(secyr.status=="TK")
zombies <- which(secyr.status=="L")

####looking for patterns in which trees were resurrected 
dead.duff <- duff.per.cons[which(secyr.status=="TK")]
zombie.duff <- duff.per.cons[which(secyr.status=="L")]

t.test(dead.duff, zombie.duff)

boxplot(dead.duff, zombie.duff, names = c("Still dead", "Zombie"), col=c("gold","green"), main="2 year postmortem on 2017 'dead' oaks, n=14", ylab="Percent duff consumption", cex=1.5)

