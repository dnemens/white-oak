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

#create matrices for predictor, response variables
gps <- data.frame(lat.r, long.r)
all.response <- data.frame(CVS, charht.stan, charbase, chardbh, sprout.vol, duff.per.cons)
response3 <- data.frame(charht.stan, charbase, chardbh)

#########################################################################
#Mantel test
gps.dist <- vegdist(gps, method = "euclidean") #creates distance matrix of relativized lat/long using euclidean distance measure
all.response.dist <- vegdist(all.response, method="euclidean") #creates a distance matrix of relativized response variables using eucl distance

plot (all.response.dist~gps.dist)

latlong.man <- mantel(xdis = all.response.dist, ydis = gps.dist, method = "pearson", permutations = 999)

str(latlong.man)
latlong.man$statistic #.0988 -- indicated very little correlation between the matrices
latlong.man$signif #.001 
############################################################################
#####PCA
#creates PCA of correlated predictor variables
pred1 <- data.frame (height, cbh, dbh)
pred1.PCA <- prcomp(pred1, scale.=TRUE, center=TRUE)
summary (pred1.PCA)
print(pred1.PCA)
plot (pred1.PCA)
biplot(pred1.PCA)
plot (pred1)

 pred1.PCA2 <- princomp(scale(pred1)) #same as above, but prints loadings -- why is one missing?  
summary (pred1.PCA2, loadings = TRUE)
biplot(pred1.PCA2)

##################################################################################
#NMDS for 
#checking various dimensions to look for changes in stress
all.nmds2 <- metaMDS(all.response, autotransform = FALSE, distance = "euclidean", k=2, maxit = 300, try = 40, trymax=100)
all.nmds3 <- metaMDS(all.response, autotransform = FALSE, distance = "euclidean", k=3, maxit = 300, try = 40, trymax=100)
all.nmds4 <- metaMDS(all.response, autotransform = FALSE, distance = "euclidean", k=4, maxit = 300, try = 40, trymax=100)
all.nmds5 <- metaMDS(all.response, autotransform = FALSE, distance = "euclidean", k=5, maxit = 300, try = 40, trymax=100)
plot (x=1:4, y=c(all.nmds2$stress, all.nmds3$stress, all.nmds4$stress, all.nmds5$stress)) #scree plot to determine best # of dimensions 
all.nmds3$stress #=.04 

#final choice of dimensions = 3.  stress =.07
all.nmds <- metaMDS(all.response, autotransform = FALSE, distance = "euclidean", k=3, maxit = 300, try = 40, trymax=100)
#checking fit
stressplot(object = all.nmds, lwd=5)

nmds.scar <- MDSrotate(all.nmds, scar)

  
plot (nmds.scar, display = "sites", type = "n")
ordispider(nmds.scar, scar, col="darkgrey")
points(nmds.scar$points[scar==1,], pch=17, col="red")
points(nmds.scar$points[scar==0,], pch=19, col="royalblue")

scar.fit <- envfit(nmds.scar~scar) #statistical test for significance of scarring factor

#adding vectors
all.fit <- envfit(all.nmds~dbh+cbh+height)
plot (all.nmds, display = "sites", xaxt="n", yaxt="n")
plot(all.fit)
  
##################################################################################




#exploratory plots
X11(width=6,height=8)
par(mfrow=c(3,3))

plot (crownsprout~CVS)
text (5,70,"r2=.12")
text (9,60, "pval<.0001")
plot (duffcons, CVS)
plot(duffcons,crownsprout)
plot (CVS~cbh)
plot (crownsprout~cbh)
plot (duffcons~depth)
text (.6,.9,"r2=.64")
text (1,.8, "pval<.0001")
plot(charht,crownsprout)
plot (charht,CVS)
plot (charbase, duffcons)
plot (charht~charbase)
plot (charht~chardbh)
dev.off()


boxplot (scar, charht, ylab = "char height (cm)")
summary (aov(charht~scar))


#logistic regression of crown sprout likelihood 
crownsprout  <- if_else(crownsprout>0, 1, 0)
logreg <- glm(crownsprout~CVS,family=binomial(link='logit'))
summary (logreg)
anova (logreg, test="Chisq")
#plot
plot (crownsprout~CVS, pch=20, col="blue", main="Logistic regression of post-Storrie sprouts killed in Chips Fire", xlab="Chips Fire RdNBR", ylab="% mortality of post-Storrie sprouts")
curve(predict(logreg,data.frame(CVS=x),type="resp"),add=TRUE) # draws a curve based on prediction from logistic regression model
abline(v=69,col="green", lwd=3)
text (-400, .9, "ChiSq<.0001", cex = .7)


