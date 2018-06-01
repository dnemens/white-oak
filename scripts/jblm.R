oaks <- read.csv("~/Grad School/JBLM project/analyses/post_merge_forR.csv")
oaks <- read.csv("F:/502/post_merge_forR.csv")

library (plyr)
library (dplyr)
library (vegan)
library(ggplot2)
library (rcompanion)

oaks = oaks[!oaks$tag==469,] #removes tree that has no spatial coordinates

site <- oaks$site

# imports tree characteristics (first set of explanatory variables)
height <- oaks$height.m
cbh <- oaks$crown.base.ht.m
dbh <- oaks$DBH
scar <- oaks$scar
scar[is.na(scar)] <- 0
scar  <- as.factor(if_else(scar>0, 1, 0)) 
#turns photo numbers (or lack of) into 1's(for scarred) and 0's (for unscarred trees)
levels(scar) <- c('unscarred', 'scarred')

#imports fire effects variables
CVS <- oaks$CVS
charht <- oaks$bole.char.ht.cm
charbase <- oaks$bole.ch.base.perc*100
chardbh <- oaks$bole.ch.DBH.perc
sprout.vol <- oaks$Cvsprouting.percent

depth.cons <- oaks$mean.depth.consumed.cm
duff.depth <- oaks$mean.duff.depth.cm
duff.per.cons <- oaks$percent.duff.cons

depth.cons <- depth.cons[which(depth.cons>0)]
duff.per.cons <- duff.per.cons[which(duff.per.cons>0)]
duff.depth <- duff.depth[which(duff.depth>0)]
df <- data.frame(depth.cons, duff.depth, duff.per.cons)

#####################################################
#lm for duff % consumption vs. duff depth
duff.lm <- lm(duff.per.cons~duff.depth)
summary(lm)

plot(duff.per.cons~duff.depth, pch=19, xlab="Duff Depth (cm)", ylab="Duff consumption (%)", cex.axis.title=3) 
abline(duff.lm)
curve(predict(duff.glm,data.frame(duff.depth=x),type="resp", lwd=2),add=TRUE, lwd=2)
########################################################
duff.glm2 <- glm(depth.cons~duff.depth, family = "Gamma"(link="log"))
summary(duff.glm2)
nagelkerke(duff.glm2)

plot(depth.cons~duff.depth, pch=19, xlab="Duff Depth (cm)", ylab="Duff consumption (cm)", cex.lab=1.5) 
curve(predict(duff.glm2,data.frame(duff.depth=x),type="resp"),add=TRUE, lwd=2, col="blue")
pred  <- predict.glm (duff.glm2, se.fit=TRUE, newdata=df,type = "resp")
x1 <- seq(min(duff.depth), max(duff.depth), length.out = 109)
polygon(c(duff.depth, rev(duff.depth)), c(pred$fit-1.96*pred$se.fit, rev(pred$fit+1.96*pred$se.fit)), border="red", col=rgb(0,0,0,0.15))
lines(df[,1], df2[,2], lty = 'dashed', col = 'red')
#lines(newx, y$fit+1.96*(y$se.fit), lty = 'dashed', col = 'darkgrey')
text(1, 9, "P<0.0001\nR²=0.60", cex=.8)


ggplot(data=df, aes(x=duff.depth, y=depth.cons))+
  geom_point(aes(duff.depth, y=depth.cons), color="blue", cex=1.8)+
  theme_linedraw()+
  theme(panel.grid = element_blank())+
  geom_smooth(method = "glm", method.args = list(family="Gamma"), data=df, span=1)+
  labs(y="Duff depth consumed (cm)", x="Duff depth (cm)")+
  theme(axis.title=element_text(size=18))+
  annotate("text", x=0.8, y=10, label ="P<.0001\nPsuedo R²=0.60", cex=3.5)+
  ylim(0,11.5)

CVS.d <- CVS[which(depth.cons>0)]
cvsd <- lm (CVS.d~depth.cons)
plot(CVS.d~depth.cons, pch=19, xlab="Duff consumed (cm)", ylab="Crown scorch (%)", cex.lab=1.5) 
curve(predict(cvsd,data.frame(duff.depth=x),type="resp"),add=TRUE, lwd=2, col="blue")


#########################################################################

#logistic regression of crown sprout likelihood 
sprout.vol2  <- if_else(sprout.vol>0, 1, 0)
logreg <- glm(sprout.vol2~CVS,family=binomial(link='logit'))
summary (logreg)
anova (logreg, test="Chisq")
library(pROC)
auc(logreg$model$sprout.vol, logreg$model$CVS) #ROC = .79
#plot
plot (sprout.vol2~CVS, pch=20, col="blue", xlab="Crown volume scorched (%)", ylab="Crown volume sprouting", cex.lab=1.5)
curve(predict(logreg,data.frame(CVS=x),type="resp"),add=TRUE, lwd=3, col="red") # draws a curve based on prediction from logistic regression model
text (0.06, .9, "ChiSq<.0001\nROC = .79", cex = .8, font = 2)
points(CVS, sprout.vol, col="darkorange", cex=1.4, pch=19)

#or
library(popbio)
logi.hist.plot(CVS,sprout.vol2,boxp=FALSE,type="hist",col="gray")
