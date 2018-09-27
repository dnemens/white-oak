oaks <- read.csv("C:/Users/dnemens/Dropbox/OWO/white-oak/data sheets/post_merge_forR.csv")

#loads necessary libraries
library(MASS)
library(tidyverse)
library(GGally)

site <- oaks$site

#import size variables
height <- oaks$height
height.r <- (height/max(height))*100  #relativizes height for inclusion in mlr model
dbh <- oaks$DBH
dbh.r <- (dbh/max(dbh))*100 #relativizes height for inclusion in mlr model
cbh <- oaks$crown.base.ht.m

#imports fire effects variables
#above-ground
cvs <- oaks$CVS
charht <- oaks$bole.char.ht
charbase <- oaks$bole.ch.base.perc
chardbh <- oaks$bole.ch.DBH.perc
#below-ground
duff.per.cons <- oaks$percent.duff.cons

#scar stuff
scar <- as.numeric(oaks$scar)
scar[is.na(scar)] <- 0
scar  <- as.factor(if_else(scar>0, 1, 0)) 
#turns photo numbers (or lack of) into 1's(for scarred) and 0's (for unscarred trees)
levels(scar) <- c('Unscarred', 'Scarred')

#fall effects
sprout.vol <- oaks$Cvsprouting.percent[which(site!="HLD02")]
#spring response
dieback <- oaks$dieback
#######
#prelim look at data

#create df with relevant variables
oaks2 <- data.frame(height, dbh, cbh, cvs, charht, charbase, chardbh, duff.per.cons, sprout.vol, dieback)

#create comparison plots of all variables
plots <- ggpairs(oaks2)
ggsave("~/plots/all.variables.pdf", device="pdf", plot=plots, width = 20, height = 15, units = "cm", dpi=300)

#############################################
#models!
rmse <- function(x) {sqrt(mean(x^2))}

###if model includes transformations (example is for log trans), residuals must be back-transformed:
#rmse <- function(pred, obs) sqrt(mean((pred - obs)^2))
#preds <- predict(best.mod)
#rmse(obs = dieback, pred = exp(preds))


#amend data frame for modeling
oak.sub <- oaks %>%
  select (DBH.cm:Cvsprouting.percent, -stems, -CVC, -mean.depth.consumed.cm, -mean.duff.depth.cm, dieback)

#run pca script first!
#add pca scores from pca of size metrics (ht, dbh, crown base ht)
oak.subs <- data.frame(oak.sub, scores)

oak.sub <- oak.subs[,4:11]

#################################
##linear model with all predictors
mod1 <- lm(dieback~., data = oak.sub)
summary(mod1)
AIC(mod1) 
rmse(mod1$residuals) 

step.mod <- stepAIC(mod1, direction="backward", scope = ~.*.)
summary(step.mod)

#best model from of stepwise regression 
best.mod <- lm(dieback ~ cvs + chardbh + scores)
summary(best.mod)
AIC(best.mod) #1799
rmse(best.mod$residuals) #20.73

#run a forward stepwise regression starting with best model
step.bestmod <- stepAIC(best.mod, direction = "forward", scope = ~.*.)
#old model: mod <- lm(dieback~I(cvs^2)*sprout.vol*dbh)
 
#final model from 2 runs
best2 <-  lm(dieback ~ cvs + chardbh +  cvs:scores + scores)
summary(best2)
AIC(best2) #1781
rmse(best2$residuals) #19.73

##########same process without the pca of size metrics
oak.sub2 <- oaks %>%
  select (DBH.cm:Cvsprouting.percent, -stems, -CVC, -mean.depth.consumed.cm, -mean.duff.depth.cm, dieback)
mod2 <- lm(formula = dieback~.*., data=oak.sub2)
summary(mod2)
step2 <- stepAIC(mod2, direction = "backward", scope = ~.*.)
summary(step2)

best2 <- lm(dieback ~ height + cvs + chardbh )
summary(best2)

####################################
#or glm with gamma dist?  
modg <- glm((dieback)~., family=Gamma(link="sqrt"), data = oak.sub)
summary(modg)
AIC(modg)
##
modgl <- glm((dieback)~., family=Gamma(link="log"), data = oak.sub)
summary(modgl)
AIC(modgl)
rmse(modgl$residuals)
modg$aic

step.mod <- stepAIC(mod1, direction="backward", scope = ~.*.)
summary(step.mod)

best.glm <- glm((dieback)~ cvs + chardbh + scores, family=Gamma(link="log"))
summary(best.glm)
rmse(best.glm$residuals)
AIC(best.glm)

step.bestglm <- stepAIC(best.glm, direction = "forward", scope = ~.*.)
summary(step.bestglm)
#no improvement adding interaction terms

step.bestglm$aic
rmse(step.bestglm$residuals)
step.bestglm$aic

##############################################
#creates summary table of all model runs with eval criteria used
pooter <- function(predictors, response = "dieback"){
  x <- list()
  temp <- paste(predictors, collapse = " + ")
  x$formula <- paste(response, "~", temp)
  mod <- glm(as.formula(x$formula), family=Gamma(link="log"))
  x$AIC <- AIC(mod)
  x$RMSE <- rmse(mod$residuals)
  x$R2 <- summary(mod)$r.squared
  return(x)
}

pooter(predictors = c("cvs", "charht", "chardbh","charbase", "duff.per.cons", "sprout.vol", "scores"))

combos <- list()
combos[[1]] <- c("cvs", "charht", "chardbh","charbase", "duff.per.cons", "sprout.vol", "scores")
combos[[2]] <- c("cvs",  "chardbh","charbase", "duff.per.cons", "sprout.vol", "scores")
combos[[3]] <- c("cvs", "chardbh", "duff.per.cons", "sprout.vol", "scores")
combos[[4]] <- c("cvs", "chardbh", "sprout.vol", "scores")
combos[[5]] <- c("cvs", "chardbh", "scores")
combos[[6]] <- c("cvs", "chardbh", "scores", "cvs:scores")

results <- t(sapply(combos, pooter))
results

#create table with pander or xtable or mumant##

####################################
#plot cvs - dieback
### save tiff ###
tiff(file="C:/Users/dnemens/Dropbox/OWO/plots/dieback.tiff", width = 7, height = 5, units="in", res=300)
#plot
#sets margins
par(mar=c(4,5,1,1))
plot(dieback~jitter(cvs, 5), col = "#FF000050", pch=19, cex=1.1, xlab="Scorch (%)", 
     ylab="Crown dieback (%)", cex.lab=2) 
rect(80, -5, 102, 105, col = rgb(0,0,0, alpha =.2), border = NA)
dev.off()
#add horizontal line @ 50% dieback
#abline(h=50, lwd=2)
#highlight dead trees
#symbols(100, 100, circles=.1, add=TRUE, fg="gold", lwd=3)

  
  
  

  
