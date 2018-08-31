oaks <- read.csv("C:/Users/dnemens/Dropbox/OWO/white-oak/data sheets/post_merge_forR.csv")


#loads necessary libraries
library(tidyverse)
library(MASS)
library(GGally)


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
sprout.vol <- oaks$Cvsprouting.percent
#spring response
dieback <- oaks$dieback
#######
#create df with relevant variables
oaks2 <- data.frame(height, dbh, cbh, cvs, charht, charbase, chardbh, duff.per.cons, sprout.vol, dieback)
#create comparison plots of all variables
plots <- ggpairs(oaks2)
ggsave("all.variables.pdf", device="pdf", plot=plots, width = 20, height = 15, units = "cm", dpi=300)

#################################
#measure length of vectors with conditions
length(which(dieback==100))
###########################################
#if we take scorch out of the picture (only 100% CVS), which variable becomes most important?
sc100 <- subset(oaks, CVS==100)
mod1 <- lm(dieback~., data = sc100)
mod2 <- lm(dieback~height, data=sc100)

#############################################
rmse <- function(x) {sqrt(mean(x^2))}

#linear model 
mod <- lm(dieback~I(cvs^2)*sprout.vol*dbh)
summary(mod)

mod1b <- lm(dieback~cvs)
summary(mod1b)


#linear model with all predictors
mod1 <- lm(dieback~.+I(CVS^2), data = oaks)
mod1a <- lm(dieback~., data = oaks)
stepAIC(mod1a, direction="forward", scope = ~.*.)
summary(mod1a)
AIC(mod1) #270
rmse(mod1$residuals) #9.57



#demonstrates need for sq term in model
boxcox(lm(dieback+.01~cvs))

#using three predictors, incl dbh, CVs2
mod2 <- lm(dieback~I(CVS^2)+sprout.vol+dbh+CVS, data=oaks)
summary(mod2) #r2 =0.54
AIC (mod2) #1796
rmse(mod2$residuals) #20.51

#using three sig pred from lm, incl height, CVS2, sprout
mod3 <- lm(dieback~height+sprout.vol+I(CVS^2)+CVS)
summary(mod3) #r2 = .54
AIC(mod3) #1791
rmse(mod3$residuals) #20.24

#taking sq term out
mod3a <- lm(dieback~height+sprout.vol+CVS)
summary(mod3a) #r2 = .49
AIC(mod3a) #1811
rmse(mod3a$residuals) #21.35

#using ht, dbh, sprout, cvs2 & cvs
mod4 <- lm(dieback~I(CVS^2)+sprout.vol+dbh+height+CVS, data=oaks)
summary(mod4) 
AIC (mod4) #1791
rmse(mod4$residuals) #20.51


#only CVS
mod5 <- lm(dieback~I(CVS^2)) #AIC 1388, r2 .42, rmse 23.6
mod6 <- lm(dieback~CVS) #AIC 1403, r2 .36 without square term, 24.7
mod7 <- lm(dieback~CVS+ht+sprout.vol + I(CVS^2)) 

summary(mod6)

slopes <- 2*mod6$coefficients[5]*CVS + mod6$coefficients[2]
mean(slopes)

AIC(mod6) 
rmse(mod6$residuals)
############################################
#bin tree size 
bins <- cut(height, breaks = 5)
bin.df <- data.frame(oaks$site, bins)
mutate(bin.df, bin=mean(bins))
#####################################################
#prep data from mlr for barplot
se <- summary(mod3a)$coefficients[,2]
dat <- data.frame(mod3a$assign, mod3a$coefficients, se)
dat <- rename(dat, Variable=mod3a.assign, Effect_Size=mod3a.coefficients)
dat <- filter(dat, Variable>0)
dat$Variable[dat$Variable == 1] <-  "Height"
dat$Variable[dat$Variable == 2] <-  "Sprouting"
dat$Variable[dat$Variable == 3] <-  "Scorch"

############################################
#ggplot of effect sizes from mod3a

a <- ggplot(dat, aes(Variable))+
  geom_col(aes(x=Variable, y=Effect_Size), fill=c("turquoise3", "turquoise3", "#F73C09"))+
  theme(axis.title = element_blank(), axis.text =element_blank())+
  geom_errorbar(aes(ymin=Effect_Size-se, ymax=Effect_Size+se), width=.1, size=.7)+
  #geom_hline(aes(yintercept=0), colour="black", size=1)+
  theme_void()+
  theme(plot.background = element_rect(fill="#E9B449", color="#E9B449"), panel.grid = element_blank())
  #geom_text(aes(label = Effect_Size, y=pos), size = 3, hjust = 0.5, vjust = 3)

?ggsave("", a)

####################################################
#plotz scorch vs. dieback with regression line in basic R plot
#prep data for plotting
newx <- order(CVS)
y=predict(mod4, se.fit = TRUE, type="response")
#plot
#sets margins
par(mar=c(4,5,1,1))
plot(dieback~jitter(CVS, 5), col = "#FF000050", pch=19, cex=1.8, xlab="Scorch (%)", ylab="Crown dieback (%)", cex.lab=2)
#adds confidence intervals
polygon(c(CVS[newx], rev(CVS[newx])), c(y$fit[newx]-1.96*y$se.fit[newx], rev(y$fit[newx]+1.96*y$se.fit[newx])), border="black", col=rgb(0,0,0,0.15))
curve(predict(mod4,data.frame(CVS=x),type="resp"), lwd=2, add=T, col="blue")
#adds text
text(5,95,"P<0.0001\nR?=0.42", font=2, cex=1.5)

segments(CVS-5, (mod6$fitted.values-5)*slopes, CVS+5, (mod6$fitted.values+5)*slopes)

#ggplot of above - but wonky regression line
ggplot(oaks, aes(x=CVS, y=dieback))+
  geom_point (aes(CVS, dieback), position="jitter", cex=5, pch=19, alpha=1/4, color="darkorange")+
  theme(axis.title = element_text(size=20))+
  geom_smooth(span=7)
  geom_smooth(method = "lm", formula = y ~ poly(x+.01, 2), colour = "blue", span = 10) 
############################################################
#plot of duff consumption vs. dieback showing no effect
  par(mar=c(4,5,1,1))
  plot(dieback~jitter(duff.per.cons, 5), col = "#00550050", pch=19, cex=1.8, xlab="Duff consumption (%)", ylab="Crown dieback (%)", cex.lab=2)
  
###########################################################  
#ggplotz the mlr point size = ht, color scale = % sprouting
ggplot(oaks, aes(x=CVS, y=dieback))+
geom_point (aes(fill=sprout.vol), position="jitter", cex=(sqrt(ht)*6), pch=21, alpha=1/2)+
  theme_classic()+
  scale_fill_gradient(low="yellow", high = "darkorange", limits = c(0,100)) + 
  labs(x = "Crown scorch (%)", y="Crown dieback (%)")+
  theme(axis.title = element_text(size=20))+
  labs(fill = "Post-fire sprouting volume (%)")+
  theme(legend.position = c(.02,.97), legend.justification = c(0,1))+
  theme(legend.key = element_rect(colour = 'black', linetype = "solid"))+
  ylim(-6, 100)

############################################################
  #post-fire sprouting vs. crown scorch
  par(mar=c(5,5,1,1))
  plot(sprout.vol~jitter(cvs, 5), col = "darkorange", pch=21, cex=1.8, 
       xlab="Scorch (%)", ylab="Sprouting (%)", cex.lab=2, ylim=c(0,100))
  
  
  

  
