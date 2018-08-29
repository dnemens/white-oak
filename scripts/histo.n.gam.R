oaks <- read.csv("C:/Users/dnemens/Dropbox/OWO/white-oak/data sheets/post_merge_forR.csv")

library (tidyverse)
library(ggplot2)
library(MASS)

site <- oaks$site
tag <- oaks$tag

# imports tree characteristics (first set of explanatory variables)
height <- oaks$height.m
cbh <- oaks$crown.base.ht.m
dbh <- oaks$DBH
scar <- oaks$scar
scar[is.na(scar)] <- 0
scar  <- as.factor(if_else(scar>0, 1, 0)) 
#turns photo numbers (or lack of) into 1's(for scarred) and 0's (for unscarred trees)
levels(scar) <- c("Unscarred", "Scarred")

#imports fire effects variables
CVS <- oaks$CVS
charht <- oaks$bole.char.ht.cm
charbase <- oaks$bole.ch.base.perc
chardbh <- oaks$bole.ch.DBH.perc

cons <- oaks$mean.depth.consumed.cm
depth.cons <- cons[which(cons>0)]
duff.per.cons <- oaks$percent.duff.cons*100
duff.depth <- oaks$mean.duff.depth.cm
duff.depth <- duff.depth[which(duff.depth>0)]

dieback <- as.numeric(oaks$dieback)
sprout.vol <- oaks$Cvsprouting.percent
#############################################################
#histogram of diameter distribution with live/dead trees
Mortality <- if_else(dieback==100, "Top-killed", "Alive")
df <- data.frame(dieback, dbh, Mortality)
df$Mortality<- factor(df$Mortality, levels=levels(df$Mortality)[order(levels(df$Mortality), decreasing = TRUE)])

ggplot(df, aes(x=dbh, fill=Mortality))+ 
  geom_histogram(binwidth=5, position="stack", colour="black") +
  theme_bw()+
  theme(panel.grid.major=element_blank(), panel.grid.minor = element_blank(), legend.position = c(.8,.8), axis.title = element_text(size=24), axis.text = element_text(size=16),  legend.text = element_text(size=15), legend.title = element_blank()) +
  scale_fill_manual(values=c("grey20", "grey80")) +
  xlim(0,70) +
  scale_y_continuous(limits = c(0,50), expand = c(0, 0)) +
  xlab("Diameter (cm)") +
  ylab("Frequency") +
  guides(fill = guide_legend(reverse = F))+
  theme(plot.margin=unit(c(.7,1,.5,.5),"cm"))

ggsave("dbh_hist.tiff", device="tiff", dpi = 300, width = 20, height= 15, units = "cm")

############################################################
#logistic regression of bud break vs. fire effects 
bud.break.log  <- if_else(bud.break>0, 1, 0)
logreg <- glm(bud.break.log~charht+duff.per.cons+cons+chardbh+charbase+sprout.vol,family=binomial(link='logit'))
summary(logreg)
stepAIC(logreg, direction="backward", scope = ~.*.)
logregC <- glm(bud.break.log~CVS, family=binomial(link = "logit"))

###########################################################
#glm with gamma distribution bud.break vs fire effects
glm.reg <- glm((bud.break+.1)~charht+CVS+duff.per.cons+cons+chardbh+charbase+sprout.vol, family=Gamma(link="log"))
summary(glm.reg)
stepAIC(glm.reg, direction="backward", scope = ~.*.)

#glm gamma with only CVS
glm.regCVS <- glm((bud.break+.1)~CVS, family=Gamma(link="log"))
summary(glm.regCVS)

#glm with CVS and post-fire sprouting predicting bud break
glm2 <- glm((bud.break+.01)~CVS+sprout.vol, family = Gamma(link="log"))
summary(glm2)

#prep data for plotting
newx <- order(CVS)
y=predict(glm.regCVS, se.fit = TRUE, type="response")

#plot of bud break vs. CVS with regression line & confid intervals
plot(bud.break~jitter(CVS, 5), ylab="Spring bud break (%)", xlab="Crown volume scorched (%)", pch = 19, col="black")
curve(predict(glm.regCVS,data.frame(CVS=x),type="resp", lwd=2),add=TRUE, lwd=2)

#adds confidence intervals
polygon(c(CVS[newx], rev(CVS[newx])), c(y$fit[newx]-1.96*y$se.fit[newx], rev(y$fit[newx]+1.96*y$se.fit[newx])), border="black", col=rgb(0,0,0,0.15))

#gam
library(mgcv)
mod2 <- gam(bud.break+.1 ~ s(CVS, bs="cs", k=3), family=Gamma(link="log"))
y2=predict(mod2, data.frame(x=CVS), se.fit = TRUE, type="response")
summary (mod2) 
gam.check(mod2)

#plot
xx <- order(CVS)
plot(bud.break~jitter(CVS, 5), ylab="Spring bud break (%)", xlab="Crown volume scorched (%)", pch = 19, col="black")
points(CVS[xx], y2$fit[xx], type="l", lwd=3, col="red")
polygon(c(CVS[xx], rev(CVS[xx])), c(y2$fit[xx]-1.96*y2$se.fit[xx], rev(y2$fit[xx]+1.96*y2$se.fit[xx])), border="black", col=rgb(0,0,0,0.15)) 
