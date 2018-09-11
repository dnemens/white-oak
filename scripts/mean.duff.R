oaks <- read.csv("C:/Users/dnemens/Dropbox/OWO/white-oak/data sheets/post_merge_forR.csv")

library (plyr)
library (dplyr)
library(ggplot2)
library(MASS)

site <- oaks$site
tag <- oaks$tag

depth.cons <- oaks$mean.depth.consumed.cm
summarydepth.cons <- depth.cons[which(depth.cons>0)]

duff.depth <- oaks$mean.duff.depth.cm

duff.per.cons <- oaks$percent.duff.cons*100

################################################################
#duff consumption vs. depth 
library (rcompanion)
library(MASS)

glm.duff <- glm((depth.cons+.01)~(log(duff.depth+.01)), family=Gamma(link="log"), control = list(maxit = 50))
lm.duff <- lm(depth.cons~(duff.depth^2))
summary(lm.duff)

summary(glm.duff)
nagelkerke(glm.duff)
plot(depth.cons~duff.depth, xlab="Mean duff depth (cm)", ylab= "Mean duff consumed (cm)", cex.lab=1.4, pch=20, col="red4")
curve(predict(lm.duff, data.frame(duff.depth=x),type="response"), add=TRUE, lwd=2)
curve(predict(glm.duff,data.frame(duff.depth=x),type="response", lwd=2),add=TRUE, lwd=2)
xx <- order(duff.depth)
y <- predict(glm.duff,data.frame(x=duff.depth), se.fit = T, type="response")

#points(duff.depth[xx], y$fit[xx]-1.96*y$se.fit[xx], type="l", lwd=1, col=2)
#points(chiprdnbr[xx], y$fit[xx]+1.96*y$se.fit[xx], type="l", lwd=1, col=2)
polygon(c(duff.depth[xx], rev(duff.depth[xx])), c(y$fit[xx]-1.96*y$se.fit[xx], rev(y$fit[xx]+1.96*y$se.fit[xx])), border="black", col=rgb(0,0,0,0.15))
text(x=1, y=9.5, labels="P<0.0001\nPseudo R?=0.68", cex=.8, font=2)

######################################################################
#plot of duff consumption by mean duff depth per tree in ggplot
dfduff <- data.frame(duff.depth, duff.cons)

ggplot(dfduff, aes(x=duff.depth, y=depth.cons))+
  geom_point (aes(duff.depth, depth.cons), cex=4, pch=19, alpha=1/3, position="jitter", color="grey28")+
  theme_bw()+
  ylim(-.4,12)+
  theme(axis.title = element_text(size=25), axis.text = element_text(size=16), panel.grid = element_blank())+
  geom_smooth(colour="black", span=3)+
  labs(x="Mean duff depth (cm)", y="Mean duff consumption (cm)")+
  annotate("text", x= .7, y= 10, fontface=2, label="R^2 == 0.72", parse = TRUE, size=6)+
  annotate("text", x= .7, y= 11, label="P<0.0001", size=6)
######################################################################
#mean duff percent consumption by mean duff depth
df.duff.per <- data.frame(duff.depth, duff.per.cons)

lm.duff.per <- lm(duff.per.cons~duff.depth)
summary(lm.duff.per)
plot(duff.per.cons~duff.depth, xlab="Mean duff depth (cm)", ylab= "Mean duff consumed (%)", cex.lab=1.4, pch=20, col="red4")
curve(predict(lm.duff.per, data.frame(duff.depth=x),type="response"), add=TRUE, lwd=2)

ggplot(df.duff.per, aes(x=duff.depth, y=duff.per.cons))+
  geom_point (aes(duff.depth, duff.per.cons), cex=4, pch=19, alpha=1/3, position="jitter", color="grey28")+
  theme_bw()+
  theme(axis.title = element_text(size=25), axis.text = element_text(size=16), panel.grid = element_blank())+
  geom_smooth(colour="black", span=6)+
  labs(x="Mean duff depth (cm)", y="Mean duff consumption (%)")+
  annotate("text", x= .7, y= 10, fontface=2, label="R^2 == 0.72", parse = TRUE, size=6)+
  annotate("text", x= .7, y= 11, label="P<0.0001", size=6)
############################################################################
df.duff.per <- data.frame(duff.depth, duff.per.cons)

lm.duff.per <- lm(duff.per.cons~duff.depth)
summary(lm.duff.per)
plot(duff.per.cons~duff.depth, xlab="Mean duff depth (cm)", ylab= "Mean duff consumed (%)", cex.lab=1.4, pch=20, col="red4")
curve(predict(lm.duff.per, data.frame(duff.depth=x),type="response"), add=TRUE, lwd=2)

ggplot(df.duff.per, aes(x=duff.depth, y=duff.per.cons))+
  geom_point (aes(duff.depth, duff.per.cons), cex=4, pch=19, alpha=1/3, position="jitter", color="grey28")+
  theme_bw()+
  theme(axis.title = element_text(size=25), axis.text = element_text(size=16), panel.grid = element_blank())+
  geom_smooth(colour="black", span=6)+
  labs(x="Mean duff depth (cm)", y="Mean duff consumption (%)")+
  annotate("text", x= .7, y= 10, fontface=2, label="R^2 == 0.72", parse = TRUE, size=6)+
  annotate("text", x= .7, y= 11, label="P<0.0001", size=6)

