duff_pins <- read.csv("~/Grad School/JBLM project/analyses/duff_pins.csv")
trees <- read.csv("~/Grad School/JBLM project/analyses/depths.heat.map.csv")
duff_pins [is.na(duff_pins)] <- 0

new <- merge (duff_pins, trees, by="tag")

library(dplyr)
library(tidyr)

attach(new)

df <- gather(new, pinID, consump, 3:10)
hld <- subset(df, tag>300) #creates df of holden trees
uw <- subset(df, tag<301) #creates df of upper weir trees

#holden
# grab a simple base map 
wash <- get_map(location=c(lon = -122.7063, lat=46.91624), maptype = "satellite", zoom = "auto")

ggmap(wash) + geom_point(data = hld, aes(x = Long, y = Lat, color = depth.consumed)) + 
  scale_color_gradient()

# get spatial extent of data
ext <- extent(as.matrix(hld[,3:4]))

#######################################################
#plot of duff depth vs. consumption on a per-pin basis
pins <- read.csv("~/Grad School/JBLM project/analyses/just pins.csv")
pins <- subset(pins, pins$duff.depth!=25)

rmse <- function(error) {sqrt(mean(error^2))}

cons <- pins$depth.cons
duff <- pins$duff.depth
perc.df <- mutate(pins, perc.cons=(cons/duff)*100)
perc <- perc.df$perc.cons


mod1 <- lm(cons~duff)
mod2 <- lm(cons~log(duff))
summary(mod1)

plot(duff, cons)
curve(predict(mod1, data.frame(duff=x),type="response"), add=TRUE, col="red", lwd=2)
xx <- order(duff)
y <- predict(mod1,data.frame(x=duff), se.fit = T, type="response")
 
polygon(c(duff[xx], rev(duff[xx])), c(y$fit[xx]-1.96*y$se.fit[xx], rev(y$fit[xx]+1.96*y$se.fit[xx])), border="black", col=rgb(0,0,0,0.15))
text(x=1, y=9.5, labels="P<0.0001\nPseudo R?=0.68", cex=.8, font=2)
#################################################
#same in ggplot
library(ggplot2)

ggplot(pins, aes(x=duff, y=cons))+
  geom_point (aes(duff, cons), cex=4, pch=19, alpha=1/4, color="black")+
  theme_bw()+
  ylim(.3,17)+
  theme(axis.title = element_text(size=25), axis.text = element_text(size=16), panel.grid = element_blank())+
  geom_smooth(colour="black", span=3)+
  labs(x="Duff depth (cm)", y="Duff consumption (cm)")+
  annotate("text", x= 2, y= 15, fontface=2, label="R ^ 2 == 0.79", parse = TRUE, size=6)+
  annotate("text", x= 2, y= 16, label="P<0.0001", size=6)
 
#####################################################
ggplot(pins, aes(x=duff, y=perc))+
  geom_point (aes(duff, perc), cex=4, pch=19, alpha=1/4, color="black")+
  theme_bw()+
  theme(axis.title = element_text(size=25), axis.text = element_text(size=16), panel.grid = element_blank())+
  geom_smooth(colour="black", span=3)+
  labs(x="Duff depth (cm)", y="Duff consumption (%)")+
  annotate("text", x= 2, y= 15, fontface=2, label="R ^ 2 == 0.79", parse = TRUE, size=6)+
  annotate("text", x= 2, y= 16, label="P<0.0001", size=6)

