#plotz!###

oaks <- read.csv("C:/Users/dnemens/Dropbox/OWO/white-oak/data sheets/post_merge_forR.csv")
#run pca script first!!  
oaks.scores <- data.frame(oaks, scores)
####################################
#plot cvs - dieback
cvs <- oaks$CVS
dieback <- oaks$dieback

### save tiff ###
tiff(file="C:/Users/dnemens/Dropbox/OWO/plots/dieback.tiff", width = 7, height = 5, units="in", res=800)
#plot
#sets margins
par(mar=c(4.5,4.3,1,1))
plot(dieback~jitter(cvs, 5), col = "#FF000050", pch=19, cex=1.6, xlab="Scorch (%)", 
     ylab="Crown dieback (%)", cex.lab=1.5) 
rect(80, -5, 102, 105, col = rgb(0,0,0, alpha =.2), border = NA)
dev.off()

#add horizontal line @ 50% dieback
#abline(h=50, lwd=2)
#highlight dead trees
#symbols(100, 100, circles=.1, add=TRUE, fg="gold", lwd=3)

##################################
#ggplotz the mlr point size = dbh, color scale = height
try <- subset(oaks.scores, oaks.scores$CVS==100)
cvst <- try$CVS
diebackt <- try$dieback
sizet <- try$scores
htt <- try$height.m
dbht <- try$DBH.cm

ggplot(try, aes(x=htt, y=diebackt))+
  geom_point (aes(), position="jitter", cex=(sqrt(dbht)*3), pch=21, fill="darkorange", alpha=.6)+
  theme_bw()+
  #scale_fill_gradient(low="yellow", high = "darkorange", limits = c(2,18), breaks = c(2, 10, 18), labels= c("0",10,"18")) + 
  labs(x = "Height (m)", y="Crown dieback (%)")+
  theme(axis.title = element_text(size=16), panel.grid = element_blank(), axis.text = element_text(color = "black", size=10))+
  #labs(fill = "Height (m)")+
  theme(legend.position = c(1,.67), legend.justification = c(1,0))+
  theme(legend.key = element_rect(colour = 'black', linetype = "solid"))+
  ylim(-6, 105)+
  xlim(0,20)
  #annotate ("segment", x=-3, xend = 3, y=-5, yend=-5, colour = "grey", alpha = .8, size=6)

  #aes(fill="vector")
ggsave("C:/Users/dnemens/Dropbox/OWO/plots/scorched_size.tiff", device = "tiff", width = 6, height = 4.5, units = "in", dpi = 800)
