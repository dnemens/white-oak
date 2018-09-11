#old code
#slopes <- 2*mod6$coefficients[5]*CVS + mod6$coefficients[2]
#mean(slopes)

############################################
#bin tree size 
bins <- cut(height, breaks = 10)
bin.df <- data.frame(oaks$height.m, bins)
bin.df2 <- bin.df %>%
  group_by(bins) %>%
  summarise(height=mean(bin.df(height.m))) 

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
newx <- order(cvs)
y=predict(mod4, se.fit = TRUE, type="response")
#plot
#sets margins
par(mar=c(4,5,1,1))
plot(dieback~jitter(cvs, 5), col = "#FF000050", pch=19, cex=1.8, xlab="Scorch (%)", ylab="Crown dieback (%)", cex.lab=2)
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
#ggplotz the mlr point size = ht, color scale = size
try <- subset(oak, oak$CVS==100)
cvs <- try$CVS
dieback <- try$dieback
size <- try$scores
ht <- try$height.m
dbh <- try$DBH.cm
ggplot(try, aes(x=size, y=dieback))+
  geom_point (aes(fill=ht), position="jitter", cex=(sqrt(dbh)*5), pch=21, alpha=1/2)+
  theme_classic()+
  scale_fill_gradient(low="yellow", high = "darkorange", limits = c(2,18)) + 
  labs(x = "Tree size", y="Crown dieback (%)")+
  theme(axis.title = element_text(size=20))+
  labs(fill = "Height (m)")+
  theme(legend.position = c(1,.77), legend.justification = c(1.5,0))+
  theme(legend.key = element_rect(colour = 'black', linetype = "solid"))+
  ylim(-6, 105)

############################################################
#post-fire sprouting vs. crown scorch
par(mar=c(5,5,1,1))
plot(sprout.vol~jitter(cvs, 5), col = "darkorange", pch=21, cex=1.8, 
     xlab="Scorch (%)", ylab="Sprouting (%)", cex.lab=2, ylim=c(0,100))