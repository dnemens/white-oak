library(raster)    # for rasters in R
library(gstat)     # for interpolating between points
library(ggplot2)   # for pretty plots
library(ggmap)     # for pretty maps
library(rasterVis) # for pretty raster maps
library(dplyr)     # for piping operater `%>%`

# read in lat/lon data
tree <- read.csv("~/Grad School/JBLM project/analyses/depths.heat.map.csv")
tree = tree[!tree$tag==469,] #removes tree that has no spatial coordinates
hld <- subset(tree, tree$tag>300)
uw <- subset(tree, tree$tag<301)

#holden
# grab a simple base map 
wash <- get_map(location=c(lon = -122.7063, lat=46.91624), maptype = "satellite", zoom = "auto")

ggmap(wash) + geom_point(data = hld, aes(x = Long, y = Lat, color = depth.consumed)) + 
  scale_color_gradient()

# get spatial extent of data
ext <- extent(as.matrix(hld[,3:4]))

# create raster that specifies resolution of interpolation
r <- raster(ext, nrows = 1000, ncols = 1000)

# rasterize the depth data
ras <- rasterize(x = hld[,3:4], y = r, field = hld$depth.consumed)

# make a kriging model
mod <- gstat(formula = depth.consumed~1, locations = ~ Long  + Lat, data = hld)

# use kriging model to interpolate between the data points
z <- interpolate(ras, mod, xyNames = c("Long", "Lat"))

# results!
mytheme <- theme(axis.title = element_blank(), panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank())

HLD <- ggplot() + 
  #geom_tile(aes(fill = value)) +
  scale_fill_distiller(palette = "RdYlGn") +
  geom_point(data = hld, aes(x = Long, y = Lat, color = depth.consumed), cex=2.5) +   
  scale_color_gradient(low="yellow", high="red", guide="colourbar", breaks=c(0,2,4,6,8,10), labels =c(0,2,4,6,8,10), name="Depth consumed (cm)")+
  ggtitle("Holden Woods")+
  labs(x="", y="")+
  mytheme

######################################################################
#Upper Weir
# grab a simple base map 
wash <- get_map(location=c(lon = -122.7063, lat=46.91624), maptype = "satellite", zoom = "auto")

ggmap(wash) + geom_point(data = uw, aes(x = Long, y = Lat, color = depth.consumed)) + 
  scale_color_gradient()

# get spatial extent of data
ext <- extent(as.matrix(uw[,3:4]))

# create raster that specifies resolution of interpolation
r <- raster(ext, nrows = 1000, ncols = 1000)

# rasterize the depth data
ras <- rasterize(x = uw[,3:4], y = r, field = uw$depth.consumed)

# make a kriging model
mod <- gstat(formula = depth.consumed~1, locations = ~ Long  + Lat, data = uw)

# use kriging model to interpolate between the data points
z <- interpolate(ras, mod, xyNames = c("Long", "Lat"))

# results!
UP <- ggplot() + 
  #geom_tile(aes(fill = value)) +
  scale_fill_distiller(palette = "RdYlGn") +
  geom_point(data = uw, aes(x = Long, y = Lat, color = depth.consumed), cex=2.5) + 
  scale_color_gradient(low="yellow2", high="red", guide="colourbar", breaks=c(0,2,4,6,8,10), labels =c(0,2,4,6,8,10))+
  labs(x="", y="")  +
  theme(legend.position="none")+
  mytheme +
  ggtitle("Upper Weir") + 
  annotate("text", x=-122.7087, y=46.91750, label = "Top", fontface=2) +
  annotate("text", x=-122.7067, y=46.91750, label = "Bottom", fontface=2)

#################################
#plot both side by side
require(gridExtra)
plot1 <- UP
plot2 <- HLD
grid.arrange(plot1, plot2, ncol=2)