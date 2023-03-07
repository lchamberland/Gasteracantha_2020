setwd("~/Desktop/Desktopfolder/Gasteracantha/Gasteracantha_2019/Regression/Mantel")

geoframe <- read.csv(file ="prunded_coords_formantel.csv", header=TRUE)
require(ggplot2)
#data(diamonds)
#qplot(carat, price, data = diamonds, colour = color)

Long = geoframe$Longitude      # the eruption durations 
Lat = geoframe$Latitude         # the waiting interval 
color = geoframe$color

color_pallete_function <- colorRampPalette(
  colors = c("black", "grey", "orange", "red", "purple", "yellow", "pink"),
  space = "Lab" # Option used when colors do not represent a quantitative scale
)

colors = c("black", "grey", "orange", "red", "purple", "yellow", "pink")

num_colors <- nlevels(geoframe$color)
diamond_color_colors <- color_pallet_function(num_colors)

col=c("black", "grey", "orange", "red", "purple", "yellow", "pink") 

quartz()
qplot(Long, Lat, xlab="Longitude",  ylab="Latitude", colour = color, background="white")     

col=c(“red”, “blue”), c(“Male”, “Female”)
sp





library(ade4)

geo <- read.table(file ="geodist.csv")
as.matrix(geo)


gen <- read.table(file ="gendist.csv")
as.matrix(gen)                   

mantel.rtest(geo.dists, gen.dists, nrepet = 9999)

library(ade4)
