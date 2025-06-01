library(chronosphere)
library(ggplot2)
setwd("/Users/max/Desktop/Illustration")

dat <- read.csv2("pbdb_data_final_occurence.csv", sep=",", stringsAsFactors=F)

# Plotting: I recommend plotting the processed data. You can do it with these
# You can find more about this in the Climate and Earth System Data class' material
# (vectors)

# plotting a png image

ne <- fetch("NaturalEarth")

# for faster plotting, consider plotting only the collections (less duplicates of coordinates)
# Normalizing data for collection coordinates (every row is one collection), with the
# basic longitude and latitude information
colls <- unique(dat[, c("collection_no", "lng", "lat")])

# this should work...
dir.create("export", showWarnings=FALSE)
png("export/worldmap.png", height=3000, width=6000)
plot(ne$geometry, col="grey",border=NA)
points(colls$lng, colls$lat, pch=16,col="red",cex=3)

# Add latitude and longitude grid lines
grid(nx=18, ny=9, col="black", lty="solid", lwd=2.5) # nx and ny: number of vertical and horizontal lines

# Add axis labels for longitude (x-axis) and latitude (y-axis)
lon_labels <- seq(par("usr")[1], par("usr")[2], length.out=19)  # 18 segments = 19 ticks
lat_labels <- seq(par("usr")[3], par("usr")[4], length.out=10)  # 9 segments = 10 ticks

axis(1, at=lon_labels, labels=round(lon_labels, 1)) # bottom, longitude
axis(2, at=lat_labels, labels=round(lat_labels, 1)) # left, latitude

dev.off()

