#Code for clipping the India Land Cover map for the PTR

#---- Set working directory
setwd("C://Users//mprat//Documents//LandEco-India//Landscape_Ecology")

#---- libraries
library(raster)
library(rgdal)

#opern raster layer from all India
ras <- raster("F:/BIS_BR_VEG_mosaic/BIS_VG_India_final.tif")
plot(ras)
projection(ras) #check projection
#reduce extent
extent.new=extent(92.55,93.15,26.9,27.3)
ras2 <- crop(ras,extent.new)

#open shapefile for PTR
PTR.limit <- readOGR("./Shapefiles/2013-14/Pakke Tiger Reserve Boundary.shp")
projection(PTR.limit) #check projection is the same
plot(PTR.limit, col = "black", add = TRUE)

#crop
PTR.LC <- mask(ras2,PTR.limit)
unique(PTR.LC) # 13 different cover types
plot(PTR.LC, col = terrain.colors(13))  
extent(PTR.LC)


#save
writeRaster(PTR.LC6, filename = "PTR_LC")
