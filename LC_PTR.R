#Code for clipping the India Land Cover map for the PTR

#---- Set working directory
#setwd("/Users/martaprat/Documents//LandscapeEcology/Landscape_Ecology")
#setwd("C://Users//mprat//Documents//LandEco-India//Landscape_Ecology")

#---- libraries
install.packages("sp")
library(sp)
library(raster)
library(rgdal)



#opern raster layer from all India
ras <- raster("F:/BIS_BR_VEG_mosaic/BIS_VG_India_final.tif")
plot(ras)
projection(ras) #check projection
#reduce extent
extent.new=extent(92.55,93.15,26.9,27.3)
ras2 <- crop(ras,extent.new)

##inland water
#PLOT WATER shapefiles polygon
PTR.INW.P<- readOGR("./IND_wat/IND_water_Areas_dcw.shp")
project(PTR.INW)
plot(PTR.INW)
#PLOT WATER LINES
PTR.INW.l<- readOGR("./IND_wat/IND_water_lines_dcw.shp")
project(PTR.INW.l)
plot(PTR.INW.l)

#open shapefile for PTR
PTR.limit <- readOGR("./Shapefiles/2013-14/Pakke Tiger Reserve Boundary.shp")
projection(PTR.limit) #check projection is the same
plot(PTR.limit)
plot(PTR.limit, col = "black", add = TRUE)


#crop
PTR.LC <- mask(ras2,PTR.limit)
unique(PTR.LC) # 13 different cover types
plot(PTR.LC, col = terrain.colors(13))  
extent(PTR.LC)


#save
writeRaster(PTR.LC, filename = "PTR_LC")



##--- read the necessary maps
cam.trap <- readOGR("./Shapefiles/2013-14/Camera Trap Locations 2013-14.shp")
LC <- raster("PTR_LC")
projection(LC)
projection(cam.trap)

#change projection for the camera traps
cam.trap <- spTransform(cam.trap, CRS(projection(LC)))
plot(LC)
plot(cam.trap, add=T)
