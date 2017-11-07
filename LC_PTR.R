#Code for clipping the India Land Cover map for the PTR

#---- Set working directory
setwd("/Users/martaprat/Documents//LandscapeEcology/Landscape_Ecology")
setwd("C://Users//mprat//Documents//LandEco-India//Landscape_Ecology")

#---- libraries
library(raster)
library(rgdal)

######### ------ Clip India raster into PTR
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
writeRaster(PTR.LC, filename = "PTR_LC")


######### ------ Assign LC classes PTR (NOT FINISHED)
LC <- raster("PTR_LC")
plot(LC)
unique(values(LC)) #find the different land cover classes contained in the raster
reclas <- data.frame(habitat = c("-","Sub-tropical broadleaved evergreen","Himalayan moist temperate",
                                 "Tropical semi-evergreen","Tropical moist deciduous","Temperate coniferous",
                                 "Bamboo sp.","Degraded forest","Grassland"),
                     values = c(0,16,19,22,23,31,40,106,135,173,180,190,191))
######### ------ Plot camera location into PTR
##--- read the necessary maps
cam.trap <- readOGR("./Shapefiles/2013-14/Camera Trap Locations 2013-14.shp")
LC <- raster("PTR_LC")
projection(LC)
projection(cam.trap)

#change projection for the camera traps
cam.trap <- spTransform(cam.trap, CRS(projection(LC)))
plot(LC)
plot(cam.trap, add=T)


