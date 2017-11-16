#Code for clipping the India Land Cover map for the PTR

#---- Set working directory
#setwd("/Users/martaprat/Documents//LandscapeEcology/Landscape_Ecology")
#setwd("C://Users//mprat//Documents//LandEco-India//Landscape_Ecology")

#---- libraries
install.packages("sp")
library(sp)
library(raster)
library(rgdal)

#<<<<<<< HEAD


#=======
######### ------ Clip India raster into PTR
#>>>>>>># 4d9ab03e57667950e463cf55feffa270b03fe1aa
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
PTR.limit <- readOGR("./2013-14/Pakke Tiger Reserve Boundary.shp")
projection(PTR.limit) #check projection is the same
plot(PTR.limit, add=TRUE)
#plot(PTR.limit, col = "black", add = TRUE)

##open camera trap shapefile
PTR.CT <- readOGR("./2013-14/Camera Trap Locations 2013-14.shp")
projection(PTR.CT) #check projection is the same
spTransform(PTR.CT,"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
plot(PTR.limit,  add = TRUE)
points(PTR.CT, add= TRUE)


#turn a village csv into a spatial object
df.vill<-read.csv("village_pakke.csv", header=T)
head(df.vill)
coordinates(df.vill) <- (df.vill$lat+df.vill$long) #columns that correspond to the lat/long in the csv
proj4string(df.vill) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ") #geogrpahic cooridnate system
#don't use projected yet, becuase it needs to read using lat/long first. You can project after
class(df.vill)
project(df.vill)

plot(PTR.limit)
points(df.vill)


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
cam.trap$x_coord
LC <- raster("PTR_LC")
projection(LC)
projection(cam.trap)

#change projection for the camera traps
cam.trap <- spTransform(cam.trap, CRS(projection(LC)))
plot(LC)
plot(cam.trap, add=T)


