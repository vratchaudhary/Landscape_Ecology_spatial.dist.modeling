#Code for clipping the India Land Cover map for the PTR

#---- Set working directory
#setwd("/Users/martaprat/Documents//LandscapeEcology/Landscape_Ecology")
setwd("C://Users//mprat//Documents//LandEco-India//Landscape_Ecology")

#---- libraries
library(sp)
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

##inland water
#PLOT WATER shapefiles polygon (not using this one, there is no polygon within PTR)
PTR.INW.P<- readOGR("/Users/martaprat/Dropbox/LandscapeEcology - Pakke Tiger Reserve/Shape.files/IND_water_Areas_dcw.shp")
projection(PTR.INW.P)
plot(PTR.INW.P)
#PLOT WATER LINES
PTR.INW.l<- readOGR("/Users/martaprat/Dropbox/LandscapeEcology - Pakke Tiger Reserve/Shape.files/IND_water_lines_dcw.shp")
projection(PTR.INW.l)
plot(PTR.INW.l)

#open shapefile for PTR
PTR.limit <- readOGR("/Users/martaprat/Dropbox/LandscapeEcology - Pakke Tiger Reserve/Shape.files/Pakke Tiger Reserve Boundary.shp")
projection(PTR.limit) #check projection is the same
plot(PTR.limit)
plot(PTR.limit, add = TRUE)


#crop -- LC raster
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

LC <- as.factor(LC)
LC.class <-levels(LC)[[1]] #find the different land cover classes contained in the raster
LC.class[,"landcover"] <- c("-","Sub-tropical broadleaved evergreen","Himalayan moist temperate",
                             "Tropical semi-evergreen","Tropical moist deciduous","Temperate coniferous",
                             "Bamboo sp.","Degraded forest","Grassland","missing","Barren land",
                             "Water body", "Wetland")
levels(LC) <- LC.class
LC.class[,"area"] <- tapply(area(LC), LC[], sum)
LC.class[,"prop.area"] <- LC.class$area/sum(LC.class$area)

#plot
quartz()
land_col = c("white","darkgreen","yellowgreen","lightgreen","green","darkorange","orange","yellow",
             "chocolate4","black","chocolate1","blue", "seagreen")
plot(LC, legend = T, col = land_col)



######### ------ Plot camera location into PTR
##--- read the necessary maps
#--- Camera trap locations
cam.trap <- readOGR("./Shapefiles/2013-14/Camera Trap Locations 2013-14.shp")
LC <- raster("PTR_LC")
projection(LC)
projection(cam.trap)

#--- Grid
cam.trap.grid <- readOGR("./Shapefiles/2013-14/Grid_Along_Boundary_utm.shp")
LC <- raster("PTR_LC")
projection(LC)
projection(cam.trap)

#change projection for the camera traps
cam.trap <- spTransform(cam.trap, CRS(projection(LC)))
plot(LC)
plot(cam.trap, add=T, col= "red")

cam.hab <- extract(LC,cam.trap)


