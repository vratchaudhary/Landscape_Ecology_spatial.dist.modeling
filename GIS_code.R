#script for the Landscape Ecology project
#exploring GIS maps

#set working directory
setwd('/Users/martaprat/Documents/LandscapeEcology/GroupProject/maps') #marta

#libraries
library(raster)

quartz()
dem1 <- raster("./ASTGTM2_N26E092/ASTGTM2_N26E092_dem.tif") 
dem2 <- raster("./ASTGTM2_N26E093/ASTGTM2_N26E093_dem.tif") 
dem3 <- raster("./ASTGTM2_N27E092/ASTGTM2_N27E092_dem.tif") 
dem4 <- raster("./ASTGTM2_N27E093/ASTGTM2_N27E093_dem.tif") 

dem.g <- mosaic(dem1,dem2,dem3,dem4, fun = mean) 
plot(dem.g)

dim(dem.g)
extent(dem.g)
res(dem.g)
dem.g
projection(dem.g)

PTR.limit <- readOGR("/Users/martaprat/Dropbox/LandscapeEcology - Pakke Tiger Reserve/Shape.files/Pakke Tiger Reserve Boundary.shp")
projection(PTR.limit) #check projection is the same
PTR.limit 
plot(dem.g)
plot(PTR.limit, add = T)

# crop the DEM
PTR.dem <- mask(dem.g,PTR.limit)
PTR.dem <- crop(PTR.dem,extent(PTR.limit))
plot(PTR.dem)

writeRaster(PTR.dem, filename = "PTR_DEM")

############################################################
# Calculate NDVI

setwd('/Users/martaprat/Documents/LandscapeEcology/GroupProject/maps/Landsat8_2013')

landsat <- stack("LC08_L1TP_136041_20131217_20170427_01_T1_B1.TIF",
                 "LC08_L1TP_136041_20131217_20170427_01_T1_B2.TIF",
                 "LC08_L1TP_136041_20131217_20170427_01_T1_B3.TIF",
                 "LC08_L1TP_136041_20131217_20170427_01_T1_B4.TIF",
                 "LC08_L1TP_136041_20131217_20170427_01_T1_B5.TIF",
                 "LC08_L1TP_136041_20131217_20170427_01_T1_B6.TIF",
                 "LC08_L1TP_136041_20131217_20170427_01_T1_B7.TIF")
landsat@layers
plotRGB(landsat, r = 4, g = 3, b = 2, scale = 50000)

band4 <- raster(landsat, layer = 4)
band5 <- raster(landsat, layer = 5)
NDVI <- (band5-band4)/(band4+band5)
plot(NDVI)

# crop the NDVI
PTR.limit <- spTransform(PTR.limit, projection(NDVI))
PTR.ndvi <- mask(NDVI,PTR.limit)
PTR.ndvi <- crop(PTR.ndvi,extent(PTR.limit))
plot(PTR.ndvi)

writeRaster(PTR.ndvi, filename = "PTR_NDVI")


