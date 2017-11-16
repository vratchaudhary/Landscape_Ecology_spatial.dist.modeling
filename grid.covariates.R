#Landscape ecology project 
#Code to obtain gird level information of the camera trap locations in PTR

#--- working directory
setwd("/Users/martaprat/Documents/LandscapeEcology/Landscape_Ecology")

#--- pacages
library(raster)
library(rgdal)
library(geosphere)

#--- read the data with the locations of the camera traps
cam.trap <- readOGR("./Shapefiles/2013-14/Camera Trap Locations 2013-14.shp")
LC <- raster("PTR_LC")
cam.trap.grid <- readOGR("/Users/martaprat/Dropbox/LandscapeEcology - Pakke Tiger Reserve/shape.files/Grid_Along_Boundary_utm.shp")
plot(cam.trap.grid)

projection(LC)
projection(cam.trap)
projection(cam.trap.grid)
cam.trap <- spTransform(cam.trap, CRS(projection(LC)))
cam.trap.grid <- spTransform(cam.trap.grid, CRS(projection(LC)))
plot(LC)
plot(cam.trap, add = T, col = "red")
plot(cam.trap.grid, add = T)
# explore the data
cam.trap@data
cam.trap.grid@data

# intersect camera locations with grid cells
cam.trap.loc <- intersect(cam.trap,cam.trap.grid)
cam.trap.loc@data

# check which cameras are located within the same grid cell
sum <- as.data.frame(table(cam.trap.loc@data$ID_Grid))
dup <- as.integer(as.character(sum$Var1[which(sum$Freq > 1)]))

# extract those cameras that are duplicated within a grid cell
cam.trap.dup <- cam.trap.loc[which(cam.trap.loc$ID_Grid %in% dup), c("x_coord","y_coord","ID_Grid")]
cam.trap.dup@data

# visualize where they are located
plot(LC)
plot(cam.trap.grid, add = T)
plot(cam.trap.dup, add = T, col = "blue")

# obtain middle point between two camera trap locations
cam.135 <- cam.trap.dup[cam.trap.dup$ID_Grid == 135,]
cam.135@data #same location, keep only one
cam.136 <- cam.trap.dup[cam.trap.dup$ID_Grid == 136,]
cam.136@data #same location, keep only one
cam.201 <- cam.trap.dup[cam.trap.dup$ID_Grid == 201,]
cam.201@data 
cam.201@data$x_coord <- as.numeric(as.character(cam.201@data$x_coord))
cam.201.coord <- as.data.frame(apply(cam.201@data,2,mean)) #calculate aritmetic mean
cam.201.coord <- SpatialPoints(cam.201.coord[1:2,1]) ####--- cannot make them coordinates, resume from here
cam.201.mid <- SpatialPoints(c(as.factor(cam.201.mid[1]),cam.201.mid[2]))

str(cam.201@data)
str(cam.trap.loc)
