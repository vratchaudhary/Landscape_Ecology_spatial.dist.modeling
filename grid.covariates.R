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

proj.longlat <- projection(LC)
proj.utm <- projection(cam.trap)
projection(cam.trap.grid)
cam.trap <- spTransform(cam.trap, CRS(proj.longlat))
cam.trap.grid <- spTransform(cam.trap.grid, CRS(proj.longlat))
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
mid.df <- cam.trap.dup[1:10,]#data frame that will store all the locations 
mid.df@data$x_coord <- as.numeric(as.character(mid.df@data$x_coord))
for(i in 1:length(dup)){
  grid <- dup[i] #id of the grid
  cam.num <- cam.trap.dup[cam.trap.dup$ID_Grid == grid,] #extract the cameras for that grid
  cam.num@data$x_coord <- as.numeric(as.character(cam.num@data$x_coord)) #transform xcoord to num
  cam.coord <- as.data.frame(t(apply(cam.num@data,2,mean))) #calculate aritmetic mean
  cam.coord.x <- cam.coord[1,1] #extract x coord
  cam.coord.y <- cam.coord[1,2] #extract y coord
  
  #if coord are same, keep them
  if (cam.coord.x == cam.num@data$x_coord[1] && cam.coord.y == cam.num@data$y_coord[1]){
    mid.df[i,] <- cam.num@data[1,]
    mid.df@coords[i,] <- cam.num@coords[1,]
  }
  #if coord are different, create a new SpatialPointsDataFrame
  if(cam.coord.x != cam.num@data$x_coord[1] || cam.coord.y != cam.num@data$y_coord[1]){
    cam.coord.xy <- cbind(cam.coord.x,cam.coord.y)
    cam.mid <- SpatialPoints(cam.coord.xy, CRS(proj.utm)) 
    cam.mid <- spTransform(cam.mid, CRS(proj.longlat))
    cam.mid <- SpatialPointsDataFrame(cam.mid, cam.coord, bbox = as.matrix(extent(cam.trap)))
    mid.df[i,] <- cam.mid@data[1,]
    mid.df@coords[i,] <- cam.mid@coords[1,]
  }

}

# plot the results to check if points are in between
plot(cam.trap.dup)
plot(mid.df, add = T, col = "red")

# join this mid points with the initial data frame without the duplicated cameras
cam.trap.sing <- cam.trap.loc[-which(cam.trap.loc$ID_Grid %in% dup), c("x_coord","y_coord","ID_Grid")]
cam.trap.sing@data$x_coord <- as.numeric(as.character(cam.trap.sing@data$x_coord))
cam.trap.full <- rbind(cam.trap.sing,mid.df)

# check if all the values are within the infal dataframe
plot(cam.trap.sing)
plot(mid.df, add = T, col ="blue")
plot(cam.trap.full, add = T, col ="red")

#writeOGR(cam.trap.full, dsn = "/Users/martaprat/Dropbox/LandscapeEcology - Pakke Tiger Reserve/shape.files/",
#         layer = "LandEco.cam.loc", driver = "ESRI Shapefile")


################# ===================================================== ##################################
# ---- create a 4km2 square buffer around each of the camera locations to use it as a grid
cam.trap.full <- 

################# ===================================================== ##################################
# ---- match the camera location with the cameras in the spreadsheet for a species
porc <- read.csv("/Users/martaprat/Dropbox/LandscapeEcology - Pakke Tiger Reserve/porcupine_occupancy-matrix-presence_2017-11-09_1340.csv",
                 header = F)
porc <- porc[-56,1:3]
porc.x <- porc[,3]
porc.y <- porc[,2]
porc.xy <- as.data.frame(cbind(porc.x,porc.y))
str(porc.xy)
porc.coord <- SpatialPoints(porc.xy, CRS(proj.longlat))
porc.loc <- SpatialPointsDataFrame(porc.coord, data = porc, bbox = as.matrix(extent(cam.trap)))


cam.new <- readOGR("/Users/martaprat/Dropbox/LandscapeEcology - Pakke Tiger Reserve/shape.files/LandEco.cam.loc.shp")

plot(porc.loc)
plot(LC, add = T)
plot(cam.new, add = T, col = "blue")
