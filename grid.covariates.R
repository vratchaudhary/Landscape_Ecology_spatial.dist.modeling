#Landscape ecology project 
#Code to obtain gird level information of the camera trap locations in PTR

#--- working directory
setwd("/Users/martaprat/Documents/LandscapeEcology/Landscape_Ecology")

#--- pacages
library(raster)
library(rgdal)
library(geosphere)
library(rgeos)

#--- read the data with the locations of the camera traps
cam.trap <- readOGR("./Shapefiles/2013-14/Camera Trap Locations 2013-14.shp")
LC <- raster("/Users/martaprat/Dropbox/LandscapeEcology - Pakke Tiger Reserve/PTR_LC")
cam.trap.grid <- readOGR("/Users/martaprat/Dropbox/LandscapeEcology - Pakke Tiger Reserve/shape.files/Grid_Along_Boundary_utm.shp")

projection(LC)
proj.utm <- projection(cam.trap)
projection(cam.trap.grid)
LC <- projectRaster(LC, crs=proj.utm)
plot(LC)
plot(cam.trap, add = T, col = "red")
plot(cam.trap.grid, add = T)

# explore the data
cam.trap@data
cam.trap.grid@data

# intersect camera locations with grid cells
cam.trap.loc <- intersect(cam.trap,cam.trap.grid)
cam.trap.loc@data

write.csv(cam.trap.loc@data, file = "cam.trap.location.csv")


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
# ---- create new shapefiles with the valid cameras
cam.trap.new <- read.csv("/Users/martaprat/Dropbox/LandscapeEcology - Pakke Tiger Reserve/all.ct.locations.csv")
cam.new.x <- cam.trap.new[,"x_coord"]
cam.new.y <- cam.trap.new[,"y_coord"]
cam.new.xy <- cbind(cam.new.x,cam.new.y)
cam.new.coord <- SpatialPoints(cam.new.xy, CRS(proj.utm))
cam.new.coord <- SpatialPointsDataFrame(cam.new.coord, cam.trap.new, bbox = as.matrix(extent(cam.trap)))
plot(LC)
plot(cam.new.coord, add = T)



# check which cameras are located within the same grid cell
sum <- as.data.frame(table(cam.new.coord@data$ID_Grid))
dup <- as.integer(as.character(sum$Var1[which(sum$Freq > 1)]))

# extract those cameras that are duplicated within a grid cell
cam.trap.dup <- cam.new.coord[which(cam.new.coord$ID_Grid %in% dup), c("x_coord","y_coord","ID_Grid")]
cam.trap.dup@data

# visualize where they are located
plot(LC)
plot(cam.trap.grid, add = T)
plot(cam.trap.dup, add = T, col = "blue")

# obtain middle point between two camera trap locations
mid.df <- cam.trap.dup[1:3,]#data frame that will store all the locations 
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
    cam.mid <- SpatialPointsDataFrame(cam.mid, cam.coord, bbox = as.matrix(extent(cam.trap)))
    mid.df[i,] <- cam.mid@data[1,]
    mid.df@coords[i,] <- cam.mid@coords[1,]
  }
  
}
mid.df@data["X"] <- c("38m","42m","14m") #add the new names for these mid points

# plot the results to check if points are in between
plot(cam.trap.dup)
plot(mid.df, add = T, col = "red")

# join this mid points with the initial data frame without the duplicated cameras
cam.trap.sing <- cam.new.coord[-which(cam.new.coord$ID_Grid %in% dup), c("x_coord","y_coord","X","ID_Grid")]
cam.trap.sing@data$x_coord <- as.numeric(as.character(cam.trap.sing@data$x_coord))
cam.trap.full <- rbind(cam.trap.sing,mid.df[,c("x_coord","y_coord","X","ID_Grid")])

# check if all the values are within the infal dataframe
plot(cam.trap.sing)
plot(mid.df, add = T, col ="blue")
plot(cam.trap.full, add = T, col ="red")

#writeOGR(cam.trap.full, dsn = "/Users/martaprat/Dropbox/LandscapeEcology - Pakke Tiger Reserve/shape.files/",
#         layer = "LandEco.cam.loc", driver = "ESRI Shapefile")

################# ===================================================== ##################################
# ---- create a 4km2 square buffer around each of the camera locations to use it as a grid
wind.4km <- focalWeight(LC, d = 4000, type = "rectangle")
plot(wind.4km)
per.LC <- focal(LC,w=wid.4km, fun=)
plot(LC)
cam.buf <- gBuffer(cam.trap.full, byid = T, id = cam.trap.full$X, width = 4000,
                   capStyle="SQUARE")
plot(cam.buf)
################# ===================================================== ##################################

