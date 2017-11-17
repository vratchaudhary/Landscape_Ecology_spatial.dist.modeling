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
cam.trap <- readOGR("/Users/martaprat/Dropbox/LandscapeEcology - Pakke Tiger Reserve/Shape.files/Camera Trap Locations 2013-14.shp")
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

#write.csv(cam.trap.loc@data, file = "cam.trap.location.csv")


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
# ---- obtain single LC types
LC <- raster("/Users/martaprat/Dropbox/LandscapeEcology - Pakke Tiger Reserve/PTR_LC")
projection(LC)
unique(values(LC))
LC <- as.factor(LC)
LC.class <- levels(LC)[[1]]
LC.class[,"landcover"] <- c("-","Sub-tropical broadleaved evergreen","Himalayan moist temperate",
                            "Tropical semi-evergreen","Tropical moist deciduous","Temperate coniferous",
                            "Bamboo sp.","Degraded forest","Grassland","missing","Barren land",
                            "Water body", "Wetland")
levels(LC) <- LC.class
LC.class[,"area"] <- tapply(area(LC), LC[], sum)
LC.class[,"prop.area"] <- LC.class$area/sum(LC.class$area)

#### --- reclassify to FOREST
#create a reclassification matrix
# 1 column -> original land-cover categories
# 2 column -> new land-cover categories
LC.cat <- unique(LC)
LC.cat.forest<-c(0,1,1,1,1,1,0,0,0,0,0,0,0) #forest will be one category and the rest will be the same (non-forest)
reclass.mat<-cbind(LC.cat,LC.cat.forest)
reclass.mat #first col: orginal; second: change to
#forest binary layer from reclassification matrix
LC.forest<-reclassify(LC,reclass.mat)
plot(LC.forest)
sum(values(LC.forest) == 1, na.rm = T)/ncell(LC.forest)

#### --- reclassify to Sub-tropical BroadLeave
LC.cat.broadl <- c(0,1,0,0,0,0,0,0,0,0,0,0,0) #forest will be one category and the rest will be the same (non-forest)
reclass.mat<-cbind(LC.cat,LC.cat.broadl)
reclass.mat #first col: orginal; second: change to
#forest binary layer from reclassification matrix
LC.broadl <- reclassify(LC,reclass.mat)
plot(LC.broadl)

#### --- reclassify to Tropical Semi-evergreen
LC.cat.semieve <- c(0,0,0,1,0,0,0,0,0,0,0,0,0) #forest will be one category and the rest will be the same (non-forest)
reclass.mat<-cbind(LC.cat,LC.cat.semieve)
reclass.mat #first col: orginal; second: change to
#forest binary layer from reclassification matrix
LC.semieve <- reclassify(LC,reclass.mat)
plot(LC.semieve)

#### --- reclassify to Tropical mosit decidious
LC.cat.dec <- c(0,0,0,0,1,0,0,0,0,0,0,0,0) #forest will be one category and the rest will be the same (non-forest)
reclass.mat<-cbind(LC.cat,LC.cat.dec)
reclass.mat #first col: orginal; second: change to
#forest binary layer from reclassification matrix
LC.dec <- reclassify(LC,reclass.mat)
plot(LC.dec)

#### --- reclassify to water
LC.cat.water <- c(0,0,0,0,0,0,0,0,0,0,0,1,1) #forest will be one category and the rest will be the same (non-forest)
reclass.mat<-cbind(LC.cat,LC.cat.water)
reclass.mat #first col: orginal; second: change to
#forest binary layer from reclassification matrix
LC.water <- reclassify(LC,reclass.mat)
plot(LC.water)

# ---- create a 4km2 square buffer around each of the camera locations to use it as a grid
# width = to the center to the edge of the grid = 1000m
cam.buf <- gBuffer(cam.trap.full, byid = T, id = cam.trap.full$X, width = 1000,
                   capStyle="SQUARE")
plot(cam.buf)
area(cam.buf) #check the area is the correct
cam.buf <- spTransform(cam.buf, projection(LC))
plot(LC.forest)
plot(cam.buf, add = T)
# ---- crop all the buffers with the different land covers created above
sum.var <- data.frame(ID = cam.trap.full$X, LC.forest = 0, LC.broadl = 0, LC.semieve = 0, LC.dec = 0) #data frame to store values
buf.forest <- extract(LC.forest, cam.buf)
for(i in 1:length(buf.forest)){
  sum.var[i,2] <- mean(buf.forest[[i]],na.rm = T)
}

buf.broadl <- extract(LC.broadl, cam.buf)
for(i in 1:length(buf.broadl)){
  sum.var[i,3] <- mean(buf.broadl[[i]],na.rm = T)
}

buf.semieve <- extract(LC.semieve, cam.buf)
for(i in 1:length(buf.semieve)){
  sum.var[i,4] <- mean(buf.semieve[[i]],na.rm = T)
}

buf.dec <- extract(LC.dec, cam.buf)
for(i in 1:length(buf.dec)){
  sum.var[i,5] <- mean(buf.dec[[i]],na.rm = T)
}

################# ===================================================== ##################################
# ---- elevation
dem <- raster("/Users/martaprat/Dropbox/LandscapeEcology - Pakke Tiger Reserve/PTR_DEM")
projection(dem)
cam.buf <- spTransform(cam.buf, projection(dem))
buf.dem <- extract(dem, cam.buf)
for(i in 1:length(buf.dem)){
  sum.var[i,"elevation"] <- mean(buf.dem[[i]],na.rm = T)
}

################# ===================================================== ##################################
# ---- elevation
ndvi <- raster("/Users/martaprat/Dropbox/LandscapeEcology - Pakke Tiger Reserve/PTR_NDVI")
projection(ndvi)
projection(cam.buf)
cam.buf <- spTransform(cam.buf, projection(ndvi))
buf.ndvi <- extract(ndvi, cam.buf)
for(i in 1:length(buf.ndvi)){
  sum.var[i,"NDVI"] <- mean(buf.ndvi[[i]],na.rm = T)
}

#write.csv(sum.var, file ="PTR.covariates.csv")

##### ---- need to finish
################# ===================================================== ##################################
# ---- distance to park boundary

PTR.limit <- readOGR("./Shapefiles/2013-14/Pakke Tiger Reserve Boundary.shp")
projection(PTR.limit) #check projection is the same
plot(PTR.limit)
plot(PTR.limit, col = "black", add = TRUE)


################# ===================================================== ##################################
# ---- distance to village
village <- read.csv("/Users/martaprat/Dropbox/LandscapeEcology - Pakke Tiger Reserve/village_pakke.csv")
village.x <- village$long
village.y <- village$lat
village.xy <- cbind(village.x,village.y)
village.coord <- SpatialPoints(village.xy, CRS("+init=epsg:3857 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
village.loc <- SpatialPointsDataFrame(village.coord,village)
projection(village.loc)
plot(village.loc)
# give it the same projection as the cameras
village.loc <- spTransform(village.loc, proj.utm)
cam.trap.full <- spTransform(cam.trap.full, proj.utm)
plot(village.loc,pch = 16, col = "blue")
plot(cam.trap.full, add = T)





