# set the working directory
setwd("/Users/martaprat/Documents/QuantEcology/Lab9_Occupancy")

#install.packages("unmarked") #it pops a seperate window that you have to accept (create a new library)
library(unmarked)

#read the data
fishcats.data <- read.table("fishcats.txt", header = T)
head(fishcats.data) #check the data

#convert the data into the right framework for unmarked
?unmarkedFrameOccu
fishcats.fmk <- unmarkedFrameOccu(y = fishcats.data[,1:6], siteCovs = fishcats.data[,7:9], 
                  obsCovs = list(Bait=fishcats.data[,10:15]))
#siteCovs are site specific
#obsCovs are not site specific, but change within sites (bite in this example)

#check what is the new data
str(fishcats.fmk)

#plot the data to see if there are some patterns
plot(fishcats.fmk)
#there are more detections after the bait is implemented (from third sampling occasion on)


#look at the proportion of sites occupied without accounting for non-detection
### naive occupancy estimate

fishcats.fmk@y #to call for variables we have to use @ instead of $
#fishcats.fmk@y = detection history

occupied.sites <- apply(fishcats.fmk@y, 2, sum) #total num of sites occupied by sampling occasion
#calculate the proportion of sites occupied

(no.of.sites <- nrow(fishcats.fmk@y)) #total number of sites
naive.occ1 <- occupied.sites/no.of.sites #naive occupancy
naive.occ1
mean.naive.occ <- mean(naive.occ1) #mean naive occupancy



######################################################################################
####### MODELING
######################################################################################
###### dot model = psi(.) and p (.)

#occu() estimates occupancy and detection in single season model
?occu

#first model no use of covariates
pdot.psidot <- occu(~1~1, data=fishcats.fmk)
pdot.psidot

#estimates are logit trasformed != probabilities
#transform back the estimates 
pdot <- backTransform(pdot.psidot, type = "det") #estimate of p
psidot <- backTransform(pdot.psidot, type = "state") #estimate of psi

#incorporating detecition probability increases the ocuppancy



######################################################################################
###### covariates model 
# baits would increase detection probability

#pbait.psidot
pbait.psidot <- occu(~Bait ~1, data = fishcats.fmk)
pbait.psidot

#obtain the estimates, but since there is more than one value, we need to do a different
#approach. Two ways

## 1st way
# detection probability for no baits (intercept =1, bait = 0)
pbait.no.bait <- backTransform(linearComb(pbait.psidot, coefficients = c(1,0), type = "det"))
# detection probability for baits (intercept =1, bait = 1)
pbait.bait <- backTransform(linearComb(pbait.psidot, coefficients = c(1,1), type = "det"))

#psi estimate
psidot2 <- backTransform(pbait.psidot, type = "state") 

## 2nd way
#use the predict(), creating a new dataset with values for bite 0 and 1
newbait <- data.frame(Bait=c(0,1))
predictedbait <- predict(pbait.psidot, newdata = newbait, type = "det", appendData = TRUE)
#with this method we also obtain the confidence intervals
#this detection probabilities could be now plotted

p.bait <- barplot(predictedbait$Predicted, xlab = "treatment", ylab = "detection probability (p)", ylim = c(0,0.6), 
                  col = c("darkgreen", "gray"), names.arg = c("no-bait", "bait"))
arrows(p.bait, predictedbait$lower, p.bait, predictedbait$upper, angle = 90, length = 0.3, code = 3)


#detection probability is higher when bait is used. Also, CI don't overlap, and we could assume that the treatment
#has statistically significant effect on the detection probability

#occupancy is veri similar


######################################################################################
###### model selection

#for the unmarked library we'll use the modSel()
#need to make a list, which contains the name of the model and the object where we stored the model
fms <- fitList("p(.)psi(.)" = pdot.psidot, "p(bait)psi(.)" = pbait.psidot)
modSel(fms)

#nPars = number of parameters
#delta = dAIC

#best model here would be the one that includes bait as a covariate for detection probability





######################################################################################
######################################################################################
###### ASSIGNMENT   ##################################################################

#model occupancy based on covariates (distance to water, disturbance and shrub density)
#use bait as a covariate for detection probability

# model1 p(bait)psi(distance to water)
# model2 p(bait)psi(disturbance)
# model3 p(bait)psi(shrub density)
# model4 p(bait)psi(distance to water + disturbance)


#estimate occupancy and detection probability under the different models
pbait.psiwater <- occu(~Bait ~Dist.water, data = fishcats.fmk)
pbait.psidist <- occu(~Bait ~Disturbance, data = fishcats.fmk)
pbait.psishrub <- occu(~Bait ~Shrub, data = fishcats.fmk)
pbait.psiwaterdist <- occu(~Bait ~Dist.water + Disturbance, data = fishcats.fmk)

#include all the models in a list
fms <- fitList("p(.)psi(.)" = pdot.psidot,
               "p(bait)psi(.)" = pbait.psidot,
               "p(bait)psi(dwater)" = pbait.psiwater,
               "p(bait)psi(disturbance)" = pbait.psidist,
               "p(bait)psi(shrub)" = pbait.psishrub,
               "p(bait)psi(water+disturbance)" = pbait.psiwaterdist)
modSel(fms)

## Q2 
# p for baited and non-baited occasions
newbait <- data.frame(Bait=c(0,1))
predicted.bait.best <- predict(pbait.psiwaterdist, newdata = newbait, type = "det", appendData = TRUE)
predicted.bait.best


## Q3
####### b
#get the range of this variable
min.dist <- min(fishcats.data$Dist.water)
max.dist <- max(fishcats.data$Dist.water)

#it goes from 0 to 129
#generate values within this range
water.dist <- seq(0,130, by = 10)

#create an object that contains the same # of values as the length of water.dist
#set all the values equal to "Med"
dist.values <- rep("Med", length(water.dist))

#specify that it is a factor variable with 4 levels
dist.values <- factor(dist.values, levels = c("Low", "Med", "High", "VHigh"))

#include a column with distance to water values & another for disturbance values
fishcats.new <- data.frame(Dist.water = water.dist, Disturbance = dist.values)

#predict the values from the new data
predictedoccu <- predict(pbait.psiwaterdist, newdata = fishcats.new, type = "state", 
                         appendData = T)
predictedoccu


#plot the values with CI lines
quartz()
par(oma = c(1.5,1,1,1), mar = c(6,4,3,2))
plot(predictedoccu$Predicted~predictedoccu$Dist.water, type = "l", ylim = c(0,1), xlab = "Distance to water (m)",
     ylab = "Predicted occupancy (psi)", main = "Fishing cat occupancy at medium disturbance")
lines(predictedoccu$lower~predictedoccu$Dist.water, lty = 2)
lines(predictedoccu$upper~predictedoccu$Dist.water, lty = 2)
mtext("Fig 1. Predicted occupancy estimates for the fishing cat, as a function of the distance to water bodies,
      and a medium level of disturbance. Dashed lines show 95% CI", cex = 0.8, side=1, outer = T)
      
dev.copy2pdf(file = "Q3b.occup.med.dist.pdf")


##### a
#obtain the mean value of the water distances
water.mean <- mean(fishcats.data$Dist.water)

#create the 4 disturbance levels
dist.values.a <- factor(dist.values, c("Low", "Med", "High", "VHigh"))

#create a data frame that contains the same water dinstance but different categories of 
#disturbance
fishcats.new.a <- data.frame(Dist.water = water.mean, Disturbance = dist.values.a)

#predict the values from the new data
predictedoccu.a <- predict(pbait.psiwaterdist, newdata = fishcats.new.a, type = "state", 
                         appendData = T)
predictedoccu.a
