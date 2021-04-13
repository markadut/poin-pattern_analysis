# reading data and converting them to dataframes R 
data <- read.csv("/Users/markadut/Desktop/2019_groceries.csv")
geo_data = read.csv("/Users/markadut/Downloads/Main_US_GroceryStores_Aug2019-GEOMETRY-2019-08-30/Main_US_GroceryStores_Aug2019-GEOMETRY-2019-08-30.csv")

zip <- max(data$zip_code)

retval <- subset(data, zip_code == max(zip_code))
print(retval)

brand <- subset(data, (brands == "Whole Foods Market" & city == "los angeles")) #data shows whole foods markets data in la
print.data.frame(brand)

# downloading all necessary libraries and packages
library(sf)
library(maptools)
library(raster)
library(spatstat)

install.packages(c("cowplot", "googleway", "ggplot2", "ggrepel", "ggspatial", "libwgeom", "sf", "rnaturalearth", "rnaturalearthdata"))

library("ggplot2")
theme_set(theme_bw())

library("rnaturalearth")
library("rnaturalearthdata")

x <- geo_data[, 3] #extracting longitude data from grocery store df
y <- geo_data[, 2] #extracting latitude data from grocery store df
xy_df <- data.frame(x, y)

#convert grocery store x y coordinates into a separate dataframe consisting of only 
#longs and lats

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)
#plot(x,y, pch=19)


#start visualizing the grocery store data on the USA continental map
p <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = xy_df, aes(x = x, y = y), size = 0.5, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-128, -66), ylim = c(24, 50), expand = FALSE)

plot(p)

# merge the Geo data and grocery data
merged_grocery_data <- merge(data,geo_data,by="safegraph_place_id")
california_grocery_stores <- subset(merged_grocery_data, (state == "ca"))
california_polygon_data <- merged_grocery_data[, 17]
california_polygon_dataframe <- data.frame(california_polygon_data)
california_longs =  california_grocery_stores[, 16]
california_lats =  california_grocery_stores[, 15]
california_xy = data.frame(california_longs, california_lats)

# plotting grocery stores in only California

california_groceries <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = california_xy, aes(x = california_longs, y = california_lats), size = 0.5, 
             shape = 23, fill = "darkred") +
  coord_sf(xlim = c(-125, -115), ylim = c(32, 42), expand = FALSE)

print(california_groceries)

# Point Pattern Analysis

library(rgdal)
library(raster)

# Creating raster obejcts for the us state pop density .tif files
img <- raster("/Users/markadut/Desktop/cali_projected_raster.tif")
pop  <- as.im(img)
plot(pop)

hist(pop, main=NULL, las=1)# pop density raster layer historgram
pop.lg <- log(pop)
hist(pop.lg, main=NULL, las=1)

setwd("/Users/markadut/Desktop/DATA 1150 FELLOW/3_9_NewFiles") # Replace the file path with the actual location of your shapefiles.

library(rgdal)

#plotting the California shape file border
s  <- readOGR(".","CA_boundary")
ca <- as.owin(s)
plot(ca)

#reading California_grocery_stores shapefile - got from ArcGIS
c  <- readOGR(".", "Groceries")  
groceries <- as(c, "ppp")             # Create .ppp object
plot(groceries, main=NULL, cols=rgb(0,0,0,.2), pch=20)

marks(groceries)  <- NULL
Window(groceries) <- ca

Q <- quadratcount(groceries, nx= 6, ny=6) 
plot(groceries, main=NULL, cols="grey70",pch=20) # add quadrant grid
plot(Q, add = TRUE) 

# Compute the density for each quadrat
Q.d <- intensity(Q)
# Plot the density
plot(intensity(Q, image=TRUE), main=NULL, las=1)  # Plot density raster
plot(groceries, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points

# scaling the groceries data shape as well as the Cali shapefile and the population density layer
groceries.km <- rescale(groceries, 1000, "km")
ca.km <- rescale(ca, 1000, "km")
pop.km  <- rescale(pop, 1000, "km")
pop.lg.km <- rescale(pop.lg, 1000, "km")

# Compute the density for each quadrant (in counts per km2)
Q  <- quadratcount(groceries.km, nx= 6, ny=6)
Q.d <- intensity(Q)

# Plot the density
plot(intensity(Q, image=TRUE), main=NULL, las=1)  # Plot density raster
plot(groceries.km, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)  # Add points

# we divide the population density covariate into tessellated surfaces
brk  <- c(-Inf, 2, 4, 6, Inf)  # Define the breaks
Zcut <- cut(pop, breaks=brk, labels=1:4)  # Classify the raster
E <- tess(image=Zcut)  # Create a tessellated surface

plot(E, main="", las=1)

plot(intensity(Q, image=TRUE), las=1, main=NULL)
plot(groceries.km, pch=20, cex=0.6, col=rgb(1,1,1,.5), add=TRUE)

# changing the color scheme of the graph
cl <-  interp.colours(c("lightyellow", "orange" ,"red"), E$n)
plot( intensity(Q, image=TRUE), las=1, col=cl, main=NULL)
plot(groceries.km, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)

#Calculate Isotropic Kernel Intensity Estimate of the Point Pattern
K1 <- density(groceries.km) # Using the default bandwidth
plot(K1, main=NULL, las=1)
contour(K1, add=TRUE)

#In this next chunk, a 150 km bandwidth (sigma = 150) is used.
#Note that the length unit is extracted from the point layer’s mapping units

K2 <- density(groceries.km, sigma=150) # Using a 150km bandwidth
plot(K2, main=NULL, las=1)
contour(K2, add=TRUE)

#The kernel defaults to a gaussian smoothing function. 
#The smoothing function can be changed to a quartic, 
#disc or epanechnikov function. For example, to change the kernel to a 
#disc function type:

K3 <- density(groceries.km, kernel = "disc", sigma=150) # Using a 150km bandwidth
plot(K3, main=NULL, las=1)
contour(K3, add=TRUE)

# Create the Poisson point process model
PPM1 <- ppm(groceries.km ~ pop.km)
# Plot the relationship
a <- plot(effectfun(PPM1, "pop.km", se.fit=TRUE), main=NULL, 
          las=1)

PPM1

# setting the 1st nearest neighbor distance to 1
mean(nndist(groceries.km, k=1))

# setting the 1st nearest neighbor distance to 2
mean(nndist(groceries.km, k=2))

#nearest neighbor analysis
ANN <- apply(nndist(groceries.km, k=1:100),2,FUN=mean)
plot(ANN ~ eval(1:100), type="b", main=NULL, las=1, xlab = "neighbor order number",  ylab = "ANN (km)")

#K estimate function/plot
K <- Kest(groceries.km)
plot(K, main=NULL, las=1)

#L estimate function/plot
L <- Lest(groceries.km, main=NULL)
plot(L, main=NULL, las=1)

#Now, we can plot the L function as well as the Lexpected function
plot(L, . -r ~ r, main=NULL, las=1)

#Analysis using Pair Correlation Function
g  <- pcf(groceries.km)
plot(g, main=NULL, las=1)

ann.p <- mean(nndist(groceries.km, k=1))
ann.p

n <- 599L               # setting the number of simulations
ann.r <- vector(length = n) # creating empty object to store simulation results (ANN values)
for (i in 1:n){
  rand.p   <- rpoint(n=groceries.km$n, win=ca.km)  # generating random point locations
  ann.r[i] <- mean(nndist(rand.p, k=1))  # Tally the ANN values
}

plot(rand.p, pch=16, main=NULL, cols=rgb(0,0,0,0.5))

hist(ann.r, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r))
abline(v=ann.p, col="blue")

n <- 599L
ann.r <- vector(length=n)
for (i in 1:n){
  rand.p  <- rpoint(n=groceries.km$n, f=pop.km)  # substituting ca.km to pop.km
  ann.r[i] <- mean(nndist(rand.p, k=1))
}

Window(rand.p) <- ca.km  # Replace raster mask with ma.km window
plot(rand.p, pch=16, main=NULL, cols=rgb(0,0,0,0.5))

hist(ann.r, main=NULL, las=1, breaks=40, col="bisque", xlim=range(ann.p, ann.r))
abline(v=ann.p, col="blue")

N.greater <- sum(ann.r > ann.p)
p <- min(N.greater + 1, n + 1 - N.greater) / (n +1)
p

PPM1 <- ppm(groceries.km  ~ pop.km) # note that when we use pop.lg.km there are missing values or NA vals
PPM1

PPM0 <- ppm(groceries.km ~ 1)
PPM0

groceries.km$n / area(ma.km) 

anova(PPM0, PPM1, test="LRT")
