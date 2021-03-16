# Variable Selection
# Open libraries
library(usdm)
library(raster)

# Get WorldClim data:
s <- getData('worldclim', var='bio', res=10)
plot(s[[1]])
# Set an extent or shapefile of my study area:
e <- extent(c(0, 20, 0, 20))
plot(e, add=T)
# Crop stack with extent object:
s2 <- crop(s, e)
plot(s2[[1]])
# Subset stack into selected variables.
# I will consider here a tropical plant species.
s3 <- subset(s2, c("bio1", "bio2", "bio3", "bio4", "bio6", "bio7", 
                   "bio12", "bio13", "bio14", "bio15", "bio16", "bio17"))

# You can see with the following code that BIO16 and BIO13 are highly correlated,
# as well as BIO17 and BIO14.
layerStats(s3, 'pearson', na.rm=T)

# Now let's run the VIF analysis.
# You can see all VIF values with:
vif(s3) 
# These values change with the set of variables and mask used.

# To filter variables, we run:
v1 <- vifcor(s3)
v1

# We can change threshold for a more conservative approach:
v2 <- vifcor(s3, th=0.5)
v2

# to keep only variables selected by vifcor we can run:
s4 <- exclude(s3,v1)

# An alternative approach would be use vifstep function, which does not consider correlation:
vifstep(s3)
# You may note that results are not very much different.