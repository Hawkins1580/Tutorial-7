
# Starting Tutorial #7


# Installing a bunch of packages
install.packages(c("dplyr", "ggplot2"))
install.packages(c("lubridate", "forecast", "raster", "sf"))

library(dplyr)
library(ggplot2)
library(lubridate)
library(forecast)
library(raster)
library(sf)


# pre outbreak image
pre <- stack("/cloud/project/activity07/ldd_pre.tif")

# post outbreak image
post <- stack("/cloud/project/activity07/ldd_post.tif")

plot(pre, col=grey(1:100/100) )




par(mfrow=c(1,2))

plotRGB(post, r=3,g=2,b=1, # image and bands
        scale=13000, # some urban or clouds can have reflectance > 1
        stretch="lin", # contrast stretch
        main="True color",
        axes=TRUE)
plotRGB(post, r=4,g=3,b=2, # image and bands
        scale=13000,# max data value
        stretch="lin", # contrast stretch
        main="False color",
        axes=TRUE)


# Vector Data

northBound <- st_read("/cloud/project/activity07/north_bound.shp")
midBound <- st_read("/cloud/project/activity07/mid_bound.shp")


plotRGB(post, r=3,g=2,b=1, scale=13000, stretch="lin")
plot(northBound$geometry, border="tomato3", fill=NA, lwd=3, add=TRUE)
plot(midBound$geometry, border="royalblue4",fill=NA, lwd=3, add=TRUE)



# NDVI

#calculate NDVI pre from NIR (4th band) and Red (3rd band)
ndvi_pre <- (pre[[4]]-pre[[3]])/(pre[[4]]+pre[[3]])
#calculate NDVI post
ndvi_post <- (post[[4]]-post[[3]])/(post[[4]]+post[[3]])

par(mfrow=c(1,2))
plot(ndvi_pre)
plot(ndvi_post)

ndviDiff <- ndvi_post - ndvi_pre
plot(ndviDiff)



# Exctracting data in a vector geometry

# return a vector of cell values from the first polygon
ndvi_North <- extract(ndviDiff,northBound)[[1]]
# return a vector of cell values from the first polygon
ndvi_Mid <- extract(ndviDiff,midBound)[[1]]


ndviDataframe <- data.frame(location=c(rep("North", length(ndvi_North)),
                                       rep("Mid", length(ndvi_Mid))),
                            ndvi.diff =c(ndvi_North,ndvi_Mid))


ggplot(data=ndviDataframe,
       aes(x=location,ndvi.diff))+
  geom_boxplot(outlier.shape = NA)+
  ylim(-0.5,0.5)



