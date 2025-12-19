# requirements ----
library(lasR)
library(lidR)
library(sf)
library(terra)
library(rgl) #probably not necessary on windows

# import data ----
gps <- read_sf("./data/gps/poly.shp") # contains two polygons
las <- readLAS("./large_data/lidar/reduced.las")

# crop lidar data to polygon extent
las <- clip_roi(las, gps)
plot1 <- las[[1]] #dry
plot2 <- las[[2]] #wet

rglwidget() #workaround for my botched OpenGL install; probably not necessary on your devices
plot(plot2, colors = "RGB") #check out both of them ofc

# save those suckers ----
writeLAS(plot1, "./data/lidar/plot1.las")
writeLAS(plot2, "./data/lidar/plot2.las")

# use lasR to extract just the ground data & rasterize it for ease-of-use elsewhere

path <- list.files("./data/lidar", pattern = ".las", full.names = TRUE)
pipeline <- lasR::triangulate(filter = keep_ground()) + lasR::rasterize(res = 0.4) #play with resolution until there are no gaps

#plot1
output1 <- exec(pipeline, on = path[[1]])
png("./visualizations/las_raster_plot1.png", width = 900, height = 700)
plot(output1, pax = list(cex.axis = 2), plg = list(cex = 3), mar = c(3, 3, 3, 10))
dev.off()

writeRaster(output1, "./data/lidar/las_raster_plot1.tif")


#plot2
output2 <- exec(pipeline, on = path[[2]])

png("./visualizations/las_raster_plot2.png", width = 900, height = 700)
plot(output2, pax = list(cex.axis = 2), plg = list(cex = 3), mar = c(3, 3, 3, 10))
dev.off()

writeRaster(output2, "./data/lidar/las_raster_plot2.tif")

# to-do ----
#adjusting the raster plot output scales to the same values for ease of comparison





