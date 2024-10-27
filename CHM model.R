require(lidR)
require(rlas) # Necessary for writelax
require(rgdal) # Writing to shp or raster
require(tictoc) # for tic() toc() function
library(tidyverse)
library(raster)
library(sf)
library(whitebox)
library(tmap)
library(stars)
library(rayshader)
library(rgl)
require(lidR) # Most of the LiDAR processing
require(rlas) # Necessary for writelax
require(rgdal) # Writing to shp or raster
require(tictoc) # for timing
require(sp) # A few spatial operations
require(concaveman) # For concave hulls

install.packages("BiocManager") 
BiocManager::install("EBImage")
library(EBImage)


path0= "C:\\Users\\Etudiant\\Documents\\LAZ"
path_in=paste0(path0,"\\input")
path_out=paste0(path0,"\\output")

las = readLAS(paste0(path_in,"\\zone_delimitee.laz"),select = "xyzirc")

las_orig<- las
las_check(las)
summary(las)

sort(unique(las@data$Classification))
plot(las, color = "Classification")
plot3d(las, color = "Classification")

las_class <-filter_poi(las, Classification == 5)
plot(las_class)


#las <- readLAS(paste0(path_in,"\\lidar.copc.laz"), filter="-keep_class 2 5") # Keep high vegetation and ground point classes`

dtm <- grid_terrain(las, algorithm = knnidw(k = 8, p = 2))
summary(dtm)
las_normalized <- normalize_height(las, dtm)
plot(las_normalized)

# Utiliser l'algorithme k-NN avec distance euclidienne pour créer le DTM
dtm_knn <- grid_terrain(las, algorithm = knnidw(k = 8, p = 2))
las_normalized_knn <- normalize_height(las, dtm_knn)
plot(las_normalized_knn)


# Create a filter to remove points above 95th percentile of height
lasfilternoise = function(las, sensitivity)
{
  p95 <- grid_metrics(las, ~quantile(Z, probs = 0.95), 10)
  las <- merge_spatial(las, p95, "p95")
  las <- filter_poi(las, Z < p95*sensitivity)
  las$p95 <- NULL
  return(las)
}

las_denoised <- lasfilternoise(las_normalized, sensitivity = 1.2)

plot(las_denoised)

chm <- grid_canopy(las_denoised, 0.5, pitfree(c(0,2,5,10,15), c(3,1.5), subcircle = 0.2))
plot_dtm3d(chm)

ker <- matrix(1,5,5)
chm_s <- focal(chm, w = ker, fun = median)
plot (chm_s)
algo <- watershed(chm_s)
las_watershed  <- lastrees(las_denoised, algo)

# remove points that are not assigned to a tree
trees <- lasfilter(las_watershed, !is.na(treeID))

# View the results
plot(trees, color = "treeID", colorPalette = pastel.colors(100))



las = readLAS(paste0(path_in,"\\zone_delimitee.laz"),select = "xyzr", filter = "-drop_z_below 0")

chm <- rasterize_canopy(las, 0.5, pitfree(subcircle = 0.2))
plot(las, bg = "white", size = 4)

ttops <- locate_trees(las, lmf(ws = 400))

plot(chm, col = height.colors(3))
plot(sf::st_geometry(ttops), add = TRUE, pch = 10)

x <- plot(las, bg = "white", size = 4)
add_treetops3d(x, ttops)
