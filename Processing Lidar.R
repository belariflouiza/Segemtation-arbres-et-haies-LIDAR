install.packages("devtools")
devtools::install_github("Jean-Romain/lidRviewer")
library(sf)
library(lidR)
library(lidRviewer)
library(dplyr)
library(mmand)
library(mapview)
library(qgisprocess)
library(terra)

source("https://raw.githubusercontent.com/Jean-Romain/lidRviewer/master/sdl.R")
remotes::install_github("Jean-Romain/lidRviewer")

library(lidR)

help(decimate_points)

View(decimate_points)

rm(list = ls())

path0= "C:\\Users\\Etudiant\\Documents\\LAZ"
path_in=paste0(path0,"\\input")
path_out=paste0(path0,"\\output")

las = readLAS(paste0(path_in,"\\lidar.copc.laz"),select = "xyzirc")
las_orig<- las
plot(las)
summary(las)




num_iterations <- 10
thinned_list <- list()
time_list <- list()

for (i in 1:num_iterations) {
  T1<-Sys.time()
  set.seed(i)
  
  
  thinned <- decimate_points(las, random(i))
  
  
  thinned_list[[i]] <- thinned
  T2<-Sys.time()
  Tdiff= difftime(T2, T1)
  thinned_list[[i]] <- thinned
  time_list[[i]] <- Tdiff
  
  
}

summary(las)


for (i in 1:num_iterations) {
  cat("Summary for thinned", i, ":\n")
  summary(thinned_list[[i]])
  summary(time_list[[i]])
}




Tdiff= difftime(T2, T1)
(Tdiff)

T1<-Sys.time()
thinned2 <- decimate_points(las, homogenize(1,5))
T2<-Sys.time()
summary(thinned2)
plot(thinned2)

Tdiff= difftime(T2, T1)
(Tdiff)


T1<-Sys.time()
thinned3 = decimate_points(las, highest(5))
T2<-Sys.time()
summary(thinned3)
plot(thinned3)


las<- thinned3
las_orig<- las


Tdiff= difftime(T2, T1)
(Tdiff)

plot(las, color = "Classification")
summary(las)
las<- las_orig 


las<- classify_ground(las,algorithm = pmf(ws = 5, th = 3), last_returns = FALSE)
las_ground <- las
summary(las)

plot(las_ground, color = "Classification")
summary(las_ground)

las_ground_normalized<- normalize_height(las_ground,knnidw(k=20,p=2))
plot(las_ground_normalized)
summary(las_ground_normalized)
las_ground_bak<- las_ground

las_ground_normalized<- filter_poi(las_ground_normalized,(Z >= 0))
las_ground_normalized<- filter_poi(las_ground_normalized,(Z < 2))

lasunngrd<- grid_metrics(las_ground_normalized, func=min(Zref), 2)
summary(lasunngrd)
writeRaster(lasunngrd, filename = file.path(path_out,"treeainZ4.tif"), format="GTiff", overwrite=TRUE)
plot(las_ground_normalized,color="Z")
plot(las_ground_normalized,color="Zref")


las<- las_orig
plot(las)

dtm1<-grid_terrain(las, res = 0.25, algorithm = knnidw(k=5,p = 0.5), keep_lowest = TRUE)
plot(dtm1)
summary(dtm1)
writeRaster(dtm1 ,filename = file.path(path_out,"dtm5_05.tif"), format="GTiff",overwrite=TRUE)

chm <- grid_canopy(las, res = 1, pitfree(c(0,2,5,10,15), c(0, 1.5))) #je dois faire la mise à jour des variables et supprimer les extension des paramètres les plus compliqué
plot(chm)
writeRaster(chm,filename = file.path(path_out,"chm_1m.tif"), format="GTiff",overwrite=TRUE)

ttops = locate_trees(las_ground_normalized, lmf(ws=2, hmin=1, shape = "circular"))
# je dois afficher la longitude latitude de tous les objets et essayé une methode de rechercje local longitude te latitude des objets 
writeOGR(obj=ttops,dsn=path_out, layer="ttops2", driver="ESRI Shapefile", overwrite=TRUE)
# je dois suuprimer la partie concernée par les hyperparamètres qui ne sont mis en place par la pratqiue generale de la forme
# la plus adroite au champs de vision les plus interesse des plus meséricordieux de ne pas lacher la franche maitrise.
library(dplyr)
las<- segment_trees(las,li2012(R=3,speed_up = 5))
col <- pastel.colors(200)
plot(las, color = "treeID", colorPalette = col,backend = "lidRviewer")
metric<- tree_metrics(las,.stdtreemetrics)
length(unique(las$treeID) |> na.omit())
summary(metric)
nrows()
count(metric, "treeID")

# Canopy height model
chm_p2r_05 <- rasterize_canopy(las, 0.5, p2r(subcircle = 0.2), pkg = "terra")

# Smoothed by median filter
kernel <- matrix(1, 3, 3)
chm_p2r_05_smoothed <- terra::focal(chm_p2r_05, w = kernel, fun = median, na.rm = TRUE)
ttops_chm_p2r_05_smoothed <- locate_trees(chm_p2r_05_smoothed, lmf(5))

# Apply the algorithm
algo <- dalponte2016(chm_p2r_05_smoothed, ttops_chm_p2r_05_smoothed)
las <- segment_trees(las, algo)

# Plot with height labels and tree crowns
plot(las, bg = "white", size = 4, color = "treeID", height = TRUE, treetops = TRUE)

