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