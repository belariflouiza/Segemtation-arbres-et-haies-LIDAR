install.packages("ade4")
library(ade4)




# erase all
cat("\014")
rm(list = ls())
# knit options
knitr::opts_chunk$set(echo = TRUE)
# Set so that long lines in R will be wrapped:
knitr::opts_chunk$set(tidy.opts = list(width.cutoff = 80), tidy = TRUE)
knitr::opts_chunk$set(fig.align = "center")
# for display of rgl in html
knitr::knit_hooks$set(webgl = rgl::hook_webgl)
# output to html
html <- TRUE

# load dataset from package (default)
data(tree_inventory_chablais3, package = "lidaRtRee")
write.csv(tree_inventory_chablais3, file = "C:/Users/Etudiant/Documents/LAZ/input/file_laz.csv", row.names = FALSE)

# Afficher un message pour confirmer l'enregistrement du fichier
cat("Les données ont été enregistrées dans le fichier","C:/Users/Etudiant/Documents/LAZ/input", "\n")

# import field inventory
fichier <- "chablais3_listeR.csv"
tree_inventory_chablais3 <- read.csv(file = fichier, sep = ";", header = F, stringsAsFactors = TRUE)
names(tree_inventory_chablais3) <- c("x", "y", "d", "h", "n", "s", "e", "t")
# save as rda for later access
# save(tree_inventory_chablais3,file="tree_inventory_chablais3.rda")

head(tree_inventory_chablais3, n = 3L)

# display inventoried trees
lidaRtRee::plot_tree_inventory(tree_inventory_chablais3
  [, c("x", "y")],
  tree_inventory_chablais3$h,
  species = as.character(tree_inventory_chablais3$s)
)


# use table of species of package lidaRtRee to always use the same color for a given species
plot.species <- lidaRtRee::species_color()[levels(tree_inventory_chablais3$s), "col"]
library(ggplot2)
ggplot(tree_inventory_chablais3, aes(x = x, y = y, group = s)) +
  geom_point(aes(color = s, size = d)) +
  coord_sf(datum = 2154) +
  scale_color_manual(values = plot.species) +
  scale_radius(name = "Diameter") +
  geom_text(aes(label = n, size = 20), hjust = 0, vjust = 1) +
  labs(color = "Species") # titre de la légende

# duplicate coordinates to ensure they remain in the data.frame
tree_inventory_chablais3[, c("X", "Y")] <- tree_inventory_chablais3[, c("x", "y")]
# convert to spatial sf object
tree_inventory_chablais3 <- sf::st_as_sf(tree_inventory_chablais3, coords = c("X", "Y"), crs = 2154)
# buffer to apply around ROI (meters)
ROI_buffer <- 10
# ROI limits: bounding box of trees
ROI_range <- round(sf::st_bbox(tree_inventory_chablais3))

# load data in package lidaRtRee (default)
LASfile <- system.file("extdata", "las_chablais3.laz", package="lidaRtRee")
las_chablais3 <- lidR::readLAS(LASfile)
# set projection
lidR::projection(las_chablais3) <- 2154

# directory for laz files
lazdir <- "C:/Users/Etudiant/Documents/LAZ/input"
# build catalog of files
# specifying ALS data
cata <- lidR::readALSLAScatalog(lazdir)
# set coordinate system
lidR::projection(cata) <- 2154
# extract points in ROI plus additional 5m buffer
las_chablais3 <- lidR::clip_roi(
  cata,
  ROI_range + (ROI_buffer + 5) * c(-1, -1, 1, 1)
)
# save as rda for easier access:
# save(las_chablais3, file="las_chablais3.rda", compress = "bzip2")

# define extent and resolution of raster
output_raster <- terra::rast(resolution = 0.5,
                             xmin = ROI_range$xmin - ROI_buffer,
                             xmax = ROI_range$xmax + ROI_buffer,
                             ymin = ROI_range$ymin - ROI_buffer,
                             ymax = ROI_range$ymax + ROI_buffer,
                             crs = sf::st_crs(las_chablais3)$wkt
)
# terrain model computed from points classified as ground
dtm <- lidR::rasterize_terrain(las_chablais3, output_raster, lidR::tin())
# surface model
dsm <- lidR::rasterize_canopy(las_chablais3, output_raster, lidR::p2r())
# canopy height model
chm <- dsm - dtm
# save for later use
# chm_chablais3 <- terra::wrap(chm); save(chm_chablais3, file = "~/R/lidaRtRee/data/chm_chablais3.rda")

par(mfrow = c(1, 3))
# display DTM
terra::plot(dtm, main = "DTM")
# display DSM
terra::plot(dsm, main = "DSM")
# display CHM
terra::plot(chm, main = "CHM")


# plot mask computation based on inventoried positions
# convex hull of union of points geometry
mask_chull <- sf::st_convex_hull(sf::st_union(sf::st_geometry(tree_inventory_chablais3)))
# union of buffers around points geometry
mask_tree <- sf::st_union(sf::st_buffer(sf::st_geometry(tree_inventory_chablais3),
                                        2.1 + 0.14 * tree_inventory_chablais3$h))
# union of convex hull and tree buffers
mask_plot_v <- sf::st_union(mask_chull, mask_tree)
# rasterize mask
mask_plot <- terra::rasterize(terra::vect(mask_plot_v), dsm)

# display CHM
terra::plot(chm,
            col = gray(seq(0, 1, 1 / 255)),
            main = "Canopy Height Model and tree positions"
)
# add inventoried trees
lidaRtRee::plot_tree_inventory(tree_inventory_chablais3[, c("x", "y")],
                               tree_inventory_chablais3$h,
                               species = as.character(tree_inventory_chablais3$s), add = TRUE
)
# display plot mask
terra::plot(mask_plot_v, border = "red", add = TRUE)

# tree detection (default settings), applied on canopy height model
segms <- lidaRtRee::tree_segmentation(chm)
#
par(mfrow = c(1, 3))
# display pre-processed chm
terra::plot(segms$smoothed_dem, main = "Pre-processed CHM")
# display selected local maxima
terra::plot(segms$local_maxima, main = "Selected local maxima")
# display segments, except ground segment
dummy <- segms$segments_id
dummy[dummy == 0] <- NA
terra::plot(dummy, main = "Segments (random colors)",
            col = rainbow(8), type = "classes", legend = FALSE)

# tree extraction only inside plot mask for subsequent comparison
apices <- lidaRtRee::tree_extraction(segms, r_mask = mask_plot, crown = TRUE)
# convert WKT field to polygons
crowns <- sf::st_as_sf(sf::st_drop_geometry(apices), wkt = "crown")
# remove WKT field from apices
apices <- apices[, -which(names(apices)=="crown")]
head(apices, n = 3L)
#
# display initial image
terra::plot(chm, col = gray(seq(0, 1, 1 / 255)), main = "CHM and detected positions")
# display segments border
terra::plot(sf::st_geometry(crowns), border = "white", add = T, col = NA)
# display plot mask
terra::plot(mask_plot_v, border = "red", add = T)
# display detected apices
plot(apices["h"], col = "blue", cex = apices$h / 20, pch = 2, add = TRUE)


# match detected apices with field trees based on relative distance of apices
matched <- lidaRtRee::tree_matching(
  tree_inventory_chablais3[, c("x", "y", "h")],
  apices[, c("x", "y", "h")]
)
# display matching results
lidaRtRee::plot_matched(
  tree_inventory_chablais3[, c("x", "y", "h")],
  apices[, c("x", "y", "h")], matched, chm, mask_plot_v
)

# height histogram of detections
detection_stats <- lidaRtRee::hist_detection(
  tree_inventory_chablais3[, c("x", "y", "h")],
  apices[, c("x", "y", "h")], matched
)

# height histogram of detections
detection_stats <- lidaRtRee::hist_detection(
  tree_inventory_chablais3[, c("x", "y", "h")],
  apices[, c("x", "y", "h")], matched
)

height_reg <- lidaRtRee::height_regression(
  tree_inventory_chablais3[, c("x", "y", "h")],
  apices[, c("x", "y", "h")],
  matched,
  species = tree_inventory_chablais3$s
)

# linear regression between reference height and estimated height
height_reg <- lidaRtRee::height_regression(
  tree_inventory_chablais3[, c("x", "y", "h")],
  apices[, c("x", "y", "h")],
  matched,
  species = tree_inventory_chablais3$s
)

# normalize point cloud
lasn <- lidR::normalize_height(las_chablais3, lidR::tin())
# add segment id in LAS object
lasn <- lidR::merge_spatial(lasn, segms$segments_id, "seg_id")
# put all seg_id values in ordered list
list_seg_id <- sort(unique(lasn$seg_id))
# set names of list equal to values
names(list_seg_id) <- list_seg_id
# extract point cloud for each segment id in a list
las_l <- lapply(list_seg_id, function(x) {lidR::filter_poi(lasn, seg_id == x)})

# compute basic las metrics in each segment
metrics <- lidaRtRee::clouds_metrics(las_l, func = ~ list(
  maxZ = max(Z), meanZ = mean(Z),
  sdZ = sd(Z), meanI = mean(Intensity),
  sdI = sd(Intensity)
))
# add segment id attribute
metrics$seg_id <- row.names(metrics)
head(metrics, n = 3L)

# associate each reference tree with the segment that contains its trunk.
dummy <- terra::extract(
  segms$segments_id,
  sf::st_coordinates(tree_inventory_chablais3)
)
tree_inventory_chablais3$seg_id <- dummy$segments_id
# create new data.frame by merging metrics and inventoried trees (without geometry)
# based on segment id
tree_metrics <- base::merge(sf::st_drop_geometry(tree_inventory_chablais3), metrics)
# remove non-tree segment
tree_metrics <- tree_metrics[tree_metrics$seg_id != 0, ]
# add metrics to extracted apices data.frame
apices <- base::merge(apices, metrics, by.x = "id", by.y = "seg_id", all.x = T)


# create new variable orderer by segment id then decreasing height
tree_metrics_h <- tree_metrics[order(tree_metrics$seg_id, -tree_metrics$h), ]
i <- 2
# leave only first (highest) tree per segment id
while (i < nrow(tree_metrics_h)) {
  if (tree_metrics_h$seg_id[i] == tree_metrics_h$seg_id[i + 1]) {
    tree_metrics_h <- tree_metrics_h[-(i + 1), ]
  } else {
    i <- i + 1
  }
}
tree_metrics_h$s <- factor(tree_metrics_h$s)

par(mfrow = c(1, 2))
boxplot(meanI ~ s,
        data = tree_metrics[, c("s", "maxZ", "meanZ", "sdZ", "meanI", "sdI")],
        ylab = "Mean intensity in segment", xlab = "Specie",
        main = "All inventoried trees", las = 2, varwidth = TRUE
)
boxplot(meanI ~ s,
        data = tree_metrics_h, ylab = "Mean intensity in segment",
        xlab = "Specie", main = "Highest inventoried tree in segment", las = 2,
        varwidth = TRUE
)

par(mfrow = c(2,3))
boxplot(maxZ ~ s,
        data = tree_metrics_h, ylab = "Max height in segment",
        xlab = "Specie", main = "Max height by species", las = 2,
        varwidth = TRUE
)
boxplot(meanZ ~ s,
        data = tree_metrics_h, ylab = "Mean height in segment",
        xlab = "Specie", main = "Mean height by species", las = 2,
        varwidth = TRUE
)
boxplot(sdZ ~ s,
        data = tree_metrics_h, ylab = "Sd(height) in segment",
        xlab = "Specie", main = "Sd(height) by species", las = 2,
        varwidth = TRUE
)
boxplot(meanI ~ s,
        data = tree_metrics_h, ylab = "Mean intensity in segment",
        xlab = "Specie", main = "Mean intensity by species", las = 2,
        varwidth = TRUE
)
boxplot(sdI ~ s,
        data = tree_metrics_h, ylab = "Sd(intensity) in segment",
        xlab = "Specie", main = "Sd(intensity) by species", las = 2,
        varwidth = TRUE
)


# principal component analysis
pca <-
  ade4::dudi.pca(tree_metrics_h[, c("maxZ", "meanZ", "sdZ", "meanI", "sdI")], scannf = F)
# linear discriminant analysis
lda <- ade4::discrimin(pca, tree_metrics_h$s, scannf = F, nf = 2)
plot(lda)

tree_metrics_h$Groups <- tree_metrics_h$s
levels(tree_metrics_h$Groups)[!is.element(levels(tree_metrics_h$Groups), c("ABAL", "PIAB"))] <-
  "Other"


lda <-
  ade4::discrimin(pca, tree_metrics_h$Groups, scannf = F, nf = 2)
plot(lda)

lda_MASS <-
  MASS::lda(tree_metrics_h[, c("maxZ", "meanZ", "sdZ", "meanI", "sdI")],
            tree_metrics_h$Groups, CV = TRUE)
# confusion matrix
matrix_confusion <- table(tree_metrics_h$Groups, lda_MASS$class)
matrix_confusion
# percentage of good classification
round(sum(diag(matrix_confusion)) / sum(matrix_confusion) * 100, 1)
# confidence interval of the percentage
binom.test(sum(diag(matrix_confusion)), sum(matrix_confusion))$conf.int


# build model
lda_MASS <-
  MASS::lda(tree_metrics_h[, c("maxZ", "meanZ", "sdZ", "meanI", "sdI")],
            tree_metrics_h$Groups)
# apply model to metrics
metrics$predicted_s <-
  predict(lda_MASS, metrics[, c("maxZ", "meanZ", "sdZ", "meanI", "sdI")])$class
# apply model to trees
tree_metrics_h$predicted_s <- predict(lda_MASS,
                                      tree_metrics_h[, c("maxZ", "meanZ", "sdZ", "meanI", "sdI")])$class


# create image of predicted species
species <- terra::deepcopy(segms$segments_id)
# replace segment id by id of predicted species in image
terra::values(species) <-
  as.numeric(metrics$predicted_s)[match(terra::values(segms$segments_id), metrics$seg_id)]
# remove ground segment
species[segms$segments_id == 0] <- NA
# build raster attribute table rat
rat <- data.frame(id = 1:length(levels(metrics$predicted_s)),
                  Species = levels(metrics$predicted_s))
# retrieve reference colors
rat$col <- lidaRtRee::species_color()[rat$Species, "col"]
# set NA color to green
rat$col[is.na(rat$col)] <- "green"
# convert to factor (add RAT in SpatRaster)
levels(species) <- rat
# display results
terra::plot(species, col = rat$col)
terra::plot(sf::st_geometry(crowns), add = TRUE, border = "black")
lidaRtRee::plot_tree_inventory(tree_metrics_h
                               [, c("x", "y")],
                               tree_metrics_h$h,
                               bg = lidaRtRee::species_color()[as.character(tree_metrics_h$s), "col"],
                               col = "black",
                               pch = 21, add = TRUE
)




# extract all apices
apices_all <- lidaRtRee::tree_extraction(segms)
# round height values to cm
apices_all$h <- round(apices_all$h, 2)
# save outputs
terra::writeRaster(chm, file = "../data/output/chm.tif", overwrite = TRUE)
terra::writeRaster(species, file = "../data/output/r_species.tif", overwrite = TRUE)
terra::writeRaster(segms$segments_id, file = "../data/output/r_segments.tif", overwrite = TRUE)
sf::st_write(crowns, "../data/output/crowns.gpkg", delete_dsn = TRUE)
sf::st_write(apices_all, "../data/output/apices.gpkg", delete_dsn = TRUE)
write.csv(tree_metrics_h, "../data/output/correct_detections.csv", row.names = FALSE)
write.csv(sf::st_drop_geometry(tree_inventory_chablais3), "../data/output/tree_inventory_chablais.csv", row.names = FALSE)





rgl::par3d(mouseMode = "trackball") # parameters for interaction with mouse
# select segment points and offset them to avoid truncated coordinates in 3d plot
points.seg <- lasn[which(lasn$seg_id != 0), c("X", "Y", "Z", "seg_id")]
points.seg$X <- points.seg$X - 974300
points.seg$Y <- points.seg$Y - 6581600
# plot point cloud
rgl::plot3d(points.seg@data[, c("X", "Y", "Z")], col = points.seg$seg_id %% 10 + 1, aspect = FALSE)
#
# add inventoried trees
tree_inventory_chablais3$z <- 0
for (i in 1:nrow(tree_inventory_chablais3))
{
  rgl::lines3d(
    rbind(
      tree_inventory_chablais3$x[i] - 974300,
      tree_inventory_chablais3$x[i] - 974300
    ),
    rbind(
      tree_inventory_chablais3$y[i] - 6581600,
      tree_inventory_chablais3$y[i] - 6581600
    ),
    rbind(
      tree_inventory_chablais3$z[i],
      tree_inventory_chablais3$z[i] + tree_inventory_chablais3$h[i]
    )
  )
}
# Inventoried trees
# Using package rLiDAR, a 3d view of the field inventory can be displayed
# shape different between coniferous / deciduous
rgl::rgl.open()
rgl::rgl.bg(color = "white")
for (i in 1:nrow(tree_inventory_chablais3))
{
  if (is.na(tree_inventory_chablais3$h[i]) |
      is.na(tree_inventory_chablais3$d[i])) {
    next
  }
  if (!is.element(
    as.character(tree_inventory_chablais3$s[i]),
    c("ABAL", "PIAB", "TABA")
  )) {
    rLiDAR::LiDARForestStand(
      crownshape = "halfellipsoid",
      CL = 0.6 * tree_inventory_chablais3$h[i],
      CW = tree_inventory_chablais3$h[i] / 4,
      HCB = 0.4 * tree_inventory_chablais3$h[i],
      dbh = tree_inventory_chablais3$d[i] / 50,
      resolution = "high",
      X = tree_inventory_chablais3$x[i],
      Y = tree_inventory_chablais3$y[i],
      mesh = F
    )
  } else {
    rLiDAR::LiDARForestStand(
      crownshape = "cone",
      CL = 0.5 * tree_inventory_chablais3$h[i],
      CW = tree_inventory_chablais3$h[i] / 4,
      HCB = 0.5 * tree_inventory_chablais3$h[i],
      dbh = tree_inventory_chablais3$d[i] / 50,
      resolution = "high",
      X = tree_inventory_chablais3$x[i],
      Y = tree_inventory_chablais3$y[i],
      mesh = F,
      stemcolor = "burlywood4",
      crowncolor = "darkgreen"
    )
  }
}
# virtual apices from detection
coord <- data.frame(sf::st_coordinates(apices))
# shape different based on mean lidar intensity value (threshold 55)
library(rLiDAR)
rgl::rgl.open()
rgl::rgl.bg(color = "white")
for (i in 1:nrow(apices))
{
  if (apices$meanI[i] > 55) {
    rLiDAR::LiDARForestStand(
      crownshape = "halfellipsoid",
      CL = 0.6 * apices$h[i],
      CW = sqrt(4 * apices$s[i] / pi),
      HCB = 0.4 * apices$h[i],
      dbh = apices$h[i] / 50,
      resolution = "high",
      X = coord$X[i],
      Y = coord$Y[i],
      mesh = F
    )
  } else {
    rLiDAR::LiDARForestStand(
      crownshape = "cone",
      CL = 0.5 * apices$h[i],
      CW = sqrt(4 * apices$s[i] / pi),
      HCB = 0.5 * apices$h[i],
      dbh = apices$h[i] / 50,
      resolution = "high",
      X = coord$X[i],
      Y = coord$Y[i],
      mesh = F,
      stemcolor = "burlywood4",
      crowncolor = "darkgreen"
    )
  }
}

