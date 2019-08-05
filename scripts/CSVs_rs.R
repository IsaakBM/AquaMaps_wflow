# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# AIM: Function that reads a AquaMaps .csv files and returns rasters? by species
# path: directory of AquaMaps species .csv files
# outdir: where to put the raster species files
# olayer: species from which ocean layer to overlap with bathymetry shapefile
# resolution: upscale or downscale?
# bathymetry_shp: a bathymetry shapefile to constrain species cells (ETOPO dataset)

# Input Files
# 1. species information (e.g. species ID, taxonomy, "reviewed" or not, but without spatial information)  = hcaf.csv
# 2. information for each half-degree cell (e.g. max/min depth, salinity, temperature, etc) = hpen.csv
# 3. a lookup table of which species are found in each half-degree cell (and the "probability of occurrence" of that species in that cell)   = cell.csv

aqua_rs <- function(path, resolution, bathymetry_shp) { # kill the cells that are not according with the bathymetry... 
  
  library(dplyr)
  library(raster)
  library(sf)
  
  
  # .csv files by species
  files_csv <- list.files(path = path, pattern = ".csv", full.names = TRUE) 
  rs_list <- vector("list", length(files_csv))
  # Loops to create rasters (0.5 degree res)
  rs_list <- foreach(i = 1:length(files_csv), .packages = c("dplyr", "raster")) %dopar% {
    single <- read.csv(path[i])
    rs_list[[i]] <- rasterFromXYZ(as.data.frame(single)[, c("CenterLong", "CenterLat", "probability", "TempPrefMax", "SalinityPrefMax")]) # 0.5 deg of resolution  
  } 
  
  
  # 2. ETOPO database overlap with distribution: 
  # match NA cells with current dsitribution cells of species in order to get an uniform representation of layers  
  # Bathymetry layers
    bathy <- raster(bathymetry_shp)
      bathy[] <- ifelse(bathy[] > 0, NA, bathy[]) # keeping only "negative" value (i.e. depths)
     if (olayer == "surface") {
      bathy_layer[] <- ifelse(bathy[] > 0, NA, bathy[])
    } else if (olayer == "mesopelagic") {
      bathy_layer[] <- ifelse(bathy[] > -200, NA, bathy[])
    } else if (olayer == "bathypelagic") {
      bathy_layer[] <- ifelse(bathy[] > -1000, NA, bathy[])
    } else if (olayer == "abyssopelagic") {
      bathy_layer[] <- ifelse(bathy[] > -4000, NA, bathy[])
    } else {
      bathy_layer[] <- ifelse(bathy[] > 0, NA, bathy[])
    }
  
  single <- subset(rs_list[[i]])    
  dt1 <- mask(single, resample(list_depts[[8]], single, resample = "bilinear"))
  up <- resample(rs_list[[j]], bathy, resample = "bilinear")    

  
  
  
  
  
    
}



library(raster)
library(dplyr)
library(sf)
dat <- read.csv("CSVs/Fis-33198_abyssopelagic.csv")
rs_trial <- rasterFromXYZ(as.data.frame(dat)[, c("CenterLong", "CenterLat", "probability", "TempPrefMax", "SalinityPrefMax")]) # 0.5 deg of resolution
plot(rs_trial$probability)
rs_trial <- raster::aggregate(x = rs_trial, fact = 2)
plot(rs_trial)

rs <- raster(ncol = 360, nrow = 180)
rs[] <- 1:ncell(rs)
up <- resample(rs_trial, rs, resample = "bilinear") # up-scaled to match the 1deg-resolution with climate models using bilenar interpolation
plot(up$probability)
writeRaster(up, "rasters/new_aqm2.grd", overwrite = TRUE)

new_up <- as.data.frame(rasterToPoints(up)) # 1 degree spatial resolution
# plot(boundaries(rs_trial, type = "inner")) # inner boundaries for species distribution edges

wb <- "/Users/bri273/Dropbox/02_/Chapter02/ShapeFiles/WorldBorders/"
wb <- st_read(wb)
wb_sp <- as(wb, "Spatial")

graphics.off()
pdf(file = "figs/Prob_Fis-33198_abyssopelagic.pdf", width = 38, height = 20)
plot(up$probability)
plot(wb_sp, add = TRUE, col = "gray54")
dev.off()
# species ID to math with hcaf distributions and extract depth (min max mean) and thermal + pH limits

rs <- raster(ncol = 360, nrow = 291)


######### ETOPO database to check species distributions and fixed that within the function

# # Bathymetry layers
#   depths <- raster("ShapeFiles/ETOPO1/ETOPO1_ocean.grd")
#     depths[] <- ifelse(depths[] > 0, NA, depths[])
#       depths1 <- depths
#       depths2 <- depths
#   depths <- raster("ShapeFiles/ETOPO1/ETOPO1_ocean.grd")
#     depths[] <- ifelse(depths[] > -200, NA, depths[])
#       depths3 <- depths
#   depths <- raster("ShapeFiles/ETOPO1/ETOPO1_ocean.grd")
#     depths[] <- ifelse(depths[] > -1000, NA, depths[])
#       depths4 <- depths
#       depths5 <- depths
#       depths8 <- depths
#   depths <- raster("ShapeFiles/ETOPO1/ETOPO1_ocean.grd")
#     depths[] <- ifelse(depths[] > -4000, NA, depths[])
#       depths7 <- depths
#   depths <- raster("ShapeFiles/ETOPO1/ETOPO1_ocean.grd")
#     depths[] <- ifelse(depths[] > -2000, NA, depths[])
#       depths6 <- depths
# 
# list_depts <- list(depths1, depths2, depths3, depths4, depths5, depths6, depths7, depths8)
# SOAR::Store(list_depts)
a2 <- stack("Data03/08_bottomHigher1000m_rcp85/02_ensemble-average/thetao_08_bottomHigher1000m_AnnualEnsembleMean_rcp85_r1i1p1_2006-2100.grd")
system.time(for (i in 1:95) { # 7 minutes for hist and ~ 14 for projections
  if(i == 1) {
    single <- subset(a2, i)
    dt1 <- mask(single, resample(list_depts[[8]], single, resample = "bilinear"))
    st1 <- dt1
    
  } else {
    single <- subset(a2, i)
    dt1 <- mask(single, resample(list_depts[[8]], single, resample = "bilinear"))
    st1 <- stack(st1, dt1)
  }
  print(paste0(i, " of ", 95))
})




