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
# 1. 
# 2. 
# 3. 

aqua_rs <- function(path, bathymetry_shp, olayer) { # kill the cells that are not according with the bathymetry... 
  
  library(dplyr)
  library(raster)
  library(sf)
  library(foreach)
  library(doParallel)
  
  # .csv files by species
  files_csv <- list.files(path = path, pattern = ".csv", full.names = TRUE) 
  rs_list <- vector("list", length(files_csv))
  # Set up parallel structure
  cores  <-  detectCores()
  ncores <- cores -1 
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
    # Loops to create rasters (0.5 degree res)
    rs_list <- foreach(i = 1:length(files_csv), .packages = c("dplyr", "raster")) %dopar% {
      single <- read.csv(files_csv[i])
      if(nrow(single) > 1) { # some species have only 1 cell (avoid them)
        rs_list[[i]] <- rasterFromXYZ(as.data.frame(single)[, c("CenterLong", "CenterLat", "probability", "TempPrefMin","TempPrefMax", "SalinityPrefMin","SalinityPrefMax")])   
      }
    } 
  
  # 2. ETOPO database overlap with distribution: 
  # match NA cells with current dsitribution cells of species in order to get an uniform representation of layers  
  # Bathymetry layers
    bathy <- raster(bathymetry_shp)
      bathy[] <- ifelse(bathy[] > 0, NA, bathy[]) # keeping only "negative" value (i.e. depths) <- <- ? to make a copy?
     if (olayer == "surface") {
      bathy[] <- ifelse(bathy[] > 0, NA, bathy[])
    } else if (olayer == "mesopelagic") {
      bathy[] <- ifelse(bathy[] > -200, NA, bathy[])
    } else if (olayer == "bathypelagic") {
      bathy[] <- ifelse(bathy[] > -1000, NA, bathy[])
    } else if (olayer == "abyssopelagic") {
      bathy[] <- ifelse(bathy[] > -4000, NA, bathy[])
    } else {
      bathy[] <- ifelse(bathy[] > 0, NA, bathy[])
    }
  
  # 3. Match Bathymetry and species raster distributions spatial resolution
  rs_final <- vector("list", length(rs_list))
  for (j in 1:length(rs_list)) {
    # single <- subset(rs_list[[j]])    
    rs_final[[j]] <- mask(rs_list[[j]], resample(bathy, rs_list[[j]], resample = "bilinear"))
    print(paste0(j, " of ", length(rs_list)))
    
    # name.csv <- paste(IDs_df[[j]][1,1], olayer, sep = "_")
    # writeRaster(IDs_df[[j]], paste(outdir, name.csv, ".csv", sep = ""), row.names = FALSE)
    # print(paste0(j, " of ", length(IDs_df)))
  }
  return(rs_final)
}
