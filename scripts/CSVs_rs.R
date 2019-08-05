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
  # Set up parallel structure
  cores  <-  detectCores()
  ncores <- cores -1 
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
    # Loops to create rasters (0.5 degree res)
    rs_list <- foreach(i = 1:length(files_csv), .packages = c("dplyr", "raster")) %dopar% {
      single <- read.csv(path[i])
      rs_list[[i]] <- rasterFromXYZ(as.data.frame(single)
                                    [, c("CenterLong", "CenterLat", "probability", "TempPrefMin","TempPrefMax", "SalinityPrefMin","SalinityPrefMax")])  
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
  
  # 3. Match Bathymetry and species raster distributions spatial resolution
  rs_final <- vector("list", length(rs_list))
  for (j in 1:length(rs_list)) {
    single <- subset(rs_list[[j]])    
    rs_final[[j]] <- mask(rs_list, resample(bathy_layer, rs_list, resample = "bilinear"))
  }
  return(rs_final)
}
