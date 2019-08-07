# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# AIM: Function that reads a AquaMaps .csv files and returns rasters? by species
# path: directory of AquaMaps species .csv files
# outdir: 
# olayer: species from which ocean layer to overlap with bathymetry shapefile
# bathymetry_shp: a bathymetry shapefile to constrain species cells (ETOPO dataset)

# Input Files
# 1. 
# 2. 
# 3. 

aqua_rs <- function(path, outdir, bathymetry_shp, olayer) { # kill the cells that are not according with the bathymetry... 
  
  library(dplyr)
  library(raster)
  library(sf)
  library(foreach)
  library(doParallel)
  
  # .csv files by species
  files_csv <- list.files(path = path, pattern = ".csv", full.names = TRUE) 
  rs_list <- vector("list", length(files_csv))
    # Loops to read every .csv file and create 0.5 deg rasters
    rs_list <- vector("list", length(files_csv))
    for (i in 1:length(files_csv)) {
      single <- read.csv(files_csv[i])
      if(nrow(single) > 1) { # or maybe > 10 like Rafaela said?
        rs_list[[i]] <- rasterFromXYZ(as.data.frame(single)
                                      [, c("CenterLong", "CenterLat", "probability", "TempPrefMin","TempPrefMax", "SalinityPrefMin","SalinityPrefMax")])
        print(paste0(i, " of ", length(files_csv)))
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
    # Creating a projected 0.5 deg raster
    rs <- raster(ncol = 720, nrow = 360)
    rs[] <- 1:ncell(rs)
    # Set up parallel structure
    cores  <-  detectCores()
    ncores <- cores -1 
    cl <- makeCluster(ncores)
    registerDoParallel(cl)
    # Parallel Loop
      rs_final <- foreach(j  = 1:length(rs_list), .packages = c("raster", "dplyr", "sf")) %dopar% {
        if(length(rs_list[[j]]) != 0) { # in case we have a NULL element in a list (species with only 1 or <= 10 cells)
          # constrain cells 
          single <- mask(rs_list[[j]], resample(bathy, rs_list[[j]], resample = "bilinear"))
          rs_final[[j]] <- resample(single, rs, resample = "bilinear") # projecting raster 0.5 deg
        }
      }
      stopCluster(cl)
  # 4. Writing rasters (not enough memory will create an error with temporal raster files)
    for(k in 1:length(rs_final)) {
      if(length(rs_final[[k]]) != 0) {
        name.rs <- paste(read.csv(files_csv[k])[1,1], olayer, sep = "_")
        writeRaster(rs_final[[k]], paste(outdir, name.rs, ".grd", sep = ""), overwrite = TRUE)
        print(paste0(k, " of ", length(rs_final)))
      }
    }
  return(rs_final)
}

