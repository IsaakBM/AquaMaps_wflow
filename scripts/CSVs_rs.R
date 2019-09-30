# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# AIM: Function that reads a AquaMaps .csv files and returns rasters? by species
# path: directory of AquaMaps species .csv files
# outdir: where to put the .csv species files
# olayer: species from which ocean layer to overlap with bathymetry shapefile
# bathymetry_shp: a bathymetry shapefile to constrain species cells (ETOPO dataset)

# ADD DATA (SPECIES OR RICHNESS) ARGUMENT (THIS WILL NOT WORK FOR RICHNESS BECAUSE YOU WILL NOT HAVE IN .CVS TEM, SALI JUST RICHNESS)

aqua_rs <- function(path, outdir, bathymetry_shp, olayer) { # kill the cells that are not according with the bathymetry... 
  
  library(dplyr)
  library(raster)
  library(foreach)
  library(doParallel)
  library(rslurm)
  
  # 1) .csv files by species
  files_csv <- list.files(path = path, pattern = ".csv", full.names = TRUE)
  
  # 2) ETOPO database overlap with distribution: 
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
  
   # 3) Match Bathymetry and species raster distributions spatial resolution
    rs_final <- vector("list", length(files_csv))
    # Creating a projected 0.5 deg raster
      rs <- raster(ncol = 720, nrow = 360)
      rs[] <- 1:ncell(rs)
    # Set up parallel structure
      cores  <-  detectCores()
      ncores <- cores -1 
      cl <- makeCluster(ncores)
      registerDoParallel(cl)
      # Parallel Loop
        rs_final <- foreach(j  = 1:length(files_csv), .packages = c("raster", "dplyr")) %dopar% {
          single <- read.csv(files_csv[j])
          if(nrow(single) >= 4 & mean(single$CenterLat) != (single$CenterLat[1])) { 
            rs1 <- rasterFromXYZ(as.data.frame(single) 
                                 [, c("CenterLong", "CenterLat", "probability", "TempPrefMin","TempPrefMax", "SalinityPrefMin","SalinityPrefMax")])
              rs1 <- mask(rs1, resample(bathy, rs1, resample = "bilinear"))
            rs_final[[j]] <- resample(rs1, rs, resample = "bilinear") # projecting raster 0.5 deg
            }
            }
            stopCluster(cl)
        names(rs_final) <- lapply(files_csv, FUN = function(x) strsplit(basename(x), "_")[[1]][[1]])
        rs_final <- rs_final[lapply(rs_final, length) > 0]
  
    # 4. Writing rasters (not enough memory will create an error with temporal raster files)
      for(k in 1:length(rs_final)) {
        if(length(rs_final[[k]]) != 0) {
          name.rs <- paste(names(rs_final[i]), olayer, sep = "_")
          writeRaster(rs_final[[k]], paste(outdir, name.rs, ".grd", sep = ""), overwrite = TRUE)
          print(paste0(k, " of ", length(rs_final)))
          }
        }
  return(rs_final)
}

system.time(aqua_rs(path = "CSVs/01_surface_mediterranean",
                    outdir = "rasters/01_surface_mediterranean/",
                    bathymetry_shp = "ETOPO1_05deg/ETOPO1_ocean.grd",
                    olayer = "surface"))



