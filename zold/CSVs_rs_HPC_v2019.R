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
      ncores <- cores -1 # set 23 for cluster
      cl <- makeCluster(ncores)
      registerDoParallel(cl)
      # Parallel Loop
        rs_final <- foreach(j  = 1:length(files_csv), .packages = c("raster", "dplyr")) %dopar% {
          single <- read.csv(files_csv[j])
          ns <- basename(files_csv[j])
            code <- unlist(lapply(ns, function(x) strsplit(x, "_")[[1]][[1]]))
          if(nrow(single) >= 10 & mean(single$CenterLat) != (single$CenterLat[1])) { 
            rs1 <- rasterFromXYZ(as.data.frame(single) 
                                 [, c("CenterLong", "CenterLat", "Probability", "TempPrefMin","TempPrefMax", "SalinityPrefMin","SalinityPrefMax", "OxyPrefMin", "OxyPrefMax")])
              rs1 <- mask(rs1, resample(bathy, rs1, resample = "ngb")) # match cells with the appropiate bathymetry
            rs_final <- resample(rs1, rs, resample = "ngb") # projecting raster 0.5 deg
            
            name.rs <- paste(code, olayer, sep = "_")
            writeRaster(rs_final, paste(outdir, name.rs, ".grd", sep = ""), overwrite = TRUE)
          }
        }
        stopCluster(cl)
}

# system.time(aqua_rs(path = "/QRISdata/Q1216/BritoMorales/AquaMaps_wflow/CSVs/v2019a/01_surface",
#                     outdir = "/QRISdata/Q1216/BritoMorales/AquaMaps_wflow/rasters/v2019a/",
#                     bathymetry_shp = "/QRISdata/Q1216/BritoMorales/AquaMaps_wflow/ETOPO1_05deg/ETOPO1_ocean.grd",
#                     olayer = "surface"))

system.time(aqua_rs(path = "/Users/bri273/Desktop/AquaMaps_wflow/CSVs/01_surface_mediterranean",
                    outdir = "/Users/bri273/Desktop/AquaMaps_wflow/rasters/01_surface_mediterranean/",
                    bathymetry_shp = "/Users/bri273/Desktop/AquaMaps_wflow/ETOPO1_05deg/ETOPO1_ocean.grd",
                    olayer = "surface"))
