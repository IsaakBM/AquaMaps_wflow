# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# AIM: Create a general dataframe that would be the core for generate input file for prioritizr analyses (conventional MARXAN)
# path: folder's name where species conservation feature files are located
# outdir: where to put the final sf-.rds object
# pu_shp: 

features_pus <- function(path, outdir, pu_shp, olayer) { 

####################################################################################
####### Defining the main packages (tryining to auto this)
####################################################################################
  # List of pacakges that we will use
    list.of.packages <- c("raster", "rgdal", "rgeos", "sf", "dplyr", "doParallel", "stringr", "sf", "lwgeom", "data.table")
  # If is not installed, install the pacakge
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages)
    # Load packages
    lapply(list.of.packages, require, character.only = TRUE)
    
####################################################################################
####### 
####################################################################################
  #
    if(stringr::str_detect(string = shapefile, pattern = ".rds") == TRUE) {
      shp_PU_sf <- readRDS(shapefile)
    } else if (stringr::str_detect(string = shapefile, pattern = ".shp") == TRUE) {
      shp_PU_sf <- st_read(shapefile)
    }
    
    col_ns <- colnames(shp_PU_sf)
    col_ns[1] <- ifelse(col_ns[1] != "layer", "layer", col_ns[1])
    colnames(shp_PU_sf) <- col_ns
    shp_PU_sf <- shp_PU_sf %>%
      dplyr::mutate (area_km2 = as.numeric(st_area(shp_PU_sf)/1e+06))
    pu_min_area <- min(shp_PU_sf$area_km2)

  # Reading conservation features .rds files (AquaMaps)
    dir <- path
    pattern1 <-  c(paste0("*", ".*.rds$"), paste0("*", ".*shp$"))
    files <- list.files(path = dir, pattern = paste0(pattern1, collapse = "|"), full.names = TRUE)

####################################################################################
####### 
####################################################################################
  # Loop through each file
    files_list <- vector("list", length = length(files)) # to allocate results
  # Begin the parallel structure
    ncores <- detectCores()
    ncores <- ncores - 1 
    cl <- makeCluster(ncores)
    registerDoParallel(cl)
    # A parallel Loop
      PU_list <- foreach(i = 1:length(files), .packages = c("raster", "sf", "dplyr", "stringr", "lwgeom", "data.table")) %dopar% {
        # Reading conservation features
        if(stringr::str_detect(string = region, pattern = ".rds") == TRUE) {
          single <- readRDS(files[i])
        } else if (stringr::str_detect(string = region, pattern = ".shp") == TRUE) {
          single <- st_read(files[i])
          }
        # Intersects every conservation feature with planning unit region
          pu_int <- st_intersection(shp_PU_sf, single) %>% 
            filter(st_geometry_type(.) %in% c("POLYGON", "MULTIPOLYGON")) # we want just the polygons/multi not extra geometries
        # Filter the intersection with the world polygon data to get the exact layer names
          if(nrow(pu_int) > 0) { # to avoid empty sf objects because some species are mainly at EEZs
            pu_int_b <- pu_int[pu_int$layer %in% shp_PU_sf$layer, ] %>% 
              group_by(layer) %>% 
              summarise(layer2 = unique(layer))
            # Calculating area info + type of feature
              pu_int_b <- pu_int_b %>%
                dplyr::mutate (area_km2 = as.numeric(st_area(pu_int_b)/1e+06),
                               feature_names = paste(unlist(strsplit(basename(files[i]), "_"))[1], olayer, sep = "_")) %>%
                dplyr::rename(pu = layer) %>%
                dplyr::filter(area_km2 >= pu_min_area) %>% 
                as.data.frame()
              files_list[[i]] <- pu_int_b 
              }
          }
      stopCluster(cl)

####################################################################################
####### 
####################################################################################
  # Final sf dataframe with all species information and write that object (main object to develop marxan input files)
    PU_list_b <- data.table::rbindlist(PU_list, use.names = TRUE)  
    # Write the object
      pu_csv <- paste(olayer, ".csv", sep = "")
      fwrite(dplyr::select(PU_list_b, -geometry, -layer2), paste(outdir, pu_csv, sep = ""))
  return(PU_list_b)
  }
  
  system.time(marxan_inputs(path = "RDS",
                            outdir = "shapefiles_rasters/",
                            shapefile = "InputFiles/PUsPacific/PacificABNJGrid_05deg.rds",
                            olayer = "surface"))
  
  
  
  
