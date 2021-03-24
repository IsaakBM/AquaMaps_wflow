# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# AIM: Function that reads a AquaMaps files and returns .csv species files
# path: folder's name where aquampas.csv files are located
# outdir: where to put the .csv species files
# olayer: for what ocean layer do you want the species
# prob_threshold: 
# sp_env: Species envelope. >= 10 good cells (1) or 3-9 good cells (2). If you want all, write 1|2
# type: "Pacific" or "Normal"
# region: a raster (or shapefile) of your region of interest. If you don't know how, just load a global raster or shapefile and then use 
  # the interactive drawExtent() function to get a new crop raster (or shapefile). The use this object in region argument to rin the function

# Input Files
# 1. species information (e.g. species ID, taxonomy, "reviewed" or not, but without spatial information)  = hcaf.csv
# 2. information for each half-degree cell (e.g. max/min depth, salinity, temperature, etc) = hpen.csv
# 3. a lookup table of which species are found in each half-degree cell (and the "probability of occurrence" of that species in that cell)   = cell.csv


aqua_start <- function(path, outdir, olayer, prob_threshold, sp_env, type, region, ...) {
  
  library(raster)
  library(data.table)
  library(dplyr)
  library(tidyselect)
  library(foreach)
  library(doParallel)
  library(magrittr)
  library(ggplot2)
  library(sf)
  
####################################################################################
####### Defining the main packages (tryining to auto this)
####################################################################################
  # List of pacakges that we will use
    list.of.packages <- c("raster", "data.table", "dplyr", "foreach", "doParallel", "ggplot2", "magrittr", "sf", "tidyselect")
    # If is not installed, install the pacakge
      new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
      if(length(new.packages)) install.packages(new.packages)
    # Load packages
      lapply(list.of.packages, require, character.only = TRUE)

####################################################################################
####### Establish the main data to use
####################################################################################
  # AquaMaps' directory path
    dir <- path
      first_csv <- list.files(path = dir, pattern = "*hcaf.*.csv$", full.names = TRUE)
      second_csv <- list.files(path = dir, pattern = "*hspen.*.csv$", full.names = TRUE)
      third_csv <- list.files(path = dir, pattern = "*occursum.*.csv$", full.names = TRUE)
  # An empty raster at 0.5 deg (AquaMaps resolution grid) that will be use to mask
    rs <- raster(ncol = 720, nrow = 360)
    rs[] <- 1:ncell(rs)
  # Reading input files
    # If the region is a raster (.grd/tif) or a .rds, read it and then create a global extent for that file
      if(stringr::str_detect(string = region, pattern = ".grd") == TRUE) {
        region <- readAll(raster(region))
        region <- resample(region, rs, resample = "ngb")
      } else if (stringr::str_detect(string = region, pattern = ".rds") == TRUE) {
        region <- readRDS(region)
        region <- resample(region, rs, resample = "ngb")
      } else if (stringr::str_detect(string = region, pattern = ".tif") == TRUE) {
        region <- readAll(raster(region))
        region <- resample(region, rs, resample = "ngb")
      }
    # Reading the hcaf_species_native file
      hcaf <- fread(first_csv) %>% 
        dplyr::select(SpeciesID, CenterLat, CenterLong, Probability) %>% 
        dplyr::filter(Probability >= prob_threshold)
    # Reading the hspen file and select the Rank
      hspen <- fread(second_csv) %>% 
        dplyr::select(tidyselect::matches("Species|Pelagic|Depth|Oxy|Temp|Salinity|Rank")) %>% 
        dplyr::filter(Rank == sp_env)
    # Reading the speciesoccursum file 
      speciesInfo <- fread(third_csv, fill = TRUE)
    
  # Filtering by depth ocean layers before loops
    if(olayer == "surface") {
        hspen_v2 <- hspen %>% filter(DepthPrefMin <= 200 | DepthPrefMax <= 200)
      } else if (olayer == "mesopelagic") {
        hspen_v2 <- hspen %>% filter(DepthPrefMin > 200 & DepthPrefMin <= 1000 | DepthPrefMax > 200 & DepthPrefMax <= 1000)
      } else if (olayer == "bathyabyssopelagic") {
        hspen_v2 <- hspen %>% filter(DepthPrefMin > 1000 | DepthPrefMax > 1000)
      } else if (olayer == "all") {
        hspen_v2 <- hspen %>% filter(DepthPrefMin >= 0 | DepthPrefMax >= 0)
      } else {
        hspen_v2 <- hspen
      }
    speciesID <- hspen_v2$SpeciesID # how many species?
    IDs_df <- vector("list", length(speciesID))
    
    
####################################################################################
####### Getting the species distribution by region 
####################################################################################    
  # Set up parallel structure
    ncores <- detectCores()
    cl <- makeCluster(ncores -1)
    registerDoParallel(cl)
    # A parallel Loop
      IDs_df <- foreach(i = 1:length(speciesID), .packages = c("raster", "data.table", "dplyr", "magrittr", "sf")) %dopar% { # if you dont add any combine argument it will return a list
        x <- hcaf[hcaf$SpeciesID == speciesID[i],]
        y <- hspen_v2[hspen_v2$SpeciesID == speciesID[i],]
        z <- left_join(x = x, y = y, by = "SpeciesID")
        # Consider only species that has more than 10 cells in the study area
          if(nrow(z) >= 10 & mean(z$CenterLat) != (z$CenterLat[1])) {
            # From dataframe to raster
              rs1 <- rasterFromXYZ(as.data.frame(z) 
                                   [, c("CenterLong", "CenterLat", 
                                        "Probability", 
                                        "TempPrefMin","TempPrefMax", 
                                        "SalinityPrefMin","SalinityPrefMax", 
                                        "OxyPrefMin", "OxyPrefMax")])
              rs_final <- resample(rs1, rs, resample = "ngb") # projecting raster 0.5 deg just in case :-)
            # From Raster to Shapefile
              rs_final <- subset(rs_final, 1) # here I'm just getting the first layer (Probability) to avoid creating a big object per file.
              if(is.na(rs_final@crs) == TRUE) {crs(rs_final) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")} else {rs_final <- rs_final}
              rs_final <- mask(rs_final, region) # mask the species distribution with the region of interest
              # 
                if(is.nan(mean(rs_final[], na.rm = TRUE)) == FALSE) {
                  # Transform AquaMaps species raster into an sf spatial polygon dataframe
                    sd_rs1 <- as(rs_final, "SpatialPolygonsDataFrame")
                    sd_rs1 <- spTransform(sd_rs1, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) # just in case! # here there is an error... empty object needs to be
                    sd_rs1$Probability <- seq(1, length(sd_rs1)) 
                  # If you want maps by Pacific centred or NOT
                    if(type == "Pacific") {
                      # Define a long & slim polygon that overlaps the meridian line & set its CRS to match that of world
                        polygon <- st_polygon(x = list(rbind(c(-0.0001, 90),
                                                             c(0, 90),
                                                             c(0, -90),
                                                             c(-0.0001, -90),
                                                             c(-0.0001, 90)))) %>%
                          st_sfc() %>%
                          st_set_crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
                      # Transform the species distribution polygon object to a Pacific-centred projection polygon object
                        sd_rs1_robinson <- sd_rs1 %>% 
                        st_as_sf() %>% 
                        st_difference(polygon) %>% 
                        st_transform(crs = "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
                      # There is a line in the middle of Antarctica. This is because we have split the map after reprojection. We need to fix this:
                        bbox1 <-  st_bbox(sd_rs1_robinson)
                        bbox1[c(1,3)]  <-  c(-1e-5,1e-5)
                        polygon1 <- st_as_sfc(bbox1)
                        crosses1 <- sd_rs1_robinson %>%
                          st_intersects(polygon1) %>%
                          sapply(length) %>%
                          as.logical %>%
                          which
                        # Adding buffer 0
                          sd_rs1_robinson[crosses1, ] %<>%
                            st_buffer(0)
                      # Writing the object 
                        name_sps <- paste(z[1,1], olayer, sep = "_") 
                        saveRDS(sd_rs1_robinson, paste(outdir, name_sps, ".rds", sep = ""))
                        IDs_df[[i]] <- rasterToPoints(rs_final) %>% 
                          as.data.frame() %>%
                          dplyr::select(x, y) %>% 
                          dplyr::mutate(species = as.character(z[1,1]))
                        } else {
                          # Transform the species distribution polygon object to a common projection polygon object
                            sd_rs1_latlon <- sd_rs1 %>% 
                            st_as_sf() %>% 
                            st_transform(crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
                          # Writing the object 
                            name_sps <- paste(z[1,1], olayer, sep = "_") 
                            saveRDS(sd_rs1_latlon, paste(outdir, name_sps, ".rds", sep = ""))
                            IDs_df[[i]] <- rasterToPoints(rs_final) %>% 
                              as.data.frame() %>%
                              dplyr::select(x, y) %>% 
                              dplyr::mutate(species = as.character(z[1,1]))
                  }
              }
          }
        } # parallel loop ending bracket
      stopCluster(cl)
      IDs_df <- IDs_df[!sapply(IDs_df, is.null)] # removing NULL elements from list
      
####################################################################################
####### 
####################################################################################
  # Defining outcome
    sp_richness <- data.table::rbindlist(IDs_df, use.names = TRUE)
      sp_richness <- sp_richness %>% 
        dplyr::group_by(x, y) %>% 
        dplyr::summarise(richness = n()) %>% 
        data.frame()
  # Converting the richness data frame to a raster
      rs_richness <- rasterFromXYZ(sp_richness)
      rs_richness_final <- resample(rs_richness, rs, resample = "ngb")
      if(is.na(rs_richness_final@crs) == TRUE) {crs(rs_richness_final) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")} else {rs_richness_final <- rs_richness_final}
  # Transform richness raster to an sf spatial polygon dataframe
    rs1_richness <- as(rs_richness_final, "SpatialPolygonsDataFrame")
    rs1_richness <- spTransform(rs1_richness, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) # just in case! # here there is an error... empty object needs to be
    if(type == "Pacific") {
      # Define a long & slim polygon that overlaps the meridian line & set its CRS to match that of world
        polygon <- st_polygon(x = list(rbind(c(-0.0001, 90),
                                             c(0, 90),
                                             c(0, -90),
                                             c(-0.0001, -90),
                                             c(-0.0001, 90)))) %>%
          st_sfc() %>%
          st_set_crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
      # Transform the species distribution polygon object to a Pacific-centred projection polygon object
        richness_robinson <- rs1_richness %>% 
          st_as_sf() %>% 
          st_difference(polygon) %>% 
          st_transform(crs = "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
      # There is a line in the middle of Antarctica. This is because we have split the map after reprojection. We need to fix this:
        bbox1 <-  st_bbox(richness_robinson)
        bbox1[c(1,3)]  <-  c(-1e-5,1e-5)
        polygon1 <- st_as_sfc(bbox1)
        crosses1 <- richness_robinson %>%
          st_intersects(polygon1) %>%
          sapply(length) %>%
          as.logical %>%
          which
        # Adding buffer 0
          richness_robinson[crosses1, ] %<>%
            st_buffer(0)
        # Writing the object 
          name_obj <- paste("01_spp-richness", olayer, sep = "_")
          saveRDS(richness_robinson, paste(outdir, name_obj, ".rds", sep = ""))
    } else {
      # Transform the species distribution polygon object to a common projection polygon object
        richness_latlon <- rs1_richness %>% 
          st_as_sf() %>% 
          st_transform(crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
        # Writing the object 
        name_obj <- paste("01_spp-richness", olayer, sep = "_")
        saveRDS(richness_robinson, paste(outdir, name_obj, ".rds", sep = ""))
    }
    
####################################################################################
####### 
####################################################################################
  # Summ table with species taxonomic info per ocean layer
    spp_all <- data.table::rbindlist(IDs_df, use.names = TRUE)
    speciesInfo <- speciesInfo[speciesInfo$speciesID %in% spp_all$species, ]
    name.sum <- paste("01_speciesInfo", olayer, sep = "_")
    write.csv(speciesInfo, paste(outdir, name.sum, ".csv", sep = ""), row.names = FALSE)
}


system.time(aqua_start(path = "/Users/bri273/Desktop/AquaMaps_wflow/AquaMaps/v2019a",
                       outdir = "/Users/bri273/Desktop/AquaMaps_wflow/CSVs/04_abyssopelagic_mediterranean/",
                       olayer = "bathyabyssopelagic",
                       prob_threshold = 0.5,
                       sp_env = 1,
                       type = "Pacific",
                       region = "/Users/bri273/Desktop/AquaMaps_wflow/ETOPO1_05deg/etopos_mediterranean.grd"))

