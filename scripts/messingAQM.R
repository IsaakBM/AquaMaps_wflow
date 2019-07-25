# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# AIM: Function that reads a XXCX and returns a .csv files per species
# path: directory of aquampas.csv files
# outdir: where to put the .csv species files
# olayer: for what ocean layer do you want the species
# variable: what variable envelope do you want? # maybe all of them and then any want can pick

aqua_start <- function(path, outdir, olayer, variable, probability, ...) { # filter by depth? perhaps 4 layers? 
  
  library(data.table)
  library(dplyr)
  library(foreach)
  library(doParallel)
  
  outdir <- "CSVs/"
  olayer <- "surface"
  
  # Set up to run trajectories in parallel
  cores  <-  detectCores()
  ncores <- cores -1 
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  
  # folders <- dir("../AquaMaps_wflow/")
  
  
  # Reading input files
  hcaf <- fread("hcaf_species_native_richness_gte10.csv") %>% 
    dplyr::select(SpeciesID, CenterLat, CenterLong, probability) %>% 
    dplyr::filter(probability >= 0.4) # or == probability
    # [1] "SpeciesID"   "CsquareCode"
    # [3] "probability" "CenterLat"  
    # [5] "CenterLong"  "LOICZID" 
  
  hspen <- fread("AquaMaps/hspen_richness_all_gte10_240616.csv") %>% 
    dplyr::select(c(2:16)) # SpeciesID + DepthEnvelope + tempEnvelop + SalinityEnvelope
    # "SpeciesID"  
    # "DepthMin" "DepthPrefMin" "DepthPrefMax" "DepthMax" "MeanDepth"
    # "Pelagic"
    # "TempMin" "TempPrefMin" "TempPrefMax" "TempMax"
    # "SalinityMin" "SalinityPrefMin" "SalinityPrefMax" "SalinityMax"
  
  
  # Define ocean layers and 
  
  if (olayer == "surface") {
    
    hspen_sf <- hspen %>% filter(DepthPrefMax >= 0 & DepthPrefMax < 200) # DepthMean? ~10k species
      speciesID <- hspen_sf$SpeciesID[1:100]
      
    
    IDs_sf <- vector("list", length(speciesID))
    
    cuts <- cut(length(speciesID), ncores)
    levels(cuts)
    
    system.time(foreach(i = 1:length(speciesID), .combine = rbind, .packages = c("data.table", "dplyr"), .multicombine = TRUE) %dopar% {
        x <- hcaf[hcaf$SpeciesID == speciesID[i],]
        y <- hspen_sf[hspen_sf$SpeciesID == speciesID[i],]
        z <- left_join(x = x, y = y, by = "SpeciesID")
        IDs_sf[[i]] <- z
      }) # 1.6 minutes
    stopCluster(cl)
    
    IDs_sf <- vector("list", length(speciesID))
    system.time(for(i in 1:length(speciesID)) {
      
      x <- hcaf[hcaf$SpeciesID == speciesID[i],]
      y <- hspen_sf[hspen_sf$SpeciesID == speciesID[i],]
      z <- left_join(x = x, y = y, by = "SpeciesID")
      IDs_sf[[i]] <- z
      name.csv <- paste(speciesID[i], sep = "_")
        write.csv(IDs_sf[[i]], paste("CSVs/", name.csv, ".csv", sep = ""), row.names = FALSE)
      print(paste0(i, " of ", length(speciesID)))
    }) # ~1 minute for 60 species...
    
    
    hspen_sf <- hspen %>% filter(DepthPrefMax >= 0 & DepthPrefMax < 200) # DepthMean? ~10k species
    speciesID <- hspen_sf$SpeciesID[1:5]
    
    
    x <- hcaf[hcaf$SpeciesID == speciesID[1],]
    y <- hspen_sf[hspen_sf$SpeciesID == speciesID[1],]
    z <- left_join(x = x, y = y, by = "SpeciesID")
    
    # IDs_sf[[i]] <- left_join(x = hcaf[hcaf$SpeciesID == speciesID[i],], y = hspen_sf[hspen_sf$SpeciesID == speciesID[i],], by = "SpeciesID")
    
    
    
    IDs_csv <- paste(speciesID[i])
    write.csv(IDs_sf[[i]], paste(outdir, IDs_csv, ".csv", sep = ""), row.names = FALSE)
    
  } else if (olayer == mesopelagic) {
    hspen_mp <- hspen %>% filter(DepthPrefMax >= 200 & DepthPrefMax < 1000) # DepthMean?
  } else if (olayer == bathypelagic) {
    hspen_bp <- hspen %>% filter(DepthPrefMax >= 1000 & DepthPrefMax < 4000) # DepthMean?
  } else if (olayer == abyssopelagic) {
    hspen_abp <- hspen %>% filter(DepthPrefMax >= 4000) # DepthMean?
  } else {
    hspen_all <- hspen
  }
  
}
  
  rs_trial <- rasterFromXYZ(as.data.frame(new_aqm2)[, c("CenterLong", "CenterLat", "probability", "TempPrefMax", "SalinityPrefMax")]) # 0.5 deg of resolution
    plot(rs_trial)
    rs_trial <- raster::aggregate(x = rs_trial, fact = 2)
    plot(rs_trial)
  
    rs <- raster(ncol = 360, nrow = 180)
    rs[] <- 1:ncell(rs)
    up <- resample(rs_trial, rs, resample = "bilinear") # up-scaled to match the 1deg-resolution with climate models using bilenar interpolation
    plot(up)
    writeRaster(up, "rasters/new_aqm2.grd", overwrite = TRUE)
    
    new_up <- as.data.frame(rasterToPoints(up)) # 1 degree spatial resolution
    # plot(boundaries(rs_trial, type = "inner")) # inner boundaries for species distribution edges
    
    wb <- "/Users/bri273/Dropbox/02_/Chapter02/ShapeFiles/WorldBorders/"
      wb <- st_read(wb)
      wb_sp <- as(wb, "Spatial")
    
    graphics.off()
    pdf(file = "figs/SalinityPrefMax_sp1.pdf", width = 38, height = 20)
    plot(up$SalinityPrefMax)
    plot(wb_sp, add = TRUE, col = "gray54")
    dev.off()
# species ID to math with hcaf distributions and extract depth (min max mean) and thermal + pH limits

