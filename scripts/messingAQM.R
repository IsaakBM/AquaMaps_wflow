# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# AIM: Function that reads a XXCX and returns a .csv files per species
# path: directory of aquampas.csv files
# outdir: where to put the .csv species files
# olayer: for what ocean layer do you want the species
# variable: what variable envelope do you want? # maybe all of them and then any want can pick

aqua_start <- function(path, outdir, olayer, variable, ...) { # filter by depth? perhaps 4 layers? 
  
  
}

# 
library(data.table)
library(dplyr)
library(raster)
library(sf)

# reading files
hcaf <- fread("AquaMaps/hcaf_species_native_richness_gte10.csv")
  # glimpse(hcaf)
  # [1] "SpeciesID"   "CsquareCode"
  # [3] "probability" "CenterLat"  
  # [5] "CenterLong"  "LOICZID" 
  hcaf <- hcaf %>% dplyr::select(SpeciesID, CenterLat, CenterLong, probability)
hspen <- fread("AquaMaps/hspen_richness_all_gte10_240616.csv")
  # glimpse(hspen)
  # "SpeciesID"  
  # "DepthMin" "DepthPrefMin" "DepthPrefMax" "DepthMax" "MeanDepth"
  # "Pelagic"
  # "TempMin" "TempPrefMin" "TempPrefMax" "TempMax"
  # "SalinityMin" "SalinityPrefMin" "SalinityPrefMax" "SalinityMax"
  hspen <- hspen %>% dplyr::select(c(2:16))
  meso <- hspen %>% filter(DepthPrefMax >= 200 & DepthPrefMax < 1000) 
  glimpse(meso)

  # for (i in SpeciesID) {do the left join and then write it into an csv formar? to much?}
  speciesID <- hspen$SpeciesID
  
  new_aqm <- left_join(x = hcaf[hcaf$SpeciesID == speciesID[1],], y = hspen[hspen$SpeciesID == speciesID[1],], by = "SpeciesID")
  glimpse(new_aqm)
  write.csv(new_aqm, "new_aqm.csv", row.names = FALSE)  # .CSV names based on speciesID
  new_aqm2 <- new_aqm %>% filter(probability >= 0.4)
  glimpse(new_aqm2)
  
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

