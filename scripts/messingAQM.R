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
    IDs_sf <- vector("list", length(speciesID)) # create  vector to allocate results
   # Lopps
    for(i in 1:length(speciesID)) {
      x <- hcaf[hcaf$SpeciesID == speciesID[i],]
      y <- hspen_sf[hspen_sf$SpeciesID == speciesID[i],]
      z <- left_join(x = x, y = y, by = "SpeciesID")
      IDs_sf[[i]] <- z
      name.csv <- paste(speciesID[i], sep = "_")
      write.csv(IDs_sf[[i]], paste("CSVs/", name.csv, ".csv", sep = ""), row.names = FALSE)
      print(paste0(i, " of ", length(speciesID)))
      } # ~1.5 hour for ~10k species
        # ### figure it out if this structure runs in parallel
        # IDs_sf <- vector("list", length(speciesID))
        # cuts <- cut(length(speciesID), ncores)
        # levels(cuts)
        # system.time(foreach(i = 1:length(speciesID), .combine = rbind, .packages = c("data.table", "dplyr"), .multicombine = TRUE) %dopar% {
        #   x <- hcaf[hcaf$SpeciesID == speciesID[i],]
        #   y <- hspen_sf[hspen_sf$SpeciesID == speciesID[i],]
        #   z <- left_join(x = x, y = y, by = "SpeciesID")
        #   IDs_sf[[i]] <- z
        # }) # 1.6 minutes
        # stopCluster(cl)
  } else if (olayer == mesopelagic) {
    hspen_mp <- hspen %>% filter(DepthPrefMax >= 200 & DepthPrefMax < 1000) # DepthMean?
    speciesID <- hspen_sf$SpeciesID[1:100]
    IDs_mp <- vector("list", length(speciesID)) # create  vector to allocate results
    
  } else if (olayer == bathypelagic) {
    hspen_bp <- hspen %>% filter(DepthPrefMax >= 1000 & DepthPrefMax < 4000) # DepthMean?
  } else if (olayer == abyssopelagic) {
    hspen_abp <- hspen %>% filter(DepthPrefMax >= 4000) # DepthMean?
  } else {
    hspen_all <- hspen
  }
  
}
  
  