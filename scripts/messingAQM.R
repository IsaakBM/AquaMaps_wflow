# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# AIM: Function that reads a AquaMaps files and returns .csv species files
# path: directory of aquampas.csv files
# outdir: where to put the .csv species files
# olayer: for what ocean layer do you want the species
# variable: what variable envelope do you want? # maybe all of them and then any want can pick (not really)

# Input Files
# 1. species information (e.g. species ID, taxonomy, "reviewed" or not, but without spatial information)  = cell.csv
# 2. information for each half-degree cell (e.g. max/min depth, salinity, temperature, etc) = hpen.csv
# 3. a lookup table of which species are found in each half-degree cell (and the "probability of occurrence" of that species in that cell)   = hcaf.csv


aqua_start <- function(path, outdir, olayer, prob_threshold, ...) { # filter by depth? perhaps 4 layers? 
  
  library(data.table)
  library(dplyr)
  library(foreach)
  library(doParallel)
  
  # file's names
  dir <- dir(path, pattern = ".csv")
  first_csv <- paste(path, dir[2], sep = "")
  second_csv <- paste(path, dir[3], sep = "")
  
  # Reading input files
  hcaf <- fread(first_csv) %>% 
    dplyr::select(SpeciesID, CenterLat, CenterLong, probability) %>% 
    dplyr::filter(probability >= prob_threshold) # or == probability
    # [1] "SpeciesID"   "CsquareCode"
    # [3] "probability" "CenterLat"  
    # [5] "CenterLong"  "LOICZID" 
  hspen <- fread(second_csv) %>% 
    dplyr::select(c(2:16)) # SpeciesID + DepthEnvelope + tempEnvelop + SalinityEnvelope
    # "SpeciesID"  
    # "DepthMin" "DepthPrefMin" "DepthPrefMax" "DepthMax" "MeanDepth"
    # "Pelagic"
    # "TempMin" "TempPrefMin" "TempPrefMax" "TempMax"
    # "SalinityMin" "SalinityPrefMin" "SalinityPrefMax" "SalinityMax"
  
  # Filtering by layers before for loops
  if(olayer == "surface") {
    hspen_v2 <- hspen %>% filter(DepthPrefMax >= 0 & DepthPrefMax < 200)
  } else if (olayer == "mesopelagic") {
    hspen_v2 <- hspen %>% filter(DepthPrefMax >= 200 & DepthPrefMax < 1000)
  } else if (olayer == "bathypelagic") {
    hspen_v2 <- hspen %>% filter(DepthPrefMax >= 1000 & DepthPrefMax < 4000)
  } else if (olayer == "abyssopelagic") {
    hspen_v2 <- hspen %>% filter(DepthPrefMax >= 4000)
  } else {
    hspen_v2 <- hspen
  }
    speciesID <- hspen_v2$SpeciesID # how many species?
    IDs_df <- vector("list", length(speciesID)) # create  vector to allocate results
  
  # Set up parallel structure
    cores  <-  detectCores()
    ncores <- cores -1 
    cl <- makeCluster(ncores)
    registerDoParallel(cl)
    
  # Loops (~34 minutes with 11 cores)
    IDs_df <- foreach(i = 1:length(speciesID), .packages = c("data.table", "dplyr")) %dopar% { # if you dont add any combine argument it will return a list
      x <- hcaf[hcaf$SpeciesID == speciesID[i],]
      y <- hspen_v2[hspen_v2$SpeciesID == speciesID[i],]
      z <- left_join(x = x, y = y, by = "SpeciesID")
      IDs_df[[i]] <- z
    }
    stopCluster(cl)
    # write list elements (speciesID)
      for(j in 1:length(IDs_df)) {
        name.csv <- paste(speciesID[j], olayer, sep = "_")
        write.csv(IDs_df[[j]], paste(outdir, name.csv, ".csv", sep = ""), row.names = FALSE)
        print(paste0(j, " of ", length(speciesID)))
      }
    return(IDs_df)
}
  
  