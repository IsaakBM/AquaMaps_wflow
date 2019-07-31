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
# 1. species information (e.g. species ID, taxonomy, "reviewed" or not, but without spatial information)
# 2. information for each half-degree cell (e.g. max/min depth, salinity, temperature, etc)
# 3. a lookup table of which species are found in each half-degree cell (and the "probability of occurrence" of that species in that cell)  


aqua_start <- function(path, outdir, olayer, probability, ...) { # filter by depth? perhaps 4 layers? 
  
  library(data.table)
  library(dplyr)
  library(foreach)
  library(doParallel)
  
  # folders <- dir("../AquaMaps_wflow/")
  # Reading input files
  hcaf <- fread("hcaf_species_native_richness_gte10.csv") %>% 
    dplyr::select(SpeciesID, CenterLat, CenterLong, probability) %>% 
    dplyr::filter(probability >= 0.4) # or == probability
    # [1] "SpeciesID"   "CsquareCode"
    # [3] "probability" "CenterLat"  
    # [5] "CenterLong"  "LOICZID" 
  hspen <- fread("hspen_richness_all_gte10_240616.csv") %>% 
    dplyr::select(c(2:16)) # SpeciesID + DepthEnvelope + tempEnvelop + SalinityEnvelope
    # "SpeciesID"  
    # "DepthMin" "DepthPrefMin" "DepthPrefMax" "DepthMax" "MeanDepth"
    # "Pelagic"
    # "TempMin" "TempPrefMin" "TempPrefMax" "TempMax"
    # "SalinityMin" "SalinityPrefMin" "SalinityPrefMax" "SalinityMax"
  
  # Set up parallel structure
  cores  <-  detectCores()
  ncores <- cores -1 
  cl <- makeCluster(ncores)
  registerDoParallel(cl)
  
  # Define ocean layers and perform the analysis
  if (olayer == "surface") {
    hspen_sf <- hspen %>% filter(DepthPrefMax >= 0 & DepthPrefMax < 200) # DepthMean? ~10k species
      speciesID <- hspen_sf$SpeciesID # 10106 species
    IDs_sf <- vector("list", length(speciesID)) # create  vector to allocate results
    # Loops (~34 minutes with 11 cores)
      IDs_sf <- foreach(i = 1:length(speciesID), .packages = c("data.table", "dplyr")) %dopar% { # if you dont add any combine argument it will return a list
        x <- hcaf[hcaf$SpeciesID == speciesID[i],]
        y <- hspen_sf[hspen_sf$SpeciesID == speciesID[i],]
        z <- left_join(x = x, y = y, by = "SpeciesID")
        IDs_sf[[i]] <- z
      }
      # write list elements (speciesID)
        name.csv <- paste(speciesID[i], "01_surface", sep = "_")
        write.csv(IDs_sf[[i]], paste(outdir, name.csv, ".csv", sep = ""), row.names = FALSE)
        print(paste0(i, " of ", length(speciesID)))
    
  } else if (olayer == "mesopelagic") {
    hspen_mp <- hspen %>% filter(DepthPrefMax >= 200 & DepthPrefMax < 1000) # DepthMean?
      speciesID <- hspen_mp$SpeciesID # 3130
    IDs_mp <- vector("list", length(speciesID))
    # Loops
      IDs_mp <- foreach(i = 1:length(speciesID), .packages = c("data.table", "dplyr")) %dopar% { # if you dont add any combine argument it will return a list
        x <- hcaf[hcaf$SpeciesID == speciesID[i],]
        y <- hspen_mp[hspen_mp$SpeciesID == speciesID[i],]
        z <- left_join(x = x, y = y, by = "SpeciesID")
        IDs_mp[[i]] <- z
      }
      # write list elements (speciesID)
        name.csv <- paste(speciesID[i], "02_mesopelagic", sep = "_")
        write.csv(IDs_mp[[i]], paste(outdir, name.csv, ".csv", sep = ""), row.names = FALSE)
        print(paste0(i, " of ", length(speciesID)))
    
  } else if (olayer == "bathypelagic") {
    hspen_bp <- hspen %>% filter(DepthPrefMax >= 1000 & DepthPrefMax < 4000) # DepthMean?
      speciesID <- hspen_bp$SpeciesID # 890 species
    IDs_bp <- vector("list", length(speciesID))
    # Loops
      IDs_bp <- foreach(i = 1:length(speciesID), .packages = c("data.table", "dplyr")) %dopar% { # if you dont add any combine argument it will return a list
        x <- hcaf[hcaf$SpeciesID == speciesID[i],]
        y <- hspen_bp[hspen_bp$SpeciesID == speciesID[i],]
        z <- left_join(x = x, y = y, by = "SpeciesID")
        IDs_bp[[i]] <- z
      }
      # write list elements (speciesID)
        name.csv <- paste(speciesID[i], "03_bathypelagic", sep = "_")
        write.csv(IDs_bp[[i]], paste(outdir, name.csv, ".csv", sep = ""), row.names = FALSE)
        print(paste0(i, " of ", length(speciesID)))
    
  } else if (olayer == "abyssopelagic") {
    hspen_abp <- hspen %>% filter(DepthPrefMax >= 4000) # DepthMean?
      speciesID <- hspen_abp$SpeciesID # 47 species
    IDs_abp <- vector("list", length(speciesID))
    # Loops
      IDs_abp <- foreach(i = 1:length(speciesID), .packages = c("data.table", "dplyr")) %dopar% { # if you dont add any combine argument it will return a list
        x <- hcaf[hcaf$SpeciesID == speciesID[i],]
        y <- hspen_abp[hspen_abp$SpeciesID == speciesID[i],]
        z <- left_join(x = x, y = y, by = "SpeciesID")
        IDs_abp[[i]] <- z
      }
      # write list elements (speciesID)
        name.csv <- paste(speciesID[i], "04_abyssopelagic", sep = "_")
        write.csv(IDs_abp[[i]], paste(outdir, name.csv, ".csv", sep = ""), row.names = FALSE)
        print(paste0(i, " of ", length(speciesID)))
    
  } else {
    hspen_all <- hspen
      speciesID <- hspen_bp$SpeciesID
    IDs_all <- vector("list", length(speciesID))
    # Loops
      IDs_all <- foreach(i = 1:length(speciesID), .packages = c("data.table", "dplyr")) %dopar% { # if you dont add any combine argument it will return a list
        x <- hcaf[hcaf$SpeciesID == speciesID[i],]
        y <- hspen_all[hspen_all$SpeciesID == speciesID[i],]
        z <- left_join(x = x, y = y, by = "SpeciesID")
        IDs_all[[i]] <- z
      }
      # write list elements (speciesID)
        name.csv <- paste(speciesID[i], "05_allSpecies", sep = "_")
        write.csv(IDs_all[[i]], paste(outdir, name.csv, ".csv", sep = ""), row.names = FALSE)
        print(paste0(i, " of ", length(speciesID)))
  }
  stopCluster(cl)
  
  ifelse(olayer == "surface", return(IDs_sf), 
         ifelse(olayer == "mesopelagic", return(IDs_mp), 
                ifelse(olayer == "bathypelagic", return(IDs_bp), 
                       ifelse(olayer == "abyssopelagic", return(IDs_abp), return(IDs_all)))))
  
}
  
  