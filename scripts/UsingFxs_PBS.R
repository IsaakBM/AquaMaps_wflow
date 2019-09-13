source("/Users/bri273/Desktop/AquaMaps_wflow/scripts/messingAQM.R")

abysso_spp_csv <- aqua_start(path = "/Users/bri273/Desktop/AquaMaps_wflow/AquaMaps", 
                             outdir = "/Users/bri273/Desktop/AquaMaps_wflow/CSVs/", 
                             olayer = "abyssopelagic", prob_threshold = 0.4,
                             data = "species", 
                             region = "/Users/bri273/Desktop/AquaMaps_wflow/ETOPO1_05deg/ETOPO1_ocean.grd")