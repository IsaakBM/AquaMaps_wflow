source("/30days/uqibrito/AquaMaps_wflow/scripts/messingAQM.R")

abysso_spp_csv <- aqua_start(path = "/30days/uqibrito/AquaMaps_wflow/AquaMaps", 
                             outdir = "/30days/uqibrito/AquaMaps_wflow/CSVs/", 
                             olayer = "abyssopelagic", prob_threshold = 0.4,
                             data = "species", 
                             region = "/30days/uqibrito/AquaMaps_wflow/ETOPO1_05deg/ETOPO1_ocean.grd")