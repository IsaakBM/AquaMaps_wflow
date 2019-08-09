source("scripts/messingAQM.R")
source("scripts/CSVs_rs.R")
# surface_spp_csv <- aqua_start(path = "AquaMaps/", outdir = "CSVs/", olayer = "surface", prob_threshold = 0.4, data = "species")
surface_spp_rs1 <- aqua_rs(path = "CSVs/01surface_a", outdir = "rasters/01surface/", 
                           bathymetry_shp = "shapefiles/ETOPO1_05deg/ETOPO1_ocean.grd", olayer = "surface")

meso_spp_csv <- aqua_start(path = "AquaMaps/", outdir = "CSVs/02mesopelagic", olayer = "mesopelagic", prob_threshold = 0.4, 
                           data = "species")
meso_spp_rs <- aqua_rs(path = "CSVs", outdir = "rasters/02mesopelagic/", 
                       bathymetry_shp = "shapefiles/ETOPO1_05deg/ETOPO1_ocean.grd", olayer = "mesopelagic")


bathy_spp_csv <- aqua_start(path = "AquaMaps/", outdir = "CSVs/03bathypelagic/", olayer = "bathypelagic", prob_threshold = 0.4, 
                            data = "species")
bathy_spp_rs <- aqua_rs(path = "CSVs/03bathypelagic", outdir = "rasters/03bathypelagic/", 
                        bathymetry_shp = "shapefiles/ETOPO1_05deg/ETOPO1_ocean.grd", olayer = "bathypelagic")


abysso_spp_csv <- aqua_start(path = "AquaMaps/", outdir = "CSVs/04abyssopelagic/", olayer = "abyssopelagic", prob_threshold = 0.4, 
                             data = "species")
abysso_spp_rs <- aqua_rs(path = "CSVs/04abyssopelagic", outdir = "rasters/04abyssopelagic/", 
                         bathymetry_shp = "shapefiles/ETOPO1_05deg/ETOPO1_ocean.grd", olayer = "abyssopelagic")
