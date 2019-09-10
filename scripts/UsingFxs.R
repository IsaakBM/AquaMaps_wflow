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



abysso_spp_csv <- aqua_start(path = "AquaMaps", outdir = "CSVs/", olayer = "abyssopelagic", prob_threshold = 0.4, 
                             data = "species", region = "ETOPO1_05deg/ETOPO1_ocean.grd")
abysso_spp_rs <- aqua_rs(path = "CSVs/04abyssopelagic", outdir = "rasters/04abyssopelagic/", 
                         bathymetry_shp = "shapefiles/ETOPO1_05deg/ETOPO1_ocean.grd", olayer = "abyssopelagic")


etopo <- raster("ETOPO1_05deg/ETOPO1_ocean.grd")
plot(rs)
rs2 <- drawExtent()
?raster::subset
raster::extent(rs, rs2)

rs3 <- crop(rs, rs2)
plot(rs3)
rs4 <- drawExtent()
rs3 <- crop(rs, rs4)
writeRaster(rs3, "rasters/etopos_mediterranean.grd")
mediterranean <- raster("rasters/etopos_mediterranean.grd")
plot(mediterranean)

surface_sppMed_csv <- aqua_start(path = "AquaMaps", outdir = "CSVs/", olayer = "surface", prob_threshold = 0.4, 
                             data = "richness", region = "rasters/etopos_mediterranean.grd")

surface_sppMed_rs <- rasterFromXYZ(as.data.frame(surface_sppMed_csv)[, c("CenterLong", "CenterLat", "richness")])
plot(surface_sppMed_rs)

etopo <- raster("ETOPO1_05deg/ETOPO1_ocean.grd")
a <- mask(surface_sppMed_rs, resample(etopo, surface_sppMed_rs, resample = "bilinear"))
plot(a)
b <- resample(surface_sppMed_rs, etopo, resample = "bilinear")
plot(b)

wb <- st_read("shapefiles/WorldBorders/TM_WORLD_BORDERS-0.3.shp")
wb_sp <- as(wb, "Spatial")

pdf("pdfs/richness_surface_sspMed.pdf", width = 38, height = 20)
plot(b)
plot(wb_sp, add = TRUE, col = "gray54")
dev.off()


