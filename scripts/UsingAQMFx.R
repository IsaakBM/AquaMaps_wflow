source("scripts/messingAQM.R")
abysso_spp_csv <- aqua_start(path = "AquaMaps/", outdir = "CSVs/", olayer = "abyssopelagic", prob_threshold = 0.4, data = "species")
abysso_spp_rs <- aqua_rs(path = "CSVs", bathymetry_shp = "shapefiles/ETOPO1/ETOPO1_ocean.grd", olayer = "abyssopelagic")


# plotting each file as a PDF
  wb <- "/Users/bri273/Dropbox/02_/Chapter02/ShapeFiles/WorldBorders/"
  wb <- st_read(wb)
  wb_sp <- as(wb, "Spatial")
  for(i in 1:length(trial2)) {
    if(length(trial2[[i]]) != 0) {
      graphics.off()
      name.pdf <- paste(read.csv(files_csv[i])[1,1], "abyssopelagic.pdf", sep = "_")
      pdf(file = paste("figs/", name.pdf, sep = ""), width = 38, height = 20)
      plot(trial2[[i]]$probability)
      plot(wb_sp, add = TRUE, col = "gray54")
      dev.off()
    }
  }


# creating planning units and add area of each cell (remember to estimate the area weight because tropical have more area than polar)
rs <- raster(ncol = 720, nrow = 360)
rs[] <- 1:ncell(rs)
rs2 <- rasterToPolygons(rs) # works perfect! we have to take the world border terrestrial shapefile
aw <- raster::area(rs)

# spatstat::weighted.median(ens_hist2[[7]]$voccMag[], aw[], na.rm = TRUE)

rgdal::writeOGR(rs2, "shapefiles/", "05deg_PU", "ESRI Shapefile")

graphics.off()
pdf(file = "figs/planning_units_05deg.pdf", width = 38, height = 20)
plot(rs2)
# plot(wb_sp, add = TRUE, col = "gray54")
dev.off()





