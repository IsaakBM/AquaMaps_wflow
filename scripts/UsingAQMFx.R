source("scripts/messingAQM.R")
trial <- aqua_start(path = "AquaMaps/", outdir = "CSVs/", olayer = "abyssopelagic", prob_threshold = 0.4, data = "species")
trial2 <- aqua_rs(path = "CSVs", bathymetry_shp = "shapefiles/ETOPO1/ETOPO1_ocean.grd", olayer = "abyssopelagic")


sp_richness <- do.call(rbind, trial)
sp_richness <- sp_richness %>% group_by(CenterLat, CenterLong) %>% summarise(richness = n()) %>% data.frame()
head(sp_richness)
dim(sp_richness)

single <- read.csv("CSVs/Fis-139258_abyssopelagic.csv")
rs_trial <- rasterFromXYZ(as.data.frame(single)[, c("CenterLong", "CenterLat", "probability")]) # 0.5 deg of resolution
namers <- raster(ncol = 720, nrow = 360) #720:360...0.5 degree res
rs[] <- 1:ncell(n)
trial <- resample(rs_trial, rs, resample = "bilinear")
plot(rs_trial)
plot(trial)
# plot(rs_trial$richness)
# rs_trial <- raster::aggregate(x = rs, fact = 2)
# plot(rs_trial)
files_csv <- list.files(path = "CSVs", pattern = ".csv", full.names = TRUE) 
paste(read.csv(files_csv[1])[1,1])


wb <- "/Users/bri273/Dropbox/02_/Chapter02/ShapeFiles/WorldBorders/"
wb <- st_read(wb)
wb_sp <- as(wb, "Spatial")

graphics.off()
pdf(file = "figs/abyssopelagic_06.pdf", width = 38, height = 20)
plot(trial2[[6]]$probability)
plot(wb_sp, add = TRUE, col = "gray54")
dev.off()

trial
for(j in 1:length(trial)) {
  name.csv <- paste(trial[[j]][1,1], sep = "_")
  write.csv(trial[[j]], paste("CSVs/", name.csv,".csv", sep = ""), row.names = FALSE)
  print(paste0(j, " of ", length(trial)))
  # return(IDs_df)
}


trial2
outdir = "rasters"
  for(k in 1:length(trial2)) {
    name.rs <- paste(read.csv(files_csv[k])[1,1], "abyssopelagic", sep = "_")
    writeRaster(trial2[[k]], paste(outdir, name.rs, ".grd", sep = ""), overwrite = TRUE)
    print(paste0(j, " of ", length(trial2)))
    }
name.rs <- paste(read.csv(files_csv[1])[1,1], "abyssopelagic", sep = "_")
writeRaster(trial2[[1]], paste(outdir, name.rs, ".grd", sep = ""), overwrite = TRUE)
writeRaster(readAll(trial2[[1]]), "rasters/trial.grd")



# plotting each file as a PDF
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





