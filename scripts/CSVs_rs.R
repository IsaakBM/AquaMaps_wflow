rs_trial <- rasterFromXYZ(as.data.frame(IDs_mp[[3]])[, c("CenterLong", "CenterLat", "probability", "TempPrefMax", "SalinityPrefMax")]) # 0.5 deg of resolution
plot(rs_trial$probability)
rs_trial <- raster::aggregate(x = rs_trial, fact = 2)
plot(rs_trial)

rs <- raster(ncol = 360, nrow = 180)
rs[] <- 1:ncell(rs)
up <- resample(rs_trial, rs, resample = "bilinear") # up-scaled to match the 1deg-resolution with climate models using bilenar interpolation
plot(up$probability)
writeRaster(up, "rasters/new_aqm2.grd", overwrite = TRUE)

new_up <- as.data.frame(rasterToPoints(up)) # 1 degree spatial resolution
# plot(boundaries(rs_trial, type = "inner")) # inner boundaries for species distribution edges

wb <- "/Users/bri273/Dropbox/02_/Chapter02/ShapeFiles/WorldBorders/"
wb <- st_read(wb)
wb_sp <- as(wb, "Spatial")

graphics.off()
pdf(file = "figs/Prob_sp3_mesopelagic.pdf", width = 38, height = 20)
plot(up$probability)
plot(wb_sp, add = TRUE, col = "gray54")
dev.off()
# species ID to math with hcaf distributions and extract depth (min max mean) and thermal + pH limits

