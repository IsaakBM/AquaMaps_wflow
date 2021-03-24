path = "/Users/bri273/Desktop/AquaMaps_wflow/AquaMaps/v2019a"
outdir = "/Users/bri273/Desktop/AquaMaps_wflow/"
olayer = "bathyabyssopelagic"
prob_threshold = 0.5
sp_env = 1
region = "/Users/bri273/Desktop/AquaMaps_wflow/ETOPO1_05deg/etopos_mediterranean.grd"
geo.prj = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

dir <- path
first_csv <- list.files(path = dir, pattern = "*hcaf.*.csv$", full.names = TRUE)
second_csv <- list.files(path = dir, pattern = "*hspen.*.csv$", full.names = TRUE)
third_csv <- list.files(path = dir, pattern = "*occursum.*.csv$", full.names = TRUE)

rs <- raster(ncol = 720, nrow = 360)
rs[] <- 1:ncell(rs)


if(stringr::str_detect(string = region, pattern = ".grd") == TRUE) {
  region <- readAll(raster(region))
  region <- resample(region, rs, resample = "ngb")
  } else if (stringr::str_detect(string = region, pattern = ".rds") == TRUE) {
    region <- readRDS(region)
    region <- resample(region, rs, resample = "ngb")
  } else if (stringr::str_detect(string = region, pattern = ".tif") == TRUE) {
    region <- readAll(raster(region))
    region <- resample(region, rs, resample = "ngb")
    }

hcaf <- fread(first_csv) %>% 
  dplyr::select(SpeciesID, CenterLat, CenterLong, Probability) %>% 
  dplyr::filter(Probability >= prob_threshold)

hspen <- fread(second_csv) %>% 
  dplyr::select(tidyselect::matches("Species|Pelagic|Depth|Oxy|Temp|Salinity|Rank")) %>% 
  dplyr::filter(Rank == sp_env)

speciesInfo <- fread(third_csv, fill = TRUE)

# Filtering by layers before loops
if(olayer == "surface") {
  hspen_v2 <- hspen %>% filter(DepthPrefMin <= 200 | DepthPrefMax <= 200)
} else if (olayer == "mesopelagic") {
  hspen_v2 <- hspen %>% filter(DepthPrefMin > 200 & DepthPrefMin <= 1000 | DepthPrefMax > 200 & DepthPrefMax <= 1000)
} else if (olayer == "bathyabyssopelagic") {
  hspen_v2 <- hspen %>% filter(DepthPrefMin > 1000 | DepthPrefMax > 1000)
} else if (olayer == "all") {
  hspen_v2 <- hspen %>% filter(DepthPrefMin >= 0 | DepthPrefMax >= 0)
} else {
  hspen_v2 <- hspen
}
speciesID <- hspen_v2$SpeciesID # how many species?
IDs_df <- vector("list", length(speciesID))


rs <- raster(ncol = 720, nrow = 360)
rs[] <- 1:ncell(rs)




    x <- hcaf[hcaf$SpeciesID == speciesID[1],]
    y <- hspen_v2[hspen_v2$SpeciesID == speciesID[1],]
    z <- left_join(x = x, y = y, by = "SpeciesID")
  # Consider only species that has more than 10 cells in the study area
    if(nrow(z) >= 10 & mean(z$CenterLat) != (z$CenterLat[1])) {
      # Convert a raster from the data drame species
        rs1 <- rasterFromXYZ(as.data.frame(z) 
                             [, c("CenterLong", "CenterLat", 
                                  "Probability", 
                                  "TempPrefMin","TempPrefMax", 
                                  "SalinityPrefMin","SalinityPrefMax", 
                                  "OxyPrefMin", "OxyPrefMax")])
        rs_final <- resample(rs1, rs, resample = "ngb") # projecting raster 0.5 deg just in case
      # From Raster to Shapefile
        rs_final <- subset(rs_final, 1) # here I'm just getting the first layer (Probability) to avoid creating a big object per file.
        if(is.na(rs_final@crs) == TRUE) {crs(rs_final) <- CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")} else {rs_final <- rs_final}
        rs_final <- mask(rs_final, region) # mask the species distribution with the region of interest
      # Transform AquaMaps species raster into an sf spatial polygon dataframe
        sd_rs1 <- as(rs_final, "SpatialPolygonsDataFrame")
        sd_rs1 <- spTransform(sd_rs1, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) # just in case!
        sd_rs1$Probability <- seq(1, length(sd_rs1))
      # 
        if(type == "Pacific") {
          # Define a long & slim polygon that overlaps the meridian line & set its CRS to match that of world
            polygon <- st_polygon(x = list(rbind(c(-0.0001, 90),
                                                 c(0, 90),
                                                 c(0, -90),
                                                 c(-0.0001, -90),
                                                 c(-0.0001, 90)))) %>%
              st_sfc() %>%
              st_set_crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
          # Transform the species distribution polygon object to a Pacific-centred projection polygon object
            sd_rs1_robinson <- sd_rs1 %>% 
              st_as_sf() %>% 
              st_difference(polygon) %>% 
              st_transform(crs = "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
          # There is a line in the middle of Antarctica. This is because we have split the map after reprojection. We need to fix this:
            bbox1 <-  st_bbox(sd_rs1_robinson)
            bbox1[c(1,3)]  <-  c(-1e-5,1e-5)
            polygon1 <- st_as_sfc(bbox1)
            crosses1 <- sd_rs1_robinson %>%
              st_intersects(polygon1) %>%
              sapply(length) %>%
              as.logical %>%
              which
            # Adding buffer 0
              sd_rs1_robinson[crosses1, ] %<>%
                st_buffer(0)
          # Writing thr object 
            name_sps <- paste(z[1,1], olayer, sep = "_") 
            saveRDS(sd_rs1_robinson, paste(outdir, name_sps, ".rds", sep = ""))
            IDs_df[[i]] <- rasterToPoints(rs_final) %>% 
              as.data.frame()
        } else {
          # Transform the species distribution polygon object to a Pacific-centred projection polygon object
            sd_rs1_robinson <- sd_rs1 %>% 
              st_as_sf() %>% 
              st_transform(crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
          # Writing thr object 
            name_sps <- paste(z[1,1], olayer, sep = "_") 
            saveRDS(sd_rs1_robinson, paste(outdir, name_sps, ".rds", sep = ""))
            IDs_df[[i]] <- rasterToPoints(rs_final) %>% 
              as.data.frame()
        }
      
      
    }

    
    
    
      
    # rs <- raster(ncol = 3, nrow = 1)
    # rs[] <- 1:ncell(NA)
    # # plot(rs)
    # # !any(is.na(values(rs)))
    # sd_rs1 <- as(rs, "SpatialPolygonsDataFrame")
    # sd_rs1 <- spTransform(sd_rs1, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) # just in case! # here there is an error... empty object needs to be
    # 
    # rs[!sapply(rs, is.null)]
    
    

ggplot() +
  geom_sf(data = richness_robinson) +
  geom_sf(data = world_robinson, size = 0.05, fill = "grey20") +
  ggsave("AquaMaps/test03.jpg", width = 20, height = 15, dpi = 300)
    
    if(is.nan(mean(rs_final[], na.rm = TRUE)) == FALSE) {print("all good")} 
    
    