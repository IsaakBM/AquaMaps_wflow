# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

# proj_type = moll_global <- "+proj=moll +lon_0=0 +datum=WGS84 +units=m +no_defs"
# proj_type = robin_global <- "+proj=robin +lon_0=0 +datum=WGS84 +units=m +no_defs"

# size of hexagons: 
# resolution 0.25 deg == grid_spacing(26860)
# resolution 0.5 deg == grid_spacing(53730)
# resolution 1 deg == grid_spacing(119300)

# size of squares: 
# resolution 0.25 deg == grid_spacing(25000)
# resolution 0.5 deg == grid_spacing(50000)
# resolution 1 deg == grid_spacing(111000)

library(raster)
library(sf)
library(dplyr)
library(magrittr)
library(rnaturalearth)
library(rnaturalearthdata)
library(fasterize)
library(ggplot2)

#########################################################
# Create a land shapefile Pacific centered and projected  
#########################################################
# Using land mask for nature earth package to create a projected sf/shapefile object
  world <- ne_countries(scale = 'small', returnclass = 'sf')
  # Define a long & slim polygon that overlaps the meridian line & set its CRS to match # that of world
  polygon <- st_polygon(x = list(rbind(c(-0.0001, 90),
                                       c(0, 90),
                                       c(0, -90),
                                       c(-0.0001, -90),
                                       c(-0.0001, 90)))) %>%
    st_sfc() %>%
    st_set_crs(4326)

# Modify world dataset to remove overlapping portions with world's polygons
  world2 <- world %>% 
    st_difference(polygon)
  # Perform transformation on modified version of world dataset
    world_robinson <- world2 %>% 
      st_transform(crs = "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    # Check the plot just in case
      # ggplot() +
      #   geom_sf(data = world_robinson) 
    # notice that there is a line in the middle of Antarctica. This is because we have
    # split the map after reprojection. We need to fix this:
  
  # Fix those extra boundaries
    bbox <-  st_bbox(world_robinson)
    bbox[c(1,3)]  <-  c(-1e-5,1e-5)
    polygon2 <- st_as_sfc(bbox)
    crosses <- world_robinson %>%
      st_intersects(polygon2) %>%
      sapply(length) %>%
      as.logical %>%
      which
  # Adding buffer 0
    world_robinson[crosses,] %<>%
      st_buffer(0) 
    # Check the plot again
      # ggplot() +
      #   geom_sf(data = world_robinson) # OK now looks better!
# Save the object
  # st_write(world_robinson, dsn = "files/PacificCenterLand", driver = "ESRI Shapefile")


##########################################################################################
# Create HighSeas shapefile Pacific centered that could be used to create planning units
##########################################################################################
# Land mask to inverted later
  land <- world
# Creating a empty raster at 0.5° resolution (you can increase the resolution to get a better border precision)
  rs <- raster(ncol = 720, nrow = 360) 
  rs[] <- 1:ncell(rs)
  geo.prj <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  crs(rs) <- CRS(geo.prj)
# Fasterize the land object
  land_rs <- fasterize(land, rs)
  land_rs[] <- ifelse(is.na(land_rs[]), 1, NA) # only ocean cells!
  land_rs <- setValues(raster(land_rs), land_rs[])

# Reading EEZ
  eez <- st_read("files/World_EEZ_v11_20191118/eez_v11.shp") %>% 
    filter(SOVEREIGN1 != "Antarctica") # Antarctica HAS EEZs so we must exclude the EEZ region from the shp data
  eez_sp <- as(eez, "Spatial")
# Creating the final raster
  abnj_rs <- mask(land_rs, eez_sp, inverse = TRUE)
# We can plot the object to see if it's correct
  # plot(abnj_rs) 
  # looks OK but there are some land pixels that should not be there 
  # If you convert this to a polygon you would end up with "unwanted" land polygons

# A process to delete certain agrupation of pixels
  abnj_clump <- clump(abnj_rs, directions = 8) 
  # Get frequency table    
    df_clump <- freq(abnj_clump) %>% 
      as.data.frame()
  # which rows of the data.frame are only represented by clumps under 9 pixels?
    str(which(df_clump$count <= 9))
  # which values do these correspond to?
    str(df_clump$value[which(df_clump$count <= 9)])
  # put these into a vector of clump ID's to be removed
    excludeID <- df_clump$value[which(df_clump$count <= 9)]
  # make a new raster to be sieved
    abnj_rs2 <- abnj_clump
  # assign NA to all clumps whose IDs are found in excludeID
    abnj_rs2[abnj_rs2 %in% excludeID] <- NA
    # We can plot the object to see if it's correct
      # plot(abnj_rs2)

# From Raster to Polygon
  abnj_pol <- as(abnj_rs2,  "SpatialPolygonsDataFrame")
    abnj_pol$layer <- seq(1, length(abnj_pol))
  abnj_pol <- spTransform(abnj_pol, CRS(geo.prj))
  # Now to a sf object and create ONE BIG polygon that we can use to populate with PUs
    abnj_pol_sf <- st_as_sf(abnj_pol) %>% 
      select(layer) %>% 
      summarise(total_layer = sum(layer, do_union = TRUE))
    # We can plot the object to see if it' i's correct
      # ggplot() +
      #   geom_sf(data = abnj_pol_sf) # Looks GOOD!


# Transform the High Seas object to a Pacific-centered projected shapefile  
  abnj <- abnj_pol_sf %>% 
    st_difference(polygon)
# Perform transformation
  abnj_robinson <- abnj %>% 
    st_transform(crs = "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  # We can plot the object to see if it is correct
    # ggplot() +
    #   geom_sf(data = abnj_robinson) # Looks weird abd also there is some lines due the Split process

# To fix it the same code as above
  bbox2 <-  st_bbox(abnj_robinson)
  bbox2[c(1,3)]  <-  c(-1e-5,1e-5)
  polygon3 <- st_as_sfc(bbox2)
  crosses2 <- abnj_robinson %>%
    st_intersects(polygon3) %>%
    sapply(length) %>%
    as.logical %>%
    which
  # Adding buffer 0
    abnj_robinson[crosses2, ] %<>%
      st_buffer(0) 
    # We can plot the object to see if it's correct
      # ggplot() +
      #   geom_sf(data = abnj_robinson) # Looks better
# Save the object
  # st_write(abnj_robinson, dsn = "files/PacificCenterABNJ", driver = "ESRI Shapefile")

  
##########################################################################################
# Create equal-size grids (just testing scripts but could be useful for you)
##########################################################################################
pus_global <- st_make_grid(abnj_robinson, square = F, cellsize = c(119300, 119300)) %>%
        st_sf()
# # st_write(pus_global, dsn = "files/PacificCenterABNJGrid_1deg", driver = "ESRI Shapefile")
# ggplot() +
#   geom_sf(data = pus_global) +
#   geom_sf(data = world_robinson, size = 0.05, fill = "grey20") +
#   ggsave("pdfs/PacificCenterABNJGrid_1deg.pdf", width = 20, height = 15, dpi = 300)

# pacific <- locator(4)
# plot(st_geometry(abnj_robinson))
pacific_polygon <- st_crop(abnj_robinson, xmin = -7601017, ymin = -8270780, xmax = 10059099, ymax = 7601017)
pacific_land <- st_crop(world_robinson, xmin = -7601017, ymin = -8270780, xmax = 10059099, ymax = 7601017)
# st_write(pacific_polygon, dsn = "files/PacificABNJ", driver = "ESRI Shapefile")

ggplot() +
  geom_sf(data = pacific_polygon) +
  geom_sf(data = pacific_land, size = 0.05, fill = "grey20") #+
  # ggsave("pdfs/PacificABNJ.jpg", width = 20, height = 15, dpi = 300)

# testing a 0.5 grid hexagon grid
pacific_pus <- st_make_grid(pacific_polygon, square = F, cellsize = c(53730, 53730)) %>% 
  st_sf()
# st_write(pacific_pus, dsn = "files/PacificABNJGrid_05deg", driver = "ESRI Shapefile", append = FALSE )
ggplot() +
  geom_sf(data = pacific_pus) +
  geom_sf(data = pacific_land, size = 0.05, fill = "grey20") +
  ggsave("pdfs/PacificABNJGrid_05deg.jpg", width = 20, height = 15, dpi = 300)

