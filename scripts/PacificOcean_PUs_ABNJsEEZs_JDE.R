
library(raster)
library(sf)
library(dplyr)
library(magrittr)
library(rnaturalearth)
library(fasterize)
library(ggplot2)
library(igraph)

# Best to define these first so you don't make mistakes below

rob_pacific <- "+proj=robin +lon_0=180 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
longlat <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

ocean_sf <- ne_download(scale = "large", category = "physical", type = "geography_marine_polys", returnclass = "sf") %>% 
  filter(name %in% c("North Pacific Ocean", "South Pacific Ocean", "Philippine Sea", "Coral Sea", "Tasman Sea", "South China Sea", 
                     "Sea of Japan", "Sea of Okhotsk", "Celebes Sea", "Sulu Sea", "Banda Sea", "Luzon Strait", "Java Sea", 
                     "Yellow Sea", "East China Sea", "Arafura Sea", "Timor Sea", "Gulf of Thailand", "Gulf of Carpentaria", 
                     "Bay of Plenty", "Molucca Sea", "Bismarck Sea", "Solomon Sea", "Gulf of Tonkin", "Strait of Singapore", 
                     "Makassar Strait", "Ceram Sea", "Korea Strait", "Inner Sea", "Taiwan Strait", "Shelikhova Gulf", "Bo Hai", 
                     "Great Barrier Reef", "Bering Sea", "Gulf of Alaska", "Kronotskiy Gulf", "Uda Bay", "Uchiura Bay", 
                     "Tsugaru Strait", "Tatar Strait", "La Pérouse Strait", "East Korea Bay", "Qiongzhou Strait", "Cook Strait", 
                     "Torres Strait", "Gulf of Papua", "Hangzhou Bay", "Karaginskiy Gulf", "Gulf of Kamchatka", "Joseph Bonaparte Gulf", 
                     "Gulf of Sakhalin", "Bali Sea", "Davao Gulf", "Halmahera Sea", "Selat Bali", "Gulf of Tomini", "Flores Sea", 
                     "Sibuyan Sea", "Selat Dampier", "Gulf of Buli", "Gulf of Kau", "Bohol Sea", "Surigao Strait", "Ragay Gulf", 
                     "Samar Sea", "Tayabas Bay", "Leyte Gulf", "Visayan Sea", "Savu Sea", "Yangtze River", "Gulf of Anadyr'", 
                     "Golfo de California", "Cook Inlet", "Queen Charlotte Sound", "Bristol Bay", "Dixon Entrance", "Norton Sound", 
                     "Prince William Sound", "Smith Sound", "Queen Charlotte Strait", "Baird Inlet", "Hecate Strait", "Cordova Bay", "Columbia River",
                     "Salish Sea", "Golfo de Panamá", "Golfo Corcovado", "Golfo de Penas", "Golfo de Guayaquil", "Golfo de Tehuantepec",
                     "Dixon Entrance", "Smith Sound", "Queen Charlotte Strait", "Cordova Bay" ))

# "Chukchi Sea""Gulf of Olen‘k""Chaun Bay", "Ozero Mogotoyevo","Guba Gusinaya", "Strait of Malacca"

#########################################################
# Get world map for plotting
#########################################################

# Using land mask for nature earth package to create a projected sf/shapefile object
world <- ne_countries(scale = "large", returnclass = "sf")

# Define a long & slim polygon that overlaps the meridian line & set its CRS to match 
# that of world
polygon <- st_polygon(x = list(rbind(c(-0.0001, 90),
                                     c(0, 90),
                                     c(0, -90),
                                     c(-0.0001, -90),
                                     c(-0.0001, 90)))) %>%
  st_sfc() %>%
  st_set_crs(longlat)

# Modify world dataset to remove overlapping portions with world's polygons
world2 <- world %>% 
  st_difference(polygon)

# Perform transformation on modified version of world dataset
world_robinson <- world2 %>% 
  st_transform(crs = rob_pacific)


# notice that there is a line in the middle of Antarctica. This is because we have
# split the map after reprojection. We need to fix this:
bbox <-  st_bbox(world_robinson)
bbox[c(1,3)]  <-  c(-1e-5, 1e-5)
polygon_rob <- st_as_sfc(bbox)

crosses <- world_robinson %>%
  st_intersects(polygon_rob) %>%
  sapply(length) %>%
  as.logical %>%
  which

# Adding buffer 0
world_robinson[crosses,] %<>%
  st_buffer(0)


# gg <- ggplot(data = ocean_sf, aes(fill = name), show.legend = FALSE) + 
#   geom_sf() + 
#   geom_sf(data = world, color = "grey20", fill="grey20", size=0.1, show.legend="line")
# 
# x11(width = 18, height = 5)
# gg
# ggsave("Pacific+MarginalSeas.pdf")
# ggsave("Pacific+MarginalSeas.png", dpi = 400)



# Creating a empty raster
# rs <- raster(ncol = 360*10, nrow = 180*10) 
rs <- raster(ncol = 360*2, nrow = 180*2) 
rs[] <- 1:ncell(rs)
crs(rs) <- CRS(longlat)

# Fasterize the land object
ocean_rs <- fasterize(ocean_sf, rs)
writeRaster(ocean_rs, "InputFiles/PacificCentred_05deg/PacificCentred_05deg.tif")

pacific_pol <- as(ocean_rs,  "SpatialPolygonsDataFrame")
pacific_pol$layer <- seq(1, length(pacific_pol))

# Now to a sf object and create ONE BIG polygon that we can use to populate with PUs
pacific_pol_sf <- st_as_sf(pacific_pol) %>% 
  select(layer) %>% 
  summarise(total_layer = sum(layer, do_union = TRUE))

# We can plot the object to see if it is correct
ggplot() +
  geom_sf(data = pacific_pol_sf) # Looks GOOD!

# Transform the High Seas object to a Pacific-centered projected shapefile  
pacific_robinson <- pacific_pol_sf %>% 
  st_difference(polygon) %>% 
  st_transform(crs = rob_pacific) # Perform transformation

# We can plot the object to see if it is correct
# ggplot() +
  # geom_sf(data = pacific_robinson) 

# Looks weird abd also there is some lines due the Split process

# To fix it the same code as above
bbox2 <-  st_bbox(pacific_robinson)
bbox2[c(1,3)]  <-  c(-1e-5,1e-5)
polygon3 <- st_as_sfc(bbox2)
crosses2 <- pacific_robinson %>%
  st_intersects(polygon3) %>%
  sapply(length) %>%
  as.logical %>%
  which

# Adding buffer 0
pacific_robinson[crosses2, ] %<>%
  st_buffer(0)

# We can plot the object to see if it is correct
# ggplot() +
  # geom_sf(data = pacific_robinson) # Looks better


########################################################################################
# Create equal-size grids (adapted from Jase's Code)
########################################################################################

#load Create_Planning Units function from Jase 
#(modified by Tin; modifications found within the function itself)

fCreate_PlanningUnits <- function(Bndry, LandMass, CellArea, Shape){
  
  if(Shape %in% c("hexagon", "Hexagon")){
    sq <- FALSE
    diameter <- 2 * sqrt((CellArea*1e6)/((3*sqrt(3)/2))) * sqrt(3)/2 # Diameter in m's
  }
  
  if(Shape %in% c("square", "Square")){
    sq < TRUE
    diameter <- sqrt(CellArea*1e6) # Diameter in m's
  }
  
  # First create planning units for the whole region
  PUs <- st_make_grid(Bndry,
                      square = sq,
                      cellsize = c(diameter, diameter),
                      what = "polygons") %>%
    st_sf()
  
  # Check cell size worked ok.
  print(paste0("Range of cellsize are ",
               round(as.numeric(range(units::set_units(st_area(PUs), "km^2")))[1])," km2 to ",
               round(as.numeric(range(units::set_units(st_area(PUs), "km^2")))[2])," km2")) # Check area  
  
  # First get all the PUs partially/wholly within the planning region
  logi_Reg <- st_centroid(PUs) %>%
    st_intersects(Bndry) %>%
    lengths > 0 # Get logical vector instead of sparse geometry binary
  PUs <- PUs[logi_Reg == TRUE, ]
  
  # Second, get all the pu's with < 50 % area on land (approximated from the centroid)
  logi_Ocean <- st_centroid(PUs) %>%
    st_intersects(LandMass) %>%
    lengths > 0 # Get logical vector instead of sparse geometry binary
  PUs <- PUs[logi_Ocean==FALSE, ] #modified from ==FALSE to TRUE because LandMass = ABNJ areas
  
  return(PUs)
}


#size of hexagons in km^2
#approximately 0.1 deg = 11.1km
#get the approximate area using the apothem (r) in https://www.omnicalculator.com/math/hexagon
#0.25 deg resolution == 669.9 km^2
#0.50 deg resolution == 2667.6 km^2
#0.10 deg resolution == 10670.0 km^2

CellArea <- 2667.6 # kms2 for 0.5 degree resolution
# CellArea <- 669.9 # kms2 for 0.25 degree resolution
Shape = "Hexagon" # Hexagon or Square

PUsPac <- fCreate_PlanningUnits(pacific_robinson, world_robinson, CellArea, Shape)
saveRDS(PUsPac, "PU_Output.rds")


x11(width = 18, height = 12)

ggplot() +
  # geom_sf(data = LandMass, colour = NA, fill = NA, size = 0.2, show.legend = "line") +
  geom_sf(data = world_robinson, color = "grey20", fill="grey20", size=0.1, show.legend="line") +
  geom_sf(data = PUsPac, colour = "darkgreen", fill = "lightgreen", alpha = 0.5, size = 0.1, show.legend = "line") +
  geom_sf(data = pacific_robinson, colour = "red", fill = NA, size = 0.01) +
  coord_sf(xlim = c(st_bbox(pacific_robinson)$xmin, st_bbox(pacific_robinson)$xmax), # Set limits based on Bndry bbox
           ylim = c(st_bbox(pacific_robinson)$ymin, st_bbox(pacific_robinson)$ymax),
           expand = TRUE)

ggsave("PU_Output.pdf")
ggsave("PU_Output.png", dpi = 400)


