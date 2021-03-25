# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

####################################################################################
####### Running the aqua_start function
####################################################################################
# Use the source argument to call the function into the R environment
source("scripts/00a_AQM-CSV_v2019.R") 
# Run the function
test01 <- aqua_start(path = "AquaMaps/v2019a",
                     outdir = "RDS/",
                     olayer = "surface",
                     prob_threshold = 0.5,
                     sp_env = 1,
                     type = "Pacific",
                     region = "InputFiles/PacificCentred_05deg/PacificCentred_05deg.tif")


####################################################################################
####### Plotting species richness
####################################################################################
library(RColorBrewer)
library(ggplot2)
library(sf)

dt <- readRDS("inp")
final <- dt %>%
  dplyr::mutate(richness_log = log10(richness))
  dplyr::mutate(rich_categ = ifelse(richness_log == 0, 1,
                             ifelse(richness_log > 0 & richness_log <= 1, 2,
                             ifelse(richness_log > 1 & richness_log <= 1.69897, 3,
                             ifelse(richness_log > 1.69897 & richness_log <= 2, 4, 
                             ifelse(richness_log > 2 & richness_log <= 2.69897, 5, 
                             ifelse(richness_log > 2.69897 & richness_log <= 3, 6, 7)))))))

# Defining generalities
pal_rich <- rev(brewer.pal(7, "RdYlBu"))
cv_rich <- c("1", "1 - 10", "10 - 50", "50 - 100", "100 - 500", "500 - 1000", "> 1000")
world_sf <- readRDS("InputFiles/WorldPacificCentred/WorldPacificCentred.rds")
# Plotting the figures
p <- ggplot() +
  geom_sf(data = final, aes(fill = rich_categ), color = NA) +
  geom_sf(data = world_sf, size = 0.05, fill = "grey20") +
  scale_fill_gradientn(name = "Richness",
                       colours = pal_rich,
                       limits = c(1, 7),
                       breaks = seq(1, 7, 1),
                       labels = cv_rich) +
  ggsave("Figs/PacificRichness_01.pdf", width = 20, height = 10, dpi = 300)



