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