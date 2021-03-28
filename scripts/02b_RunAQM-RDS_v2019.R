# This code was written by Isaac Brito-Morales (i.britomorales@uq.edu.au)
# Please do not distribute this code without permission.
# NO GUARANTEES THAT CODE IS CORRECT
# Caveat Emptor!

####################################################################################
####### Running 
####################################################################################
# Use the source argument to call the function into the R environment
source("scripts/02a_AQM-RDS_v2019.R") 
marxan_inputs(path = "RDS",
              outdir = "OutputFiles/",
              shapefile = "InputFiles/PUsPacific/PacificABNJGrid_05deg.rds",
              olayer = "surface")