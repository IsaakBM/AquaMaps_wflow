#!/bin/bash
#PBS -N aquamaps_test01
#PBS -l walltime=0:30:00
#PBS -l select=1:ncpus=1:mem=200GB
#PBS -j oe #  Merge standard output and standard error streams into the named file

# module load R/3.5.0

# cd $PBS_O_WORKDIR

R CMD BATCH "/Users/bri273/Desktop/AquaMaps_wflow/scripts/UsingFxs_PBS.R"