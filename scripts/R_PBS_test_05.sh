#!/bin/bash
#PBS -A qris-uq
#PBS -l walltime=2:00:00
#PBS -l select=6:ncpus=20:mem=500GB
        #+6:ncpus=4:mem=80GB
        #select=4:ncpus=12:mpiprocs=12:mem=100GB
        #select=5:ncpus=5:mem=100GB

cd $PBS_O_WORKDIR

module load parallel
module load R/3.5.0

R CMD BATCH "/30days/uqibrito/AquaMaps_wflow/scripts/CSVs_rs_HPC.R"