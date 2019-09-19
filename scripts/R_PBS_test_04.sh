#!/bin/bash
#PBS -A qris-uq
#PBS -l walltime=1:00:00
#PBS -l select=6:ncpus=4:mem=80GB

cd $PBS_O_WORKDIR

module load parallel
module load R/3.5.0

R CMD BATCH "/30days/uqibrito/AquaMaps_wflow/scripts/messingAQM.R"