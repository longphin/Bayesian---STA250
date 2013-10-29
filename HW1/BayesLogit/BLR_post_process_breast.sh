#!/bin/bash -l

module load R/3.0.0

#SBATCH --job-name=blr_mybreast
#SBATCH --output=dump/blr_mybreast.out
#SBATCH --error=dump/blr_mybreast.err

srun R --no-save --vanilla < BLR_breast.R



