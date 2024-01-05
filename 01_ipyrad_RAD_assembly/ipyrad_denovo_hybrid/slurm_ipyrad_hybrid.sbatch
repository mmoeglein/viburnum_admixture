#!/bin/bash
#SBATCH --partition pi_edwards
#SBATCH --ntasks 15
#SBATCH --mem-per-cpu 10G 
#SBATCH --time 24:00:00
#SBATCH --job-name hybrid
#SBATCH --output hybrid_output.txt

module load miniconda
source activate ipyrad_10_2019

## call ipyrad on your params file
ipyrad -p params-hybrid.txt -s 1234567
