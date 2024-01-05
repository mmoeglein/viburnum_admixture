#!/bin/bash
#SBATCH --partition pi_edwards
#SBATCH --ntasks 1
#SBATCH --mem-per-cpu 100G 
#SBATCH --time 12:00:00
#SBATCH --job-name hartout_parent
#SBATCH --output hartout_parent_output.txt

module load miniconda
source activate ipyrad_4_2021

## call ipyrad on your params file
ipyrad -p params-hartout_parent2.txt -s 1234567
