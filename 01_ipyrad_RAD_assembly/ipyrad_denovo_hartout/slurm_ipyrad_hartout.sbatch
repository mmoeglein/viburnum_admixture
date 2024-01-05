#!/bin/bash
#SBATCH --partition pi_edwards
#SBATCH --ntasks 15
#SBATCH --mem-per-cpu 10G 
#SBATCH --time 1-00:00:00
#SBATCH --job-name hartout
#SBATCH --output hartout_output.txt

module load miniconda
source activate ipyrad_10_2019

## call ipyrad on your params file
ipyrad -p params-hartout.txt -s 1234567
