#!/bin/bash
#SBATCH --nodes=1
#SBATCH --ntasks=30
#SBATCH --mem=248gb
#SBATCH --time=172:00:00
#SBATCH --partition=std
#SBATCH --account=einhofja

module load R/4.4.0-gcc14.1.0

## copy job data
cp -r server_analysis.R /tmp
cp -r data_final_de.RData /tmp

## change to working director
cd /tmp

## do the work
Rscript server_analysis.R

## copy the results if they are required
cp -r /tmp/* /...