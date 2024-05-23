#!/bin/bash
#SBATCH --job-name=OCSped.%j
#SBATCH --mail-type=END
#SBATCH --mail-user=deamorimpeixotom@ufl.edu
#SBATCH --account=mresende
#SBATCH --qos=mresende-b
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=5
#SBATCH --mem=20GB
#SBATCH --time=96:00:00
#SBATCH --output=1.tmp/1.mb2.%a.array.%A.out
#SBATCH --error=2.error/2.mb2.%a.array.%A.err
#SBATCH -a 1-20

module purge; module load R

INPUT=$(head -n $SLURM_ARRAY_TASK_ID INPUT.FILE.txt | tail -n 1)

echo $INPUT
Rscript RUNME.R $INPUT ${rep} 
