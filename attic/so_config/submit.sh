#!/bin/bash

sbatch <<EOT
#!/bin/sh
#SBATCH --time=7-00:00:0
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=32
#SBATCH --mem-per-cpu=2000
#SBATCH -A mallet
#SBATCH --exclude=tknl[01-12]

module load libxml2/2.9.4
module load gcc/7.3.0
module load libaio/0.3.110
module load r/4.0.5-py27
module load python/3.8.7
module load swig/3.0.12

srun ./run.R -r $1
EOT
