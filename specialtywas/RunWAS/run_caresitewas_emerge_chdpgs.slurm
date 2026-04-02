#!/bin/bash
#SBATCH --account=vgi
#SBATCH --ntasks=1
#SBATCH --mem=200GB
#SBATCH --time=3-00
#SBATCH --mail-user=john.p.shelley@vanderbilt.edu
#SBATCH --mail-type=ALL
#SBATCH --output=caresitewas_chd.out

module load StdEnv/2023 r/4.4.0

Rscript --no-save CareSiteWAS_eMERGE_CHDPGS.R