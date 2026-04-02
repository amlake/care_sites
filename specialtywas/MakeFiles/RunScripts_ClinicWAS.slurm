#!/bin/bash
#SBATCH --account=vgi
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --mem=300GB
#SBATCH --time=3-00
#SBATCH --mail-user=john.p.shelley@vanderbilt.edu
#SBATCH --mail-type=ALL
#SBATCH --output=runscripts_clinicwas.out

module load StdEnv/2023 r/4.4.0

cd /panfs/accrepfs.vampire/data/davis_lab/allie/care_sites/scripts/specialtywas/MakeFiles

Rscript --no-save MapCareSitestoSpecialties.r
Rscript --no-save MakeCareSiteWASfiles.r
Rscript --no-save MakeSpecialtyWASfiles_PrimaryOnly.r
Rscript --no-save MakeSpecialtyWASfiles_MultiSpecialty.r