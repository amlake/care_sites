#!/bin/bash
#SBATCH --account=vgi
#SBATCH --ntasks=1
#SBATCH --mem=120GB
#SBATCH --time=12:00:00
#SBATCH --mail-user=allison.m.lake@vanderbilt.edu
#SBATCH --mail-type=ALL

cd /data/davis_lab/allie/care_sites/data/makepgs_mdd

module --force purge
ml StdEnv/2020 plink/1.9b_6.21-x86_64

echo "running plink --score with the following parameters..."
echo "ancestry: ${ancestry}"
echo "genotype file path: ${geno}"
echo "output directory: ${outdir}"

score_file=${outdir}/${id}_pst_eff.txt
cat ${outdir}/${id}_pst_eff_a1_b0.5_phiauto_chr* > $score_file

plink --bfile ${geno} --score $score_file 2 4 6 --out ${outdir}/${id}


