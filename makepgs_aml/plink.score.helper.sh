#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --mem=240GB
#SBATCH --time=12:00:00
#SBATCH --mail-user=allison.m.lake@vanderbilt.edu
#SBATCH --mail-type=ALL

ml PLINK/1.9b_5.2

echo "running plink --score with the following parameters..."
echo "ancestry: ${ancestry}"
echo "genotype file path: ${geno}"
echo "output directory: ${outdir}"

if [ $csx == 0 ]
then
    echo -e "scores generated from PRS-CS\n\n"
    score_file=${outdir}/${id}_pst_eff.txt
    cat ${outdir}/${id}_pst_eff_a1_b0.5_phiauto_chr* > $score_file
else
    echo -e "scores generated from PRS-CSx\n\n"
    score_file=${outdir}/${id}_META_pst_eff.txt
    cat ${outdir}/${id}_META_pst_eff_a1_b0.5_phiauto_chr* > $score_file
fi

plink --bfile ${geno} --score $score_file 2 4 6 --out ${outdir}/${id}


