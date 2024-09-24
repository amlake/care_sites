#!/bin/bash
#SBATCH --ntasks=1
#SBATCH --mem=100GB
#SBATCH --time=4:00:00
#SBATCH --mail-user=allison.m.lake@vanderbilt.edu
#SBATCH --mail-type=ALL
#SBATCH --array=1-22

ml Anaconda2/4.4.0

prs_cs=/data/davis_lab/allie/programs/PRScs

echo "running PRS-CS with the following parameters..."
echo "ancestry: ${ancestry}"
echo "genotype file path: ${geno}"
echo "summary stats file path: ${sst_path}"
echo "GWAS N: ${N}"
echo "output directory: ${outdir}"

python ${prs_cs}/PRScs.py \
    --ref_dir=${prs_cs}/ld_files/ldblk_1kg_${ancestry} \
    --bim_prefix=$geno \
    --sst_file=$sst_path \
    --n_gwas=$N \
    --out_dir=${outdir}/${id} \
    --chrom=${SLURM_ARRAY_TASK_ID}

