#!/bin/bash
#SBATCH --account=vgi
#SBATCH --ntasks=1
#SBATCH --mem=64GB
#SBATCH --time=4:00:00
#SBATCH --mail-user=allison.m.lake@vanderbilt.edu
#SBATCH --mail-type=ALL
#SBATCH --array=1-22

# first need to set up the python environment
# mkdir -p ~/envs
# cd ~/envs
# python -m venv venv_prscs
# source venv_prscs/bin/activate
# pip install numpy scipy h5py
# python /data/davis_lab/nitinr/ref_scripts/PRScs_May2024/PRScs.py

cd /data/davis_lab/allie/care_sites/data/makepgs_mdd
module --force purge
ml StdEnvACCRE/2023 python/3.13.2
source ~/envs/venv_prscs/bin/activate

prs_cs=/data/davis_lab/nitinr/ref_scripts/PRScs_May2024/

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

deactivate
