cd /panfs/accrepfs.vampire/data/davis_lab/allie/care_sites/scripts/specialtywas/RunWAS

#sbatch -J CARESITE_AF run_caresitewas_emerge_afpgs.slurm
#sbatch -J CARESITE_ASTHMA run_caresitewas_emerge_asthmapgs.slurm
#sbatch -J CARESITE_ASTHMA_ASTHMASUBSET run_caresitewas_emerge_asthmapgs_subset.slurm
#sbatch -J CARESITE_BMI run_caresitewas_emerge_bmipgs.slurm
sbatch -J CARESITE_CHD run_caresitewas_emerge_chdpgs.slurm
#sbatch -J CARESITE_CKD run_caresitewas_emerge_ckdpgs.slurm
#sbatch -J CARESITE_PAU run_caresitewas_pau_highrisk.slurm
#sbatch -J CARESITE_PAU_CONT run_caresitewas_pau_continuous.slurm

#sbatch -J CARESITE_MANHATTAN run_manhattan_emerge.slurm