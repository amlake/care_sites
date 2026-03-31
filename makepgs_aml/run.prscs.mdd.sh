## Remake depression polygenic scores using updated version of PRS-CS
## For consistency with John's PRS for the care sites project
## Summary stats filtering script: https://bitbucket.org/davislabteam/generate_psych_pgs/src/master/filter_sum_stats_mdd_eur_afr.R
## Previous PRS generation script: https://bitbucket.org/davislabteam/generate_psych_pgs/src/master/run.prscs.mdd.sh

#!/bin/bash
cd /data/davis_lab/allie/care_sites/data/makepgs_mdd

## set parameters
script_dir=/data/davis_lab/allie/care_sites/scripts/makepgs_aml
script1=${script_dir}/prs.cs.helper_new.sh
script2=${script_dir}/plink.score.helper_new.sh
geno_eur=/data/davis_lab/allie/commonly_used/mega_maf_filter/plink_files/20200518_biallelic_mega_recalled.chr1-22.grid.EU.filt1.r2corrected.IBD.filtered.PI_HAT.0.2_maf0.005
sst_eur=/data/davis_lab/allie/generate_psych_pgs/sst/daner_MDDwoBP_20201001_2015iR15iex_HRC_MDDwoBP_iPSYCH2015i_UKBtransformed_Wray_FinnGen_MVPaf_2_HRC_MAF01.IMPINFO_0.771.prscs_fmt.030724.tsv
N_eur=1035760
id=mdd_eur_012626
outdir=results_${id}
mkdir -p ${outdir} 

## run PRS-CS
JOB1=$(sbatch --parsable -J MDD_PRSCS --export=id=${id},outdir=${outdir},N=${N_eur},sst_path=${sst_eur},geno=${geno_eur},ancestry=eur ${script1})
# sbatch --dependency=afterok:$JOB1 -J MDD_SCORE --export=id=${id},outdir=${outdir},geno=${geno_eur},ancestry=eur,csx=0 ${script2}