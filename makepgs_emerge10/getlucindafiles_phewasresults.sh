# sh /data/davis_lab/allie/care_sites/scripts/makepgs_emerge10/getlucindafiles_phewasresults.sh

cd /lucinda/MosleyLab/shellejp/CareSites/phewas_emerge/data/

scp shellejp@login.accre.vanderbilt.edu:/data/davis_lab/allie/care_sites/data/makepgs_emerge10/makepgs_afib/output/results_af_eur_112424/af_eur_112424.profile ./
scp shellejp@login.accre.vanderbilt.edu:/data/davis_lab/allie/care_sites/data/makepgs_emerge10/makepgs_asthma/output/results_asthma_eur_112424/asthma_eur_112424.profile ./
scp shellejp@login.accre.vanderbilt.edu:/data/davis_lab/allie/care_sites/data/makepgs_emerge10/makepgs_bmi/output/results_bmi_eur_112424/bmi_eur_112424.profile ./
scp shellejp@login.accre.vanderbilt.edu:/data/davis_lab/allie/care_sites/data/makepgs_emerge10/makepgs_chd/output/results_chd_eur_112424/chd_eur_112424.profile ./
scp shellejp@login.accre.vanderbilt.edu:/data/davis_lab/allie/care_sites/data/makepgs_emerge10/makepgs_ckd/output/results_ckd_eur_112424/ckd_eur_112424.profile ./

scp shellejp@login.accre.vanderbilt.edu:/panfs/accrepfs.vampire/data/davis_lab/allie/care_sites/data/SpecialtyWASFiles/CareSiteWAS_Demographics_MEGA_EUR.txt ./

scp shellejp@login.accre.vanderbilt.edu:/data/davis_lab/shared/genotype_data/biovu/processed/imputed/best_guess/MEGA/MEGA_recalled/20190827_MEGA.GRID.RACE.ETH.GEN.batch.PCs.covariates_EU.filt1.txt ./