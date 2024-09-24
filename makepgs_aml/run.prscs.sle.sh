#!/bin/bash
cd /data/davis_lab/allie/care_sites/data/makepgs_sle/

### Global parameters
geno_eur=/data/davis_lab/allie/commonly_used/mega_maf_filter/plink_files/20200518_biallelic_mega_recalled.chr1-22.grid.EU.filt1.r2corrected.IBD.filtered.PI_HAT.0.2_maf0.005
# geno_afr=/data/davis_lab/allie/commonly_used/mega_maf_filter/20200515_biallelic_mega_recalled.chr1-22.grid.AA.filt1.r2corrected.IBD.filtered.PI_HAT.0.2_maf0.005
sst_eur=/data/davis_lab/allie/care_sites/data/makepgs_sle/2017_Langefeld_SLE.prscs_fmt.092324.tsv
# sst_afr=
N_eur=14818
# N_afr=

### EUR PRS-CS
id=sle_eur_092324
outdir=/data/davis_lab/allie/care_sites/data/makepgs_sle/${id}
mkdir -p ${outdir}

# sbatch -J SLE_CS --export=id=${id},outdir=${outdir},N=${N_eur},sst_path=${sst_eur},geno=${geno_eur},ancestry=eur /data/davis_lab/allie/care_sites/scripts/makepgs_aml/prs.cs.helper.sh
sbatch -J SLE_SCORE --export=id=${id},outdir=${outdir},geno=${geno_eur},ancestry=eur,csx=0 /data/davis_lab/allie/care_sites/scripts/makepgs_aml/plink.score.helper.sh
