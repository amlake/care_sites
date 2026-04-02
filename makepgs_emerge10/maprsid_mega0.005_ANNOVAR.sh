cd ../../programs/annovar

perl convert2annovar.pl \
-format rsid /data/davis_lab/allie/care_sites/data/makepgs_emerge10/annovar_files/20241124_biallelic_mega_recalled.chr1-22.grid.EU.filt1.r2corrected.IBD.filtered.PI_HAT.0.2_maf0.005.rsids.annovar.input \
-dbsnpfile my_db_downloads/hg19_avsnp151.txt \
> /data/davis_lab/allie/care_sites/data/makepgs_emerge10/annovar_files/20241124_biallelic_mega_recalled.chr1-22.grid.EU.filt1.r2corrected.IBD.filtered.PI_HAT.0.2_maf0.005.rsids.annovar.output

