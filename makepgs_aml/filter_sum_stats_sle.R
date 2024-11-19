## Reformat SLE summary stats for use with PRS-CS
## Article: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5520018/
## GWAS catalog accession: https://www.ebi.ac.uk/gwas/publications/28714469
## NOTE: only European Ancestry summ stats are publicly available

setwd("/data/davis_lab/allie/care_sites/")
pacman::p_load(data.table, dplyr, stringr, tidyr, foreach, qqman)
options(stringsAsFactors = F)

#### Load and filter sum stats ####
sst <- foreach(i=1:22, .combine=rbind) %do% {
    xx <- fread(cmd = sprintf("grep -v '^#' /data/davis_lab/allie/public_data/summary_stats/2017_Langefeld_SLE/ea.imputed.chr%s.out", i))
    xx[, chr := i]
    xx <- unique(xx[all_maf > 0.01 & all_maf < 0.99]) ## apply MAF filter, remove duplicated rows
    xx
}

setnames(sst, "position", "pos")

nrow(sst) # 10499521
range(sst$all_maf) # 0.0100001 0.5000000

range(sst$all_impute_info) # 0.018146 1.000000
range(sst$all_info) #  0.0271381 1.0000000

# are the two imputation columns essentially equivalent?
cor(sst$all_impute_info, sst$all_info) #  0.9999983

sst <- sst[all_impute_info > 0.3 & all_info > 0.3]
nrow(sst) # 10003735

sample_size <- sst$cases_total + sst$controls_total
range(sample_size) # 15,426 - 16,829
unique(sample_size) # all variants have either 15,426 or 16,829 total samples

## note from Tian Ge (developer of PRS-CS) on N to use for PRS-CS: 
## "PRS-CS is fairly robust to n_gwas as shown in your experiments — as long as it’s not too far away from the true value the impact on prediction accuracy should be limited.
## We generally recommend using the effective n and remove variants that have a low sample size in meta-analysis (e.g., <50% of the total sample size)."

## Effective sample size calculated as: N_EFF = 4/[(1/ncase) + (1/ncontrol)]
sst$n_eff <-  4 / ((1/sst$cases_total) + (1/sst$controls_total))

sort(unique(sst$n_eff)) #  12126.77, 14818.33

## get median N_EFF to use for PRS-CS
median(sst$n_eff) # 14818.33

## load additional QC info
qc <- foreach(i = 1:22, .combine = rbind) %do% {
    xx <- fread(sprintf("/data/davis_lab/allie/public_data/summary_stats/2017_Langefeld_SLE/chr%s.biallelic_SNP_info.csv", i))
    xx[, chr := i]
    unique(xx)
}

names(qc) <- gsub("\"", "", names(qc))
qc <- qc[SNP %in% sst$rsid] # subset to SNPs in filtered sum stats file
nrow(qc) # 8815505

range(qc$MAF) #  0.00933492 0.50000000
range(qc$INFO_Score) # 0.288622 1.000000

## remove SNPs with MAF < 0.01 or INFO < 0.3 in this file
nrow(qc[!(MAF > 0.01 & INFO_Score > 0.3)]) # 18331
qc <- qc[MAF > 0.01 & INFO_Score > 0.3]

## remove SNPs with significant departure from Hardy-Weinberg equilibrium expectations (P<1 × 10−6 in cases, P<0.01 in controls) (as was done in the Langefeld GWAS manuscript)
range(qc$P_HWE_Control)
range(qc$P_HWE_Case)

qc <- qc[P_HWE_Case >= 1e-6 & P_HWE_Control >= 0.01]
nrow(qc) #  8694829

## remove SNPs with missing "Missing" column
qc <- qc[!is.na(Missing)]
nrow(qc) # 

## remove SNPs with missingness > 0.05 (call rate < 95%)
range(qc$Missing) # 0.0000000 0.9950174
quantile(qc$Missing)

qc <- qc[Missing <= 0.05]
nrow(qc) # 1058588

## remove SNPs with differential missingness between cases and controls (P < 0.05, as per manuscript)
qc <- qc[Pval_Diff_Miss >= 0.05]
nrow(qc) # 976426

sst <- sst[rsid %in% qc$SNP] 
nrow(sst) # 976426 SNPs in sum stats file passed these filters

## double check genome build and position columns
sst[rsid == "rs139620215:51188319:T:C", .(chr, pos, all_maf, alleleA, alleleB)] # according to dbSNP, position is 22:51188319 (GRCh37), ref / alt alleles are T/C (looks good)
sst[rsid == "rs60381075:147746:T:C", .(chr, pos, all_maf, alleleA, alleleB)] # according to dbSNP, position is  16:147746 (GRCh37), ref / alt alleles are T/C (looks good)

## Determine lambda (genomic inflation factor)
chisq <- qchisq(1 - sst$frequentist_add_wald_pvalue_1, 1)
lambda_gc <- median(chisq) / qchisq(0.5, 1)
lambda_gc <- round(lambda_gc, digits = 4)
lambda_gc #  1.2858 # seems high...

#### Manhattan and QQ plots ####
png("data/makepgs_sle/2017_Langefeld_SLE.filtered.manhattan.png")
print(manhattan(sst, chr = "chr", bp = "pos", p = "frequentist_add_wald_pvalue_1", snp = "rsid"))
dev.off()

png("data/makepgs_sle/2017_Langefeld_SLE.filtered_nochr6.manhattan.png")
print(manhattan(sst[chr!=6], chr = "chr", bp = "pos", p = "frequentist_add_wald_pvalue_1", snp = "rsid"))
dev.off()

png("data/makepgs_sle/2017_Langefeld_SLE.filtered.qq.png")
print(qq(sst$frequentist_add_wald_pvalue_1, sub = paste0("lambda = ", lambda_gc)))
dev.off()

png("data/makepgs_sle/2017_Langefeld_SLE_nochr6.filtered.qq.png")
print(qq(sst[chr!=6]$frequentist_add_wald_pvalue_1, sub = paste0("lambda = ", lambda_gc)))
dev.off()

#### Map variant IDs to rsids for PRS-CS ####
mega_bim <- fread("/data/davis_lab/allie/commonly_used/mega_maf_filter/plink_files/20200518_biallelic_mega_recalled.chr1-22.grid.EU.filt1.r2corrected.IBD.filtered.PI_HAT.0.2_maf0.005.bim")
mega_bim <- mega_bim %>% select(-V3)
names(mega_bim) <- c("chr", "rsid", "pos", "a1", "a2")

## add columns to MEGA bim with alphabetically ordered alleles
mega_bim <- mega_bim %>%
    unite("alleles_concat_1", a1:a2, sep = "", remove = F) %>%
    unite("alleles_concat_2", a2:a1, sep = "", remove = F)

setDT(mega_bim)
mega_bim[, alleles_sort := ifelse(alleles_concat_1 < alleles_concat_2, alleles_concat_1, alleles_concat_2)]
mega_bim <- mega_bim %>% select(-alleles_concat_1, -alleles_concat_2)

## confirm that there are no indels in the MEGA bim file
max(nchar(mega_bim$a1)) # 1
max(nchar(mega_bim$a2)) # 1

## check whether there are any multiallelic variants or multiple variants at the same position in the MEGA file
mega_bim[, pos_ct := .N, by = .(chr, pos)]
max(mega_bim$pos_ct) # 1
# there is at most 1 variant per position in the MEGA file, so this will make the mapping easier

## confirm that there are no indels in the SST file
max(nchar(sst$alleleA)) # 1
max(nchar(sst$alleleB)) # 1

## add columns to SST file with alphabetically ordered alleles
sst <- sst %>%
    unite("alleles_concat_1", alleleA:alleleB, sep = "", remove = F) %>%
    unite("alleles_concat_2", alleleB:alleleA, sep = "", remove = F) %>% 
    data.table()

sst[, alleles_sort := ifelse(alleles_concat_1 < alleles_concat_2, toupper(alleles_concat_1), toupper(alleles_concat_2))]
sst <- sst %>% select(-alleles_concat_1, -alleles_concat_2)

## check whether there are any multiallelic variants or multiple variants at the same position in the summ stats file
sst[, pos_ct := .N, by = .(chr, pos)]
max(sst$pos_ct) # 3

## count # of variants per position + unique allele combination
sst[, pos_allele_ct := .N, by = .(chr, pos, alleles_sort)]
max(sst$pos_allele_ct) # 1 - good

## map variants in summ stats file to rsids
setnames(mega_bim, "rsid", "rsid.mega")
sst_mapped <- merge(sst, mega_bim %>% select(-pos_ct),
    by = c("chr", "pos", "alleles_sort")
)

nrow(sst_mapped) # 687688 SNPs remaining after mapping to MEGA variants by rsid
n_distinct(sst_mapped$rsid) # 687688
n_distinct(sst_mapped$rsid.mega) # 687688

#### Write out filtered summary stats for PRS-CS ####
## PRS-CS documentation: SNP is the rsID, A1 is the effect allele, A2 is the alternative allele, BETA/OR is the effect/odds ratio of the A1 allele, P is the p-value of the effect.
## NOTE: I'm pretty sure below are the correct columns to use but it's hard to tell because the results aren't matching up with the results reported in the paper (which were apparently generated using SNPGWA instead of SNPTEST?)
sst_out <- sst_mapped %>%
    select(SNP = rsid.mega, A1 = alleleB, A2 = alleleA, BETA = "frequentist_add_beta_1:add/sle=1", P = frequentist_add_wald_pvalue_1) %>%
    data.table()

fwrite(sst_out, "data/makepgs_sle/2017_Langefeld_SLE.prscs_fmt.092324.tsv", sep = "\t")