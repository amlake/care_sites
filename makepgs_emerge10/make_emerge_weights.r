# Lennon, N.J., Kottyan, L.C., Kachulis, C. et al.  
# Selection, optimization and validation of ten chronic disease polygenic risk scores  
# for clinical implementation in diverse US populations.  
# Nat Med 30, 480–487 (2024).  
# https://doi.org/10.1038/s41591-024-02796-z

library(data.table)
library(dplyr)
library(tidyr)

setwd("/data/davis_lab/allie/care_sites/data/")

######### Get MEGA rsIDs #########
# system("bash ../scripts/makepgs_emerge10/maprsid_mega0.005_ANNOVAR.sh")
mega_rsids <- fread("./makepgs_emerge10/annovar_files/20241124_mega_maf0.005_rsids.annovar.output")
names(mega_rsids) <- c("CHR", "BP1", "BP2", "REF", "ALT", "RSID")

mega_rsids$SNPID_ref_alt <- paste(mega_rsids$CHR, mega_rsids$BP1, mega_rsids$REF, mega_rsids$ALT, sep = ":")
mega_rsids$SNPID_alt_ref <- paste(mega_rsids$CHR, mega_rsids$BP1, mega_rsids$ALT, mega_rsids$REF, sep = ":")

######### Read in PGS weights #########
# Get all files 
files <- list.files("./pgs_weights/eMERGE-implemented-PRS-models-Lennon-et-al-main/prs_files/")
files

# PGS types:
# 7 additive trait PGS (asthma, atrial fibrilation, breast cancer, coronary heart disease, hypercholesterolemia, prostate cancer, t2d)
# 2 non-additive PGS (T1D and CKD)
# 1 that is not available yet (BMI)
prs_weights_files <- files[!grepl("\\.ids$", files) & grepl("_PRS_weights\\.txt$", files)]
nonadditive_files <- files[!grepl("\\.ids$", files) & !grepl("_PRS_weights\\.txt$", files)]

######### Format PGS weights for PLINK scoring #########
# PLINK: --score <filename> [variant ID col.] [allele col.] [score col.]

# Asthma
asthma_pgs_weights <- fread("./makepgs_emerge10/pgs_weights/eMERGE-implemented-PRS-models-Lennon-et-al-main/prs_files/asthma_PRS_weights.txt")
names(asthma_pgs_weights)
names(asthma_pgs_weights)[1] <- "SNPID_emerge"  

asthma_pgs_weights %>% select(1:3) %>%
    left_join(mega_rsids, by = c("SNPID_emerge" = "SNPID_ref_alt")) %>%
    left_join(mega_rsids, by = c("SNPID_emerge" = "SNPID_alt_ref")) %>%
    count(is.na(RSID.x), is.na(RSID.y))

### Atrial Fibrilation ###
afib_pgs_weights <- fread("./makepgs_emerge10/pgs_weights/eMERGE-implemented-PRS-models-Lennon-et-al-main/prs_files/atrial_fibrilation_PRS_weights.txt") 
names(afib_pgs_weights)
names(afib_pgs_weights)[1] <- "SNPID_emerge"  

afib_pgs_weights %>% select(1:3) %>%
    left_join(mega_rsids, by = c("SNPID_emerge" = "SNPID_ref_alt")) %>%
    left_join(mega_rsids, by = c("SNPID_emerge" = "SNPID_alt_ref")) %>%
    count(is.na(RSID.x), is.na(RSID.y))

### Breast Cancer ###
brca_pgs_weights <- fread("./makepgs_emerge10/pgs_weights/eMERGE-implemented-PRS-models-Lennon-et-al-main/prs_files/breast_cancer_PRS_weights.txt")
names(brca_pgs_weights)
names(brca_pgs_weights)[1] <- "SNPID_emerge"  

brca_pgs_weights %>% select(1:3) %>%
    left_join(mega_rsids, by = c("SNPID_emerge" = "SNPID_ref_alt")) %>%
    left_join(mega_rsids, by = c("SNPID_emerge" = "SNPID_alt_ref")) %>%
    count(is.na(RSID.x), is.na(RSID.y))

### Coronary Heart Disease ###
chd_pgs_weights <- fread("./makepgs_emerge10/pgs_weights/eMERGE-implemented-PRS-models-Lennon-et-al-main/prs_files/coronary_heart_disease_PRS_weights.txt")
names(chd_pgs_weights)
names(chd_pgs_weights)[1] <- "SNPID_emerge"  

chd_pgs_weights %>% select(1:3) %>%
    left_join(mega_rsids, by = c("SNPID_emerge" = "SNPID_ref_alt")) %>%
    left_join(mega_rsids, by = c("SNPID_emerge" = "SNPID_alt_ref")) %>%
    count(is.na(RSID.x), is.na(RSID.y))

### Hypercholesterolemia ###
hld_pgs_weights <- fread("./makepgs_emerge10/pgs_weights/eMERGE-implemented-PRS-models-Lennon-et-al-main/prs_files/hypercholesterolemia_PRS_weights.txt")
names(hld_pgs_weights)
names(hld_pgs_weights)[1] <- "SNPID_emerge"  

hld_pgs_weights %>% select(1:3) %>%
    left_join(mega_rsids, by = c("SNPID_emerge" = "SNPID_ref_alt")) %>%
    left_join(mega_rsids, by = c("SNPID_emerge" = "SNPID_alt_ref")) %>%
    count(is.na(RSID.x), is.na(RSID.y))

### Prostate Cancer ###
prca_pgs_weights <- fread("./makepgs_emerge10/pgs_weights/eMERGE-implemented-PRS-models-Lennon-et-al-main/prs_files/prostate_cancer_PRS_weights.txt")
names(prca_pgs_weights)
names(prca_pgs_weights)[1] <- "SNPID_emerge"  

prca_pgs_weights %>% select(1:3) %>%
    left_join(mega_rsids, by = c("SNPID_emerge" = "SNPID_ref_alt")) %>%
    left_join(mega_rsids, by = c("SNPID_emerge" = "SNPID_alt_ref")) %>%
    count(is.na(RSID.x), is.na(RSID.y))

### T2D ###
t2d_pgs_weights <- fread("./makepgs_emerge10/pgs_weights/eMERGE-implemented-PRS-models-Lennon-et-al-main/prs_files/t2d_PRS_weights.txt")
names(t2d_pgs_weights)
names(t2d_pgs_weights)[1] <- "SNPID_emerge"  

t2d_pgs_weights %>% select(1:3) %>%
    left_join(mega_rsids, by = c("SNPID_emerge" = "SNPID_ref_alt")) %>%
    left_join(mega_rsids, by = c("SNPID_emerge" = "SNPID_alt_ref")) %>%
    count(is.na(RSID.x), is.na(RSID.y))

#### Non-additive PGS ####
### CKD ###
ckd_pgs_weights <- fread("./makepgs_emerge10/pgs_weights/eMERGE-implemented-PRS-models-Lennon-et-al-main/prs_files/ckd_PRS_weights.txt")
names(ckd_pgs_weights) 
names(ckd_pgs_weights)[1] <- "SNPID_emerge"  

ckd_pgs_weights %>% select(1:3) %>%
    left_join(mega_rsids, by = c("SNPID_emerge" = "SNPID_ref_alt")) %>%
    left_join(mega_rsids, by = c("SNPID_emerge" = "SNPID_alt_ref")) %>%
    count(is.na(RSID.x), is.na(RSID.y))

ckd_apol1_risk_alleles <- fread("./makepgs_emerge10/pgs_weights/eMERGE-implemented-PRS-models-Lennon-et-al-main/prs_files/ckd_apol1_risk_alleles.txt", header = FALSE)
names(ckd_apol1_risk_alleles)
names(ckd_apol1_risk_alleles) <- c("chr:bp:EA:NEA","EA")
# [1] "22:36661906:A:G" "G" 

### T1D ###
t1d_pgs_weights <- fread("./makepgs_emerge10/pgs_weights/eMERGE-implemented-PRS-models-Lennon-et-al-main/prs_files/t1d_PRS_weights.txt")
names(t1d_pgs_weights)
# [1] "id"     "allele" "weight"

t1d_hla_interactions_self_exclusive_sites <- fread("./makepgs_emerge10/pgs_weights/eMERGE-implemented-PRS-models-Lennon-et-al-main/prs_files/t1d_hla_interactions_self_exclusive_sites.tsv")
names(t1d_hla_interactions_self_exclusive_sites)
# [1] "id"     "chrom"  "pos"    "allele"

t1d_hla_interactions <- fread("./makepgs_emerge10/pgs_weights/eMERGE-implemented-PRS-models-Lennon-et-al-main/prs_files/t1d_hla_interactions.tsv")
names(t1d_hla_interactions)
# [1] "id_2"     "chrom_2"  "pos_2"    "allele_2" "id_1"     "chrom_1"  "pos_1"   
# [8] "allele_1" "weight"