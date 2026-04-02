# Lennon, N.J., Kottyan, L.C., Kachulis, C. et al.  
# Selection, optimization and validation of ten chronic disease polygenic risk scores  
# for clinical implementation in diverse US populations.  
# Nat Med 30, 480–487 (2024).  
# https://doi.org/10.1038/s41591-024-02796-z

### Set working directory
# source("./davis_lab/allie/care_sites/data/build_emerge_folders.r")

setwd("/data/davis_lab/allie/care_sites/data")

dir.create("makepgs_emerge10", showWarnings = FALSE, recursive = TRUE)
setwd("./makepgs_emerge10/")

### Write eMERGE conditions to list 
traits <- c("makepgs_asthma", "makepgs_afib", "makepgs_brca", "makepgs_ckd", "makepgs_chd", "makepgs_hld", "makepgs_bmi", "makepgs_prca", "makepgs_t1dm", "makepgs_t2dm")

### Loop over each trait and create directories
for (trait in traits) {
  dir.create(trait, showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(trait, "output"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(trait, "scripts"), showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(trait, "data"), showWarnings = FALSE, recursive = TRUE)
  file.copy("../makepgs_bmi/scripts/run.prscs.bmi.sh", file.path(trait, "scripts"), overwrite = TRUE)
  file.copy("../makepgs_bmi/scripts/plink.score.helper.sh", file.path(trait, "scripts"), overwrite = TRUE)
}

### Get PGS weights from eMERGE 
dir.create("pgs_weights", showWarnings = FALSE, recursive = TRUE)
setwd("pgs_weights")
system("wget https://github.com/broadinstitute/eMERGE-implemented-PRS-models-Lennon-et-al/archive/refs/heads/main.zip")
system("unzip main.zip")

## Identify the PGS with additiive vs. non-additive relations
system("ls ./eMERGE-implemented-PRS-models-Lennon-et-al-main/prs_files/")

# Get all files 
files <- list.files("./eMERGE-implemented-PRS-models-Lennon-et-al-main/prs_files/")

# Show eMERGE files
files

# PGS subtypes:
# 7 additive trait PGS (asthma, atrial fibrilation, breast cancer, coronary heart disease, hypercholesterolemia, prostate cancer, t2d)
# 2 non-additive PGS (T1D and CKD)
# 1 that is not available yet (BMI)
prs_weights_files <- files[!grepl("\\.ids$", files) & grepl("_PRS_weights\\.txt$", files)]
nonadditive_files <- files[!grepl("\\.ids$", files) & !grepl("_PRS_weights\\.txt$", files)]