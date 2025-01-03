pacman::p_load(data.table, dplyr, openxlsx, ggplot2, lubridate)
setwd("/data/davis_lab/allie/care_sites/")

#### clean person table ####
person <- fread("/data/davis_lab/shared/phenotype_data/biovu/delivered_data/sd_wide_pull/20230607_pull/20230607_sd_pull_person_all.txt.gz")
person_col_order <- copy(names(person))

person[,DOB := as.Date(birth_datetime)]

nrow(person) # 3712498

## remove GRIDs with missing birth dates
person <- person[!is.na(DOB)]
nrow(person) # 3712387 (111 lost)

## determine appropriate date cutoffs
min(person$DOB) # 1799-01-23
max(person$DOB) # 2993-12-18
quantile(person$year_of_birth, c(0.01, 0.99)) # 1918, 2020
quantile(person$year_of_birth, c(1e-5, 0.99999)) # 1848, 2023

if (FALSE) {
  pdf("20230607_sd_pull_year_of_birth_histograms.pdf", width = 10, height = 7)
  ggplot(person, aes(year_of_birth)) +
    geom_histogram(bins = 50) +
    theme_bw() +
    labs(title = "All DOBs")

  ggplot(person[year_of_birth >= 1848 & year_of_birth <= 2023], aes(year_of_birth)) +
    geom_histogram(bins = 100) +
    scale_x_continuous(breaks = seq(1840, 2023, by = 10)) + 
    theme_bw() +
    labs(title = "All DOBs >= 1848 and <= 2023")

  ggplot(person[year_of_birth >= 1880 & year_of_birth <= 2023], aes(year_of_birth)) +
    geom_histogram(bins = 100) +
    scale_x_continuous(breaks = seq(1880, 2023, by = 10)) +
    theme_bw() +
    labs(title = "All DOBs >= 1880 and <= 2023")
  dev.off()
}

#### clean ICD and CPT codes ####
codes <- fread("/data/davis_lab/shared/phenotype_data/biovu/delivered_data/sd_wide_pull/20230607_pull/20230607_sd_pull_x_codes_all.txt.gz")
codes_col_order <- copy(names(codes))

nrow(codes) # 457335383
n_distinct(codes$GRID) # 3310735 individuals with any codes

## add in birth dates
codes <- merge(person[,.(GRID,DOB)],codes,by="GRID")

## clean age at code
codes$entry_date <- as.Date(codes$entry_date)
codes[,age_at_code := unclass((entry_date-DOB)/365.25)]

codes <- codes[age_at_code>=0] # remove negative age
n_distinct(codes$GRID) # 3310481 individuals in code table now (254 lost)

## determine appropriate date cutoffs
codes[, code_year := year(entry_date)]

min(codes$entry_date) # 1978-10-25
max(codes$entry_date) # 2023-03-31
quantile(codes$code_year, c(0.01, 0.99)) # 2001-2022
quantile(codes$code_year, c(1e-5, 0.99999)) # 1989-2023

earliest_code_after_1980 <- min(codes[code_year >= 1980, code_year]) # 1986

if (FALSE) {
  pdf("20230607_sd_pull_x_codes_year_histograms.pdf", width = 10, height = 7)
  ggplot(codes, aes(code_year)) +
    geom_histogram(bins = (2023-1978+1)) +
    theme_bw() +
    labs(title = "All codes")

  ggplot(codes[code_year >= 1980 & code_year <= 2023], aes(code_year)) +
    geom_histogram(bins = (2023-earliest_code_after_1980+1)) +
    scale_x_continuous(breaks = seq(1980, 2023, by = 10)) +
    geom_vline(xintercept = earliest_code_after_1980, color = "red") +
    annotate("text", x = earliest_code_after_1980, y = 1e7, label = paste0("earliest code after 1980: ", earliest_code_after_1980), color = "red", hjust = -.2) +
    theme_bw() +
    labs(title = "All codes >= 1980 and <= 2023", y = "count")
  dev.off()
}

#### clean visits ####
visits <- fread("/data/davis_lab/shared/phenotype_data/biovu/delivered_data/sd_wide_pull/20230607_pull/20230607_sd_pull_visit_occurrence_all.txt.gz")
visits_col_order <- copy(names(visits))

nrow(visits) # 70672052
n_distinct(visits$visit_occurrence_id) # 70672052
n_distinct(visits$GRID) # 3455981

## add in birth dates
visits <- merge(person[,.(GRID,DOB)],visits,by="GRID")
visits[,DOB := as.Date(DOB)]
visits[,visit_start_date := as.Date(visit_start_date)]

## clean age at visit
visits[,age_at_visit := unclass((visit_start_date-DOB)/365.25)]
visits <- visits[age_at_visit>=0] # remove negative age at visit

n_distinct(visits$GRID) # 3454621 (1360 lost)

## determine appropriate date cutoffs
visits[, visit_year := year(visit_start_date)]

min(visits$visit_start_date) # 1942-04-28
max(visits$visit_start_date) # 2023-03-31
quantile(visits$visit_year, c(0.01, 0.99)) # 1997-2022
quantile(visits$visit_year, c(1e-5, 0.99999)) # 1983-2023

earliest_visit_after_1980 <- min(visits[visit_year >= 1980, visit_year]) # 1981

if (FALSE) {
  pdf("20230607_sd_pull_visits_year_histograms.pdf", width = 10, height = 7)
  ggplot(visits, aes(visit_year)) +
    geom_histogram(bins = (2023-1942+1)) +
    theme_bw() +
    labs(title = "All visits")

  ggplot(visits[visit_year >= 1980 & visit_year <= 2023], aes(visit_year)) +
    geom_histogram(bins = (2023-1980+1)) +
    scale_x_continuous(breaks = seq(1980, 2023, by = 10)) +
    geom_vline(xintercept = earliest_visit_after_1980, color = "red") +
    annotate("text", x = earliest_visit_after_1980, y = 2e6, label = paste0("earliest visit after 1980: ", earliest_visit_after_1980), color = "red", hjust = -.2) +
    theme_bw() +
    labs(title = "All visits >= 1980 and <= 2023")
  dev.off()
}

#### apply date filters to all data elements, remove extra columns, save files ####
# DOB: 1898 to 2023-03-31
# x_codes: 1998 to 2023-03-31
# visits: 1998 to 2023-03-31

# NOTE: We decided to use this as the minimum date filter because there is published literature stating that Starpanel was developed in 1998.

person_clean <- person[year_of_birth >= 1898 & DOB <= as.Date("2023-03-31")]
person_clean <- person_clean[,person_col_order, with = FALSE]

codes_clean <- codes[code_year >= 1998 & entry_date <= as.Date("2023-03-31")]
codes_clean <- codes_clean[,codes_col_order, with = FALSE]

visits_clean <- visits[visit_year >= 1998 & visit_start_date <= as.Date("2023-03-31")]
visits_clean <- visits_clean[,visits_col_order, with = FALSE]

nrow(person_clean) # 3711779

nrow(codes_clean) # 455596509
n_distinct(codes_clean$GRID) # 3242086

nrow(visits_clean) # 69826474
n_distinct(visits_clean$GRID) # 3364264

## determine how many indivduals are overlapping between the 3 datasets
common_grids <- intersect(intersect(person_clean$GRID, codes_clean$GRID), visits_clean$GRID)
length(common_grids) # 3213825

nrow(person_clean[GRID %in% common_grids]) # 3213825
n_distinct(codes_clean[GRID %in% common_grids]$GRID) # 3213825
n_distinct(visits_clean[GRID %in% common_grids]$GRID) # 3213825

## save files
fwrite(person_clean[GRID %in% common_grids], "data/sd_data_qc/20230607_sd_pull_person_dates_cleaned_overlapping_grids.txt", sep = "\t")
fwrite(codes_clean[GRID %in% common_grids], "data/sd_data_qc/20230607_sd_pull_x_codes_dates_cleaned_overlapping_grids.txt", sep = "\t")
fwrite(visits_clean[GRID %in% common_grids], "data/sd_data_qc/20230607_sd_pull_visit_occurrence_dates_cleaned_overlapping_grids.txt", sep = "\t")

## MEGA genotyped set - European and African ancestry QC'd
## load GRIDs from ancestry PCA analysis 
## see https://docs.google.com/document/d/1QLaFrSfTjLAhM5CEeUsv7S84DePxYMnydHAh6OIxIuI
covar_geno_eur <- fread("/data/davis_lab/shared/genotype_data/biovu/processed/imputed/best_guess/MEGA/MEGA_recalled/20200518_MEGA.GRID.RACE.ETH.GEN.batch.PCs.covariates_EU.filt1.txt")
covar_geno_afr <- fread("/data/davis_lab/shared/genotype_data/biovu/processed/imputed/best_guess/MEGA/MEGA_recalled/20200515_MEGA.GRID.RACE.ETH.GEN.batch.PCs.covariates_AA.filt1.txt")
geno_grids <- unique(c(covar_geno_eur$GRID, covar_geno_afr$GRID))
length(geno_grids) # 88107

geno_grids_overlapping <- intersect(geno_grids, common_grids)
length(geno_grids_overlapping) # 87575

## save mega subset
fwrite(person_clean[GRID %in% geno_grids_overlapping], "data/sd_data_qc/20230607_sd_pull_person_dates_cleaned_overlapping_grids_mega_grids_EU_AA.txt", sep = "\t")
fwrite(codes_clean[GRID %in% geno_grids_overlapping], "data/sd_data_qc/20230607_sd_pull_x_codes_dates_cleaned_overlapping_grids_mega_grids_EU_AA.txt", sep = "\t")
fwrite(visits_clean[GRID %in% geno_grids_overlapping], "data/sd_data_qc/20230607_sd_pull_visit_occurrence_dates_cleaned_overlapping_grids_mega_grids_EU_AA.txt", sep = "\t")

## clear temporary files
system("rm -f /tmp/*", intern = TRUE)
