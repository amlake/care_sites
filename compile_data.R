pacman::p_load(data.table, dplyr)
setwd("/data/davis_lab/allie/care_sites")

#### compile table with demographics, covariates, PGS, and phecodes ####
## load demographics
dat <- fread("/data/davis_lab/allie/generate_phecode_tables/covariate_files/20230607_sd_wide_covariates_121023.txt")

## load genotyping covariates
covar_geno_eur <- fread("/data/davis_lab/shared/genotype_data/biovu/processed/imputed/best_guess/MEGA/MEGA_recalled/20200518_MEGA.GRID.RACE.ETH.GEN.batch.PCs.covariates_EU.filt1.txt")
covar_geno_afr <- fread("/data/davis_lab/shared/genotype_data/biovu/processed/imputed/best_guess/MEGA/MEGA_recalled/20200515_MEGA.GRID.RACE.ETH.GEN.batch.PCs.covariates_AA.filt1.txt")

covar_geno <- rbind(covar_geno_eur, covar_geno_afr) %>% select(-GENDER, -RACE, -ETHNICITY)
dat <- merge(dat, covar_geno, by = "GRID")

## load and z-score normalize PGS
pgs_eur <- fread("/data/davis_lab/allie/generate_psych_pgs/output/psych_pgs_eur_030824.txt") %>%
  select(GRID, pgs.scz = pgs.scz.eur, pgs.dep = pgs.dep.eur) %>%
  mutate(genetic_ancestry = "EUR") %>%
  data.table()

pgs_eur[, pgs.scz.scale := scale(pgs.scz)]
pgs_eur[, pgs.dep.scale := scale(pgs.dep)]

pgs_afr <- fread("/data/davis_lab/allie/generate_psych_pgs/output/psych_pgs_afr_030824.txt") %>%
  select(GRID, pgs.scz = pgs.scz.afr.csx, pgs.dep = pgs.dep.afr.csx) %>%
  mutate(genetic_ancestry = "AFR") %>%
  data.table()

pgs_afr[, pgs.scz.scale := scale(pgs.scz)]
pgs_afr[, pgs.dep.scale := scale(pgs.dep)]

pgs <- rbind(pgs_eur, pgs_afr) %>% select(GRID,genetic_ancestry,pgs.scz.scale, pgs.dep.scale)
dat <- merge(dat, pgs, by = "GRID")

stopifnot(dat$mega_genetic_ancestry == dat$genetic_ancestry)
dat[, mega_genetic_ancestry := NULL]

## load phecodes for MEGA cohort
phecodes <- readRDS("/data/davis_lab/shared/phenotype_data/biovu/phecode_tables/mega_genotyped_eur_afr_ancestry_phecode_table_20230607_pull_2_distinct_dates_no_exclusions_121023.Rds")
dat <- merge(dat, phecodes, by = "GRID")

## save
saveRDS(dat, "/data/mega_eur_afr_covariates_pgs_phecodes.Rds")

#### compile visit data ####
visit <- fread("/data/davis_lab/shared/phenotype_data/biovu/delivered_data/sd_wide_pull/20230607_pull/20230607_sd_pull_visit_occurrence_all.txt.gz")

## fix missing visit end dates
visit[is.na(visit_end_date), visit_end_date := visit_start_date]

## remove visits with care_site_id = 0 (null care site)
visit <- visit[care_site_id != 0]

## remove unneeded columns
visit <- unique(visit[, .(GRID, visit_start_date, visit_end_date, care_site_id)])

## restrict to MEGA cohort
visit <- visit[GRID %in% dat$GRID]

## save
fwrite(visit, "data/mega_grids_visits_cleaned_dates_non_null_sites.csv")
