pacman::p_load(data.table, dplyr)
setwd("/data/davis_lab/allie/care_sites")

#### Compile table with demographics, covariates, PGS, and phecodes for MEGA cohort ####
## load demographics
dat <- fread("/data/davis_lab/allie/generate_phecode_tables/covariate_files/20230607_sd_wide_covariates_121023.txt")

## load genotyping covariates
covar_geno_eur <- fread("/data/davis_lab/shared/genotype_data/biovu/processed/imputed/best_guess/MEGA/MEGA_recalled/20200518_MEGA.GRID.RACE.ETH.GEN.batch.PCs.covariates_EU.filt1.txt")
covar_geno_afr <- fread("/data/davis_lab/shared/genotype_data/biovu/processed/imputed/best_guess/MEGA/MEGA_recalled/20200515_MEGA.GRID.RACE.ETH.GEN.batch.PCs.covariates_AA.filt1.txt")

covar_geno <- rbind(covar_geno_eur, covar_geno_afr) %>% select(-GENDER, -RACE, -ETHNICITY)
dat <- merge(dat, covar_geno, by = "GRID")

## load and z-score normalize psych PGS
pgs_eur <- fread("/data/davis_lab/allie/generate_psych_pgs/output/psych_pgs_eur_030824.txt") %>%
  select(GRID, pgs.scz = pgs.scz.eur, pgs.bd = pgs.bd.eur, pgs.dep = pgs.dep.eur) %>%
  mutate(genetic_ancestry = "EUR") %>%
  data.table()

pgs_eur[, pgs.scz.scale := scale(pgs.scz)]
pgs_eur[, pgs.bd.scale := scale(pgs.bd)]
pgs_eur[, pgs.dep.scale := scale(pgs.dep)]

pgs_afr <- fread("/data/davis_lab/allie/generate_psych_pgs/output/psych_pgs_afr_030824.txt") %>%
  select(GRID, pgs.scz = pgs.scz.afr.csx, pgs.bd = pgs.bd.afr.csx, pgs.dep = pgs.dep.afr.csx) %>%
  mutate(genetic_ancestry = "AFR") %>%
  data.table()

pgs_afr[, pgs.scz.scale := scale(pgs.scz)]
pgs_afr[, pgs.bd.scale := scale(pgs.bd)]
pgs_afr[, pgs.dep.scale := scale(pgs.dep)]

pgs <- rbind(pgs_eur, pgs_afr) %>% select(GRID,genetic_ancestry,pgs.scz.scale, pgs.bd.scale, pgs.dep.scale)
dat <- merge(dat, pgs, by = "GRID")

stopifnot(dat$mega_genetic_ancestry == dat$genetic_ancestry)
dat[, mega_genetic_ancestry := NULL]

## add in SLE PGS
pgs_sle <- fread("/data/davis_lab/allie/care_sites/data/makepgs_sle/sle_eur_092324/sle_eur_092324.profile") %>%
  select(GRID = IID, pgs.sle = SCORE) %>%
  data.table()

pgs_sle[, pgs.sle.scale := scale(pgs.sle)]
dat <- merge(dat, pgs_sle[,.(GRID, pgs.sle.scale)], by = "GRID", all.x = T)

## load phecodes for MEGA cohort
phecodes <- readRDS("/data/davis_lab/shared/phenotype_data/biovu/phecode_tables/mega_genotyped_eur_afr_ancestry_phecode_table_20230607_pull_2_distinct_dates_no_exclusions_121023.Rds")
dat <- merge(dat, phecodes, by = "GRID")

## save
saveRDS(dat, "data/mega_eur_afr_covariates_pgs_phecodes_092424.Rds")

#### Compile visit data for MEGA cohort ####
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


#### Compile visit data for full SD cohort ####
visit <- fread("/data/davis_lab/shared/phenotype_data/biovu/delivered_data/sd_wide_pull/20230607_pull/20230607_sd_pull_visit_occurrence_all.txt.gz")

visit <- visit %>% 
  select(GRID, visit_occurrence_id, visit_start_date, visit_end_date, visit_concept_id, care_site_id)
visit[visit_concept_id == 0, visit_type := 'unknown']
visit[visit_concept_id == 9201, visit_type := "IP"]
visit[visit_concept_id == 9202, visit_type := "OP"]
visit[visit_concept_id == 9203, visit_type := "ER"]

## if visit end date is missing, set end date = start date
visit[is.na(visit_end_date), visit_end_date := visit_start_date]
visit <- unique(visit)

nrow(visit) # 70672052
n_distinct(visit$visit_occurrence_id) # 70672052

saveRDS(visit, "/data/davis_lab/allie/care_sites/data/mdd_application/20230607_sd_pull_visit_occurrence_unique.Rds")

nrow(visit[care_site_id!=0]) / nrow(visit) # 77% of visits have a non-null care site

visit_sites <- merge(visit[care_site_id!=0], map, by = "care_site_id", allow.cartesian = T)
visit_sites <- visit_sites[SpecialtyLong!="Unclear" & SpecialtyLong!="Administrative"]

n_distinct(visit_sites$visit_occurrence_id) / nrow(visit) # 66% of visits have a care site with a defined specialty
n_distinct(visit_sites$visit_occurrence_id) / nrow(visit[care_site_id!=0]) # 85% of visits with a non-null care site have a defined specialty

saveRDS(visit_sites, "/data/davis_lab/allie/care_sites/data/mdd_application/20230607_sd_pull_visit_occurrence_defined_sites.Rds")
