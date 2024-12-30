pacman::p_load(data.table, dplyr, openxlsx)
setwd("/data/davis_lab/allie/care_sites")

#### Compile table with demographics, covariates, PGS, and phecodes for MEGA cohort ####
## load demographics
dat <- fread("/data/davis_lab/allie/generate_phecode_tables/covariate_files/20230607_sd_wide_covariates_121023.txt")
range(dat$first_day) # "1989-01-30" "2023-03-31"
range(dat$last_day) # "1989-01-30" "2023-03-31"

## load care sites specialty map
map <- fread("/data/davis_lab/allie/care_sites/output/CareSiteMap_Multispecialty_Long_UPDATED_111524.csv")

care_site_names <- read.xlsx("/data/davis_lab/allie/care_sites/output/CareSite_VisitDetailCount_UPDATED_111524.xlsx") %>%
  select(care_site_id, care_site_name) %>%
  data.table() %>%
  unique()

map <- merge(map, care_site_names, by = "care_site_id")

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

nrow(pgs_eur) # 66917

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

#### Compile visit data for full SD cohort ####
visit_full_sd <- fread("/data/davis_lab/shared/phenotype_data/biovu/delivered_data/sd_wide_pull/20230607_pull/20230607_sd_pull_visit_occurrence_all.txt.gz")

nrow(visit_full_sd) # 70672052
n_distinct(visit_full_sd$visit_occurrence_id) # 70672052
n_distinct(visit_full_sd$GRID) # 3455981

nrow(visit_full_sd[care_site_id != 0]) # 54679804
n_distinct(visit_full_sd[care_site_id != 0]$GRID) # 3274235

class(visit_full_sd$visit_occurrence_id) # integer
range(visit_full_sd$visit_occurrence_id) # 1-112444321 (don't need to worry about int64 issues)

range(visit_full_sd$visit_start_date) # "1799-02-02" "2023-03-31"
nrow(visit_full_sd[visit_start_date > as.Date("1980-01-01")]) # 70671951
nrow(visit_full_sd[visit_start_date > as.Date("1989-01-01")]) # 70637846

nrow(visit_full_sd[visit_start_date > as.Date("1980-01-01") & care_site_id != 0]) # 54679802
nrow(visit_full_sd[visit_start_date > as.Date("1989-01-01") & care_site_id != 0]) # 54670765

visit <- copy(visit_full_sd)

visit <- visit %>%
  select(GRID, visit_occurrence_id, visit_start_date, visit_end_date, visit_concept_id, care_site_id) %>% 
  as.data.table()

visit[visit_concept_id == 0, visit_type := "unknown"]
visit[visit_concept_id == 9201, visit_type := "IP"]
visit[visit_concept_id == 9202, visit_type := "OP"]
visit[visit_concept_id == 9203, visit_type := "ER"]

## if visit end date is missing, set end date = start date
visit[is.na(visit_end_date), visit_end_date := visit_start_date]
visit <- unique(visit)

nrow(visit) # 70672052
n_distinct(visit$visit_occurrence_id) # 70672052

saveRDS(visit, "/data/davis_lab/allie/care_sites/data/mdd_application/20230607_sd_pull_visit_occurrence_unique.Rds")

nrow(visit[care_site_id != 0]) # 54679804 visits have a non-null care site id
nrow(visit[care_site_id != 0]) / nrow(visit) # 77.4% of visits have a non-null care site id

visit_sites <- merge(visit[care_site_id != 0], map, by = "care_site_id", allow.cartesian = T)
visit_sites <- visit_sites[SpecialtyLong != "Unclear" & SpecialtyLong != "Administrative"]

n_distinct(visit_sites$GRID) # 3083596 individuals have at least one encounter at a care site with a defined specialty
n_distinct(visit_sites$visit_occurrence_id) # 46471048 visits have a care site with a defined specialty
n_distinct(visit_sites$visit_occurrence_id) / nrow(visit) # 65.8% of visits have a care site with a defined specialty
n_distinct(visit_sites$visit_occurrence_id) / nrow(visit[care_site_id != 0]) # 85.0% of visits with a non-null care site have a defined specialty

nrow(visit_sites) # 51751207 total rows

saveRDS(visit_sites, "/data/davis_lab/allie/care_sites/data/mdd_application/20230607_sd_pull_visit_occurrence_defined_sites.Rds")

## restrict to MEGA cohort
visit_sites_mega <- visit_sites[GRID %in% dat$GRID]

## save
saveRDS(visit_sites_mega, "/data/davis_lab/allie/care_sites/data/mdd_application/20230607_sd_pull_mega_grids_visit_occurrence_defined_sites.Rds")


