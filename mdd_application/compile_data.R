pacman::p_load(data.table, dplyr, openxlsx)
setwd("/data/davis_lab/allie/care_sites")

#### Compile table with demographics, covariates, PGS, and phecodes for MEGA cohort ####
## load demographics
## see ../sd_data_qc.R for cleaning steps
dat <- fread("data/sd_data_qc/20230607_sd_pull_person_dates_cleaned_overlapping_grids.txt")
range(dat$year_of_birth) # 1908-2023

## load care sites specialty map
map <- fread("/data/davis_lab/allie/care_sites/output/CareSiteMap_Multispecialty_Long_UPDATED_JPS_052325.csv")

care_site_names <- read.xlsx("/data/davis_lab/allie/care_sites/output/CareSite_VisitDetailCount_UPDATED_JPS_052325.xlsx") %>%
  select(care_site_id, care_site_name) %>%
  as.data.frame() %>% data.table() %>%
  unique()

map <- merge(map, care_site_names, by = "care_site_id")

## load genotyping covariates
covar_geno_eur <- fread("/data/davis_lab/shared/genotype_data/biovu/processed/imputed/best_guess/MEGA/MEGA_recalled/20200518_MEGA.GRID.RACE.ETH.GEN.batch.PCs.covariates_EU.filt1.txt") %>%
  select(-GENDER, -RACE, -ETHNICITY)

dat <- merge(dat, covar_geno_eur, by = "GRID")

## load and z-score normalize psych PGS
## PGS generated in https://bitbucket.org/davislabteam/generate_psych_pgs/src/master/run.prscs.mdd.sh
pgs_eur <- fread("/data/davis_lab/allie/generate_psych_pgs/output/psych_pgs_eur_030824.txt") %>%
  select(GRID, pgs.scz = pgs.scz.eur, pgs.bd = pgs.bd.eur, pgs.dep = pgs.dep.eur) %>%
  mutate(genetic_ancestry = "EUR") %>%
  data.table()

nrow(pgs_eur) # 66917

pgs_eur[, pgs.scz.scale := scale(pgs.scz)]
pgs_eur[, pgs.bd.scale := scale(pgs.bd)]
pgs_eur[, pgs.dep.scale := scale(pgs.dep)]

pgs_eur <- pgs_eur %>% select(GRID,genetic_ancestry,pgs.scz.scale, pgs.bd.scale, pgs.dep.scale)
dat <- merge(dat, pgs_eur, by = "GRID")

## save
saveRDS(dat, "data/mega_eur_covariates_psych_pgs_060825.Rds")

#### Compile visit data for full SD cohort ####
## see ../sd_data_qc.R for visit cleaning steps
if (FALSE) { # does not need to be rerun 06/08/25
  visit_full_sd <- fread("data/sd_data_qc/20230607_sd_pull_visit_occurrence_dates_cleaned_overlapping_grids.txt")

  nrow(visit_full_sd) # 69307208
  n_distinct(visit_full_sd$visit_occurrence_id) #  69307208
  n_distinct(visit_full_sd$GRID) # 3213825

  nrow(visit_full_sd[care_site_id != 0]) # 53934856

  class(visit_full_sd$visit_occurrence_id) # integer
  range(visit_full_sd$visit_occurrence_id) # 1-112444321 (don't need to worry about int64 issues)

  range(visit_full_sd$visit_start_date) # "1998-01-01" "2023-03-31"

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

  nrow(visit) # 69307208
  n_distinct(visit$visit_occurrence_id) # 69307208
  saveRDS(visit, "/data/davis_lab/allie/care_sites/data/mdd_application/20230607_sd_pull_visit_occurrence_unique_010525.Rds")
}

visit <- readRDS("/data/davis_lab/allie/care_sites/data/mdd_application/20230607_sd_pull_visit_occurrence_unique_010525.Rds")

nrow(visit[care_site_id != 0]) # 53934856 visits have a non-null care site id
nrow(visit[care_site_id != 0]) / nrow(visit) # 77.8% of visits have a non-null care site id

visit_sites <- merge(visit[care_site_id != 0], map, by = "care_site_id", allow.cartesian = T)
visit_sites <- visit_sites[SpecialtyLong != "Unclear" & SpecialtyLong != "Administrative"]

n_distinct(visit_sites$GRID) # 2959903 individuals have at least one encounter at a care site with a defined specialty
n_distinct(visit_sites$visit_occurrence_id) # 45854959  visits have a care site with a defined specialty
n_distinct(visit_sites$visit_occurrence_id) / nrow(visit) # 66.2% of visits have a care site with a defined specialty
n_distinct(visit_sites$visit_occurrence_id) / nrow(visit[care_site_id != 0]) # 85.0% of visits with a non-null care site have a defined specialty

nrow(visit_sites) # 51108855 total rows

saveRDS(visit_sites, "/data/davis_lab/allie/care_sites/data/mdd_application/20230607_sd_pull_visit_occurrence_defined_sites_060825.Rds")

## restrict to MEGA cohort
visit_sites_mega <- visit_sites[GRID %in% dat$GRID]

## save
saveRDS(visit_sites_mega, "/data/davis_lab/allie/care_sites/data/mdd_application/20230607_sd_pull_mega_grids_visit_occurrence_defined_sites_060825.Rds")


