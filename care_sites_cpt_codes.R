#### setup and load data ####
library(data.table)
library(dplyr)
library(foreach)
setDTthreads(0)
options(stringsAsFactors = F)

setwd("/data/davis_lab/allie/care_sites")

caresite <- fread("care_sites_x_merged.csv")
visit <- fread("/data/davis_lab/shared/phenotype_data/biovu/delivered_data/sd_wide_pull/20230607_pull/20230607_sd_pull_visit_occurrence_all.txt.gz")

cpt <- readRDS("/data/davis_lab/allie/commonly_used/20230607_sd_pull_x_codes_proc.Rds")

## fix missing visit end dates
visit[is.na(visit_end_date), visit_end_date := visit_start_date]
nrow(visit) # 70672052

## remove visits with care_site_id = 0 (null care site)
visit <- visit[care_site_id!=0]
nrow(visit) # 54679804

## remove unneeded columns
visit <- unique(visit[, .(GRID, visit_start_date, visit_end_date, care_site_id)])
nrow(visit) # 52637196

nrow(cpt) # 219419477
cpt <- unique(cpt[, .(GRID, entry_date, concept_code, concept_name, vocabulary_id)])
nrow(cpt) # 208576799

## find cpt codes co-occurring with each visit
## this join exceeds physical limits for working memory so will have to break into smaller chunks
grids_all <- data.frame(grid = sort(unique(cpt$GRID)))
grids_all$index <- 1:nrow(grids_all)
grids_all$bin <- floor(grids_all$index/1e4 +1)
grids_all <- data.table(grids_all)

cpt_visit_comb <- foreach (i=1:max(grids_all$bin), .combine=rbind) %do% {
  grids <- grids_all[bin==i,grid]
  cpt_visit <- merge(visit, cpt[GRID %in% grids], by="GRID", allow.cartesian = T)
  cpt_visit <- unique(cpt_visit[entry_date >= visit_start_date & entry_date <= visit_end_date])
  cpt_visit
}

care_site_ids <- unique(cpt_visit_comb$care_site_id)
cpt_summ_all <- foreach(i = 1:length(care_site_ids), .combine = rbind) %do% {
  this_id <- care_site_ids[i]

  cpt_summ <- cpt_visit_comb[care_site_id == this_id] %>%
    group_by(concept_name) %>%
    count() %>%
    arrange(-n)

  ## get top 5 cpt codes per care site
  out <- data.table(cpt_summ[1:5,]) %>% rename('description' = 'concept_name')
  out <- out[!is.na(n)]
  out$care_site_id <- this_id
  out[, .(care_site_id, description, n)]
}

cpt_summ_concat <- foreach(i = 1:length(care_site_ids), .combine = rbind) %do% {
  this_id <- care_site_ids[i]
  this_dat <- cpt_summ_all[care_site_id == this_id]
  out <- data.table(
    care_site_id = this_id,
    top_proc_codes = paste0(this_dat$description, collapse = ", ")
  )
  out
}

fwrite(cpt_summ_concat, "care_sites_top5_proc_codes.csv")
