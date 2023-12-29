#### setup and load data ####
library(data.table)
library(dplyr)
library(foreach)
library(PheWAS)
setDTthreads(0)
options(stringsAsFactors = F)

setwd("/data/davis_lab/allie/care_sites")

caresite <- fread("care_sites_x_merged.csv")
visit <- fread("/data/davis_lab/shared/phenotype_data/biovu/delivered_data/sd_wide_pull/20230607_pull/20230607_sd_pull_visit_occurrence_all.txt.gz")

icd <- readRDS("/data/davis_lab/allie/commonly_used/20230607_sd_pull_x_codes_icd.Rds")

## map icd codes to phecodes
icd_fmt <- unique(icd[,.(GRID,vocabulary_id,concept_code,entry_date)])
names(icd_fmt) <- c("id","vocabulary_id","code","date")

icd_mapped <- addPhecodeInfo(mapCodesToPhecodes(icd_fmt, make.distinct = F)) # N = 244195185
icd_mapped <- unique(icd_mapped) # N = 209211733
setnames(icd_mapped,"id","GRID")

## fix missing visit end dates
visit[is.na(visit_end_date), visit_end_date := visit_start_date]

## remove visits with care_site_id = 0 (null care site)
visit <- visit[care_site_id != 0]

## remove unneeded columns
visit <- unique(visit[, .(GRID, visit_start_date, visit_end_date, care_site_id)])

## find phecodes co-occurring with each visit
## this join exceeds physical limits for working memory so will have to break into smaller chunks
grids_all <- data.frame(grid = sort(unique(icd_mapped$GRID)))
grids_all$index <- 1:nrow(grids_all)
grids_all$bin <- floor(grids_all$index/1e4 +1)
grids_all <- data.table(grids_all)

phe_visit_comb <- foreach (i=1:max(grids_all$bin), .combine=rbind) %do% {
  grids <- grids_all[bin==i,grid]
  phe_visit <- merge(visit, icd_mapped[GRID %in% grids], by="GRID", allow.cartesian = T)
  phe_visit <- unique(phe_visit[date >= visit_start_date & date <= visit_end_date])
  phe_visit
}

care_site_ids <- unique(phe_visit_comb$care_site_id)
phe_summ_all <- foreach (i=1:length(care_site_ids), .combine=rbind) %do% {
  this_id <- care_site_ids[i]

  phe_summ <- phe_visit_comb[care_site_id==this_id] %>%
    group_by(description) %>%
    count() %>%
    arrange(-n)

  ## get top 5 phecodes per care site
  out <- data.table(phe_summ[1:5,])
  out <- out[!is.na(n)]
  out$care_site_id <- this_id
  out[,.(care_site_id,description,n)]
}

phe_summ_concat <- foreach (i=1:length(care_site_ids), .combine=rbind) %do% {
  this_id <- care_site_ids[i]
  this_dat <- phe_summ_all[care_site_id==this_id]
  out <- data.table(care_site_id = this_id,
                    top_phecodes = paste0(this_dat$description, collapse=", "))
  out
}

fwrite(phe_summ_concat, "care_sites_top5_phecodes.csv")
