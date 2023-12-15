#### setup and load data ####
library(data.table)
library(dplyr)
library(foreach)
options(stringsAsFactors = F)

setwd("/data/davis_lab/allie/care_sites")

caresite <- fread("care_sites_x_merged.csv")
visit <- fread("/data/davis_lab/shared/phenotype_data/biovu/delivered_data/sd_wide_pull/20230607_pull/20230607_sd_pull_visit_occurrence_all.txt.gz")
visit_detail <- fread("/data/davis_lab/shared/phenotype_data/biovu/delivered_data/sd_wide_pull/20230607_pull/20230607_sd_pull_visit_detail.txt.gz")

#### summarize by top co-occurring phecodes ####
require(PheWAS)

## map icd codes to phecodes
icd_fmt <- unique(icd[,.(GRID,vocabulary_id,concept_code,entry_date)])
names(icd_fmt) <- c("id","vocabulary_id","code","date")

icd_mapped <- addPhecodeInfo(mapCodesToPhecodes(icd_fmt, make.distinct = F))
icd_mapped <- unique(icd_mapped[,.(id,phecode,description,group)])
setnames(icd_mapped,"id","GRID")

## fix missing visit end dates
visit[is.na(visit_end_date), visit_end_date := visit_start_date]

## find phecodes co-occurring with each visit
## this join exceeds physical limits for working memory so will have to break into smaller chunks
grids_all <- data.frame(grid = sort(unique(icd_mapped$GRID)))
grids_all$index <- 1:nrow(grids_all)
grids_all$bin <- floor(grids_all$index/1e5 +1)
grids_all <- data.table(grids_all)

res <- foreach (i=1:32, .combine=rbind) %do% {
  grids <- grids_all[bin==i,grid]
  phe_visit <- merge(visit, icd_mapped[GRID %in% grids], by="GRID", allow.cartesian = T)
  phe_visit <- unique(phe_visit[entry_date >= visit_start_date & entry_date <= visit_end_date])
  phe_visit
}




