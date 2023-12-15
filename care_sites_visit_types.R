#### setup and load data ####
library(data.table)
library(dplyr)
library(foreach)
options(stringsAsFactors = F)

setwd("/data/davis_lab/allie/care_sites")

if (FALSE) {
  dat <- fread("20230607_sd_pull_care_site_all.csv")
  dat_x <- fread("20230712_x_care_site_all.csv")
  dat_merge <- merge(dat,dat_x,by="care_site_id",all.x=T)
  fwrite(dat_merge,"care_sites_x_merged.csv")
}

caresite <- fread("care_sites_x_merged.csv")
visit <- fread("/data/davis_lab/shared/phenotype_data/biovu/delivered_data/sd_wide_pull/20230607_pull/20230607_sd_pull_visit_occurrence_all.txt.gz")
visit_detail <- fread("/data/davis_lab/shared/phenotype_data/biovu/delivered_data/sd_wide_pull/20230607_pull/20230607_sd_pull_visit_detail.txt.gz")

#### basic sum stats ####
visit_summ <-  visit %>%
  group_by(care_site_id) %>%
  count() %>%
  arrange(-n) %>%
  left_join(caresite,"care_site_id")

grids1 <- unique(visit$GRID)
grids2 <- unique(visit_detail$GRID)
all(grids2 %in% grids1) # yes

visitDetail_summ <- visit_detail %>%
  group_by(care_site_id) %>%
  count() %>%
  arrange(-n) %>%
  left_join(caresite,"care_site_id")

visit_visitDetail_merged_summ <- merge(data.table(visit_summ)[,.(care_site_id,n)],
                                       data.table(visitDetail_summ)[,.(care_site_id,n)],
                                       by="care_site_id", all=T)
visit_visitDetail_merged_summ[is.na(n.x), n.x:=0]
visit_visitDetail_merged_summ[is.na(n.y), n.y:=0]
visit_visitDetail_merged_summ <- merge(caresite, visit_visitDetail_merged_summ, by="care_site_id")

unique_to_visitOcc <- visit_visitDetail_merged_summ[n.y==0]
unique_to_visitDetail <- visit_visitDetail_merged_summ[n.x==0]

#### merge visit_occurrence and visit_detail tables ####
visit$table <- "visit_occurrence"
visit_detail$table <- "visit_detail"

setnames(visit_detail,"visit_detail_concept_id","visit_concept_id")

visit <- unique(visit[,.(GRID,visit_occurrence_id,visit_concept_id,visit_start_date,visit_end_date,care_site_id,table)])
visit_detail <- unique(visit_detail[,.(GRID,visit_occurrence_id,visit_concept_id,visit_start_date,visit_end_date,care_site_id,table)])

visits_all <- unique(rbind(visit,visit_detail))

visits_all[visit_concept_id=="9201", visit_type := "inpatient"]
visits_all[visit_concept_id=="9202", visit_type := "outpatient"]
visits_all[visit_concept_id=="9203", visit_type := "er"]
visits_all[visit_concept_id=="0", visit_type := "unknown"]

visits_all[,visit_occurrence_id:=NULL]
visits_all[,visit_concept_id:=NULL]
visits_all <- unique(visits_all)

## count visit types per care site - visit occurrence table only for now!
visits_sites <- left_join(visits_all, caresite, by="care_site_id")
visits_sites_summ <- visits_sites[table=="visit_occurrence"] %>%
  group_by(care_site_id, visit_type) %>%
  count()

visits_sites_summ_wide <- dcast(data.table(visits_sites_summ), care_site_id ~ visit_type, value.var = "n", fill=0)
visits_sites_summ_wide[is.na(care_site_id), care_site_id := 99999]

visits_sites_summ_wide[,n := er+outpatient+inpatient+unknown]
visits_sites_summ_wide[,perc_outpatient := outpatient/n*100]
visits_sites_summ_wide[,perc_inpatient := inpatient/n*100]
visits_sites_summ_wide[,perc_er := er/n*100]
visits_sites_summ_wide[,perc_unknown := unknown/n*100]

visits_sites_summ_wide[perc_outpatient>=75, setting := "outpatient"]
visits_sites_summ_wide[perc_inpatient>=75, setting := "inpatient"]
visits_sites_summ_wide[perc_er>=75, setting := "er"]
visits_sites_summ_wide[perc_unknown>=75, setting := "unknown"]
visits_sites_summ_wide[is.na(setting), setting := "unknown"]

setorder(visits_sites_summ_wide, care_site_id)

fwrite(visits_sites_summ_wide, "care_site_visit_type_summary.csv")


