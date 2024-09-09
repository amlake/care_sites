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

## map visit types
visit[visit_concept_id == "9201", visit_type := "inpatient"]
visit[visit_concept_id == "9202", visit_type := "outpatient"]
visit[visit_concept_id == "9203", visit_type := "er"]
visit[visit_concept_id == "0", visit_type := "unknown"]

visit[, visit_occurrence_id := NULL]
visit[, visit_concept_id := NULL]

## count visit types per care site 
visits_sites <- left_join(visit, caresite, by="care_site_id")
visits_sites_summ <- visits_sites %>%
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

fwrite(visits_sites_summ_wide, "care_site_visit_type_summary_121523.csv")


