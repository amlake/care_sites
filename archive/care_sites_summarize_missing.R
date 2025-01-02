## setup and load data
pacman::p_load(data.table, dplyr, foreach, openxlsx)
options(stringsAsFactors = F)

setwd("/data/davis_lab/allie/care_sites")

caresite <- as.data.table(read.xlsx("CareSite_VisitDetailCount_122923.xlsx")) %>%
  select(care_site_id, specialty, OtherSpecialty_1, OtherSpecialty_2)

caresite_unclear <- caresite[specialty == "Unclear"]
caresite_admin <- caresite[specialty == "Administrative"]

visit <- fread("/data/davis_lab/shared/phenotype_data/biovu/delivered_data/sd_wide_pull/20230607_pull/20230607_sd_pull_visit_occurrence_all.txt.gz")

## how many visits have null care_site_ids?
nrow(visit[is.na(care_site_id) | care_site_id == "" | care_site_id ==0])
nrow(visit)
# 15992248 / 70672052 are missing (23%)

## how many visits map to "unclear" caresites?
nrow(visit[care_site_id %in% caresite_unclear$care_site_id])
# 8161188 / 70672052  (12%)

## how many visits map to "administrative" caresites?
nrow(visit[care_site_id %in% caresite_admin$care_site_id])
# 47568 / 70672052  (<1%)


