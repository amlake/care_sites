library(sqldf)
library(vroom)
library(data.table)
library(dplyr)
library(openxlsx)

setwd("/panfs/accrepfs.vampire/data/davis_lab/allie/care_sites")

####### Read in data
### Person
person <- vroom("/data/davis_lab/shared/phenotype_data/biovu/delivered_data/sd_wide_pull/20230607_pull/20230607_sd_pull_person_all.txt.gz", col_types="c")

demographics <- person %>% 
  mutate(DOB=as.Date(birth_datetime)) %>% 
  rename(EPIC_GENDER = "gender_source_value", EPIC_RACE = "race_source_value") %>%
  select(GRID, DOB, EPIC_GENDER, EPIC_RACE)

### Visits
visit_occurrence <- vroom("/data/davis_lab/shared/phenotype_data/biovu/delivered_data/sd_wide_pull/20230607_pull/20230607_sd_pull_visit_occurrence_all.txt.gz", col_types="c")
visits_cleaned <- visit_occurrence %>% 
  filter(visit_start_date >= "1980-01-01") %>%
  left_join(demographics[,c("GRID","EPIC_GENDER","DOB")], by="GRID") %>%
  filter(is.na(EPIC_GENDER)==FALSE) %>%
  mutate(VisitAge = as.numeric(visit_start_date-DOB)/365.25) %>%
  filter(VisitAge>=0) %>%
  select("GRID","visit_occurrence_id","visit_start_date","VisitAge","care_site_id")

visit_summary <- visits_cleaned %>% 
  group_by(GRID) %>%
  summarise(AGE_MEDIAN_VISIT=median(VisitAge), AGE_MIN_VISIT = min(VisitAge), AGE_MAX_VISIT = max(VisitAge)) %>% 
  as.data.frame()

# Write demographics file
specialtywas_demographics <- demographics %>% left_join(visit_summary, by="GRID")
write.csv(specialtywas_demographics, file="./data/SpecialtyWASFiles/SpecialtyWAS_Demographics_ENTIRESD.txt", row.names = F)

######### Create visit summary file 
### Care Site Map
CareSiteMap <- read.xlsx("./output/CareSite_VisitDetailCount_UPDATED_JPS_052325.xlsx", 1, colNames=T)
CareSiteMap_MultiSpecialty <- vroom("./output/CareSiteMap_Multispecialty_Long_UPDATED_JPS_052325.csv", delim=",")

## Primary specialty only
specialtyvisits_primaryonly <- visits_cleaned %>% 
  left_join(CareSiteMap %>% select(care_site_id,MappedSpecialty), by="care_site_id") %>%
  filter(is.na(MappedSpecialty)==FALSE & MappedSpecialty!="Unclear") %>%
  group_by(GRID, MappedSpecialty) %>% 
  summarise(visit_count=n(), first_date = min(visit_start_date), 
            minage = min(VisitAge), maxage = max(VisitAge)) %>%
  as.data.frame()

# All visits
fwrite(specialtyvisits_primaryonly, file="./data/SpecialtyWASFiles/SpecialtyVisits_PrimarySpecialty_ENTIRESD.txt")

### Multi-specialty
specialtyvisits_multi <- visits_cleaned %>% 
  left_join(CareSiteMap_MultiSpecialty, by="care_site_id") %>%
  rename(MappedSpecialty="SpecialtyLong") %>%
  filter(MappedSpecialty!="Unclear") %>%
  group_by(GRID, MappedSpecialty) %>% 
  summarise(visit_count=n(), first_date = min(visit_start_date), 
            minage = min(VisitAge), maxage = max(VisitAge)) %>%
  as.data.frame()

# All visits
fwrite(specialtyvisits_multi, file="./data/SpecialtyWASFiles/SpecialtyVisits_Multispecialty_ENTIRESD.txt")

### Care sites
visits_bycaresite <- visits_cleaned %>% 
  left_join(CareSiteMap_MultiSpecialty, by="care_site_id") %>%
  rename(MappedSpecialty="SpecialtyLong") %>%
  filter(MappedSpecialty!="Unclear") %>%
  group_by(GRID,care_site_id) %>% 
  summarise(visit_count=n(), first_date = min(visit_start_date), 
            minage = min(VisitAge), maxage = max(VisitAge)) %>%
  as.data.frame()

# All visits
fwrite(visits_bycaresite, file="./data/SpecialtyWASFiles/CareSiteVisits_ENTIRESD.txt")

