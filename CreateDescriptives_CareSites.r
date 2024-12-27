library(vroom)
library(dplyr)
library(tidyr)
library(openxlsx)
library(ggplot2)
library(ggsci)
library(ggrepel)
library(scales)
library(reshape2)
library(data.table)
devtools::install_github("PheWAS/PheWAS")

setwd("/panfs/accrepfs.vampire/data/davis_lab/allie/care_sites/")

######## Import previous version of care site map (CareSite_map_011024_release-1.0-beta) ########
CareSiteMap <- read.xlsx("./data/CareSite_map_011024_release-1/CareSite_Map_011024.xlsx",1,colNames=T,cols=c(1,4:22))

# Look at descriptive variables that need to be defined again
names(CareSiteMap)
# [1] "care_site_id"         "VisitCount"           "EarliestVisitDate"    "LatestVisitDate"     
# [5] "care_site_name"       "location_id"          "type"                 "x_dept_abbreviation" 
# [9] "x_dept_center"        "x_care_area"          "x_rev_loc_name"       "setting"             
# [13] "specialty"            "Specialty_Age"        "MultipleSpecialty"    "OtherSpecialty_1"    
# [17] "OtherSpecialty_2"     "TopPheCodes"          "TopProcCodes"         "Provider_specialties"

CareSiteMap %>% group_by(specialty,OtherSpecialty_1,OtherSpecialty_2) %>% 
  summarise(count=sum(VisitCount)) %>% arrange(desc(count))

# Define alternative maps
CareSiteMap_Wide <- CareSiteMap %>% select(1,13,15:17) %>% replace(is.na(.), 0)
CareSiteMap_Long <- read.csv("./data/CareSite_map_011024_release-1/CareSite_Map_Multispecialty_Long_Format_011024.csv")

# Note: Downstream issues when specialties with non-standard characters aren't changed.
CareSiteMap_Wide <- CareSiteMap_Wide %>%
  mutate(specialty = ifelse(specialty=="Dentistry/OMFS", "Dentistry", specialty)) %>%
  mutate(OtherSpecialty_1 = ifelse(OtherSpecialty_1=="Dentistry/OMFS", "Dentistry", OtherSpecialty_1)) %>%
  mutate(OtherSpecialty_2 = ifelse(OtherSpecialty_2=="Dentistry/OMFS", "Dentistry", OtherSpecialty_2)) %>%
  mutate(specialty = ifelse(specialty=="Gender-affirming care", "GenderAffirmingCare", specialty)) %>%
  mutate(OtherSpecialty_1 = ifelse(OtherSpecialty_1=="Gender-affirming care", "GenderAffirmingCare", OtherSpecialty_1)) %>%
  mutate(OtherSpecialty_2 = ifelse(OtherSpecialty_2=="Gender-affirming care", "GenderAffirmingCare", OtherSpecialty_2))

CareSiteMap_Long <- CareSiteMap_Long %>%
  mutate(SpecialtyLong = ifelse(SpecialtyLong=="Dentistry/OMFS", "Dentistry", SpecialtyLong)) %>%
  mutate(SpecialtyLong = ifelse(SpecialtyLong=="Gender-affirming care", "GenderAffirmingCare", SpecialtyLong))

######## Create initial care site map ########
care_site <- as.data.frame(fread("/data/davis_lab/shared/phenotype_data/biovu/delivered_data/sd_wide_pull/20230607_pull/20230607_sd_pull_care_site_all.txt.gz"))
x_care_site <- as.data.frame(fread("/data/davis_lab/shared/phenotype_data/biovu/delivered_data/sd_wide_pull/20230607_pull/20230607_sd_pull_x_care_site_all.txt.gz"))

care_site_joined <- left_join(care_site, x_care_site, by="care_site_id") %>% 
  mutate(care_site_id=as.numeric(care_site_id)) %>%
  rename(dept_specialty="specialty")

######## Table 1: Characterize care site by visit number and visit type ######## 
visit_occurrence <- as.data.frame(fread("/data/davis_lab/shared/phenotype_data/biovu/delivered_data/sd_wide_pull/20230607_pull/20230607_sd_pull_visit_occurrence_all.txt.gz"))
concept <- as.data.frame(fread("/data/davis_lab/shared/phenotype_data/biovu/delivered_data/sd_wide_pull/20230607_pull/20230607_sd_pull_concept_all.txt.gz"))

# Calculate frequency of visit location (visit_concept_id) for each care_site_id
# View(concept[concept$concept_id %in% c("9201","9202","9203"),])

visit_counts <- visit_occurrence %>% 
  filter(visit_start_date >= "1980-01-01") %>%
  mutate(visit_location = case_when(visit_concept_id == "9201" ~ "Inpatient",
                                    visit_concept_id == "9202" ~ "Outpatient",
                                    visit_concept_id == "9203" ~ "ER",
                                    visit_concept_id == "0" ~ "Unspecified",
                                    TRUE ~ NA_character_)) %>%
  group_by(care_site_id) %>%
  summarise(Visits_N=n(),
            Outpt_N = sum(visit_location=="Outpatient"),
            Outpt_Percent = paste0(round(100*Outpt_N/Visits_N,2),'%'),
            Inpt_N = sum(visit_location=="Inpatient"),
            Inpt_Percent = paste0(round(100*Inpt_N/Visits_N,2),'%'),
            Emer_N = sum(visit_location=="ER"),
            Emer_Percent = paste0(round(100*Emer_N/Visits_N,2),'%'),
            Unspecified_N = sum(visit_location=="Unspecified"),
            Unspecified_Percent = paste0(round(100*Unspecified_N/Visits_N,2),'%'),
            FirstVisit=min(visit_start_date), 
            LastVisit=max(visit_start_date)) %>%
  select(-c("Outpt_N","Inpt_N","Emer_N","Unspecified_N"))

######## Table 2: Characterize care site by patient demographics ######## 
person <- as.data.frame(fread("/data/davis_lab/shared/phenotype_data/biovu/delivered_data/sd_wide_pull/20230607_pull/20230607_sd_pull_person_all.txt.gz"))

# Calculate frequency of patient age, age specificity, gender, and gender specificity
patient_demographics <- visit_occurrence %>% 
  filter(visit_start_date >= "1980-01-01") %>%
  select(GRID,care_site_id,visit_start_date) %>%
  left_join(person[,c(12,13,6)],by="GRID") %>%
  mutate(DOB = as.Date(birth_datetime),
         visit_start_date = as.Date(visit_start_date),
         VisitAge = as.numeric((visit_start_date-DOB)/365.25),
         Gender = gender_source_value) %>%
  filter(VisitAge>=0) %>%
  group_by(care_site_id) %>%
  summarise(Visits_N=n(),
            Age_Median = median(VisitAge),
            Adult_N = sum(VisitAge>18),
            Adult_Percent = paste0(round(100*Adult_N/Visits_N,2),'%'),
            Female_N = sum(Gender=="F"),
            Female_Percent = paste0(round(100*Female_N/Visits_N,2),'%'),
            GenderSpecific = ifelse(Female_Percent>=75, "Female-Specific", 
                                    ifelse(Female_Percent<25, "Male-Specific",
                                           "NonSpecific")),
            AgeSpecific = ifelse(Adult_Percent>=75, "Adult-Specific", 
                                 ifelse(Adult_Percent<25, "Pediatric-Specific",
                                        "NonSpecific"))) %>%
  select(-c("Visits_N","Adult_N","Female_N"))

######## Table 3: Top ICD and CPT codes for each care site ######## 
### Import ICD codes and create phecodes 
x_codes <- as.data.frame(fread("/data/davis_lab/shared/phenotype_data/biovu/delivered_data/sd_wide_pull/20230607_pull/20230607_sd_pull_x_codes_all.txt.gz"))

## ICD-9-CM
x_codes_icd9cm <- subset(x_codes, vocabulary_id == "ICD9CM")
names(x_codes_icd9cm) <- c("person_id", "GRID", "entry_date", "entry_datetime", "concept_id", 
                           "concept_name", "concept_code", "vocabulary_id", "visit_occurrence_id", "type_concept_id", 
                           "item_position", "age_at_event", "x_poa")

# Map to phecodes and visits
Phecodes_ICD9_Visits <- x_codes_icd9cm %>% select(7,9) %>% 
  left_join(PheWAS::phecode_map %>% filter(vocabulary_id=="ICD9CM"), by=c("concept_code"="code")) %>%
  left_join(visit_occurrence %>% select(3,11), by="visit_occurrence_id") %>% 
  filter(is.na(phecode)==FALSE & is.na(care_site_id)==FALSE & care_site_id!="0")

## Map ICD-10-CM codes to phecodes and phecodes to visits
x_codes_icd10cm <- subset(x_codes, vocabulary_id == "ICD10CM")
names(x_codes_icd10cm) <- c("person_id", "GRID", "entry_date", "entry_datetime", "concept_id", 
                           "concept_name", "concept_code", "vocabulary_id", "visit_occurrence_id", "type_concept_id", 
                           "item_position", "age_at_event", "x_poa")

# Map to phecodes and visits
Phecodes_ICD10_Visits <- x_codes_icd10cm %>% select(7,9) %>% 
  left_join(PheWAS::phecode_map %>% filter(vocabulary_id=="ICD10CM"), by=c("concept_code"="code")) %>%
  left_join(visit_occurrence %>% select(3,11), by="visit_occurrence_id") %>% 
  filter(is.na(phecode)==FALSE & is.na(care_site_id)==FALSE & care_site_id!="0")

# Calculate ICD-9 and ICD-10 code frequencies
phecodecount_bycaresite <- rbind(Phecodes_ICD9_Visits,Phecodes_ICD10_Visits) %>% 
  group_by(care_site_id,phecode) %>% summarise(CodeCount=n()) %>% ungroup(phecode) %>%
  left_join(PheWAS::pheinfo %>% select("phecode","description"), by="phecode") %>% filter(!is.na(description)) %>%
  arrange(desc(CodeCount)) %>% mutate(freqrank = row_number()) %>% ungroup(care_site_id) %>%
  arrange(care_site_id,freqrank) %>%
  as.data.frame()

pheinfo_anno <- PheWAS::pheinfo %>% select("phecode","description","group")

# Get top 5 phecodes 
top5phe <- phecodecount_bycaresite %>%
  filter(freqrank <= 5) %>%
  select(c("care_site_id","phecode","CodeCount","freqrank")) %>%
  pivot_wider(id_cols = care_site_id, values_from = phecode, names_from = freqrank, names_prefix = "pherank_") %>%
  left_join(pheinfo_anno, by=c("pherank_1" = "phecode")) %>% rename(pherank_1_description = "description", pherank_1_category = "group") %>%
  left_join(pheinfo_anno, by=c("pherank_2" = "phecode")) %>% rename(pherank_2_description = "description", pherank_2_category = "group") %>%
  left_join(pheinfo_anno, by=c("pherank_3" = "phecode")) %>% rename(pherank_3_description = "description", pherank_3_category = "group") %>%
  left_join(pheinfo_anno, by=c("pherank_4" = "phecode")) %>% rename(pherank_4_description = "description", pherank_4_category = "group") %>%
  left_join(pheinfo_anno, by=c("pherank_5" = "phecode")) %>% rename(pherank_5_description = "description", pherank_5_category = "group") %>%
  select("care_site_id", "pherank_1", "pherank_1_description", "pherank_1_category", 
         "pherank_2", "pherank_2_description", "pherank_2_category",
         "pherank_3", "pherank_3_description", "pherank_3_category", 
         "pherank_4", "pherank_4_description", "pherank_4_category",
         "pherank_5", "pherank_5_description", "pherank_5_category")

######## Table 4: Get top 5 CPT codes for each care site ######## 
# Import CPT-4 codes 
x_codes_cpt4 <- subset(x_codes, vocabulary_id == "CPT4")
names(x_codes_cpt4) <- c("person_id", "GRID", "entry_date", "entry_datetime", "concept_id", 
                            "concept_name", "concept_code", "vocabulary_id", "visit_occurrence_id", "type_concept_id", 
                            "item_position", "age_at_event", "x_poa")

# Map to CPT codes and visits
CPTcodes_Visits <- x_codes_cpt4 %>% select(6,7,9) %>% 
  left_join(visit_occurrence %>% select(3,11), by="visit_occurrence_id") %>% 
  filter(is.na(concept_code)==FALSE & is.na(care_site_id)==FALSE & care_site_id!="0")

# Calculate CPT code frequencies
cptcount_bycaresite <- CPTcodes_Visits %>% 
  group_by(care_site_id,concept_code,concept_name) %>% summarise(CodeCount=n()) %>% ungroup(concept_code,concept_name) %>%
  arrange(desc(CodeCount)) %>% mutate(freqrank = row_number()) %>% ungroup(care_site_id) %>%
  arrange(care_site_id,freqrank) %>%
  as.data.frame()

cptinfo_anno <- cptcount_bycaresite %>% distinct(concept_code,concept_name)

# Get top 5 cpt codes 
top5cpt <- cptcount_bycaresite %>%
  filter(freqrank <= 5) %>%
  select(c("care_site_id","concept_code","CodeCount","freqrank")) %>%
  pivot_wider(id_cols = care_site_id, values_from = concept_code, names_from = freqrank, names_prefix = "cptrank_") %>%
  left_join(cptinfo_anno, by=c("cptrank_1" = "concept_code")) %>% rename(cptrank_1_description = "concept_name") %>%
  left_join(cptinfo_anno, by=c("cptrank_2" = "concept_code")) %>% rename(cptrank_2_description = "concept_name") %>%
  left_join(cptinfo_anno, by=c("cptrank_3" = "concept_code")) %>% rename(cptrank_3_description = "concept_name") %>%
  left_join(cptinfo_anno, by=c("cptrank_4" = "concept_code")) %>% rename(cptrank_4_description = "concept_name") %>%
  left_join(cptinfo_anno, by=c("cptrank_5" = "concept_code")) %>% rename(cptrank_5_description = "concept_name") %>%
  select("care_site_id", "cptrank_1", "cptrank_1_description", 
         "cptrank_2", "cptrank_2_description",
         "cptrank_3", "cptrank_3_description", 
         "cptrank_4", "cptrank_4_description",
         "cptrank_5", "cptrank_5_description")

######## Create table with all variables ######## 
fwrite(care_site_joined, file="./output/CareSiteDescriptives/omop_care_site_info.csv")
fwrite(visit_counts, file="./output/CareSiteDescriptives/visit_counts_by_caresite.csv")
fwrite(patient_demographics, file="./output/CareSiteDescriptives/patient_demographics_by_caresite.csv")
fwrite(top5phe, file="./output/CareSiteDescriptives/top5phecodes_by_caresite.csv")
fwrite(top5cpt, file="./output/CareSiteDescriptives/top5cptcodes_by_caresite.csv")

care_site_summary <- CareSiteMap_Wide %>%
  left_join(care_site_joined,by="care_site_id") %>%
  left_join(visit_counts,by="care_site_id") %>%
  left_join(patient_demographics,by="care_site_id") %>%
  left_join(top5phe,by="care_site_id") %>%
  left_join(top5cpt,by="care_site_id")

FinalSummary_CareSites <- care_site_summary %>% 
  rename(MappedSpecialty = "specialty", 
         IsMultiSpecialty = "MultipleSpecialty", 
         MultiSpecialty_Secondary = "OtherSpecialty_1", 
         MultiSpecialty_Tertiary = "OtherSpecialty_2") %>%
  mutate(IsMultiSpecialty=as.logical(IsMultiSpecialty))

write.xlsx(FinalSummary_CareSites, file="./output/CareSite_VisitDetailCount_UPDATED_111524.xlsx", 
           quote = T, rowNames = F, colWidths = "auto")

FinalSummary_CareSites_MultipleSpecialty <- FinalSummary_CareSites %>% 
  select("care_site_id", "MappedSpecialty", "MultiSpecialty_Secondary", "MultiSpecialty_Tertiary")

FinalSummary_CareSites_MultipleSpecialty %>% 
  count(MappedSpecialty==0, MultiSpecialty_Secondary==0, MultiSpecialty_Tertiary==0)
# There should be 2258 single specialty sites + 315 double specialty sites + 7 triple specialty sites
# Total in LONG format: 2258 + 315*2 + 7*3 = 2909

CareSite_Summary_Long <- reshape2::melt(FinalSummary_CareSites_MultipleSpecialty, 
                              id_vars="care_site_id", 
                              measure.vars = c("MappedSpecialty", "MultiSpecialty_Secondary", "MultiSpecialty_Tertiary")) 

CareSite_Summary_Long_Export <- CareSite_Summary_Long %>% 
  filter(value!=0) %>%
  mutate(SpecialtyLong = value) %>% 
  select("care_site_id", "SpecialtyLong")

write.csv(CareSite_Summary_Long_Export, 
          file="./output/CareSiteMap_Multispecialty_Long_UPDATED_111524.csv", row.names=F)

######## Write Supplemental Table for top 25 care sites ######## 
All <- subset(FinalSummary_CareSites,MappedSpecialty!="Unclear") %>% 
  arrange(desc(Visits_N)) %>% 
  select("care_site_id", "MappedSpecialty","IsMultiSpecialty","MultiSpecialty_Secondary","MultiSpecialty_Tertiary")

sheets <- list("VisitType" = left_join(All, visit_counts, by="care_site_id"), 
               "PatientDemographics" = left_join(All, patient_demographics, by="care_site_id"), 
               "Top5_Phecodes" = left_join(All, top5phe, by="care_site_id"),
               "Top5_CPTcodes" = left_join(All, top5cpt, by="care_site_id"))

write.xlsx(sheets, "./output/SupplementalTable_All_MostCommonCareSites.xlsx", colWidths = "auto")

# Top 25
Top25 <- subset(FinalSummary_CareSites,MappedSpecialty!="Unclear") %>% 
  arrange(desc(Visits_N)) %>% filter(row_number()<=25) %>% 
  select("care_site_id", "MappedSpecialty","IsMultiSpecialty","MultiSpecialty_Secondary","MultiSpecialty_Tertiary")

sheets <- list("VisitType" = left_join(Top25, visit_counts, by="care_site_id"), 
          "PatientDemographics" = left_join(Top25, patient_demographics, by="care_site_id"), 
          "Top5_Phecodes" = left_join(Top25, top5phe, by="care_site_id"),
          "Top5_CPTcodes" = left_join(Top25, top5cpt, by="care_site_id"))

write.xlsx(sheets, "./output/SupplementalTable_Top25MostCommonCareSites.xlsx", colWidths = "auto")
