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
library(PheWAS)

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

######## Table 1: Characterize care site by patient demographics ######## 
visit_occurrence <- as.data.frame(fread("data/sd_data_qc/20230607_sd_pull_visit_occurrence_dates_cleaned_overlapping_grids.txt"))
person <- as.data.frame(fread("data/sd_data_qc/20230607_sd_pull_person_dates_cleaned_overlapping_grids.txt"))

range(person$year_of_birth) # 1908-2023
range(visit_occurrence$visit_start_date) # 1998-01-01 to 2023-03-31

## Descriptives
# Get proportion of care sites and visits excluded
visitcounts <- visit_occurrence %>% 
  select(GRID,care_site_id,visit_start_date,visit_concept_id) %>%
  left_join(person[,c(12,13,6)],by="GRID") %>%
  left_join(CareSiteMap_Wide,by="care_site_id") %>%
  mutate(DOB = as.Date(birth_datetime),
         visit_start_date = as.Date(visit_start_date),
         VisitAge = as.numeric((visit_start_date-DOB)/365.25),
         Gender = gender_source_value) %>%
  mutate(visit_location = case_when(visit_concept_id == "9201" ~ "Inpatient",
         visit_concept_id == "9202" ~ "Outpatient",
         visit_concept_id == "9203" ~ "ER",
         visit_concept_id == "0" ~ "Unspecified",
         TRUE ~ NA_character_))

range(visitcounts$VisitAge) # 0 - 89.99863
stopifnot(min(visitcounts$VisitAge) >= 0)
stopifnot(all(!is.na(visitcounts$care_site_id)))

# total number of visits and individuals
nrow(visitcounts) # 69307208 
n_distinct(visitcounts$GRID) # 3213825

# number of care sites and number of visits with a care site
visitcounts %>% filter(care_site_id !=0) %>% summarise(n_distinct(care_site_id)) # 2544
visitcounts %>% filter(care_site_id !=0) %>% nrow() # 53934856 (77.8%)

# numbers after filtering out visits with missing care site or unclear/administrative specialties
visitcounts_mapped <- visitcounts %>% filter(care_site_id !=0 & !(specialty %in% c("Administrative", "Unclear")))
nrow(visitcounts_mapped) # 45854959
visitcounts_mapped %>% summarise(n_distinct(GRID)) # 2959903
visitcounts_mapped %>% summarise(n_distinct(care_site_id)) # 2189 (86.0%)
visitcounts_mapped %>% summarise(n_distinct(specialty)) # 58

# administrative and unclear care sites
visitcounts %>% filter(specialty == "Administrative") %>% summarise(n_distinct(care_site_id)) # 34 administrative sites
visitcounts %>% filter(specialty == "Unclear") %>% summarise(n_distinct(care_site_id)) # X unclear sites
visitcounts %>% filter(care_site_id !=0 & specialty == "Administrative") %>% nrow() # 28870 administrative visits
visitcounts %>% filter(care_site_id !=0 & specialty == "Unclear") %>% nrow() # 8051027 unclear visits

fwrite(visitcounts, file="./output/CareSiteDescriptives_AML_010425/all_visit_counts.csv")

## Make demographic summary for each care site
patient_demographics <- visit_occurrence %>% 
  select(GRID,care_site_id,visit_start_date) %>%
  left_join(person[,c(12,13,6)],by="GRID") %>%
  mutate(DOB = as.Date(birth_datetime),
         visit_start_date = as.Date(visit_start_date),
         VisitYear = format(as.Date(visit_start_date), "%Y"),
         VisitAge = as.numeric((visit_start_date-DOB)/365.25),
         Gender = gender_source_value) %>%
  group_by(care_site_id) %>%
  summarise(Person_N=n_distinct(GRID),
            Visits_N=n(),
            VisitYear_Median = median(as.numeric(VisitYear)),
            Age_Median = median(VisitAge),
            Adult_N = sum(VisitAge>18),
            Adult_Percent_Numeric = 100*Adult_N/Visits_N,
            Adult_Percent = paste0(round(Adult_Percent_Numeric,2),'%'),
            Female_N = sum(Gender=="F"),
            Female_Percent_Numeric = 100*Female_N/Visits_N,
            Female_Percent = paste0(round(Female_Percent_Numeric,2),'%'),
            GenderSpecific = ifelse(Female_Percent_Numeric>=75, "Female-Specific", 
                                    ifelse(Female_Percent_Numeric<25, "Male-Specific",
                                           "NonSpecific")),
            AgeSpecific = ifelse(Adult_Percent_Numeric>=75, "Adult-Specific", 
                                 ifelse(Adult_Percent_Numeric<25, "Pediatric-Specific",
                                        "NonSpecific")))

######## Table 2: Characterize care site by visit number and visit type ######## 
## Calculate frequency of visit location (visit_concept_id) for each care_site_id
# concept <- as.data.frame(fread("/data/davis_lab/shared/phenotype_data/biovu/delivered_data/sd_wide_pull/20230607_pull/20230607_sd_pull_concept_all.txt.gz"))
# View(concept[concept$concept_id %in% c("9201","9202","9203"),])

caresite_visitcounts <- visit_occurrence %>% 
  left_join(person[,c(12,13,6)],by="GRID") %>%
  mutate(DOB = as.Date(birth_datetime),
         visit_start_date = as.Date(visit_start_date),
         VisitYear = format(as.Date(visit_start_date), "%Y"),
         VisitAge = as.numeric((visit_start_date-DOB)/365.25),
         Gender = gender_source_value) %>%
  mutate(visit_location = case_when(visit_concept_id == "9201" ~ "Inpatient",
                                    visit_concept_id == "9202" ~ "Outpatient",
                                    visit_concept_id == "9203" ~ "ER",
                                    visit_concept_id == "0" ~ "Unspecified",
                                    TRUE ~ NA_character_)) %>%
  group_by(care_site_id) %>%
  summarise(Visits_N=n(),
            Outpt_N = sum(visit_location=="Outpatient"),
            Outpt_Percent_Numeric = 100*Outpt_N/Visits_N,
            Outpt_Percent = paste0(round(Outpt_Percent_Numeric,2),'%'),
            Inpt_N = sum(visit_location=="Inpatient"),
            Inpt_Percent_Numeric = 100*Inpt_N/Visits_N,
            Inpt_Percent = paste0(round(Inpt_Percent_Numeric,2),'%'),
            Emer_N = sum(visit_location=="ER"),
            Emer_Percent_Numeric = 100*Emer_N/Visits_N,
            Emer_Percent = paste0(round(Emer_Percent_Numeric,2),'%'),
            Unspecified_N = sum(visit_location=="Unspecified"),
            Unspecified_Percent_Numeric = 100*Unspecified_N/Visits_N,
            Unspecified_Percent = paste0(round(Unspecified_Percent_Numeric,2),'%'),
            MajorityVisitType = case_when(
              Outpt_Percent_Numeric >= 75 ~ "Outpatient",
              Inpt_Percent_Numeric >= 75 ~ "Inpatient",
              Emer_Percent_Numeric >= 75 ~ "Emergency",
              TRUE ~ "Mixed"
            ),
            FirstVisit=min(visit_start_date), 
            LastVisit=max(visit_start_date))

######## Table 3: Top ICD and CPT codes for each care site ######## 
### Import ICD codes and create phecodes 
# x_codes <- as.data.frame(fread("data/sd_data_qc/20230607_sd_pull_x_codes_dates_cleaned_overlapping_grids.txt"))

## @John I'm using the R PheWAS mapping function for simplicity
## Also it should handle the many-to-many mapping between ICD codes and phecodes by using PheWAS::phecode_rollup_map in addition to PheWAS::phecode_map

# Map to phecodes, remove duplicates (i.e. remove if the same phecode is mapped to the same visit more than once)
x_codes_icd <- subset(x_codes, vocabulary_id %in% c("ICD9CM","ICD10CM")) %>% 
  select(id = GRID, vocabulary_id, code = concept_code, visit_occurrence_id) %>%
  distinct()

phecodes <- addPhecodeInfo(mapCodesToPhecodes(x_codes_icd, make.distinct = F)) %>%
  select(GRID = id, phecode, description, group, visit_occurrence_id) %>%
  distinct()

# Map phecodes to visits
phecodes_visits <- phecodes %>% 
  left_join(visit_occurrence %>% select(3,11), by="visit_occurrence_id") %>% 
  filter(is.na(phecode)==FALSE & is.na(care_site_id)==FALSE & care_site_id!="0")

# Calculate phecode frequencies
phecodecount_bycaresite <- phecodes_visits %>% 
  group_by(care_site_id) %>% 
  mutate(UniquePhecodeCount = n_distinct(phecode), PersonCount = n_distinct(GRID)) %>%
  group_by(care_site_id,phecode,description,group) %>% summarise(CodeCount=n()) %>% ungroup(phecode,description,group) %>%
  arrange(desc(CodeCount)) %>% mutate(freqrank = row_number()) %>% ungroup(care_site_id) %>%
  arrange(care_site_id,freqrank) %>%
  as.data.frame()

## Make summary of top 5 codes for each care site
top5phe <- phecodecount_bycaresite %>%
  filter(freqrank <= 5) %>%
  select(care_site_id, phecode, description, category = group, CodeCount, freqrank) %>% 
  pivot_wider(id_cols = care_site_id, values_from = c(phecode, description, category), names_from = freqrank, names_prefix = "pherank_")

# change back to the column names that John had
colnames(top5phe) <- gsub("^(.*)_(pherank_\\d+)$", "\\2_\\1", colnames(top5phe))
colnames(top5phe) <- gsub("_phecode", "", colnames(top5phe))

top5phe <- top5phe %>% select("care_site_id", "pherank_1", "pherank_1_description", "pherank_1_category", 
         "pherank_2", "pherank_2_description", "pherank_2_category",
         "pherank_3", "pherank_3_description", "pherank_3_category", 
         "pherank_4", "pherank_4_description", "pherank_4_category",
         "pherank_5", "pherank_5_description", "pherank_5_category")

top100phe <- phecodecount_bycaresite %>%
  filter(freqrank <= 100) %>%
  select(c("care_site_id","phecode","CodeCount","freqrank"))

allphe_ranked <- phecodecount_bycaresite %>% select(care_site_id, phecode, CodeCount, freqrank)

######## Table 4: Get top 5 CPT codes for each care site ######## 
# Import CPT-4 codes 
x_codes_cpt4 <- subset(x_codes, vocabulary_id == "CPT4")
names(x_codes_cpt4) <- c("person_id", "GRID", "entry_date", "entry_datetime", "concept_id", 
                            "concept_name", "concept_code", "vocabulary_id", "visit_occurrence_id", "type_concept_id", 
                            "item_position", "age_at_event", "x_poa")

# Map to CPT codes and visits
CPTcodes_Visits <- x_codes_cpt4 %>% select(2,6,7,9) %>% 
  left_join(visit_occurrence %>% select(3,11), by="visit_occurrence_id") %>% 
  filter(is.na(concept_code)==FALSE & is.na(care_site_id)==FALSE & care_site_id!="0")

# Calculate CPT code frequencies
# @John I had to fix this error- maybe we're using different versions of dplyr?
# Error in `ungroup()`:
# ! `...` must be empty.
# ✖ Problematic argument:
# • ..1 = care_site_id
# ℹ Did you forget to name an argument?
# Had to run ungroup() and then group_by(care_site_id,concept_code,concept_name) to avoid error
# CPTcodes_Visits_test <- CPTcodes_Visits[sample(1:nrow(CPTcodes_Visits), 1e6),]
# cptcount_bycaresite <- CPTcodes_Visits_test %>%
cptcount_bycaresite <- CPTcodes_Visits %>% 
  group_by(care_site_id) %>% 
  mutate(UniqueCPTCount = n_distinct(concept_code), PersonCount = n_distinct(GRID)) %>%
  ungroup() %>%
  group_by(care_site_id,concept_code,concept_name) %>% summarise(CodeCount=n()) %>% ungroup(concept_code,concept_name) %>%
  arrange(desc(CodeCount)) %>% mutate(freqrank = row_number()) %>% ungroup(care_site_id) %>%
  arrange(care_site_id,freqrank) %>%
  as.data.frame()

cptinfo_anno <- cptcount_bycaresite %>% distinct(concept_code,concept_name)

## Make summary of top 5 codes for each care site
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

top100cpt <- cptcount_bycaresite %>%
  filter(freqrank <= 100) %>%
  select(c("care_site_id","concept_code","CodeCount","freqrank"))

allcpt_ranked <- cptcount_bycaresite %>% select(care_site_id, concept_code, CodeCount, freqrank)

######## Create table with all variables ######## 
fwrite(care_site_joined, file="./output/CareSiteDescriptives_AML_010425/omop_care_site_info.csv")
fwrite(caresite_visitcounts, file="./output/CareSiteDescriptives_AML_010425/visit_counts_by_caresite.csv")
fwrite(patient_demographics, file="./output/CareSiteDescriptives_AML_010425/patient_demographics_by_caresite.csv")
fwrite(top5phe, file="./output/CareSiteDescriptives_AML_010425/top5phecodes_by_caresite_wide.csv")
fwrite(top100phe, file="./output/CareSiteDescriptives_AML_010425/top100phecodes_by_caresite_long.csv")
fwrite(top5cpt, file="./output/CareSiteDescriptives_AML_010425/top5cptcodes_by_caresite_wide.csv")
fwrite(top100cpt, file="./output/CareSiteDescriptives_AML_010425/top100cptcodes_by_caresite_long.csv")
fwrite(allcpt_ranked, file="./output/CareSiteDescriptives_AML_010425/allcptcodes_by_caresite_long.csv")
fwrite(allphe_ranked, file = "./output/CareSiteDescriptives_AML_010425/allphecodes_by_caresite_long.csv")

care_site_joined <- fread(file="./output/CareSiteDescriptives_AML_010425/omop_care_site_info.csv")
caresite_visitcounts <- fread(file="./output/CareSiteDescriptives_AML_010425/visit_counts_by_caresite.csv")
patient_demographics <- fread(file="./output/CareSiteDescriptives_AML_010425/patient_demographics_by_caresite.csv")
top5phe <- fread(file="./output/CareSiteDescriptives_AML_010425/top5phecodes_by_caresite_wide.csv")
top100phe <- fread(file="./output/CareSiteDescriptives_AML_010425/top100phecodes_by_caresite_long.csv")
top5cpt <- fread(file="./output/CareSiteDescriptives_AML_010425/top5cptcodes_by_caresite_wide.csv")
top100cpt <- fread(file="./output/CareSiteDescriptives_AML_010425/top100cptcodes_by_caresite_long.csv")

care_site_summary <- CareSiteMap_Wide %>%
  left_join(care_site_joined,by="care_site_id") %>%
  left_join(patient_demographics,by="care_site_id") %>%
  left_join(caresite_visitcounts,by="care_site_id") %>%
  left_join(top5phe,by="care_site_id") %>%
  left_join(top5cpt,by="care_site_id")

stopifnot(care_site_summary %>% filter(Visits_N.x != Visits_N.y) %>% nrow() == 0) 
care_site_summary <- care_site_summary %>% select(-Visits_N.y) %>% rename(Visits_N = Visits_N.x)

FinalSummary_CareSites <- care_site_summary %>% 
  rename(MappedSpecialty = "specialty", 
         IsMultiSpecialty = "MultipleSpecialty", 
         MultiSpecialty_Secondary = "OtherSpecialty_1", 
         MultiSpecialty_Tertiary = "OtherSpecialty_2") %>%
  mutate(IsMultiSpecialty=as.logical(IsMultiSpecialty))

write.xlsx(FinalSummary_CareSites, file="./output/CareSite_VisitDetailCount_UPDATED_AML_010425.xlsx", 
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

nrow(CareSite_Summary_Long_Export) # 2909

write.csv(CareSite_Summary_Long_Export, 
          file="./output/CareSiteMap_Multispecialty_Long_UPDATED_AML_010425.csv", row.names=F)

# Write table with filtered care sites (no administrative or unclear specialties)
All <- subset(FinalSummary_CareSites, care_site_id!="0" & MappedSpecialty!="Administrative" & MappedSpecialty!="Unclear") %>% 
  arrange(desc(Visits_N))

write.xlsx(All, "./output/CareSite_VisitDetailCount_FILTERED_AML_010425.xlsx", colWidths = "auto")

