library(vroom)
library(dplyr)
library(tidyr)
library(openxlsx)
library(ggplot2)
library(ggsci)
library(ggthemr)
library(data.table)

ggthemr("pale")

setwd("/panfs/accrepfs.vampire/data/davis_lab/allie/care_sites")

# Import care site map data
CareSiteMap <- read.xlsx("./output/CareSite_VisitDetailCount_UPDATED_111524.xlsx", 1, colNames=T)
CareSiteMap_MultiSpecialty <- vroom("./output/CareSiteMap_Multispecialty_Long_UPDATED_111524.csv", delim=",")

# Summarize data
specialty_ncaresites <- CareSiteMap_MultiSpecialty %>% 
  select(care_site_id,SpecialtyLong) %>%
  filter(SpecialtyLong!="Unclear") %>%
  group_by(SpecialtyLong) %>%
  summarise(n = n_distinct(care_site_id)) %>%
  arrange(desc(n))

# ANNOTATE SPECIALTIES ACCORDING as SURGICAL,MEDICAL, OR OTHER 
write.csv(specialty_ncaresites, file="./output/Figures/Descriptives/Specialties_NCareSites_Label_ForAnnotation.csv", row.names = F)

x_labels <- vroom("./output/Figures/Descriptives/Specialties_NCareSites_Label_ANNOTATED.csv", col_select = c(1,3))

###### Figure 1: Number of Care Sites per Specialty ###### 
figure1 <- specialty_ncaresites %>% 
  left_join(x_labels, by="SpecialtyLong") %>% 
  filter(row_number()<=20) %>%
  mutate(Group = factor(Group, levels = c("Medical", "Surgical", "Other")))

count_caresites_byspecialty <- ggplot(figure1, aes(x = reorder(SpecialtyLong, -n), y = n, fill = Group)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Mapped Specialty", y = "Number of Care Sites") +
  theme(
    text = element_text(size = 10), 
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7), 
    axis.text.y = element_text(size = 8),  
    axis.title.x = element_text(),
    axis.title.y = element_text(),
    legend.position = "right")

ggsave("./output/Figures/Descriptives/Specialties_NCareSites.pdf", plot = count_caresites_byspecialty, 
  width = 5, height = 4, units = "in", dpi = 400, device = "pdf")

###### Figure 2: Distribution of care sites by care setting #####
figure2 <- CareSiteMap %>% 
  select(care_site_id,Outpt_Percent,Inpt_Percent,Emer_Percent,Unspecified_Percent) %>% 
  arrange(desc(Outpt_Percent)) %>%
  pivot_longer(cols = ends_with("Percent"), names_to = "care_type", values_to = "percent")

ggplot(figure2, aes(x = care_site_id, fill = care_type, y = percent)) + 
  geom_bar(position = "fill", stat = "identity") + 
  labs(x = "Care Site", y = "Percentage", fill = "Care Type") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
        
ggsave("./output/Figures/Descriptives/Figure2_CareSiteDistribution_byCareSetting.pdf", plot = last_plot(), 
       width = 8, height = 5, units = "in", dpi = 400, device = "pdf")

###### Figure 3: Distribution of care sites by EHR-reported gender #####

###### Figure 4: Distribution of care sites by age #####
###### Figure 5A-E: PheWAS Manhattan #####
###### Figure 6A-E: CareSiteWAS Manhattan #####
###### Figure 7: Asthma - top pediatric CareSiteWAS #####
###### Figure 8: CHD - top adult CareSiteWAS #####
###### Figure 9: Age at event for high-risk CHD among catheter lab visits #####
### Import ICD codes and create phecodes 
x_codes <- as.data.frame(fread("/data/davis_lab/shared/phenotype_data/biovu/delivered_data/sd_wide_pull/20230607_pull/20230607_sd_pull_x_codes_all.txt.gz"))
visit_occurrence <- as.data.frame(fread("/data/davis_lab/shared/phenotype_data/biovu/delivered_data/sd_wide_pull/20230607_pull/20230607_sd_pull_visit_occurrence_all.txt.gz"))

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

