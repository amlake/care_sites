library(vroom)
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)
library(ggplot2)
library(ggsci)
library(ggthemr)
library(ggpubr)
devtools::install_github("hms-dbmi/UpSetR")
library(ggplotify)
library(data.table)

ggthemr("pale")

setwd("/panfs/accrepfs.vampire/data/davis_lab/allie/care_sites")

# Import care site map data
CareSiteMap <- read.xlsx("./output/CareSite_VisitDetailCount_UPDATED_111524.xlsx", 1, colNames=T) %>% filter(!is.na(Visits_N))
CareSiteMap_MultiSpecialty <- vroom("./output/CareSiteMap_Multispecialty_Long_UPDATED_111524.csv", delim=",")

FilteredCareSites <- read.xlsx("./output/CareSite_VisitDetailCount_FILTERED_122924.xlsx", 1, colNames=T) %>%
  filter(!is.na(Visits_N))

###### Descriptives: Count number of unique patients and visits #####
visitcounts <- fread(file="./output/CareSiteDescriptives/all_visit_counts.csv")

# All visits: count by setting
visitsummary_all <- visitcounts %>% 
  summarise(n_person = n_distinct(GRID), n_care_sites = n_distinct(care_site_id), total_visits = n(),
          prop_outpt = sum(visit_location=="Outpatient")/total_visits, 
          prop_inpt = sum(visit_location=="Inpatient")/total_visits, 
          prop_emer = sum(visit_location=="ER")/total_visits, 
          prop_unspec = sum(visit_location=="Unspecified")/total_visits) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "n")

visitsummary_all
#   Variable                 n
#   <chr>                <dbl>
# 1 n_person      3454621     
# 2 n_care_sites     2580     
# 3 total_visits 70664418     
# 4 prop_outpt          0.893 
# 5 prop_inpt           0.0571
# 6 prop_emer           0.0269
# 7 prop_unspec         0.0233

# Mapped visits: count # of care sites and visits by mapped specialty
visitsummary_mapped <- visitcounts %>% 
  filter(care_site_id!="0") %>% 
  summarise(n_care_sites = n_distinct(care_site_id), 
            n_care_sites_filtered = n_distinct(care_site_id[specialty!="Unclear" & specialty!="Administrative"]), 
            n_care_sites_admin = n_distinct(care_site_id[specialty=="Administrative"]), 
            n_care_sites_unclear = n_distinct(care_site_id[specialty=="Unclear"]),
            n_encounters = n(), 
            n_encounters_filtered = sum(specialty != "Unclear" & specialty != "Administrative"), 
            n_encounters_admin = sum(specialty == "Administrative"), 
            n_encounters_unclear = sum(specialty == "Unclear")) %>% 
  pivot_longer(everything(), names_to = "Variable", values_to = "n")

visitsummary_mapped
#   Variable                     n
#   <chr>                    <int>
# 1 n_care_sites              2579
# 2 n_care_sites_filtered     2194
# 3 n_care_sites_admin          34
# 4 n_care_sites_unclear       351
# 5 n_encounters          54678924
# 6 n_encounters_filtered 46470263
# 7 n_encounters_admin       47562
# 8 n_encounters_unclear   8161099

CareSiteMap %>% filter(MappedSpecialty=="Administrative")

###### Supplemental Tables 1: Demographic characteristics of visits #####
## Count how many NAs there are in the descriptive variables
FilteredCareSites %>% 
  summarise(across(c(Visits_N, Adult_N, Female_N, Outpt_N, Inpt_N, Emer_N, Unspecified_N), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%
  filter(NA_Count > 0) %>%
  arrange(desc(NA_Count))
# None are missing

# Calculate statistics
visit_stats <- FilteredCareSites %>% 
  summarise(
    # Care site statistics  
    n_care_sites = n_distinct(care_site_id),n_specialties = n_distinct(MappedSpecialty),total_visits = sum(Visits_N),
    # Patient statistics
    adult_n = sum(Adult_N), pediatric_n = sum(Visits_N) - sum(Adult_N),
    female_n = sum(Female_N), male_n = sum(Visits_N) - sum(Female_N),
    # Visit statistics
    outpt_n = sum(Outpt_N), inpt_n = sum(Inpt_N), emer_n = sum(Emer_N),
    unspec_n = sum(Unspecified_N)
  )

# Create formatted version
visitsummary_pretty <- tibble(
  Characteristic = c(
    "Number of Care Sites", "Number of Specialties", "Total Visits",
    "Adult Patients", "Pediatric Patients", "Female Patients", "Male Patients",
    "Outpatient Visits", "Inpatient Visits", "Emergency Visits", "Unspecified Visits"
  ),
  
  Value = c(
    sprintf("%d", visit_stats$n_care_sites),
    sprintf("%d", visit_stats$n_specialties),
    sprintf("%d", visit_stats$total_visits),
    sprintf("%d (%.1f%%)", visit_stats$adult_n, 100*visit_stats$adult_n/visit_stats$total_visits),
    sprintf("%d (%.1f%%)", visit_stats$pediatric_n, 100*visit_stats$pediatric_n/visit_stats$total_visits),
    sprintf("%d (%.1f%%)", visit_stats$female_n, 100*visit_stats$female_n/visit_stats$total_visits),
    sprintf("%d (%.1f%%)", visit_stats$male_n, 100*visit_stats$male_n/visit_stats$total_visits),
    sprintf("%d (%.1f%%)", visit_stats$outpt_n, 100*visit_stats$outpt_n/visit_stats$total_visits),
    sprintf("%d (%.1f%%)", visit_stats$inpt_n, 100*visit_stats$inpt_n/visit_stats$total_visits),
    sprintf("%d (%.1f%%)", visit_stats$emer_n, 100*visit_stats$emer_n/visit_stats$total_visits),
    sprintf("%d (%.1f%%)", visit_stats$unspec_n, 100*visit_stats$unspec_n/visit_stats$total_visits)
  )
)

fwrite(visitsummary_pretty, "./output/Figures/Descriptives/SupplementalTable1_VisitDescriptives.csv")

###### Supplemental Table 2: Summary of care sites by demographic characteristics #####
## Count how many NAs there are in the descriptive variables
# Only pherank has missingness (missing in 95 sites 2/2 missing phecodes in sites w/ little actual billing)
FilteredCareSites %>% 
  summarise(across(c(Person_N, Visits_N, VisitYear_Median, Age_Median,
                    Adult_Percent, Female_Percent,
                    AgeSpecific, GenderSpecific, MajorityVisitType,
                    Outpt_Percent, Inpt_Percent, Emer_Percent, Unspecified_Percent,
                    pherank_1_category),
                  ~sum(is.na(.)))) %>%
  pivot_longer(everything(),
               names_to = "Variable",
               values_to = "NA_Count") %>%
  filter(NA_Count > 0) %>%
  arrange(desc(NA_Count))

FilteredCareSites %>%
  select(MappedSpecialty, MultiSpecialty_Secondary, MultiSpecialty_Tertiary) %>%
  pivot_longer(cols = everything(), names_to = "type", values_to = "specialty") %>%
  filter(specialty != "0") %>%  
  pull(specialty) %>%      
  unique()

FilteredCareSites %>%
  select(MappedSpecialty, MultiSpecialty_Secondary, MultiSpecialty_Tertiary) %>%
  pivot_longer(cols = everything(), names_to = "type", values_to = "specialty") %>%
  filter(specialty != "0") %>%  
  filter(!specialty %in% FilteredCareSites$MappedSpecialty) %>%
  pull(specialty) %>%
  unique()
# 57 PRIMARY specialties
# 58 TOTAL specialtiesbecause urgent care is only a secondary specialty

## Create table with descriptive statistics across all care sites
# Calculate all statistics once
desc_stats <- FilteredCareSites %>% 
  summarise(n_care_sites = n(),
    # Continuous variables: median (IQR), mean (SD)
    across(c(Person_N, Visits_N), list(med = median, q25 = ~quantile(., 0.25), q75 = ~quantile(., 0.75), mean = mean, sd = sd)),
    # Year and Age: median (IQR)
    across(c(VisitYear_Median, Age_Median), list(med = median, q25 = ~quantile(., 0.25), q75 = ~quantile(., 0.75))),
    # Percentages: median (min-max)
    across(c(Adult_Percent, Female_Percent, Outpt_Percent, Inpt_Percent, Emer_Percent, Unspecified_Percent),
          list(med = median, min = min, max = max))  
          )

# Create formatted version
desc_median_pretty <- tibble(
  Characteristic = c(
    "Number of Care Sites", "Unique Patients (median [25%-75%])", "Unique Patients (mean [SD])",
    "Total Visits (median [25%-75%])", "Total Visits (mean [SD])",
    "Median Visit Year (median [25%-75%])", "Median Age (median [25%-75%])",  
    "Adult Patients (%) (median [min-max])", "Female Patients (%) (median [min-max])",
    "Outpatient Visits (%) (median [min-max])", "Inpatient Visits (%) (median [min-max])",
    "Emergency Visits (%) (median [min-max])", "Unspecified Visits (%) (median [min-max])"
  ),
  
  Value = c(
    as.character(desc_stats$n_care_sites),
    sprintf("%.0f (%.0f-%.0f)", desc_stats$Person_N_med, desc_stats$Person_N_q25, desc_stats$Person_N_q75),
    sprintf("%.1f (%.1f)", desc_stats$Person_N_mean, desc_stats$Person_N_sd),
    sprintf("%.0f (%.0f-%.0f)", desc_stats$Visits_N_med, desc_stats$Visits_N_q25, desc_stats$Visits_N_q75),
    sprintf("%.1f (%.1f)", desc_stats$Visits_N_mean, desc_stats$Visits_N_sd),
    sprintf("%.0f (%.0f-%.0f)", desc_stats$VisitYear_Median_med, desc_stats$VisitYear_Median_q25, desc_stats$VisitYear_Median_q75),
    sprintf("%.0f (%.0f-%.0f)", desc_stats$Age_Median_med, desc_stats$Age_Median_q25, desc_stats$Age_Median_q75),
    sprintf("%.1f (%.1f-%.1f)", 100*desc_stats$Adult_Percent_med, 100*desc_stats$Adult_Percent_min, 100*desc_stats$Adult_Percent_max),
    sprintf("%.1f (%.1f-%.1f)", 100*desc_stats$Female_Percent_med, 100*desc_stats$Female_Percent_min, 100*desc_stats$Female_Percent_max),
    sprintf("%.1f (%.1f-%.1f)", 100*desc_stats$Outpt_Percent_med, 100*desc_stats$Outpt_Percent_min, 100*desc_stats$Outpt_Percent_max),
    sprintf("%.1f (%.1f-%.1f)", 100*desc_stats$Inpt_Percent_med, 100*desc_stats$Inpt_Percent_min, 100*desc_stats$Inpt_Percent_max),
    sprintf("%.1f (%.1f-%.1f)", 100*desc_stats$Emer_Percent_med, 100*desc_stats$Emer_Percent_min, 100*desc_stats$Emer_Percent_max),
    sprintf("%.1f (%.1f-%.1f)", 100*desc_stats$Unspecified_Percent_med, 100*desc_stats$Unspecified_Percent_min, 100*desc_stats$Unspecified_Percent_max)
  )
)

# Count each category and calculate percentages
categorical_counts <- 
bind_rows(FilteredCareSites %>% count(AgeSpecific) %>% arrange(desc(n)) %>% mutate(category_type = "Age Specific"),
          FilteredCareSites %>% count(GenderSpecific) %>% arrange(desc(n)) %>% mutate(category_type = "Gender Specific"),
          FilteredCareSites %>% count(MajorityVisitType) %>% arrange(desc(n)) %>% mutate(category_type = "Visit Type"),
          FilteredCareSites %>% count(pherank_1_category) %>% arrange(desc(n)) %>% filter(row_number()<=5) %>% mutate(category_type = "Top Phenotype")) %>%
  group_by(category_type) %>%
  mutate(percent = n/sum(n) * 100, 
          Value = sprintf("%d (%.1f%%)", n, percent), 
          Characteristic = coalesce(AgeSpecific, GenderSpecific, MajorityVisitType, pherank_1_category)) %>%
  as.data.frame() %>%
  select(Characteristic, Value)

# Combine quantitative and categorical results
desc_maintable_pretty <- bind_rows(desc_median_pretty, categorical_counts)

fwrite(desc_maintable_pretty, "./output/Figures/Descriptives/SupplementalTable2_CareSiteDescriptives.csv")

###### Supplemental Figure 1: Histogram by visit year #####
FilteredCareSites %>% group_by(x_type) %>% summarise(min(VisitYear_Median))

# Create year period factor
hist_visityear <- FilteredCareSites %>%
  mutate(Period = factor(ifelse(VisitYear_Median < 2017, "Star Panel", "eStar"), levels = c("Star Panel", "eStar"))) %>%
  ggplot(aes(x = round(VisitYear_Median, 0), fill = Period, color = Period)) +
  geom_histogram(bins = 40, alpha = 0.7, position = "identity") +
  scale_fill_manual(values = c("#3262AB", "#DE6757")) +
  scale_color_manual(values = c("black", "black")) +
  labs(x = "Median Encounter Year", y = "Number of Clinical Sites", fill = "Time Period", color = "Time Period") +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )

pdf("output/Figures/Descriptives/SupplementalFigure1_VisitYearHistogram.pdf", width = 6, height = 6)
print(hist_visityear)
dev.off()

###### Supplemental Table 3: Summary of care sites by specialty #####
desc_byspecialty_suppltable <- FilteredCareSites %>% 
  group_by(MappedSpecialty) %>%
  summarise(
    n_care_sites = n(),
    # Patient and Visit counts
    across(c(Person_N, Visits_N), list(median = median, q25 = ~quantile(., 0.25), q75 = ~quantile(., 0.75), mean = mean, sd = sd)),
    # Year and Age
    across(c(VisitYear_Median, Age_Median), list(median = median, q25 = ~quantile(., 0.25), q75 = ~quantile(., 0.75))),
    # Percentages
    across(c(Adult_Percent, Female_Percent, Outpt_Percent, Inpt_Percent, Emer_Percent, Unspecified_Percent),
          list(median = median, min = min, max = max))
  )

# Write to file
fwrite(desc_byspecialty_suppltable, "./output/Figures/Descriptives/SupplementalTable3_CareSiteDescriptivesBySpecialty.csv")

###### Figure 2: Number of Care Sites & Number of visits by setting per Specialty ###### 
## Unique specialty combinations: count # of care sites 
spec_caresite_ct <- FilteredCareSites %>% 
  group_by(MappedSpecialty) %>%
  summarise(n = n_distinct(care_site_id)) %>%
  arrange(desc(n))

# Annotate specialties by group
write.csv(spec_caresite_ct, file="./output/Figures/Descriptives/Specialties_NCareSites_Label_ForAnnotation.csv", row.names = F)

x_labels <- vroom("./output/Figures/Descriptives/Specialties_NCareSites_Label_ANNOTATED.csv", col_select=c(1,3))

# Make dataset with primary specialties only
spec_visit_ct <- FilteredCareSites %>% 
  group_by(MappedSpecialty) %>% 
  summarise(n_care_sites = n_distinct(care_site_id), total_visits = sum(Visits_N),
          prop_outpt = sum(Outpt_N)/total_visits, prop_inpt = sum(Inpt_N)/total_visits, 
          prop_emer = sum(Emer_N)/total_visits, prop_unspec = sum(Unspecified_N)/total_visits) %>%
  ungroup() %>%
  arrange(desc(total_visits)) %>%
  mutate(id = row_number()) %>%
  as.data.frame()

spec_visit_ct %>% summarise(n=n())
spec_visit_ct %>% filter(id<=25)

# Make dataset for figure
figure2 <- spec_visit_ct %>%
  filter(row_number()<=25) %>%
  select(MappedSpecialty, total_visits, n_care_sites, prop_outpt, prop_inpt, prop_emer, prop_unspec) %>%
  pivot_longer(cols = starts_with("prop_"), names_to = "Setting", values_to = "Proportion") %>% 
  mutate(Setting = case_when(Setting == "prop_outpt" ~ "Outpatient",
                             Setting == "prop_inpt" ~ "Inpatient",
                             Setting == "prop_emer" ~ "Emergency",
                             Setting == "prop_unspec" ~ "Unspecified")) %>%
  left_join(x_labels, by="MappedSpecialty") %>%
  mutate(n_visits_millions = total_visits/1e6)

specialty_names <- c("HemeOnc" = "Hematology/Oncology", "OBGYN" = "Obstetrics/Gynecology",
    "AllergyImmunology" = "Allergy/Immunology", "ENT" = "Otolaryngology",
    "PainMed" = "Pain Medicine", "PMR" = "PM&R")

# Correct specialty names
figure2$MappedSpecialty <- str_replace_all(figure2$MappedSpecialty, specialty_names)
figure2$MappedSpecialty <- str_replace_all(figure2$MappedSpecialty, "([a-z])([A-Z])", "\\1 \\2")

# Create stacked bar plot
count_caresites_byspecialty <- ggplot(figure2 %>% distinct(MappedSpecialty, n_care_sites, Group), 
       aes(x = reorder(MappedSpecialty, -n_care_sites), y = n_care_sites, fill = Group)) +
  stat_summary(geom = "col", fun = sum, width = 0.7, col = "black", alpha=1) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("Medical" = "#E64B35", "Surgical" = "#3C5488", "Other" = "#00A087")) +
  guides(color = "none") +
  labs(x = " ", y = "Number of Care Sites", fill = "Specialty Group") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")

count_visits_byspecialty <- ggplot(figure2, 
       aes(x = reorder(MappedSpecialty, -n_care_sites), y = n_visits_millions*Proportion, fill = Setting, color = Setting)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.05))) +
  guides(color = "none") +
  labs(x = " ", y = "Number of Visits (Millions)", fill = "Encounter Setting") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top")

pdf("output/Figures/Descriptives/Figure2_Specialties_NCareSites_NVisits_JPS.pdf", width = 7, height = 7)
ggarrange(count_caresites_byspecialty, count_visits_byspecialty, nrow=2, ncol=1, heights = c(1, 1.5))
dev.off()

###### Supplemental Figure 2: UpSet plot of specialty combinations ###### 
# Count by specialty
specialty_count <- CareSiteMap_MultiSpecialty %>% 
  filter(care_site_id %in% FilteredCareSites$care_site_id) %>%
  group_by(SpecialtyLong) %>% summarise(n = n_distinct(care_site_id)) %>% arrange(desc(n))

# Make combination matrix
specialty_matrix <- CareSiteMap_MultiSpecialty %>%
  filter(care_site_id %in% FilteredCareSites$care_site_id) %>%
  distinct(care_site_id, SpecialtyLong) %>%
  mutate(value = 1) %>%
  pivot_wider(
    id_cols = care_site_id,
    names_from = SpecialtyLong,
    values_from = value,
    values_fill = 0
  )

# Get total visits for each care site
visits_per_site <- FilteredCareSites %>% group_by(care_site_id) %>% summarise(total_visits = sum(Visits_N))

# Combine data
figure_suppl2 <- specialty_matrix %>%
  left_join(visits_per_site, by = "care_site_id") %>%
  select(care_site_id, total_visits, everything()) %>%
  as.data.frame()

specialty_names <- c("HemeOnc" = "Hematology/Oncology", "OBGYN" = "Obstetrics/Gynecology",
    "AllergyImmunology" = "Allergy/Immunology", "ENT" = "Otolaryngology",
    "PainMed" = "Pain Medicine", "PMR" = "PM&R")

# Correct specialty names
names(figure_suppl2) <- str_replace_all(names(figure_suppl2), specialty_names)
names(figure_suppl2) <- str_replace_all(names(figure_suppl2), "([a-z])([A-Z])", "\\1 \\2")

# Get list of specialties 
top_specialties <- FilteredCareSites %>% 
  group_by(MappedSpecialty,MultiSpecialty_Secondary,MultiSpecialty_Tertiary) %>% 
  summarise(total_visits = sum(Visits_N)) %>% 
  as.data.frame() %>%
  arrange(desc(total_visits)) %>% 
  as.data.frame()

specialty_list <- top_specialties %>% 
  select(MappedSpecialty, MultiSpecialty_Secondary, MultiSpecialty_Tertiary) %>%
  pivot_longer(cols = everything(), names_to = "type", values_to = "specialty") %>%
  filter(specialty != "0") %>% 
  pull(specialty) %>%         
  unique()  

# Count urgent care locations and number of visits
FilteredCareSites %>% 
  filter(MappedSpecialty=="PrimaryCare") %>% 
  summarise(n_care_sites = n_distinct(care_site_id), total_visits = sum(Visits_N))

FilteredCareSites %>% 
  filter(MappedSpecialty=="PrimaryCare" & MultiSpecialty_Secondary=="UrgentCare") %>% 
  summarise(n_care_sites = n_distinct(care_site_id), total_visits = sum(Visits_N))

# Apply all replacements at once
specialty_list <- str_replace_all(specialty_list, specialty_names)
specialty_list <- str_replace_all(specialty_list, "([a-z])([A-Z])", "\\1 \\2")

care_site_counts <- colSums(select(figure_suppl2, all_of(specialty_list)))
specialty_list_sorted <- names(sort(care_site_counts, decreasing = FALSE))

# Create upset plot
upsetcount_byspecialty <- upset(figure_suppl2,
      sets = specialty_list_sorted,
      order.by = "freq", keep.order = TRUE, 
      show.numbers = FALSE,
      text.scale = 1, point.size = 0.5, line.size = 0.5,  
      mb.ratio = c(0.25, 0.75), nintersects = NA,
      mainbar.y.label = " ",
      sets.x.label = "Unique locations")

pdf("output/Figures/Descriptives/SupplementalFigure2_UpsetPlot_Specialties.pdf", width = 7, height = 7)
print(upsetcount_byspecialty)
dev.off()

###### Supplemental Table 4: Top 50 care sites with annotated features #####
Top50 <- FilteredCareSites %>% 
  left_join(x_labels, by = "MappedSpecialty") %>%
  mutate(Group = factor(Group, levels = c("Medical", "Surgical", "Other"))) %>%
  arrange(desc(Visits_N)) %>% filter(row_number()<=50)

write.xlsx(Top50, "./output/Figures/Descriptives/SupplementalTable4_Top50MostCommonCareSites.xlsx", colWidths = "auto")

###### Supplemental Table 5: Cardiology care sites with annotated features #####
Cardiology <- FilteredCareSites %>% 
  filter(MappedSpecialty=="Cardiology") %>% 
  arrange(desc(Visits_N))

write.xlsx(Cardiology, "./output/Figures/Descriptives/SupplementalTable5_CardiologyCareSites.xlsx", colWidths = "auto")

###### Supplemental Table 5: all CPT codes by specialty #####
## Pivot wider so CPT code is row and specialty N, freq_rank, and enrichment are columns
###### Figure x: PheWAS Manhattan #####
###### Figure x: CareSiteWAS Manhattan #####
###### Figure x: Asthma - top pediatric CareSiteWAS #####
###### Figure x: CHD - top adult CareSiteWAS #####
###### Figure x: Age at event for high-risk CHD among catheter lab visits #####
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

