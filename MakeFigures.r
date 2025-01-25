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
library(UpSetR)
library(ggplotify)
library(data.table)
library(tibble)

ggthemr("pale")

setwd("/panfs/accrepfs.vampire/data/davis_lab/allie/care_sites")

# Import care site map data
CareSiteMap <- read.xlsx("./output/CareSite_VisitDetailCount_UPDATED_AML_010425.xlsx", 1, colNames=T) %>% filter(!is.na(Visits_N))
CareSiteMap_MultiSpecialty <- vroom("./output/CareSiteMap_Multispecialty_Long_UPDATED_AML_010425.csv", delim=",")

# Load table with filtered care sites (no administrative or unclear specialties)
FilteredCareSites <- read.xlsx("./output/CareSite_VisitDetailCount_FILTERED_AML_010425.xlsx", 1, colNames=T) %>%
  filter(!is.na(Visits_N))

###### Descriptives: Count number of unique patients and visits #####
visitcounts <- fread(file="./output/CareSiteDescriptives_AML_010425/all_visit_counts.csv")

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
# 1 n_person      3213825     
# 2 n_care_sites     2545     
# 3 total_visits 69307208     
# 4 prop_outpt          0.892 
# 5 prop_inpt           0.0578
# 6 prop_emer           0.0266
# 7 prop_unspec         0.0236

# Visits with nonmissing care site id: count # of care sites and visits by mapped specialty
visitsummary_nonmissing_caresite <- visitcounts %>% 
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

visitsummary_nonmissing_caresite
#   Variable                     n
#   <chr>                    <int>
# 1 n_care_sites              2544
# 2 n_care_sites_filtered     2189
# 3 n_care_sites_admin          34
# 4 n_care_sites_unclear       321
# 5 n_encounters          53934856
# 6 n_encounters_filtered 45854959
# 7 n_encounters_admin       28870
# 8 n_encounters_unclear   8051027

# All visits mapped to a defined, non-administrative specialty: count by setting
# Not including null care sites or unclear or administrative specialties
# @John I modified this table so it could be used to directly calculate the stats for supp table 1
visitsummary_mapped <- visitcounts %>% 
  filter(care_site_id != 0 & !(specialty %in% c("Administrative", "Unclear"))) %>%
  summarise(n_person = n_distinct(GRID), 
          n_care_sites = n_distinct(care_site_id), 
          total_visits = n(),
          n_outpt = sum(visit_location=="Outpatient"),
          n_inpt = sum(visit_location=="Inpatient"), 
          n_emer = sum(visit_location=="ER"),
          n_unspec = sum(visit_location=="Unspecified"),
          n_adult = sum(VisitAge >= 18),
          n_ped = sum(VisitAge < 18),
          n_male = sum(Gender == "M"),
          n_female = sum(Gender == "F"),
          n_unk = sum(Gender == "U")) %>%
  pivot_longer(-c(n_person, n_care_sites, total_visits), names_to = "Variable", values_to = "n") %>%
  mutate(prop = n/total_visits)

visitsummary_mapped %>% select(n_person, n_care_sites, total_visits) %>% head(1)
#   n_person n_care_sites total_visits
#      <int>        <int>        <int>
# 1  2959903         2189     45854959

visitsummary_mapped %>% select(-n_person, -n_care_sites, -total_visits)
#   Variable        n      prop
#   <chr>       <int>     <dbl>
# 1 n_outpt  40222957 0.877    
# 2 n_inpt    2722511 0.0594   
# 3 n_emer    1291659 0.0282   
# 4 n_unspec  1617832 0.0353   
# 5 n_adult  36166236 0.789    
# 6 n_ped     9688723 0.211    
# 7 n_male   19765146 0.431    
# 8 n_female 26088861 0.569    
# 9 n_unk         952 0.0000208

n_spec <- CareSiteMap_MultiSpecialty %>%
  filter(SpecialtyLong != "Unclear" & SpecialtyLong != "Administrative") %>%
  summarise(n_spec = n_distinct(SpecialtyLong))

n_spec # 58

###### Supplemental Tables 1: Demographic characteristics of visits #####
## Count how many NAs there are in the descriptive variables
FilteredCareSites %>% 
  summarise(across(c(Visits_N, Adult_N, Female_N, Outpt_N, Inpt_N, Emer_N, Unspecified_N), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "NA_Count") %>%
  filter(NA_Count > 0) %>%
  arrange(desc(NA_Count))
# None are missing

# Calculate statistics by reshaping visitsummary_mapped variable defined above
visit_stats <- visitsummary_mapped %>%
  mutate(Variable = gsub("n_", "", Variable)) %>%
  select(-n_person, -n_care_sites, -total_visits) %>%
  pivot_wider(names_from = Variable, values_from = c(n, prop))

visit_stats <- visitsummary_mapped %>%
  select(n_person, n_care_sites, total_visits) %>%
  head(1) %>%
  cbind(visit_stats)

# Create formatted version 
visitsummary_pretty <- tibble(
  Characteristic = c(
    "Number of Patients","Number of Care Sites", "Number of Specialties", "Total Visits",
    "Adult Patients", "Pediatric Patients", "Female Patients", "Male Patients", "Unknown Gender",
    "Outpatient Visits", "Inpatient Visits", "Emergency Visits", "Unspecified Visits"
  ),
  
  Value = c(
    sprintf("%d", visit_stats$n_person),
    sprintf("%d", visit_stats$n_care_sites),
    sprintf("%d", n_spec$n_spec),
    sprintf("%d", visit_stats$total_visits),
    sprintf("%d (%.1f%%)", visit_stats$n_adult, 100*visit_stats$prop_adult),
    sprintf("%d (%.1f%%)", visit_stats$n_ped, 100*visit_stats$prop_ped),
    sprintf("%d (%.1f%%)", visit_stats$n_female, 100*visit_stats$prop_female),
    sprintf("%d (%.1f%%)", visit_stats$n_male, 100*visit_stats$prop_male),
    sprintf("%d (%.1f%%)", visit_stats$n_unk, 100*visit_stats$prop_unk),
    sprintf("%d (%.1f%%)", visit_stats$n_outpt, 100*visit_stats$prop_outpt),
    sprintf("%d (%.1f%%)", visit_stats$n_inpt, 100*visit_stats$prop_inpt),
    sprintf("%d (%.1f%%)", visit_stats$n_emer, 100*visit_stats$prop_emer),
    sprintf("%d (%.1f%%)", visit_stats$n_unspec, 100*visit_stats$prop_unspec)
  )
)

fwrite(visitsummary_pretty, "./output/Figures/Descriptives/SupplementalTable1_VisitDescriptives_AML_010425.csv")

###### Supplemental Table 2: Summary of care sites by demographic characteristics #####
## Count how many NAs there are in the descriptive variables
# Only pherank has missingness (missing in 93 sites 2/2 missing phecodes in sites w/ little actual billing)
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
    across(c(Adult_Percent_Numeric, Female_Percent_Numeric, Outpt_Percent_Numeric, Inpt_Percent_Numeric, Emer_Percent_Numeric, Unspecified_Percent_Numeric),
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
    sprintf("%.1f (%.1f-%.1f)", desc_stats$Adult_Percent_Numeric_med, desc_stats$Adult_Percent_Numeric_min, desc_stats$Adult_Percent_Numeric_max),
    sprintf("%.1f (%.1f-%.1f)", desc_stats$Female_Percent_Numeric_med, desc_stats$Female_Percent_Numeric_min, desc_stats$Female_Percent_Numeric_max),
    sprintf("%.1f (%.1f-%.1f)", desc_stats$Outpt_Percent_Numeric_med, desc_stats$Outpt_Percent_Numeric_min, desc_stats$Outpt_Percent_Numeric_max),
    sprintf("%.1f (%.1f-%.1f)", desc_stats$Inpt_Percent_Numeric_med, desc_stats$Inpt_Percent_Numeric_min, desc_stats$Inpt_Percent_Numeric_max),
    sprintf("%.1f (%.1f-%.1f)", desc_stats$Emer_Percent_Numeric_med, desc_stats$Emer_Percent_Numeric_min, desc_stats$Emer_Percent_Numeric_max),
    sprintf("%.1f (%.1f-%.1f)", desc_stats$Unspecified_Percent_Numeric_med, desc_stats$Unspecified_Percent_Numeric_min, desc_stats$Unspecified_Percent_Numeric_max)
  )
)

# Count each category and calculate percentages
categorical_counts <- 
bind_rows(FilteredCareSites %>% count(AgeSpecific) %>% arrange(desc(n)) %>% mutate(category_type = "Age Specific"),
          FilteredCareSites %>% count(GenderSpecific) %>% arrange(desc(n)) %>% mutate(category_type = "Gender Specific"),
          FilteredCareSites %>% count(MajorityVisitType) %>% arrange(desc(n)) %>% mutate(category_type = "Visit Type"),
          FilteredCareSites %>% count(pherank_1_category) %>% arrange(desc(n)) %>% filter(row_number()<=5) %>% mutate(category_type = "Top Phenotype")) %>%
  mutate(AgeSpecific = ifelse(AgeSpecific == "NonSpecific", "Age-NonSpecific", AgeSpecific),
         GenderSpecific = ifelse(GenderSpecific == "NonSpecific", "Gender-NonSpecific", GenderSpecific)) %>%
  group_by(category_type) %>%
  mutate(percent = n/sum(n) * 100, 
          Value = sprintf("%d (%.1f%%)", n, percent), 
          Characteristic = coalesce(AgeSpecific, GenderSpecific, MajorityVisitType, pherank_1_category)) %>%
  as.data.frame() %>%
  select(Characteristic, Value)

# Combine quantitative and categorical results
desc_maintable_pretty <- bind_rows(desc_median_pretty, categorical_counts)

fwrite(desc_maintable_pretty, "./output/Figures/Descriptives/SupplementalTable2_CareSiteDescriptives_AML_010425.csv")

###### Supplemental Figure 1: Histogram by visit year #####
FilteredCareSites %>% group_by(x_type) %>% summarise(min(VisitYear_Median))

# Create year period factor
hist_visityear <- FilteredCareSites %>%
  mutate(Period = factor(ifelse(VisitYear_Median < 2017, "Star Panel", "eStar"), levels = c("Star Panel", "eStar"))) %>%
  mutate(VisitYear_Median = round(VisitYear_Median, 0)) %>%
  group_by(VisitYear_Median, Period) %>% 
  summarise(count = n()) %>% 
  ggplot(aes(x = VisitYear_Median, y = count, fill = Period, color = Period)) +
  geom_bar(alpha = 0.7, stat = "identity") +
  scale_fill_manual(values = c("#3262AB", "#DE6757")) +
  scale_color_manual(values = c("black", "black")) +
  scale_x_continuous(breaks = seq(1995, 2025, by = 5), limits = c(1995, 2025)) +
  scale_y_continuous(breaks = seq(0, 600, 100)) +
  geom_hline(yintercept = 0, color = "black") +
  labs(x = "Year of median encounter", y = "Number of care sites", fill = "Time Period", color = "Time Period") +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    panel.grid.minor = element_blank(),
    legend.position = c(0.2,0.8)
  )

pdf("output/Figures/Descriptives/SupplementalFigure1_VisitYearHistogram_JPS_011525.pdf", width = 6, height = 6)
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
    across(c(Adult_Percent_Numeric, Female_Percent_Numeric, Outpt_Percent_Numeric, Inpt_Percent_Numeric, Emer_Percent_Numeric, Unspecified_Percent_Numeric),
          list(median = median, min = min, max = max))
  )

# Write to file
fwrite(desc_byspecialty_suppltable, "./output/Figures/Descriptives/SupplementalTable3_CareSiteDescriptivesBySpecialty_AML_010425.csv")

###### Figure 2: Number of Care Sites & Number of visits by setting per Specialty ###### 
## Unique specialty combinations: count # of care sites 
spec_caresite_ct <- FilteredCareSites %>% 
  group_by(MappedSpecialty) %>%
  summarise(n = n_distinct(care_site_id)) %>%
  arrange(desc(n))

# Annotate specialties by group
write.csv(spec_caresite_ct, file="./output/Figures/Descriptives/Specialties_NCareSites_Label_ForAnnotation_AML_010425.csv", row.names = F)

x_labels <- vroom("./output/Figures/Descriptives/Specialties_NCareSites_Label_ANNOTATED.csv", col_select = c(1, 3))
fwrite(x_labels, "./output/Figures/Descriptives/Specialties_NCareSites_Label_ANNOTATED_AML_010425.csv")

## keep labels, update N per site with new numbers
x_labels_new <- x_labels %>% 
  left_join(spec_caresite_ct, by="MappedSpecialty") %>% 
  select(MappedSpecialty, n, Group)
  
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

# Create grouping variable for setting colors
figure2 <- figure2 %>%
  mutate(setting_group = paste0(Group, "_", Setting)) %>%
  mutate(setting_group = factor(setting_group, levels = rev(c("Medical_Outpatient", "Medical_Inpatient", "Medical_Emergency", "Medical_Unspecified",
                                                     "Surgical_Outpatient", "Surgical_Inpatient", "Surgical_Emergency", "Surgical_Unspecified",
                                                     "Other_Outpatient", "Other_Inpatient", "Other_Emergency", "Other_Unspecified"))))

color_key <- data.frame(setting_group = levels(figure2$setting_group)) %>% 
  mutate(setting_group_colors = case_when(
      setting_group == "Medical_Outpatient" ~ "#E64B35",
      setting_group == "Medical_Inpatient" ~ "#e64a358a",
      setting_group == "Medical_Emergency" ~ "#e64a351b",
      setting_group == "Surgical_Outpatient" ~ "#3C5488",
      setting_group == "Surgical_Inpatient" ~ "#3c548881",
      setting_group == "Surgical_Emergency" ~ "#3c548812",
      setting_group == "Other_Outpatient" ~ "#00A087",
      setting_group == "Other_Inpatient" ~ "#00a0887e",
      setting_group == "Other_Emergency" ~ "#00a08813",
      setting_group %in% c("Medical_Unspecified", "Surgical_Unspecified", "Other_Unspecified") ~ "darkgrey"
  ))

color_key <- setNames(color_key$setting_group_colors, color_key$setting_group)

## Make Figure 2 top panel (care site counts by specialty)
count_caresites_byspecialty <- ggplot(figure2 %>% distinct(MappedSpecialty, n_care_sites, Group), 
       aes(x = reorder(MappedSpecialty, -n_care_sites), y = n_care_sites, fill = Group)) +
  stat_summary(geom = "col", fun = sum, width = 0.7, col = "black", alpha=1) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.05))) +
  scale_fill_manual(values = c("Medical" = "#E64B35", "Surgical" = "#3C5488", "Other" = "#00A087")) +
  guides(color = "none") +
  labs(x = " ", y = "Number of Care Sites", fill = "Specialty Group") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top",
        plot.margin =  margin(t = 0, r = 0.1, b = -0.5, l = 0.1, unit = "cm"))

## Make Figure 2 bottom panel (stacked bar plot with visit counts by specialty)
count_visits_byspecialty <- ggplot(figure2, 
       aes(x = reorder(MappedSpecialty, -n_care_sites), y = n_visits_millions*Proportion, fill = setting_group)) +
  geom_bar(stat = "identity", color = "black", size = 0.2) +
  scale_fill_manual(values = color_key) +
  scale_y_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.05))) +
  guides(fill = guide_legend(nrow = 4, override.aes = list(size = 0.5))) +
  labs(x = " ", y = "Number of Visits (Millions)", fill = "Encounter Setting") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "top",
        plot.margin =  margin(t = 0, r = 0.1, b = -0.2, l = 0.1, unit = "cm"),
        legend.title = element_text(size = 8), legend.text = element_text(size = 6))

pdf("output/Figures/Descriptives/Figure2_Specialties_NCareSites_NVisits_AML_010425.pdf", width = 7, height = 7)
ggarrange(count_caresites_byspecialty, count_visits_byspecialty, nrow=2, ncol=1, heights = c(1.5, 2))
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

# First create a named vector mapping specialties to their groups
specialty_groups <- CareSiteMap_MultiSpecialty %>%
  left_join(x_labels_new, by=c("SpecialtyLong"="MappedSpecialty")) %>%
  filter(SpecialtyLong!="Unclear") %>%
  mutate(Group = ifelse(SpecialtyLong=="UrgentCare", "Medical", Group)) %>%
  distinct(SpecialtyLong, Group) %>%
  mutate(SpecialtyLong = str_replace_all(SpecialtyLong, specialty_names)) %>%
  mutate(SpecialtyLong = str_replace_all(SpecialtyLong, "([a-z])([A-Z])", "\\1 \\2")) %>%
  deframe()

# Define colors for each group
group_colors <- c("Medical" = "#E64B35", "Surgical" = "#3C5488", "Other" = "#00A087")

# Create color vector matching the sorted specialty list
set_colors <- group_colors[specialty_groups[specialty_list_sorted]]

# Create upset plot
upsetcount_byspecialty <- upset(figure_suppl2,
      sets = specialty_list_sorted,
      order.by = "freq", keep.order = TRUE, 
      show.numbers = FALSE,
      text.scale = 1, point.size = 0.5, line.size = 0.5,  
      mb.ratio = c(0.25, 0.75), nintersects = NA,
      mainbar.y.label = " ",
      sets.x.label = "Unique care sites",
      sets.bar.color = set_colors)

pdf("output/Figures/Descriptives/SupplementalFigure2_UpsetPlot_Specialties_JPS_011525.pdf", width = 7, height = 7)
print(upsetcount_byspecialty)
dev.off()

###### Supplemental Table 4: Top 50 care sites with annotated features #####
Top50 <- FilteredCareSites %>% 
  left_join(x_labels, by = "MappedSpecialty") %>%
  mutate(Group = factor(Group, levels = c("Medical", "Surgical", "Other"))) %>%
  arrange(desc(Visits_N)) %>% filter(row_number()<=50)

write.xlsx(Top50, "./output/Figures/Descriptives/SupplementalTable4_Top50MostCommonCareSites_AML_010425.xlsx", colWidths = "auto")

###### Supplemental Table 5: Cardiology care sites with annotated features #####
Cardiology <- FilteredCareSites %>% 
  filter(MappedSpecialty=="Cardiology") %>% 
  arrange(desc(Visits_N))

write.xlsx(Cardiology, "./output/Figures/Descriptives/SupplementalTable5_CardiologyCareSites_AML_010425.xlsx", colWidths = "auto")