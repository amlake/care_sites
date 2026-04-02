library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
#library(tidyverse)
library(openxlsx)
library(vroom)
library(data.table)
library(foreach)
library(PheWAS)
library(plotly)
library(ggpubr)
library(scales)
library(RColorBrewer)
library(ggrepel)
library(ggthemr)
library(gridExtra)

setwd("/data/davis_lab/allie/care_sites/output/Figures/CareSiteWAS/")

##### Import data #####
# Import care site map data
CareSiteMap <- read.xlsx("../../CareSite_VisitDetailCount_UPDATED_JPS_052325.xlsx", 1, colNames=T) %>% filter(!is.na(Visits_N))
CareSiteMap_MultiSpecialty <- vroom("../../CareSiteMap_Multispecialty_Long_UPDATED_JPS_052325.csv", delim=",")

# Load table with filtered care sites (no administrative or unclear specialties)
FilteredCareSites <- read.xlsx("../../CareSite_VisitDetailCount_FILTERED_JPS_052325.xlsx", 1, colNames=T) %>%
  filter(!is.na(Visits_N))

# Import MEGA data for eMERGE analysis
emerge_pgs <- fread("/data/davis_lab/allie/care_sites/data/makepgs_emerge10/emerge_standardized_pgs.txt")
names(emerge_pgs)[1] <- "GRID"

person <- fread("/data/davis_lab/allie/care_sites/data/sd_data_qc/20230607_sd_pull_person_dates_cleaned_overlapping_grids_mega_grids_EU_AA.txt")
person %>% count()
# 87,575

x_codes <- fread("/data/davis_lab/allie/care_sites/data/sd_data_qc/20230607_sd_pull_x_codes_dates_cleaned_overlapping_grids_mega_grids_EU_AA.txt")
visit_occurrence <- fread("/data/davis_lab/allie/care_sites/data/sd_data_qc/20230607_sd_pull_visit_occurrence_dates_cleaned_overlapping_grids_mega_grids_EU_AA.txt")

# Check overlap between datasets
emerge_pgs %>% filter(GRID %in% person$GRID) %>% nrow()
emerge_pgs %>% filter(GRID %in% x_codes$GRID) %>% nrow()
emerge_pgs %>% filter(GRID %in% visit_occurrence$GRID) %>% nrow()

emerge_pgs_cleaned <- emerge_pgs %>% filter(GRID %in% person$GRID)
emerge_pgs_cleaned %>% count()
# 66,491

person %>% filter(GRID %in% emerge_pgs$GRID) %>% count()
# 66,491

##### Make phecode-by-visit data #####
### ICD-9-CM
## Import codes
x_codes_icd9cm <- subset(x_codes, vocabulary_id == "ICD9CM")
names(x_codes_icd9cm) <- c("person_id", "GRID", "entry_date", "entry_datetime", "concept_id", 
                            "concept_name", "concept_code", "vocabulary_id", "visit_occurrence_id", "type_concept_id", 
                            "item_position", "age_at_event", "x_poa")

# Map to phecodes and visits
phecodes_icd9cm_byvisit <- x_codes_icd9cm %>% select(2,3,7,9,12) %>% 
    left_join(PheWAS::phecode_map %>% filter(vocabulary_id=="ICD9CM"), by=c("concept_code"="code")) %>%
    left_join(visit_occurrence %>% select(3,11), by="visit_occurrence_id") %>% 
    filter(is.na(phecode)==FALSE & is.na(care_site_id)==FALSE & care_site_id!="0")

### ICD-10-CM
## Import codes
x_codes_icd10cm <- subset(x_codes, vocabulary_id == "ICD10CM")
names(x_codes_icd10cm) <- c("person_id", "GRID", "entry_date", "entry_datetime", "concept_id", 
                              "concept_name", "concept_code", "vocabulary_id", "visit_occurrence_id", "type_concept_id", 
                              "item_position", "age_at_event", "x_poa")

# Map to phecodes and visits
phecodes_icd10cm_byvisit <- x_codes_icd10cm %>% select(2,3,7,9,12) %>% 
    left_join(PheWAS::phecode_map %>% filter(vocabulary_id=="ICD10CM"), by=c("concept_code"="code")) %>%
    left_join(visit_occurrence %>% select(3,11), by="visit_occurrence_id") %>% 
    filter(is.na(phecode)==FALSE & is.na(care_site_id)==FALSE & care_site_id!="0")

### Combine phecodes
phecodes_by_visit <- rbind(phecodes_icd9cm_byvisit, phecodes_icd10cm_byvisit)

# Get PheWAS annotation file
pheinfo_anno <- PheWAS::pheinfo %>% select("phecode","description","group")

##### eMERGE analysis #####
caresitewas_demographics <- vroom("/data/davis_lab/allie/care_sites/data/SpecialtyWASFiles/CareSiteWAS_Demographics_MEGA_EUR.txt")
caresitewas_demographics %>% count(GRID %in% person$GRID, GRID %in% emerge_pgs$GRID)
# Additional 162 were excluded after QC'd file update
caresitewas_demographics_cleaned <- caresitewas_demographics %>% filter(GRID %in% person$GRID)

PC <- read.table("/data/davis_lab/shared/genotype_data/biovu/processed/imputed/best_guess/MEGA/MEGA_recalled/MEGA_recalled/20190827_MEGA.GRID.RACE.ETH.GEN.batch.PCs.covariates_EU.filt1.txt", 
                 as.is=T, header=T, sep="\t",quote = "")
colnames(PC)[1] <- "GRID" 

emerge_person_cleaned <- person %>% 
    left_join(emerge_pgs_cleaned %>% 
        select(GRID, pgs.chd.PGS_highrisk, pgs.chd.PGS_percentile), by="GRID") %>% 
    left_join(caresitewas_demographics_cleaned %>% select(GRID, AGE_MIN_VISIT, AGE_MEDIAN_VISIT), by="GRID") %>%    
    left_join(PC %>% select(GRID, PC1, PC2, PC3, PC4, PC5, PC6, PC7, PC8, PC9, PC10), by="GRID") %>%
    filter(is.na(pgs.chd.PGS_highrisk)==FALSE) %>%
    filter(AGE_MIN_VISIT>=18)

emerge_person_cleaned %>% 
    summarise(n_people = n_distinct(GRID), n_highrisk = sum(pgs.chd.PGS_highrisk==1), perc_highrisk = n_highrisk/n_people)
# 2645 (4.8%)

# MEGA EUR Adults
emerge_person_cleaned %>% 
    summarise(n_people = n_distinct(GRID), 
                age_mean = mean(AGE_MEDIAN_VISIT),
                age_sd = sd(AGE_MEDIAN_VISIT),
                age_median = median(AGE_MEDIAN_VISIT), 
                age_25 = quantile(AGE_MEDIAN_VISIT, 0.25), 
                age_75 = quantile(AGE_MEDIAN_VISIT, 0.75), 
                female_n = sum(gender_source_value=="F"),
                female_perc = female_n/n_people,
                highrisk_n = sum(pgs.chd.PGS_highrisk==1),
                highrisk_perc = highrisk_n/n_people)
# n_people age_mean   age_sd age_median   age_25   age_75 female_n female_perc highrisk_n highrisk_perc
# 1    55020 56.24056 15.76122   57.54415 45.36071 68.00821    30844   0.5605961       2645    0.04807343

emerge_person_cleaned %>% 
    mutate(race_category = case_when(
        race_source_value == "W" ~ "White",
        race_source_value == "B" ~ "Black",
        race_source_value == "A" ~ "Asian",
        str_detect(race_source_value, ",") ~ "Multiple",
        race_source_value == "U" ~ "Unspecified",
        TRUE ~ "Other")) %>%
    group_by(race_category) %>%
    summarise(N = n(), Percent = round(N/nrow(emerge_person_cleaned)*100, 1))

# High risk 
emerge_person_cleaned_highrisk <- emerge_person_cleaned %>% filter(pgs.chd.PGS_highrisk==1)

emerge_person_cleaned_highrisk %>% 
    summarise(n_people = n_distinct(GRID), 
                age_mean = mean(AGE_MEDIAN_VISIT),
                age_sd = sd(AGE_MEDIAN_VISIT),
                age_median = median(AGE_MEDIAN_VISIT), 
                age_25 = quantile(AGE_MEDIAN_VISIT, 0.25), 
                age_75 = quantile(AGE_MEDIAN_VISIT, 0.75), 
                female_n = sum(gender_source_value=="F"),
                female_perc = female_n/n_people)
# n_people age_mean   age_sd age_median   age_25   age_75 female_n female_perc
# 1     2645 55.18348 14.70945   56.33812 44.87064 65.81383     1508   0.5701323

emerge_person_cleaned_highrisk %>% 
    mutate(race_category = case_when(
        race_source_value == "W" ~ "White",
        race_source_value == "B" ~ "Black",
        race_source_value == "A" ~ "Asian",
        str_detect(race_source_value, ",") ~ "Multiple",
        race_source_value == "U" ~ "Unspecified",
        TRUE ~ "Other")) %>%
    group_by(race_category) %>%
    summarise(N = n(), Percent = round(N/nrow(emerge_person_cleaned_highrisk)*100, 1))

##### ClinicWAS for CHD: identify associated sites ##### 
clinicwas_chd_18plus <- vroom("/data/davis_lab/allie/care_sites/output/CareSiteWAS_eMERGE_PGS/CHD/CareSiteVisits_1orMore.txt.gz_NON_GenderSpecific.Rda18orOver.txt")
clinicwas_chd_18plus <- clinicwas_chd_18plus %>% 
    rename(care_site_id="phenotype") %>% 
    filter(MappedSpecialty != "Phlebotomy" & MappedSpecialty != "Radiology"  & MappedSpecialty != "Research") %>%
    filter(n_cases >=100)

# Look at Bonferonni-significant subset
bonf_threshold <- -log10(0.05/nrow(clinicwas_chd_18plus))
bonf_threshold
# Threshold = 4.077004

caresites_bonfsig <- clinicwas_chd_18plus %>% filter(negLogPval >= bonf_threshold) %>% arrange(desc(negLogPval)) 
caresites_bonfsig %>% count()
# 19

# Get cath lab sites. 
# Include vascular labs (labelled Vascular Surgery) but exclude vascular labs that are mis-labelled as Cardiology.
caresites_cathlab <- clinicwas_chd_18plus %>% 
    filter(grepl("cath", care_site_name, ignore.case = TRUE) | 
        grepl("vascular lab", care_site_name, ignore.case = TRUE) | 
        grepl("cardiology interventional", care_site_name, ignore.case = TRUE)) %>% 
    filter(!grepl("cath ep", care_site_name, ignore.case = TRUE))

caresites_cathlab %>% filter(care_site_id %in% caresites_bonfsig$care_site_id) %>% select(care_site_name)
# 1 CATH LAB SPEC ADULT              
# 2 CATH LAB ADULT                   
# 3 VASCULAR LAB                     
# 4 CARDIOLOGY INTERVENTIONAL MCE 5  
# 5 ZZZ-CARDIOLOGY INTERVENTIONAL OHO
# 6 VASCULAR LAB MCE 5 

# Specify sites to highlight
caresites_cathlab_highlight <- caresites_cathlab %>%
    filter(!(grepl("vascular lab", care_site_name, ignore.case = TRUE) & MappedSpecialty == "Cardiology")) %>%
    mutate(is_cathlab = TRUE)

# Create labels
labels <- clinicwas_chd_18plus %>%
    filter(care_site_id %in% caresites_bonfsig$care_site_id) %>%
    arrange(desc(abs(negLogPval))) %>%
    group_by(MappedSpecialty) %>%
    slice(1:5) %>%
    ungroup() %>%
    select(care_site_id, care_site_name)

#### Manhattan plot ####
source("/data/davis_lab/allie/care_sites/scripts/specialtywas/RunWAS/manhattan_bar_mod_clinicwas.R")

ggthemr_reset()
caresite_plot_df <- clinicwas_chd_18plus %>% 
  filter(MappedSpecialty != "Phlebotomy" & MappedSpecialty != "Radiology"  & MappedSpecialty != "Research") %>%
  filter(n_total >=100) %>%
  left_join(caresites_cathlab_highlight %>% select(care_site_id, is_cathlab), by="care_site_id") %>%
  select(care_site_id, care_site_name, specialty = MappedSpecialty, n_phe_per_site = n_total, 
    freq_phe_per_site = negLogPval, is_cathlab) %>%
  data.table()

manhattan <- caresite_manhattan_bar(caresite_plot_df, 
    points = TRUE, 
    custom_labels = labels, 
    highlight_var = "is_cathlab", 
    bonf=bonf_threshold) + 
    xlab("Care sites sorted by specialty") +
    ylab(expression(-log[10]~italic(P)))

pdf("Figure6_CHD_Manhattan_plot.pdf", width = 10, height = 6)    
print(manhattan)
dev.off() 

#### Forest plot ####
# Prepare data 
forest_plot_df <- clinicwas_chd_18plus %>% 
    filter(MappedSpecialty != "Phlebotomy" & MappedSpecialty != "Radiology" & MappedSpecialty != "Research") %>%
    filter(n_total >= 100) %>%
    left_join(caresites_cathlab_highlight %>% select(care_site_id, is_cathlab), by="care_site_id") %>%
    filter(negLogPval >= bonf_threshold) %>%
    arrange(desc(OddsRatio)) %>%
    filter(row_number()<=20) %>%
    mutate(is_cathlab = ifelse(is.na(is_cathlab), FALSE, is_cathlab),
        care_site_name = gsub("ZZZ-", "", care_site_name))

# First, create the color vector outside of the ggplot call
y_axis_colors <- ifelse(forest_plot_df$is_cathlab, "#C5AF27", "black")
y_axis_colors <- y_axis_colors[order(forest_plot_df$OddsRatio, decreasing = FALSE)]

# Then use it in the plot 
forest <- ggplot(forest_plot_df, aes(y = reorder(care_site_name, OddsRatio))) +
    geom_vline(xintercept = 1, linetype = "solid", color = "black") +
    geom_errorbarh(aes(xmin = LCI, xmax = UCI, color = is_cathlab), 
                   height = 0.2, size = 0.5) +
    geom_point(aes(x = OddsRatio, color = is_cathlab)) +
    xlim(0.8,4.5) +
    scale_color_manual(values = c("FALSE" = "black", "TRUE" = "#F1D52F")) +
    scale_x_log10(breaks = c(0.5, 1, 1.5, 2, 3, 4),
                  labels = c("0.5", "1.0", "1.5", "2.0", "3.0", "4.0")) +
    labs(x = "Odds Ratio (95% CI)", y = "Care site") +
    ggtitle("ClinicWAS: top 20 associations") +
    theme_minimal(base_family = "Helvetica") +
    theme(text = element_text(size = 11),
        axis.text.y = element_text(size = 9, color = y_axis_colors),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank())

pdf("Figure6_CHD_Forest_plot.pdf", width = 6, height = 4)    
print(forest)
dev.off()

#### Write Supplemental Data 4 - ClinicWAS results ####
SupplData4_CHD <- clinicwas_chd_18plus %>%
  left_join(caresites_cathlab_highlight %>% select(care_site_id, is_cathlab), by = "care_site_id") %>%
  mutate(
    is_cathlab = ifelse(is.na(is_cathlab), FALSE, is_cathlab),
    is_bonferroni_sig = negLogPval >= bonf_threshold
  ) %>%
  select(
    care_site_id, care_site_name, MappedSpecialty, n_cases,
    OddsRatio, LCI, UCI, p, negLogPval,
    is_bonferroni_sig, is_cathlab
  ) %>%
  arrange(desc(negLogPval))

write.xlsx(SupplData4_CHD, "SupplData4_ClinicWAS_CHD.xlsx", colWidths = "auto")

###### CHD @ Cath lab: Test association between genetic risk and age at onset ######
# Identify CHD phenotypes
chd_phewas <- readRDS("/data/davis_lab/allie/care_sites/output/PheWAS_eMERGE_PGS/chd_pgs_phewas_adult_cohort_122224.Rds")

phecodes_byvisit_chd <- phecodes_by_visit %>% 
    filter(phecode %in% c("411.1","411.2","411.3","411.4") & 
    care_site_id %in% caresites_cathlab$care_site_id)

phecodes_byvisit_chd_minage_byphecode <- phecodes_byvisit_chd %>% 
    group_by(GRID, phecode) %>% 
    summarise(Phecode_FirstAge=min(age_at_event))

# Make cath lab-CHD dataset
phecodes_byvisit_chd_minage <- phecodes_byvisit_chd_minage_byphecode %>% 
    group_by(GRID) %>% 
    summarise(Phecode_FirstAge=min(Phecode_FirstAge))

emerge_cathlab_chd <- emerge_person_cleaned %>% 
    left_join(phecodes_byvisit_chd_minage,by="GRID") %>%
    filter(is.na(Phecode_FirstAge)==FALSE)

# Overall distribution by gender and risk group
emerge_cathlab_chd %>% count(people = n_distinct(GRID))
# 5120

emerge_cathlab_chd %>% count(pgs.chd.PGS_highrisk, gender_source_value)
# pgs.chd.PGS_highrisk gender_source_value    n
# 1:                    0                   F 1569
# 2:                    0                   M 3271
# 3:                    1                   F  103
# 4:                    1                   M  177

emerge_cathlab_chd %>% 
    group_by(gender_source_value) %>% 
    summarise(mean(Phecode_FirstAge), median(Phecode_FirstAge), 
        q25 = quantile(Phecode_FirstAge, 0.25), 
        q75 = quantile(Phecode_FirstAge, 0.75))

# Demographics
emerge_cathlab_chd %>% 
    summarise(n_people = n_distinct(GRID), 
                age_mean = mean(AGE_MEDIAN_VISIT),  
                age_sd = sd(AGE_MEDIAN_VISIT),
                age_median = median(AGE_MEDIAN_VISIT), 
                age_25 = quantile(AGE_MEDIAN_VISIT, 0.25), 
                age_75 = quantile(AGE_MEDIAN_VISIT, 0.75), 
                female_n = sum(gender_source_value=="F"),
                female_perc = female_n/n_people,
                highrisk_n = sum(pgs.chd.PGS_highrisk==1),
                highrisk_perc = highrisk_n/n_people)
# n_people age_mean   age_sd age_median   age_25   age_75 female_n female_perc
# 1     5120 66.38713 10.65976   67.32033 59.28268 74.23032     1672   0.3265625
# highrisk_n highrisk_perc
# 1        280      0.0546875

emerge_cathlab_chd %>% 
    mutate(race_category = case_when(
        race_source_value == "W" ~ "White",
        race_source_value == "B" ~ "Black",
        race_source_value == "A" ~ "Asian",
        str_detect(race_source_value, ",") ~ "Multiple",
        race_source_value == "U" ~ "Unspecified",
        TRUE ~ "Other")) %>%
    group_by(race_category) %>%
    summarise(N = n(), Percent = round(N/nrow(emerge_cathlab_chd)*100, 1))

### Figure: Age distribution by risk group ###
t_result <- t.test(Phecode_FirstAge ~ pgs.chd.PGS_highrisk, data = emerge_cathlab_chd)
t_result$estimate[1] - t_result$estimate[2]
t_result$conf.int
# mean in group 0 
#       3.604044 
#[1] 2.345373 4.862715

t_result_men <- t.test(Phecode_FirstAge ~ pgs.chd.PGS_highrisk, data = emerge_cathlab_chd %>% filter(gender_source_value=="M"))
t_result_men$estimate[1] - t_result_men$estimate[2]
t_result_men$conf.int
# mean in group 0 
#       3.065961 
#[1] 1.487265 4.644656

t_result_women <- t.test(Phecode_FirstAge ~ pgs.chd.PGS_highrisk, data = emerge_cathlab_chd %>% filter(gender_source_value=="F"))
t_result_women$estimate[1] - t_result_women$estimate[2]
t_result_women$conf.int
# mean in group 0 
#       4.60472 
# [1] 2.489668 6.719772

ggthemr("pale")

emerge_cathlab_chd_m <- emerge_cathlab_chd %>% filter(gender_source_value=="M")

emerge_cathlab_chd_m %>% 
    group_by(pgs.chd.PGS_highrisk) %>% 
    summarise(age = mean(Phecode_FirstAge))

men <- ggplot(emerge_cathlab_chd_m, 
    aes(x = factor(pgs.chd.PGS_highrisk), y = Phecode_FirstAge, fill = factor(pgs.chd.PGS_highrisk))) +
  geom_violin(alpha = 0.5, width=0.4) +
  geom_boxplot(width = 0.2, alpha = 0.7, color = "black", outlier.shape = NA) +
  scale_fill_manual(values = c("0" = "#939ce0", "1" = "#ff7591"), labels = c("Low", "High"), name = "Genetic Risk") +
  labs(x = " ", y = "Age at first encounter", title="Men") +
  scale_x_discrete(labels = c(" ", " ")) +
  stat_compare_means(method = "t.test", 
                    label.x = 1.5, label.y = max(emerge_cathlab_chd_m$Phecode_FirstAge, na.rm=TRUE)) +
  theme_minimal(base_family = "Helvetica") +
        theme(
            text = element_text(size = 11),
            axis.text.x = element_blank(),
            axis.title.x = element_text(hjust = 0.5),
            axis.text.y = element_text(size = 9),
            plot.title = element_text(hjust = 0.5),
            axis.title.y = element_text(hjust = 0.5),
            legend.position = "none",
            panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
            plot.margin =  margin(t = 0.25, r = 0.25, b = 0.25, l = 0.25, unit = "cm")
        )

emerge_cathlab_chd_f <- emerge_cathlab_chd %>% filter(gender_source_value=="F")

women <- ggplot(emerge_cathlab_chd_f, 
    aes(x = factor(pgs.chd.PGS_highrisk), y = Phecode_FirstAge, fill = factor(pgs.chd.PGS_highrisk))) +
  geom_violin(alpha = 0.5, width=0.4) +
  geom_boxplot(width = 0.2, alpha = 0.7, color = "black", outlier.shape = NA) +
  scale_fill_manual(values = c("0" = "#939ce0", "1" = "#ff7591"), labels = c("Low", "High"), name = "Genetic Risk") +
  labs(x = " ", y = " ", title="Women") +
  scale_x_discrete(labels = c(" ", " ")) +
  stat_compare_means(method = "t.test", 
                    label.x = 1.5, label.y = max(emerge_cathlab_chd_f$Phecode_FirstAge, na.rm=TRUE)) +
  theme_minimal(base_family = "Helvetica") +
        theme(
            text = element_text(size = 11),
            axis.text.x = element_blank(),
            axis.title.x = element_text(hjust = 0.5),
            axis.text.y = element_text(size = 9),
            plot.title = element_text(hjust = 0.5),
            axis.title.y = element_text(hjust = 0.5),
            legend.position = "none",
            panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
            plot.margin =  margin(t = 0.25, r = 0.25, b = 0.25, l = 0.25, unit = "cm")
        )

violin <- ggarrange(men, women, nrow=1, ncol=2, common.legend = TRUE, legend="bottom") %>%
    annotate_figure(top = text_grob("Patients undergoing catheterization for CHD\nN=5,120", color = "black", face = "bold", size = 14))

pdf("Figure6_CHD_age_distribution_by_risk.pdf", width = 6, height = 6)    
print(violin)
dev.off()

pdf("Figure6_CHD_ClinicWAS.pdf", width = 12, height = 8)    
ggarrange(ggarrange(manhattan, ncol = 1, widths = 1, labels = "A"),
    ggarrange(forest, violin, ncol = 2, widths = c(1, 1), labels = c("B", "C")),
    nrow = 2, heights = c(1, 1))
dev.off()

# Logistic regression
logistic_data <- emerge_cathlab_chd %>% 
    mutate(earlyonset = case_when(
        gender_source_value == "F" & Phecode_FirstAge < 65 ~ 1,
        gender_source_value == "M" & Phecode_FirstAge < 55 ~ 1,
        TRUE ~ 0
    ))

earlyonsetchd_pgs <- glm(earlyonset ~ pgs.chd.PGS_highrisk + gender_source_value +PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, 
    data = logistic_data, family = binomial())
summary(earlyonsetchd_pgs)
# Estimate Std. Error z value Pr(>|z|)
# pgs.chd.PGS_highrisk  0.72339    0.13174   5.491 3.99e-08 ***
#   gender_source_valueM -1.37696    0.06571 -20.956  < 2e-16 ***
#   PC1                   0.11195    1.28434   0.087   0.9305    
# PC2                  -3.34344    2.42945  -1.376   0.1688    
# PC3                   2.05672    3.50498   0.587   0.5573

exp(cbind(OR = coef(earlyonsetchd_pgs), confint(earlyonsetchd_pgs)))
# OR        2.5 %       97.5 %
# pgs.chd.PGS_highrisk  2.061406838 1.590470e+00     2.666767
# gender_source_valueM  0.252345539 2.217819e-01     0.286944
# PC1                   1.118458184 8.468812e-02    13.228009
# PC2                   0.035315349 3.182330e-04     4.588761
# PC3                   7.820239106 8.174092e-03  8386.176011

intxn_sexpgs <- glm(earlyonset ~ pgs.chd.PGS_highrisk + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10 +
    gender_source_value + pgs.chd.PGS_highrisk*gender_source_value,
    data = logistic_data, family = binomial())
summary(intxn_sexpgs)
exp(cbind(OR = coef(intxn_sexpgs), confint(intxn_sexpgs)))
