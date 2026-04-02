library(tidyr)
library(dplyr)
library(tidytext)
library(ggplot2)
library(scales)
library(psych) 
library(plotly)
library(tm)
library(tidytext)
library(openxlsx)
library(vroom)
library(data.table)
library(forcats)

setwd("/data/davis_lab/allie/care_sites/output/SpecialtyPrediction")

CareSiteMap <- read.xlsx("../CareSite_VisitDetailCount_UPDATED_JPS_052325.xlsx", 1, colNames=T)
CareSiteMap_MultiSpecialty <- vroom("../CareSiteMap_Multispecialty_Long_UPDATED_JPS_052325.csv", delim=",")
SpecialtyGroups <- vroom("../Figures/Descriptives/Specialties_NCareSites_Label_ANNOTATED.csv")

FilteredCareSites <- read.xlsx("../CareSite_VisitDetailCount_FILTERED_JPS_052325.xlsx", 1, colNames=T) 
FilteredCareSites <- FilteredCareSites %>% 
  left_join(SpecialtyGroups %>% select(MappedSpecialty,Group), by="MappedSpecialty")

### CPT
topcpt <- fread(file="../CareSiteDescriptives/allcptcodes_by_caresite_long.csv")
topcpt <- topcpt %>% filter(care_site_id %in% FilteredCareSites$care_site_id)

# Import concept names
cpt_names <- vroom("/data/davis_lab/allie/care_sites/output/SpecialtyPrediction/cptcode_conceptnames.txt", quote = "\"")

topcpt_specialtysum <- left_join(topcpt, FilteredCareSites %>% select(care_site_id, MappedSpecialty), by="care_site_id") %>%
  filter(!is.na(MappedSpecialty)) %>% 
  group_by(MappedSpecialty,concept_code) %>% 
  summarise(CodeCount=sum(CodeCount)) %>%
  ungroup(concept_code) %>%
  as.data.frame()

topcpt_tf <- topcpt_specialtysum %>%
  group_by(MappedSpecialty) %>% 
  mutate(TotalCodes=sum(CodeCount)) %>% 
  ungroup() %>%
  mutate(TF=CodeCount/TotalCodes) %>%
  left_join(cpt_names, by="concept_code")

code_specialty_count <- topcpt_tf %>%
  group_by(concept_code) %>%
  summarise(SpecialtyCount=n_distinct(MappedSpecialty))

total_specialties <- topcpt_tf %>% 
  summarise(TotalSpecialties=n_distinct(MappedSpecialty)) %>% 
  pull(TotalSpecialties)

topcpt_final <- topcpt_tf %>% 
  filter(CodeCount>100) %>%
  left_join(code_specialty_count, by="concept_code") %>%
  mutate(IDF=log(total_specialties/SpecialtyCount), TFIDF = TF*IDF) %>%
  group_by(MappedSpecialty) %>% arrange(desc(TFIDF)) %>% mutate(tfidf_rank = row_number()) %>%
  ungroup() %>%
  select("MappedSpecialty", "concept_code", "concept_name", "CodeCount", "TotalCodes", "TF", "SpecialtyCount", "IDF", "TFIDF", "tfidf_rank")

View(topcpt_final)

#### Write Supplemental Data #5 ####
write.xlsx(topcpt_final, file="/data/davis_lab/allie/care_sites/output/SpecialtyPrediction/SupplData5_specialty_cptrank.xlsx")

#### Create bar plots for top 5 codes by specialty ####
# Function to get top 5 codes by specialty
gettop5byspecialty <- function(data, outcome) {
  data %>%
    group_by(MappedSpecialty) %>%
    arrange(desc(!!sym(outcome))) %>%
    filter(row_number() <= 5) %>%
    ungroup()
}

# Define specialties to exclude
exclude_specialties <- c("Pharmacy", "SocialWork", "PalliativeCare", 
                         "GenderAffirmingCare", "Toxicology", "Research")

# Get top 5 by TF first to establish specialty ordering
top5cpt_tf <- gettop5byspecialty(topcpt_final, "TF") %>% 
  filter(!(MappedSpecialty %in% exclude_specialties))

# Create consistent specialty order based on max TF (used for both plots)
specialty_order <- top5cpt_tf %>%
  group_by(MappedSpecialty) %>%
  summarise(max_tf = max(TF)) %>%
  arrange(desc(max_tf)) %>%
  pull(MappedSpecialty)

# Prepare TF data
top5cpt_tf_pre <- top5cpt_tf %>% 
  arrange(MappedSpecialty, desc(TF)) %>%
  group_by(MappedSpecialty) %>%
  mutate(order = row_number()) %>%
  ungroup() %>%
  mutate(MappedSpecialty = factor(MappedSpecialty, levels = specialty_order),
         code_label = paste(MappedSpecialty, concept_code, sep = "_"),
         code_label = fct_reorder(code_label, -order))  # Reversed order

write.csv(top5cpt_tf_pre, "top5cpt_tf_preanno.csv", row.names = F)

# Prepare TF-IDF data with SAME specialty order
top5cpt_tfidf_pre <- gettop5byspecialty(topcpt_final, "TFIDF") %>% 
  filter(!(MappedSpecialty %in% exclude_specialties)) %>%
  arrange(MappedSpecialty, desc(TFIDF)) %>%
  group_by(MappedSpecialty) %>%
  mutate(order = row_number()) %>%
  ungroup() %>%
  mutate(MappedSpecialty = factor(MappedSpecialty, levels = specialty_order),
         code_label = paste(MappedSpecialty, concept_code, sep = "_"),
         code_label = fct_reorder(code_label, -order))  # Reversed order

write.csv(top5cpt_tfidf_pre, "top5cpt_tfidf_preanno.csv", row.names = F)

## Import files with abbreviated concept names
top5cpt_tf <- fread(file="top5cpt_tf_annotated.csv") %>%
  mutate(MappedSpecialty = factor(MappedSpecialty, levels = specialty_order),
         name_label = reorder_within(name_label, TF, MappedSpecialty))

top5cpt_tfidf <- fread(file="top5cpt_tfidf_annotated.csv") %>%
  mutate(MappedSpecialty = factor(MappedSpecialty, levels = specialty_order),
         name_label = reorder_within(name_label, TFIDF, MappedSpecialty))  # Changed TF to TFIDF

# Specs
ncol_facet <- 5

# Generate TF plot
p1 <- ggplot(top5cpt_tf, aes(x = name_label, y = TF, fill = MappedSpecialty)) +
  geom_bar(stat = "identity") +
  facet_wrap(~MappedSpecialty, scales = "free_y", ncol = ncol_facet) +  # Changed to "free"
  coord_flip() +
  scale_x_reordered() +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_viridis_d(option = "turbo") +
  labs(x = "CPT Code", 
       y = "Term Frequency (TF)", 
       title = NULL) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    axis.title = element_text(hjust = 0.5, size = 11, face = "bold"),
    strip.text = element_text(size = 8, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
    legend.position = "none",
    panel.spacing = unit(0.1, "lines")
  )

# Generate TF-IDF plot
p2 <- ggplot(top5cpt_tfidf, aes(x = name_label, y = TFIDF, fill = MappedSpecialty)) +
  geom_bar(stat = "identity") +
  facet_wrap(~MappedSpecialty, scales = "free_y", ncol = ncol_facet) +  # Changed to "free"
  coord_flip() +
  scale_x_reordered() +
  scale_fill_viridis_d(option = "turbo") +
  labs(x = "CPT Code", 
       y = "TF-IDF Score", 
       title = NULL) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(size = 8),
    axis.title = element_text(hjust = 0.5, size = 11, face = "bold"),
    strip.text = element_text(size = 8, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
    legend.position = "none",
    panel.spacing = unit(0.1, "lines")
  )

# Save plots 
ggsave("../Figures/SpecialtyPrediction/SupplFigure7_CPT_TFbySpecialty.pdf", 
       plot = p1, width = 12, height = 12, dpi = 300)
ggsave("../Figures/SpecialtyPrediction/SupplFigure8_CPT_TFIDFbySpecialty.pdf", 
       plot = p2, width = 12, height = 12, dpi = 300)
