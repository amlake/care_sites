#module load GCC/11.3.0 OpenMPI/4.1.4 R/4.2.1

#library(umap)
#library(devtools)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(psych) 
library(plotly)
library(tm)
library(tidytext)
library(openxlsx)
library(vroom)
library(data.table)

setwd("/data/davis_lab/allie/care_sites/output/SpecialtyPrediction")

CareSiteMap <- read.xlsx("../CareSite_VisitDetailCount_UPDATED_JPS_052325.xlsx", 1, colNames=T)
CareSiteMap_MultiSpecialty <- vroom("../CareSiteMap_Multispecialty_Long_UPDATED_JPS_052325.csv", delim=",")
SpecialtyGroups <- vroom("../Figures/Descriptives/Specialties_NCareSites_Label_ANNOTATED.csv")

FilteredCareSites <- read.xlsx("../CareSite_VisitDetailCount_FILTERED_JPS_052325.xlsx", 1, colNames=T) 
FilteredCareSites <- FilteredCareSites %>% 
  left_join(SpecialtyGroups %>% select(MappedSpecialty,Group), by="MappedSpecialty")

######## Get count of how many phecodes are needed to capture 99% of visits ########
topphe <- fread(file="../CareSiteDescriptives/allphecodes_by_caresite_long.csv") %>% rename(concept_code="phecode")
topphe <- topphe %>% filter(care_site_id %in% FilteredCareSites$care_site_id)

### How many unique codes are needed to capture 99% of visits?
# Create table with cumulative percent showing the how many top codes are required to reach saturation 
topphe_saturation <- topphe %>%
  arrange(care_site_id, desc(CodeCount)) %>%
  group_by(care_site_id) %>%
  mutate(UniqueCodes = n_distinct(concept_code), TotalCodeCount = sum(CodeCount), Cumulative_Percent = cumsum(CodeCount)/TotalCodeCount, rank = dense_rank(desc(CodeCount))) %>%
  as.data.frame()

topphe_saturation %>% summarise(median(UniqueCodes), quantile(UniqueCodes, 0.25), quantile(UniqueCodes, 0.75))

# Get max rank of the codes that make up the 99% 
topphe_saturation_99 <- topphe_saturation %>% 
  group_by(care_site_id) %>%
  arrange(care_site_id, desc(CodeCount)) %>%
  filter(Cumulative_Percent<=99) %>% 
  top_n(n=1) %>% slice(1) %>% as.data.frame()

median_rank <- topphe_saturation_99 %>% summarise(median = median(rank), q25 = quantile(rank, 0.25), q75 = quantile(rank, 0.75))

p <- ggplot(topphe_saturation_99, aes(x=rank)) +
  geom_histogram(binwidth=30, fill="#3262AB", color="black",boundary = 0, closed = "left") +
  labs(x="# of unique Phecodes needed to capture 99% of billing activity", y="# of care sites") +
  geom_hline(yintercept=0, color="black") + geom_vline(xintercept=0, color="black") +
  geom_vline(xintercept=median_rank$median, color="red", linetype="dashed") +
  theme_minimal() +
  theme(text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 12, hjust = 0.5))

ggsave("../Figures/SpecialtyPrediction/topphe_saturation_99.pdf", plot = p, width = 5, height = 5, dpi = 300)

## Calculate TF-IDF
topphe <- topphe %>% filter(freqrank<=100)

topphe_tf <- left_join(topphe, FilteredCareSites %>% select(care_site_id, MappedSpecialty), by="care_site_id") %>%
  filter(!is.na(MappedSpecialty)) %>%
  group_by(care_site_id) %>%
  mutate(TotalCodes=sum(CodeCount)) %>%
  ungroup() %>%
  mutate(TF=CodeCount/TotalCodes, CumulativePercent = cumsum(TF))

code_care_site_count <- topphe_tf %>%
  group_by(concept_code) %>%
  summarise(CareSiteCount=n_distinct(care_site_id))

total_care_sites <- topphe_tf %>%
  summarise(TotalCareSites=n_distinct(care_site_id)) %>%
  pull(TotalCareSites)

topphe_final <- topphe_tf %>%
  left_join(code_care_site_count, by="concept_code") %>%
  mutate(IDF=log(total_care_sites/CareSiteCount), TFIDF = TF*IDF, TFIDF_scaled = scale(TFIDF))

######## Get TF-IDF for top 100 most frequent CPT codes by care site ########
topcpt <- fread(file="../CareSiteDescriptives/allcptcodes_by_caresite_long.csv")
topcpt <- topcpt %>% filter(care_site_id %in% FilteredCareSites$care_site_id)

topcpt %>% count(n_distinct(care_site_id))

## How many unique codes are needed to capture 99% of visits?
# Make file with cumulative total
topcpt_saturation <- topcpt %>%
  arrange(care_site_id, desc(CodeCount)) %>%
  group_by(care_site_id) %>%
  mutate(UniqueCodes = n_distinct(concept_code), TotalCodeCount = sum(CodeCount), Cumulative_Percent = cumsum(CodeCount)/TotalCodeCount, rank = dense_rank(desc(CodeCount))) %>%
  as.data.frame()

topcpt_saturation %>% summarise(median(UniqueCodes), quantile(UniqueCodes, 0.25), quantile(UniqueCodes, 0.75))

# Get max rank of the codes that make up the 99% 
topcpt_saturation_99 <- topcpt_saturation %>% 
  group_by(care_site_id) %>%
  arrange(care_site_id, desc(CodeCount)) %>%
  filter(Cumulative_Percent<=99) %>% 
  top_n(n=1) %>% slice(1) %>% as.data.frame()

median_rank <- topcpt_saturation_99 %>% summarise(median = median(rank), q25 = quantile(rank, 0.25), q75 = quantile(rank, 0.75))
print(median_rank)

p <- ggplot(topcpt_saturation_99, aes(x=rank)) +
  geom_histogram(binwidth=15, fill="#3262AB", color="black",boundary = 0, closed = "left") +
  labs(x="# of unique CPT codes needed to capture 99% of billing activity", y="# of care sites") +
  geom_hline(yintercept=0, color="black") + geom_vline(xintercept=0, color="black") +
  geom_vline(xintercept=median_rank$median, color="red", linetype="dashed") +
  theme_minimal() +
  theme(text = element_text(size = 11),
        axis.title = element_text(size = 11),
        axis.text = element_text(size = 10),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 11, hjust = 0.5))

ggsave("../Figures/SpecialtyPrediction/topcpt_saturation_99.pdf", plot = p, width = 5, height = 5, dpi = 300)

## Calculate TF-IDF
topcpt <- topcpt %>% filter(freqrank<=100)

topcpt_tf <- left_join(topcpt, FilteredCareSites %>% select(care_site_id, MappedSpecialty), by="care_site_id") %>%
  filter(!is.na(MappedSpecialty)) %>% 
  group_by(care_site_id) %>% 
  filter(is.na(CodeCount)==FALSE) %>%
  mutate(TotalCodes=sum(CodeCount)) %>% 
  ungroup() %>%
  mutate(TF=CodeCount/TotalCodes, CumulativePercent = cumsum(TF))

code_care_site_count <- topcpt_tf %>%
  group_by(concept_code) %>%
  summarise(CareSiteCount=n_distinct(care_site_id))

total_care_sites <- topcpt_tf %>% 
  summarise(TotalCareSites=n_distinct(care_site_id)) %>% 
  pull(TotalCareSites)

topcpt_final <- topcpt_tf %>% 
  left_join(code_care_site_count, by="concept_code") %>%
  mutate(IDF=log(total_care_sites/CareSiteCount), TFIDF = TF*IDF, TFIDF_scaled = scale(TFIDF))

####### Make final datasets for dimensionality reduction #######
topphecpt_final <- rbind(topphe_final, topcpt_final)

## Make file with counts of care site-code pairs
topphe_long <- topphe_final %>% 
  group_by(care_site_id) %>% 
  arrange(care_site_id, desc(TFIDF)) %>% 
  as.data.frame()

topcpt_long <- topcpt_final %>% 
  group_by(care_site_id) %>% 
  arrange(care_site_id, desc(TFIDF)) %>% 
  as.data.frame()

topphecpt_long <- topphecpt_final %>% 
  group_by(care_site_id) %>% 
  arrange(care_site_id, desc(TFIDF)) %>% 
  as.data.frame()

write.table(topphe_long, file = "./long_topcodes_phe_bycaresite.txt", sep = "\t", row.names = FALSE)
write.table(topcpt_long, file = "./long_topcodes_cpt_bycaresite.txt", sep = "\t", row.names = FALSE)
write.table(topphecpt_long, file = "./long_topcodes_phecpt_bycaresite.txt", sep = "\t", row.names = FALSE)

## Pivot wider to document-term matrix
topphe_long <- fread("./long_topcodes_phe_bycaresite.txt")
topcpt_long <- fread("./long_topcodes_cpt_bycaresite.txt")
topphecpt_long <- fread("./long_topcodes_phecpt_bycaresite.txt")

topphe_wide_dtm_df <- topphe_long %>% 
  select("care_site_id","concept_code","TFIDF_scaled") %>% 
  pivot_wider(names_from = concept_code, values_from = TFIDF_scaled, names_prefix = "Code_") %>% 
  mutate_all(~replace_na(., 0)) %>% 
  as.data.frame()

topcpt_wide_dtm_df <- topcpt_long %>% 
  select("care_site_id","concept_code","TFIDF_scaled") %>% 
  pivot_wider(names_from = concept_code, values_from = TFIDF_scaled, names_prefix = "Code_") %>% 
  mutate_all(~replace_na(., 0)) %>% 
  as.data.frame()

topphecpt_wide_dtm_df <- topphecpt_long %>% 
  select("care_site_id","concept_code","TFIDF_scaled") %>% 
  pivot_wider(names_from = concept_code, values_from = TFIDF_scaled, names_prefix = "Code_") %>% 
  mutate_all(~replace_na(., 0)) %>% 
  as.data.frame()
  
write.table(topphe_wide_dtm_df, file = "./dtm_topcodes_phe_bycaresite.txt", sep = "\t", row.names = FALSE)
write.table(topcpt_wide_dtm_df, file = "./dtm_topcodes_cpt_bycaresite.txt", sep = "\t", row.names = FALSE)
write.table(topphecpt_wide_dtm_df, file = "./dtm_topcodes_phecpt_bycaresite.txt", sep = "\t", row.names = FALSE)