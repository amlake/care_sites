# Load necessary libraries
library(PheWAS)
library(sqldf)
library(tidyr)
library(vroom)
library(data.table)
library(dplyr)

setwd("/data/davis_lab/allie/care_sites/data/makepgs_emerge10/")

# List of file paths and their corresponding PGS names
pgs_files <- list.files(path = "./", pattern = "profile", full.names = TRUE, recursive = TRUE)
pgs_names <- c("afib", "asthma", "bmi", "chd", "ckd", "asthma_nohla")  # Update this list to match the files' order

# Function to process each file
process_pgs <- function(file_path, pgs_name) {
  genotypes <- read.table(file_path, as.is = TRUE, header = TRUE, sep = "", quote = "")
  colnames(genotypes)[1] <- "BlankID"
  colnames(genotypes)[2] <- "FID"
  
  # Standardize
  genotypes$PGS_std <- as.numeric(scale(genotypes$SCORE, center = TRUE, scale = TRUE))
  genotypes$PGS_percentile <- rank(genotypes$PGS_std) / length(genotypes$PGS_std) * 100
  genotypes$PGS_group <- cut(genotypes$PGS_percentile, 
                             breaks = c(0, 1, 5, 10, 90, 95, 99, 100),
                             labels = c("Bottom 1%", "1-5%", "5-10%", "10-90%", "90-95%", "95-99%", "Top 1%"),
                             include.lowest = TRUE)
  genotypes$PGS_highrisk <- ifelse(genotypes$PGS_percentile > 95, 1, 0)
  
  # Add PGS name
  genotypes$PGS_name <- pgs_name
  
  # Summarize high risk
  summary <- genotypes %>% 
    mutate(PGS_highrisk = ifelse(PGS_percentile > 95, 1, 0)) %>% 
    group_by(PGS_highrisk) %>% 
    summarise(median_PGS_std = median(PGS_std), 
              Q05_PGS_std = quantile(PGS_std, 0.05), 
              Q25_PGS_std = quantile(PGS_std, 0.25), 
              Q75_PGS_std = quantile(PGS_std, 0.75), 
              Q95_PGS_std = quantile(PGS_std, 0.95))
  
  # Add PGS name
  summary$PGS_name <- pgs_name
  
  list(summary = summary, genotypes = genotypes)
}

# Loop through the list of PGS files and process each
results <- lapply(seq_along(pgs_files), function(i) process_pgs(pgs_files[i], pgs_names[i]))

# Combine summaries and detailed data into separate data frames
combined_riskthreshold <- bind_rows(lapply(results, function(x) x$summary), .id = "set")
combined_pgsresults <- bind_rows(lapply(results, function(x) x$genotypes), .id = "set")

# Print the results
print(combined_riskthreshold)
print(combined_pgsresults)

# Add high risk binary predictor
combined_pgsresults <- combined_pgsresults %>% 
  mutate(PGS_highrisk = ifelse(PGS_percentile > 95, 1, 0)) %>%
  select(FID, PGS_name, PGS_percentile, PGS_highrisk) 

# rename pgs variables
pgs_afib <- combined_pgsresults %>% 
  filter(PGS_name=="afib") %>%
  rename_with(~ paste0("pgs.", gsub("^pgs\\.", "", "afib"), ".", .), c("PGS_percentile", "PGS_highrisk")) %>%
  select(-"PGS_name")

pgs_asthma <- combined_pgsresults %>% 
  filter(PGS_name=="asthma") %>%
  rename_with(~ paste0("pgs.", gsub("^pgs\\.", "", "asthma"), ".", .), c("PGS_percentile", "PGS_highrisk")) %>%
  select(-"PGS_name")

pgs_asthma_nohla <- combined_pgsresults %>% 
  filter(PGS_name=="asthma_nohla") %>%
  rename_with(~ paste0("pgs.", gsub("^pgs\\.", "", "asthma_nohla"), ".", .), c("PGS_percentile", "PGS_highrisk")) %>%
  select(-"PGS_name")

pgs_bmi <- combined_pgsresults %>% 
  filter(PGS_name=="bmi") %>% 
  rename_with(~ paste0("pgs.", gsub("^pgs\\.", "", "bmi"), ".", .), c("PGS_percentile", "PGS_highrisk")) %>%
  select(-"PGS_name")

pgs_chd <- combined_pgsresults %>% 
  filter(PGS_name=="chd") %>%
  rename_with(~ paste0("pgs.", gsub("^pgs\\.", "", "chd"), ".", .), c("PGS_percentile", "PGS_highrisk")) %>%
  select(-"PGS_name")

pgs_ckd <- combined_pgsresults %>% 
  filter(PGS_name=="ckd") %>% 
  rename_with(~ paste0("pgs.", gsub("^pgs\\.", "", "ckd"), ".", .), c("PGS_percentile", "PGS_highrisk")) %>%
  select(-"PGS_name")

# combine 
final_pgs <- pgs_afib %>% 
  left_join(pgs_asthma, by="FID") %>%
  left_join(pgs_asthma_nohla, by="FID") %>%
  left_join(pgs_bmi, by="FID") %>%
  left_join(pgs_chd, by="FID") %>%
  left_join(pgs_ckd, by="FID")

fwrite(final_pgs, file="./emerge_standardized_pgs.txt")
