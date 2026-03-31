setwd("/data/davis_lab/allie/care_sites/")

library(data.table)
library(stringr)
setDTthreads(64)

stringify <- function(xx, sep, add_quote=T) {
  str <- as.character(xx)
  if (add_quote) {str <- paste0("'",xx,"'")}
  str <-  paste0(str,collapse = sep)
  str
}

## load list of antidepressants
list_med <- fread("data/mdd_application/curated_antidepressant_list_033126.csv")
names(list_med) <- gsub(" ","_",names(list_med))

list_med_vec <- sort(toupper(c(list_med$Generic_name, list_med[!is.na(Brand_name) & Brand_name!="",Brand_name])))
list_med_regex <-  stringify(list_med_vec,"|", add_quote=F)

## load in drug_exposure table (takes ~20-25 mins)
message("loading drug_exposure table...")
d <- fread("/data/davis_lab/shared/phenotype_data/biovu/delivered_data/sd_wide_pull/20230607_pull/20230607_sd_pull_drug_exposure_all.txt.gz",quote="")

## regex search for the relevant drugs
## this step takes ~1 hour
d_med <- d[grepl(list_med_regex,toupper(drug_source_value))]
rm(d)
d_med[,matched_drug := str_extract(toupper(drug_source_value), list_med_regex)]


## convert brand names to generic
list_med_brand <- list_med[!is.na(Brand_name),.(toupper(Generic_name), toupper(Brand_name))] 
names(list_med_brand) <- c("generic","brand")
nrow(d_med) # 4894947
d_med <- merge(d_med, list_med_brand, by.x="matched_drug", by.y="brand", all.x=T)
nrow(d_med) # 4894947 - just checking that rows in dat didn't match to multiple rows in list_med_brand, creating duplicate entries
d_med[,matched_drug_generic := ifelse(is.na(generic),matched_drug,generic)]

stopifnot(nrow(d_med[is.na(matched_drug_generic)])==0)

## map drug_type_concept_id
drug_type_map <- data.frame(
  drug_type_concept_id = c(38000175, 38000177, 38000178, 38000179, 38000180, 44787730, 2002843132),
  drug_type_concept_name = c("Prescription dispensed in pharmacy", "Prescription written","Medication list entry",
                   "Physician administered drug (identified as procedure)", "Inpatient administration",
                   "Patient Self-Reported Medication", "CPT Codes in Drug")
)
d_med <- merge(d_med, drug_type_map, by="drug_type_concept_id", all.x=T)

## create final cleaned table
d_med_clean <- d_med[,.(GRID,drug_exposure_id,drug_type_concept_id,drug_type_concept_name,matched_drug,matched_drug_generic,drug_concept_id,drug_exposure_start_date,drug_exposure_end_date,quantity,route_concept_id,provider_id,visit_occurrence_id,drug_source_value,drug_source_concept_id,route_source_value,dose_unit_source_value)]
setorder(d_med_clean,GRID,drug_exposure_start_date)
d_med_clean[,matched_drug_generic := str_to_upper(matched_drug_generic)]
d_med_clean[,matched_drug := str_to_upper(matched_drug)]

length(unique(d_med_clean$GRID)) # X individuals
rm(d_med)

## get drug_exposure_id for mapping to x_drug_exposure table
ids <- unique(d_med_clean$drug_exposure_id)

## load X_DRUG_EXPOSURE table, which contains some additional fields
## this takes ~30 minutes to load
message("loading x_drug_exposure table...")
xd <- fread("/data/davis_lab/shared/phenotype_data/biovu/delivered_data/sd_wide_pull/20230607_pull/20230607_sd_pull_x_drug_exposure_all.txt.gz",quote="")
xd_subset <- xd[drug_exposure_id %in% ids]
rm(xd)
length(unique(xd_subset$GRID)) # X individuals

xd_subset_clean <- xd_subset[,.(GRID,drug_exposure_id,x_doc_type,x_doc_stype,x_dose,x_drug_form,x_strength,x_frequency,x_ndc,x_rxnorm_source)]

## merge drug_exposure and x_drug_exposure tables
merged <- merge(d_med_clean, xd_subset_clean, by=c("drug_exposure_id","GRID"),all.x=T)
nrow(merged[is.na(x_doc_type)]) # 0 med records did not map to the X table

## remove if start_date > end_date
merged_clean <- merged[is.na(drug_exposure_end_date) | as.Date(drug_exposure_start_date) <= as.Date(drug_exposure_end_date)]

## remove if start date < 1990 or > 2023
merged_clean <- merged_clean[drug_exposure_start_date >= as.Date("1990-01-01") & drug_exposure_start_date < as.Date("2023-06-01")]

## write out merged file
fwrite(merged_clean, "data/mdd_application/sd_wide_antidepressants_merged_xtable_20230607_sd_pull.tsv", sep = "\t")
