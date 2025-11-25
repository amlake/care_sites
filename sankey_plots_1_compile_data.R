## Care site Sankey plots step 1: compile data 
## Run this step on ACCRE, then download exported data run plot-generating script on Macbook because of ACCRE graphics issues
## step 2 script: sankey_plots_2_make_plots_run_local.R

setwd("/data/davis_lab/allie/care_sites/")
pacman::p_load(data.table, dplyr, foreach, PheWAS, ggplot2, plotly, ggpubr, scales, RColorBrewer, ggrepel, kableExtra, broom, networkD3, xlsx, stringr)

#### load care site map (long format) and annotation files ####
map <- fread("/data/davis_lab/allie/care_sites/output/CareSiteMap_Multispecialty_Long_UPDATED_JPS_052325.csv")
site_info <- fread("/data/davis_lab/allie/care_sites/output/CareSiteDescriptives_AML_010425/omop_care_site_info.csv") # TODO need to replace with updated file
site_phe <- fread("/data/davis_lab/allie/care_sites/output/CareSiteDescriptives_AML_010425/top5phecodes_by_caresite_wide.csv") # TODO need to replace with updated file
site_visits <- fread("/data/davis_lab/allie/care_sites/output/CareSiteDescriptives_AML_010425/visit_counts_by_caresite.csv") # TODO need to replace with updated file

map <- merge(map, site_info, by = "care_site_id")
map <- merge(map, site_visits, by = "care_site_id")
map <- merge(map, site_phe, by = "care_site_id")
map <- map[SpecialtyLong != "Unclear" & SpecialtyLong != "Administrative" & SpecialtyLong != "Phlebotomy" & SpecialtyLong !="Radiology"]
map$care_site_id <- as.character(map$care_site_id)


#### Identify and fix duplicate care site names for plots ####
## (So that each care site will have a unique name)
dups <- unique(copy(map[,.(care_site_id, care_site_name)]))
dups[,n_name_per_id := .N, by = care_site_id]
dups[,n_id_per_name := .N, by = care_site_name]

dups[n_name_per_id>1] # none
dups[n_id_per_name>1] # 26 of these

dups[, new_name := care_site_name]
dups[n_id_per_name>1, new_name := paste0(care_site_name, "_", care_site_id)]

map <- merge(map, dups[,.(care_site_id, new_name)], by = "care_site_id")
map[,care_site_name := new_name]
map[,new_name:=NULL]

stopifnot(n_distinct(map$care_site_id) == n_distinct(map$care_site_name))

#### Select most frequently visited sites for plotting ####
## Select top 50 sites (by visit count) within each category (medical, surgical, other)
Specialties_3group <- fread("/data/davis_lab/allie/care_sites/output/Figures/Descriptives/Specialties_NCareSites_Label_ANNOTATED_JPS_052325.csv")
map <- merge(map, Specialties_3group, by.x = "SpecialtyLong", by.y = "MappedSpecialty")

setorder(map, -Visits_N)
map[,visit_rank := 1:.N, by = Group]

## Sort sites by phecode category
setorder(map, pherank_1_category, SpecialtyLong)
# setorder(map, SpecialtyLong, pherank_1_category)

#### Export data ####
fwrite(map, "data/map_formatted_for_sankey_plots_temp_112525.tsv", sep = "\t")
