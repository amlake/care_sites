# README

## Scripts and data
Working directory: /data/davis_lab/allie/care_sites

### :white_check_mark: Manually annotated care site map
* ./data/CareSite_map_011024_release-1/CareSite_Map_011024.xlsx
* ./data/CareSite_map_011024_release-1/CareSite_Map_Multispecialty_Long_Format_011024.csv

### :white_check_mark: [sd_data_qc.R](https://bitbucket.org/davislabteam/care_sites/src/master/sd_data_qc.R): Load and clean tables from 2023-06-07 SD pull on ACCRE
* Clean PERSON table
    * Remove GRIDs with missing birth dates
    * DOB filter: 1898 to 2023-03-31
* Clean ICD and CPT codes (X_CODES table)
    * Remove codes with negative age at code
    * Code date filter: 1998 to 2023-03-31
* Clean encounters (VISIT_OCCURRENCE table)
    * Remove encounters with negative age at encounter
    * Encounter date filter: 1998 to 2023-03-31
* Output files:
    * Full SD
        * ./data/sd_data_qc/20230607_sd_pull_person_dates_cleaned_overlapping_grids.txt
        * ./data/sd_data_qc/20230607_sd_pull_x_codes_dates_cleaned_overlapping_grids.txt
        * ./data/sd_data_qc/20230607_sd_pull_visit_occurrence_dates_cleaned_overlapping_grids.txt
    * Intersection with MEGA EUR and AFR set (not IBD filtered)
        * ./data/sd_data_qc/20230607_sd_pull_person_dates_cleaned_overlapping_grids_mega_grids_EU_AA.txt
        * ./data/sd_data_qc/20230607_sd_pull_x_codes_dates_cleaned_overlapping_grids_mega_grids_EU_AA.txt
        * ./data/sd_data_qc/20230607_sd_pull_visit_occurrence_dates_cleaned_overlapping_grids_mega_grids_EU_AA.txt

### :bangbang: [CreateDescriptives_CareSites.r](https://bitbucket.org/davislabteam/care_sites/src/master/CreateDescriptives_CareSites.r)
* Characterize care site by patient demographics
* Characterize care site by visit number and visit type
* Top ICD and CPT codes for each care site
* Get top 5 CPT codes for each care site
* Create table with all variables
### :bangbang: [MakeFigures.r](https://bitbucket.org/davislabteam/care_sites/src/master/MakeFigures.r)
### :bangbang: [stankey.Rmd](https://bitbucket.org/davislabteam/care_sites/src/master/stankey.Rmd): Make Sankey plots
* Depends on: [CreateDescriptives_CareSites.r](https://bitbucket.org/davislabteam/care_sites/src/master/CreateDescriptives_CareSites.r)
    
