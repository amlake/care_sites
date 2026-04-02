# README

## Care sites project scripts
ACCRE working directory: /data/davis_lab/allie/care_sites

### [sd_data_qc.R](https://github.com/amlake/care_sites/blob/master/sd_data_qc.R): Load and clean tables from 2023-06-07 SD pull on ACCRE
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
     * ./data/sd_data_qc/20230607_sd_pull_person_dates_cleaned_overlapping_grids.txt
     * ./data/sd_data_qc/20230607_sd_pull_x_codes_dates_cleaned_overlapping_grids.txt
     * ./data/sd_data_qc/20230607_sd_pull_visit_occurrence_dates_cleaned_overlapping_grids.txt

### [CreateDescriptives_CareSites.r](https://github.com/amlake/care_sites/blob/master/CreateDescriptives_CareSites.r)
* Characterize care site by patient demographics
* Characterize care site by visit number and visit type
* Top ICD and CPT codes for each care site
* Get top 5 CPT codes for each care site
* Create table with all variables
  
### [MakeFigures.r](https://github.com/amlake/care_sites/blob/master/MakeFigures.r)

### Make Sankey plots
* [sankey_plots_1_compile_data.R](https://github.com/amlake/care_sites/blob/master/sankey_plots_1_compile_data.R)
   * Depends on: [CreateDescriptives_CareSites.r](https://bitbucket.org/davislabteam/care_sites/src/master/CreateDescriptives_CareSites.r)
   * Exclude from plots: unknown, administrative, phlebotomy, radiology, research
* [sankey_plots_2_make_plots_run_local.R](https://github.com/amlake/care_sites/blob/master/sankey_plots_2_make_plots_run_local.R)

### Depression analyses
#### [compile_data.R](https://github.com/amlake/care_sites/blob/master/mdd_application/compile_data.R)
#### [mdd_care_sites.Rmd](https://github.com/amlake/care_sites/blob/master/mdd_application/mdd_care_sites.Rmd)
#### [prs_regression.Rmd](https://github.com/amlake/care_sites/blob/master/mdd_application/prs_regression.Rmd)
#### [antidepressant_care_sites.Rmd](https://github.com/amlake/care_sites/blob/master/mdd_application/antidepressant_care_sites.Rmd)
#### [depression_st_analyses_descriptive_table.R](https://github.com/amlake/care_sites/blob/master/mdd_application/depression_st_analyses_descriptive_table.R)
