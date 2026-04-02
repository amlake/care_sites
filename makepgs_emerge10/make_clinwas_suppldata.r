library(dplyr)
library(vroom)
library(openxlsx)

setwd("/data/davis_lab/allie/care_sites/output/eMERGE_Analyses")

##### ClinicWAS for CHD: identify associated sites ##### 
clinicwas_chd_18plus <- vroom("/data/davis_lab/allie/care_sites/output/CareSiteWAS_eMERGE_PGS/CHD/CareSiteVisits_1orMore.txt.gz_NON_GenderSpecific.Rda18orOver.txt")
clinicwas_chd_18plus <- clinicwas_chd_18plus %>% 
    rename(care_site_id="phenotype") %>% 
    filter(MappedSpecialty != "Phlebotomy" & MappedSpecialty != "Radiology"  & MappedSpecialty != "Research") %>%
    filter(n_cases >=100) %>%
    mutate(BonfSignif = negLogPval >= -log10(0.05/nrow(.)))  # Add Bonferroni significance indicator

# ##### ClinicWAS for Asthma: identify associated sites ##### 
# clinicwas_asthma_under18 <- vroom("/data/davis_lab/allie/care_sites/output/CareSiteWAS_eMERGE_PGS/Asthma/CareSiteVisits_1orMore.txt.gz_NON_GenderSpecific.RdaUnder18.txt")
# clinicwas_asthma_under18 <- clinicwas_asthma_under18 %>% 
#     rename(care_site_id="phenotype") %>% 
#     filter(MappedSpecialty != "Phlebotomy" & MappedSpecialty != "Radiology"  & MappedSpecialty != "Research") %>%
#     filter(n_cases >=100) %>%
#     mutate(BonfSignif = negLogPval >= -log10(0.05/nrow(.)))  # Add Bonferroni significance indicator
# 
# ##### PheWAS for Asthma risk ##### 
# asthma_pgs_phewas_ped_endo <- readRDS("/data/davis_lab/allie/care_sites/output/PheWAS_eMERGE_PGS/asthma_pgs_phewas_percentile_pediatric_endo_cohort_122224.Rds")
# asthma_pgs_phewas_pediatric <- readRDS("/data/davis_lab/allie/care_sites/output/PheWAS_eMERGE_PGS/asthma_pgs_phewas_percentile_pediatric_cohort_011725.Rds")
# asthma_pgs_phewas_ped_endo_nohla <- readRDS("/data/davis_lab/allie/care_sites/output/PheWAS_eMERGE_PGS/asthma_nohla_pgs_phewas_percentile_pediatric_endo_cohort_011725.Rds")

# Create Excel workbook with both datasets
wb <- createWorkbook()
addWorksheet(wb, "CHD_ClinicWAS")
# addWorksheet(wb, "Asthma_ClinicWAS")
# addWorksheet(wb, "Asthma_PedEndoPheWAS")
# addWorksheet(wb, "Asthma_PedPheWAS")
# addWorksheet(wb, "Asthma_PedEndoPheWAS_noHLA")

writeData(wb, "CHD_ClinicWAS", clinicwas_chd_18plus %>% arrange(desc(negLogPval)))
# writeData(wb, "Asthma_ClinicWAS", clinicwas_asthma_under18 %>% arrange(desc(negLogPval)))
# writeData(wb, "Asthma_PedEndoPheWAS", asthma_pgs_phewas_ped_endo %>% arrange(p))
# writeData(wb, "Asthma_PedPheWAS", asthma_pgs_phewas_pediatric %>% arrange(p))
# writeData(wb, "Asthma_PedEndoPheWAS_noHLA", asthma_pgs_phewas_ped_endo_nohla %>% arrange(p))

saveWorkbook(wb, "SupplData4_ClinicWAS_CHD.xlsx", overwrite = TRUE) 