library(sqldf)
library(doBy)
library(tidyr)
library(data.table)
library(vroom)
library(dplyr)

setwd("/panfs/accrepfs.vampire/data/davis_lab/allie/care_sites/data/SpecialtyWASFiles/")

## Read in the Master demographics table.
specialtywas_demographics  <- vroom("SpecialtyWAS_Demographics_ENTIRESD.txt",show_col_types = FALSE)
nrow(specialtywas_demographics)
# 3712498

# Exclude subset without visits 
NoVisits <- specialtywas_demographics[is.na(specialtywas_demographics$AGE_MEDIAN_VISIT),]
nrow(NoVisits)
# 257877

Demographics <- specialtywas_demographics %>% filter(!(GRID %in% NoVisits$GRID))

### Create a age grouping variable.
GetAgeGrouping <- function(age) {
  
  if (age <10) { AgeGroup <- 1}
  else if (age <20) { AgeGroup <- 2}
  else if (age <30) { AgeGroup <- 3}
  else if (age <40) { AgeGroup <- 4}
  else if (age <50) { AgeGroup <- 5}
  else if (age <60) { AgeGroup <- 6}
  else if (age <70) { AgeGroup <- 7}
  else if (age <80) { AgeGroup <- 8}
  else  { AgeGroup <- 9}
  
  return(AgeGroup)
 }

DECADE_BIRTH <- apply(Demographics[,c('AGE_MEDIAN_VISIT'),drop=FALSE], 1, 
                  function(x) GetAgeGrouping(x['AGE_MEDIAN_VISIT']))

Demographics <- cbind(Demographics,DECADE_BIRTH)

#Read in the phenotype data.
CareSiteVisits <- vroom("CareSiteVisits_ENTIRESD.txt")
colnames(CareSiteVisits) <- c("GRID","JD_CODE","JD_CODE_COUNT","FIRSTDATE","MINAGE","MAXAGE")

# Define subject list
Cohort_GRIDs_EUR <- vroom("/data/davis_lab/allie/commonly_used/mega_maf_filter/plink_files/20200518_biallelic_mega_recalled.chr1-22.grid.EU.filt1.r2corrected.IBD.filtered.PI_HAT.0.2_maf0.005.fam", 
                        col_select = 2, col_names = F)
names(Cohort_GRIDs_EUR) <- "GRID"

Cohort_GRIDs_AFR <- vroom("/data/davis_lab/allie/commonly_used/mega_maf_filter/plink_files/20200515_biallelic_mega_recalled.chr1-22.grid.AA.filt1.r2corrected.IBD.filtered.PI_HAT.0.2_maf0.005.fam", 
                      col_select = 2, col_names = F)
names(Cohort_GRIDs_AFR) <- "GRID"

Cohort_GRIDs <- rbind(Cohort_GRIDs_EUR,Cohort_GRIDs_AFR)

SubjectMaster <- Cohort_GRIDs %>% left_join(Demographics,by="GRID") %>% filter(!is.na(AGE_MEDIAN_VISIT))

table(SubjectMaster$DECADE_BIRTH)

#Merge the phecodes with the subject list.
PhecodeDemo <- CareSiteVisits %>% filter(GRID %in% SubjectMaster$GRID)

#Get the age ranges (P1, P99) by JD_CODE
t1 <-summaryBy(MINAGE ~ JD_CODE, data = PhecodeDemo, 
                         FUN = function(x) { c(count=length(x),quantile=quantile(as.numeric(x),probs=c(0.01,0.99),na.rm=TRUE) ) })
t2 <-summaryBy(MAXAGE ~ JD_CODE, data = PhecodeDemo, 
                         FUN = function(x) { c(count=length(x),quantile=quantile(as.numeric(x),probs=c(0.01,0.99),na.rm=TRUE) ) })

JD_quantiles <- cbind(t1[,c('JD_CODE','MINAGE.count','MINAGE.quantile.1%')],t2[,c('MAXAGE.quantile.99%')])
colnames(JD_quantiles) <- c("JD_CODE","NumberSubjects","DECADE_BIRTH_Q01","DECADE_BIRTH_Q99")

######################################################################################
#For controls:
#   Make sure age is within quantiles.

## Create an index for the phecode table.
PhecodeDemo_Table <- as.data.table(PhecodeDemo)
#PhecodeDemo_Table$JD_CODE <- as.character(PhecodeDemo_Table$JD_CODE)

setkey(PhecodeDemo_Table, 'JD_CODE')

## Not applicable to PhecodeX
# Create an index for the phecode table.
# PhewasExcludeExpanded_Table <- as.data.table(PhewasExcludeExpanded[-1,])
# setkey(PhewasExcludeExpanded_Table, 'JD_CODE')

## Get subjects who are excluded based on pheWAS code.
# First parameter initiates function for the phecode provided. 
# Second parameter is the number of ICD codes to define a case.

GetCaseControl <-function(jdcode,Number_JD_Codes) {
  # Step 1: Get 1% and 99% for phecode
  CurrentJDcode <- subset(JD_quantiles, JD_CODE==jdcode)
  
  # Step 2: Get list of case subjects to exclude from control subjects
  # PE_t <- PhewasExcludeExpanded_Table[.(jdcode), allow.cartesian=TRUE]
  # PE_t_ExcludeList <- as.vector(as.character(sqldf('select distinct ExcludeCode from PE_t;')[,1]))
  # phenoSubset_Exclude <- PhecodeDemo_Table[.(PE_t_ExcludeList)]
  
  # Step 3: Because key is set as JD_CODE, PhecodeDemo_Table[.(jdcode)] selects all rows where key=jdcode
  phenoSubset <- PhecodeDemo_Table[.(jdcode), allow.cartesian=TRUE]
  
  # Step 4: Select control subjects WITHOUT age exclusions
  CasesAndControls <- sqldf(
    paste0('select distinct * from 
          (
            select distinct CurrentJDcode.JD_CODE, SubjectMaster.GRID as FID, SubjectMaster.GRID as IID, 0 as phenotype
               from SubjectMaster,CurrentJDcode
               where SubjectMaster.GRID not in (SELECT DISTINCT GRID FROM phenoSubset)
               and SubjectMaster.AGE_MAX_VISIT >= ', CurrentJDcode$DECADE_BIRTH_Q01, '
               and SubjectMaster.AGE_MIN_VISIT <= ', CurrentJDcode$DECADE_BIRTH_Q99, '

              UNION
          
           select distinct CurrentJDcode.JD_CODE, phenoSubset.GRID as FID, phenoSubset.GRID as IID, 1 as phenotype
               from phenoSubset,CurrentJDcode
               where phenoSubset.JD_CODE=CurrentJDcode.JD_CODE
                and phenoSubset.JD_CODE_COUNT>=',Number_JD_Codes,'
          )')
  )
  return(CasesAndControls)
}

#Only keep these phenotypes with >=10 observations.
remove(CasesAndControls,CurrentJDcode,SubjectsToExcludeCODEs)

JD_quantiles_GT10 <- subset(JD_quantiles, NumberSubjects > 10)

####################################
#1 or more cases.
# Because it is a data.table, PhecodeDemo_Table[,"JD_CODE] only selects unique codes. Table is initiated with first code observed.

CasesAndControls <- GetCaseControl(jdcode=JD_quantiles_GT10[1,"JD_CODE"],1)
fwrite(CasesAndControls, "CareSiteVisits_1orMore.txt.gz", sep="\t", row.names=FALSE, quote=FALSE, append = FALSE)
table(CasesAndControls$phenotype)

for (i in 2:nrow(JD_quantiles_GT10)) {
  fwrite(GetCaseControl(jdcode=JD_quantiles_GT10[i,"JD_CODE"],1), "CareSiteVisits_1orMore.txt.gz", sep="\t", row.names=FALSE,quote=FALSE, col.names=FALSE, append = TRUE)
}

####################################
#2 or more cases.
CasesAndControls <- GetCaseControl(jdcode=JD_quantiles_GT10[1,"JD_CODE"],2)
fwrite(CasesAndControls, "CareSiteVisits_2orMore.txt.gz", sep="\t", row.names=FALSE,quote=FALSE, append = FALSE)
table(CasesAndControls$phenotype)

for (i in 2:nrow(JD_quantiles_GT10)) {
  fwrite(GetCaseControl(jdcode=JD_quantiles_GT10[i,"JD_CODE"],2), "CareSiteVisits_2orMore.txt.gz", sep="\t", row.names=FALSE,quote=FALSE, col.names=FALSE, append = TRUE)
}

#Save the demographics data.
write.table(SubjectMaster, "CareSiteWAS_Demographics_MEGA.txt", sep="\t", row.names=FALSE,quote=FALSE)
write.table(SubjectMaster %>% filter(GRID %in% Cohort_GRIDs_EUR$GRID), "CareSiteWAS_Demographics_MEGA_EUR.txt", sep="\t", row.names=FALSE,quote=FALSE)
write.table(SubjectMaster %>% filter(GRID %in% Cohort_GRIDs_AFR$GRID), "CareSiteWAS_Demographics_MEGA_AFR.txt", sep="\t", row.names=FALSE,quote=FALSE)

#Identify the gender-specific phenotypes.
GenderCount <- CareSiteVisits %>% 
  left_join(SubjectMaster,by="GRID") %>%
  filter(!is.na(EPIC_GENDER)) %>%
  group_by(JD_CODE) %>%
  summarise(total = n(), Males = sum(EPIC_GENDER=="M"))

GenderCount$PercentMale <- (GenderCount$Males/GenderCount$total)

GenderSpecificPhenotypes <- data.frame(subset(GenderCount,(GenderCount$PercentMale <0.05  | GenderCount$PercentMale >0.95))[,"JD_CODE"])
colnames(GenderSpecificPhenotypes) <- "phenotype"

NotGenderSpecificPhenotypes <- data.frame(subset(GenderCount,!(GenderCount$PercentMale <0.05  | GenderCount$PercentMale >0.95))[,"JD_CODE"])
colnames(NotGenderSpecificPhenotypes) <- "phenotype"

#Save the transposed data set.
PheWASWrapper <- function(file) {
  Pheno  <- vroom(file)
  colnames(Pheno) <- c("phenotype","ID","IID","IsCase")
  PhenoTF=Pheno$IsCase>0
  Pheno2 <- cbind(Pheno,PhenoTF)
  
  #Transpose the data set.
  P <- Pheno2[,c("ID","phenotype","PhenoTF")]
  phenotypes <- spread(P,phenotype,PhenoTF)
  
  #Save the transposed data set.
  save(phenotypes,file=paste0(file,".Rda"))
  
  #Create the gender-specific phenotypes.
  t <- merge(Pheno,GenderSpecificPhenotypes)
  PhenoTF=t$IsCase>0
  Pheno2 <- cbind(t,PhenoTF)
  
  #Transpose the data set.
  P <- Pheno2[,c("ID","phenotype","PhenoTF")]
  phenotypes <- spread(P,phenotype,PhenoTF)
  
  #Save the transposed data set.
  save(phenotypes,file=paste0(file,"_GenderSpecific.Rda"))
  
  #Create the NOT gender-specific phenotypes.
  t <- merge(Pheno,NotGenderSpecificPhenotypes)
  PhenoTF=t$IsCase>0
  Pheno2 <- cbind(t,PhenoTF)
  
  #Transpose the data set.
  P <- Pheno2[,c("ID","phenotype","PhenoTF")]
  phenotypes <- spread(P,phenotype,PhenoTF)
  
  #Save the transposed data set.
  save(phenotypes,file=paste0(file,"_NON_GenderSpecific.Rda"))
}

PheWASWrapper("CareSiteVisits_1orMore.txt.gz") 
PheWASWrapper("CareSiteVisits_2orMore.txt.gz") 
