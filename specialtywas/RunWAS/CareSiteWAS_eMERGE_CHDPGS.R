# devtools::install_github("PheWAS/PheWAS")
library(PheWAS)
library(sqldf)
library(tidyr)
library(dplyr)
library(vroom)
library(openxlsx)

setwd("/panfs/accrepfs.vampire/data/davis_lab/allie/care_sites/output/CareSiteWAS_eMERGE_PGS/CHD")

#Read in the genetic risk score.
genotypes <- read.table("../../../data/makepgs_emerge10/makepgs_chd/output/results_chd_eur_111725/chd_eur_111725.profile",as.is=T, header=T, sep="",quote = "")
colnames(genotypes)[2] <- "ID" 

#Standardize
genotypes$PGS_std <- as.numeric(scale(genotypes$SCORE, center = TRUE, scale = TRUE))
genotypes$PGS_percentile <- rank(genotypes$PGS_std)/length(genotypes$PGS_std)*100
genotypes$PGS_group <- cut(genotypes$PGS_percentile, 
                          breaks = c(0, 1, 5, 10, 90, 95, 99, 100),
                          labels = c("Bottom 1%", "1-5%", "5-10%", "10-90%", "90-95%", "95-99%", "Top 1%"),
                          include.lowest = TRUE)

genotypes %>% count(PGS_group)

genotypes$PGS_highrisk <- ifelse(genotypes$PGS_percentile>95, 1, 0)
genotypes %>% 
  group_by(PGS_highrisk) %>% 
  summarise(median(PGS_std), 
            quantile(PGS_std, 0.05), 
            quantile(PGS_std, 0.25), 
            quantile(PGS_std, 0.75), 
            quantile(PGS_std, 0.95))

covar <- vroom("../../../data/SpecialtyWASFiles/CareSiteWAS_Demographics_MEGA_EUR.txt")
colnames(covar)[1] <- "ID" 

PC <- read.table("/data/davis_lab/shared/genotype_data/biovu/processed/imputed/best_guess/MEGA/MEGA_recalled/MEGA_recalled/20190827_MEGA.GRID.RACE.ETH.GEN.batch.PCs.covariates_EU.filt1.txt", 
                 as.is=T, header=T, sep="\t",quote = "")
colnames(PC)[1] <- "ID" 

covar_PC <- covar %>%
  filter(ID %in% PC$ID & !is.na(AGE_MEDIAN_VISIT)) %>% 
  left_join(PC[,c("ID","PC1","PC2","PC3","PC4","PC5")], by="ID")

#Force the age to be numeric.

covar_PC$AgeMedian <- as.numeric(covar_PC$AGE_MEDIAN_VISIT)

PheWASWrapper <- function(covar_PC, Adjustgender, file, FileExtension) {
  
  #Load the transposed pheWAS data.
  
  load(paste0("../../../data/SpecialtyWASFiles/",file))
  print(nrow(phenotypes))
  
  #Run the PheWAS.
  
  if(Adjustgender==TRUE) {
    results=phewas(phenotypes,genotypes[,c("ID","PGS_highrisk")],cores=16,min.records=10,covariates=covar_PC[,c("ID","EPIC_GENDER","AgeMedian","PC1","PC2","PC3","PC4","PC5")],additive.genotypes=F) 
  }
  else {
    results=phewas(phenotypes,genotypes[,c("ID","PGS_highrisk")],cores=16,min.records=10,covariates=covar_PC[,c("ID","AgeMedian","PC1","PC2","PC3","PC4","PC5")],additive.genotypes=F) 
  }
  
  print(nrow(results))  
  print(head(results))  
  
  #Make variables
  LCI <- round(exp(results$beta-(1.96*results$SE)),digits=2)
  UCI <- round(exp(results$beta+(1.96*results$SE)),digits=2)
  negLogPval=round(-1*log10(results$p),digits=2)
  results1 <- cbind(results,LCI,UCI,negLogPval)
  colnames(results1)[colnames(results1)=="OR"] <- "OddsRatio"
  results1$OddsRatio <- round(results1$OddsRatio,digits=2)
  
  # Annotate
  map <- read.xlsx("/panfs/accrepfs.vampire/data/davis_lab/allie/care_sites/output/CareSite_VisitDetailCount_UPDATED_JPS_052325.xlsx")
  map1 <- map %>% 
    mutate(care_site_id = as.character(care_site_id)) %>%
    select(care_site_id, care_site_name, MappedSpecialty, IsMultiSpecialty, MultiSpecialty_Secondary, MultiSpecialty_Tertiary)
  results2 <- results1 %>% left_join(map1, by=c("phenotype"="care_site_id"))
  
  write.table(results2 %>% arrange(desc(negLogPval)), paste0(file,FileExtension,".txt") , sep="\t", row.names=FALSE,quote=FALSE)
}

#All subjects
PheWASWrapper(covar_PC,TRUE,"CareSiteVisits_1orMore.txt.gz.Rda","AllSubjects") 
PheWASWrapper(covar_PC,TRUE,"CareSiteVisits_2orMore.txt.gz.Rda","AllSubjects") 

#Age groups.
PheWASWrapper(subset(covar_PC,AGE_MIN_VISIT>=18),TRUE,"CareSiteVisits_1orMore.txt.gz_NON_GenderSpecific.Rda","18orOver") 
PheWASWrapper(subset(covar_PC,AGE_MIN_VISIT>=18),TRUE,"CareSiteVisits_2orMore.txt.gz_NON_GenderSpecific.Rda","18orOver") 

PheWASWrapper(subset(covar_PC,AGE_MIN_VISIT<18),TRUE,"CareSiteVisits_1orMore.txt.gz_NON_GenderSpecific.Rda","Under18") 
PheWASWrapper(subset(covar_PC,AGE_MIN_VISIT<18),TRUE,"CareSiteVisits_2orMore.txt.gz_NON_GenderSpecific.Rda","Under18") 

#Gender-specific analyses.
PheWASWrapper(covar_PC,FALSE,"CareSiteVisits_1orMore.txt.gz_GenderSpecific.Rda","AllSubjects") 
PheWASWrapper(covar_PC,FALSE,"CareSiteVisits_1orMore.txt.gz_GenderSpecific.Rda","AllSubjects") 