library(vroom)
library(dplyr)
library(tidyr)
library(openxlsx)
library(ggplot2)
library(ggsci)
library(ggthemr)
library(data.table)

ggthemr("pale")

setwd("/panfs/accrepfs.vampire/data/davis_lab/allie/care_sites")

# Import care site map data
CareSiteMap <- read.xlsx("./output/CareSite_VisitDetailCount_UPDATED_111524.xlsx", 1, colNames=T)
CareSiteMap_MultiSpecialty <- vroom("./output/CareSiteMap_Multispecialty_Long_UPDATED_111524.csv", delim=",")

# Summarize data
specialty_ncaresites <- CareSiteMap_MultiSpecialty %>% 
  select(care_site_id,SpecialtyLong) %>%
  filter(SpecialtyLong!="Unclear") %>%
  group_by(SpecialtyLong) %>%
  summarise(n = n_distinct(care_site_id)) %>%
  arrange(desc(n))

# ANNOTATE SPECIALTIES ACCORDING as SURGICAL,MEDICAL, OR OTHER 
# write.csv(specialty_ncaresites, file="./output/Figures/Descriptives/Specialties_NCareSites_Label_ForAnnotation.csv", row.names = F)

x_labels <- vroom("./output/Figures/Descriptives/Specialties_NCareSites_Label_ANNOTATED.csv", col_select = c(1,3))

###### Figure 1: Number of Care Sites per Specialty ###### 
figure1 <- specialty_ncaresites %>% 
  left_join(x_labels, by="SpecialtyLong") %>% 
  filter(row_number()<=20) %>%
  mutate(Group = factor(Group, levels = c("Medical", "Surgical", "Other")))

count_caresites_byspecialty <- ggplot(figure1, aes(x = reorder(SpecialtyLong, -n), y = n, fill = Group)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Mapped Specialty", y = "Number of Care Sites") +
  theme(
    text = element_text(size = 10), 
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7), 
    axis.text.y = element_text(size = 8),  
    axis.title.x = element_text(),
    axis.title.y = element_text(),
    legend.position = "right")

# ggsave("./output/Figures/Descriptives/Specialties_NCareSites.pdf", plot = count_caresites_byspecialty, 
#   width = 5, height = 4, units = "in", dpi = 400, device = "pdf")

###### Figure 1 alternate version: Number of Care Sites and Number of Visits per Specialty ###### 
spec_visit_ct <- CareSiteMap %>%
  select(care_site_id, Visits_N) %>%
  inner_join(CareSiteMap_MultiSpecialty, by="care_site_id") %>%
  group_by(SpecialtyLong) %>%
  summarise(Visits = sum(Visits_N)) %>%
  select(SpecialtyLong, Visits) %>% 
  distinct()

figure1_alt <- specialty_ncaresites %>%
  left_join(x_labels, by = "SpecialtyLong") %>%
  mutate(Group = factor(Group, levels = c("Medical", "Surgical", "Other"))) %>%
  left_join(spec_visit_ct, by="SpecialtyLong") %>%
  rename(n_sites = n) %>%
  mutate(n_visits_millions = Visits/1e6) %>%
  arrange(desc(n_visits_millions)) %>%
  head(n=20) %>%
  pivot_longer(cols = c(n_sites, n_visits_millions), names_to = "variable", values_to = "n") %>%
  mutate(var_label = ifelse(variable=="n_sites", "Number of Care Sites", "Number of Visits (Millions)"))

figure1_alt$var_label <- factor(figure1_alt$var_label, levels = c("Number of Visits (Millions)", "Number of Care Sites"))

count_caresites_visits_byspecialty <- ggplot(figure1_alt, aes(x = reorder(SpecialtyLong, -Visits), y = n, fill = Group)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "Mapped Specialty", y = "") +
  facet_wrap(~var_label, scales = "free_y", nrow = 2, strip.position = "left") +
  theme(
    text = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(),
    legend.position = "right",
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text = element_text(size = 12),
    plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 1, "cm")
  )

pdf("output/Specialties_NCareSites_NVisits.pdf", width = 7, height = 7)
print(count_caresites_visits_byspecialty)
dev.off()