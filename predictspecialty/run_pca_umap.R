# install.packages("RSpectra", dependencies = TRUE, INSTALL_opts = '--no-lock')
# install.packages("uwot", dependencies = TRUE, INSTALL_opts = '--no-lock')
library(uwot)
library(tidyr)
library(dplyr)
library(ggplot2)
library(scales)
library(psych) 
library(plotly)
library(tm)
library(tidytext)
library(ggpubr)
library(pathwork)

setwd("/data/davis_lab/allie/care_sites/output/SpecialtyPrediction/")

CareSiteMap <- read.xlsx("../CareSite_VisitDetailCount_UPDATED_JPS_052325.xlsx", 1, colNames=T)
CareSiteMap_MultiSpecialty <- vroom("../CareSiteMap_Multispecialty_Long_UPDATED_JPS_052325.csv", delim=",")
SpecialtyGroups <- vroom("../Figures/Descriptives/Specialties_NCareSites_Label_ANNOTATED.csv")

FilteredCareSites <- read.xlsx("../CareSite_VisitDetailCount_FILTERED_JPS_052325.xlsx", 1, colNames=T) 
FilteredCareSites <- FilteredCareSites %>% 
  left_join(SpecialtyGroups %>% select(MappedSpecialty,Group), by="MappedSpecialty")

# Import CPT and phecode rankings
topcpt_dtm <- vroom("./dtm_topcodes_cpt_bycaresite.txt")
topphe_dtm <- vroom("./dtm_topcodes_phe_bycaresite.txt")
topphecpt_dtm <- vroom("./dtm_topcodes_phecpt_bycaresite.txt")

###### Make principal components for combined phecode & CPT code dataset ######
pca_fit <- prcomp(topphe_dtm, scale. = TRUE)

# Calculate PCs needed for 95% variance
cum_prop_var_explained <- data.frame(cumulative_summary = cumsum(pca_fit$sdev^2/sum(pca_fit$sdev^2)))
n_pc_95 <- which(cum_prop_var_explained$cumulative_summary >= 0.95)[1]
cat("PCs for 95% variance:", n_pc_95, "\n")

# Get PCA subset and merge with labels
pca_subset <- as.data.frame(pca_fit$x[, 1:n_pc_95])
pca_subset$care_site_id <- topphe_dtm$care_site_id

pca_labeled <- pca_subset %>%
  left_join(FilteredCareSites %>% select(care_site_id, MappedSpecialty, Group), by = "care_site_id") %>%
  filter(!is.na(MappedSpecialty))

# Extract matrix for UMAP (exclude care_site_id, MappedSpecialty, Group)
pca_matrix <- as.matrix(pca_labeled %>% select(starts_with("PC")))

# Unsupervised UMAP
set.seed(142)
umap_fit <- uwot::umap(pca_matrix,
                       n_neighbors = 30,
                       min_dist = 0.05,
                       spread = 1.5,
                       metric = "cosine",
                       n_epochs = 500,
                       negative_sample_rate = 10)

# Create mapping dataframe
umap_mapping <- data.frame(
  UMAP1 = umap_fit[, 1],
  UMAP2 = umap_fit[, 2],
  care_site_id = pca_labeled$care_site_id,
  MappedSpecialty = pca_labeled$MappedSpecialty,
  Group = pca_labeled$Group
)

# Get all unique specialties
all_specialties <- sort(unique(umap_mapping$MappedSpecialty))
n_specialties <- length(all_specialties)

# High-contrast global palette - hand-picked distinct colors
global_colors <- c(
  "#e6194b", "#3cb44b", "#ffe119", "#4363d8", "#f58231",
  "#911eb4", "#46f0f0", "#f032e6", "#bcf60c", "#fabebe",
  "#008080", "#e6beff", "#9a6324", "#800000", "#aaffc3",
  "#808000", "#ffd8b1", "#000075", "#469990", "#dcbeff",
  "#1E90FF", "#FF6347", "#32CD32", "#FFD700", "#8A2BE2",
  "#00CED1", "#FF4500", "#2E8B57", "#DC143C", "#00FA9A",
  "#4169E1", "#FF69B4", "#8B0000", "#00FF7F", "#B22222",
  "#20B2AA", "#FF1493", "#228B22", "#FF8C00", "#48D1CC",
  "#C71585", "#006400", "#DB7093", "#556B2F", "#CD853F",
  "#DAA520", "#6B8E23", "#D2691E", "#CD5C5C", "#8B4513",
  "#B8860B", "#2F4F4F", "#A0522D", "#D2B48C", "#BC8F8F"
)[1:n_specialties]

names(global_colors) <- all_specialties

# Calculate shared axis limits from full data
x_range <- range(umap_mapping$UMAP1, na.rm = TRUE)
y_range <- range(umap_mapping$UMAP2, na.rm = TRUE)

x_buffer <- (x_range[2] - x_range[1]) * 0.05
y_buffer <- (y_range[2] - y_range[1]) * 0.05

x_lim <- c(x_range[1] - x_buffer, x_range[2] + x_buffer)
y_lim <- c(y_range[1] - y_buffer, y_range[2] + y_buffer)

# All sites - NO legend
p1 <- ggplot(data = umap_mapping, aes(x = UMAP1, y = UMAP2, color = MappedSpecialty)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "UMAP1", y = "UMAP2", title = "All Sites") +
  theme_minimal() +
  coord_fixed(xlim = x_lim, ylim = y_lim) +
  theme(text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none",
        plot.title = element_text(size = 16, hjust = 0.5, face = "bold")) +
  scale_color_manual(values = global_colors)

# Medical specialties - WITH legend
p2 <- ggplot(data = umap_mapping %>% filter(Group == "Medical"), aes(x = UMAP1, y = UMAP2, color = MappedSpecialty)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "UMAP1", y = "UMAP2", title = "Medical Specialties") +
  theme_minimal() +
  coord_fixed(xlim = x_lim, ylim = y_lim) +
  theme(text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "right",
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.key.size = unit(0.5, "cm"),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_color_manual(values = global_colors) +
  guides(color = guide_legend(ncol = 1))

# Surgical specialties - WITH legend
p3 <- ggplot(data = umap_mapping %>% filter(Group == "Surgical"), aes(x = UMAP1, y = UMAP2, color = MappedSpecialty)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "UMAP1", y = "UMAP2", title = "Surgical Specialties") +
  theme_minimal() +
  coord_fixed(xlim = x_lim, ylim = y_lim) +
  theme(text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "right",
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.key.size = unit(0.5, "cm"),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_color_manual(values = global_colors) +
  guides(color = guide_legend(ncol = 1))

# Other specialties - WITH legend
p4 <- ggplot(data = umap_mapping %>% filter(Group == "Other"), aes(x = UMAP1, y = UMAP2, color = MappedSpecialty)) +
  geom_point(size = 2, alpha = 0.7) +
  labs(x = "UMAP1", y = "UMAP2", title = "Other Specialties") +
  theme_minimal() +
  coord_fixed(xlim = x_lim, ylim = y_lim) +
  theme(text = element_text(size = 12),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = "right",
        legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.key.size = unit(0.5, "cm"),
        plot.title = element_text(size = 14, hjust = 0.5, face = "bold")) +
  scale_color_manual(values = global_colors) +
  guides(color = guide_legend(ncol = 1))

# Combine in 2x2 grid
combined_plot <- (p1 + p2) / (p3 + p4) +
  plot_annotation(tag_levels = 'A', 
                  theme = theme(plot.tag = element_text(size = 18, face = "bold")))

# Save combined plot
ggsave("../Figures/SpecialtyPrediction/umap_combined.pdf", 
       plot = combined_plot, 
       width = 18, 
       height = 16, 
       dpi = 400)

# Save individual plots
ggsave("../Figures/SpecialtyPrediction/umap_all_sites.pdf", plot = p1, width = 10, height = 10, dpi = 400)
ggsave("../Figures/SpecialtyPrediction/umap_medical.pdf", plot = p2, width = 10, height = 10, dpi = 400)
ggsave("../Figures/SpecialtyPrediction/umap_surgical.pdf", plot = p3, width = 10, height = 10, dpi = 400)
ggsave("../Figures/SpecialtyPrediction/umap_other.pdf", plot = p4, width = 10, height = 10, dpi = 400)