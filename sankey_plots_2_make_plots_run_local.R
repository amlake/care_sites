## Care site Sankey plots step 2: make plots
## Run locally on Allie's macbook because of graphics issues on ACCRE

setwd("/data/davis_lab/allie/care_sites/")
pacman::p_load(data.table, dplyr, foreach, PheWAS, ggplot2, plotly, ggpubr, scales, RColorBrewer, ggrepel, kableExtra, broom, networkD3, xlsx, stringr)

#### Load data exported from ACCRE ####


#### Define plotting function ####
## care site --> phecode --> specialty (divided into medical, surgical, other)
sankey <- function(Map) {
  if (FALSE) { ## testing
    Map <- map[visit_rank <= 50 & Group == "Medical"]
  }
  
  sites <- unique(Map$care_site_name)
  specs <- unique(Map$SpecialtyLong)
  
  ## change name of ENT specialty, which is also the name of a care site
  specs[specs == "ENT"] <- "Otolaryngology"
  Map[SpecialtyLong == "ENT", SpecialtyLong := "Otolaryngology"]
  
  ## create nodes for each care site and each specialty
  nodes <- data.table(name = c(sites, specs))
  
  ## make sure there are no other duplicates
  stopifnot(n_distinct(nodes$name) == nrow(nodes))
  
  nodes[, id := 0:(.N-1)]
  
  ## create links between care site nodes and specialty nodes
  links <- merge(nodes[,.(id, name)], Map[,.(care_site_name, SpecialtyLong)], by.x = "name", by.y = "care_site_name")
  setnames(links, "id", "source")
  
  links <- merge(links, nodes[,.(id, name)], by.x = "SpecialtyLong", by.y = "name")
  setnames(links, "id", "target")
  
  links$value <- 10
  
  nodes <- nodes[,.(id, name)]
  links <- links[,.(source, target, value)]
  
  site_spec_links <- copy(links)
  
  ## create nodes for phecodes
  phenodes <- Map %>%
    select(name = pherank_1_category) %>% 
    data.table() %>% unique()
  phenodes[,id := nrow(nodes):(nrow(nodes)+.N-1)]
  
  nodes <- rbind(nodes,phenodes)
  
  ## create links between care site nodes and phecode nodes
  site_phe_links <- merge(nodes, Map[,.(care_site_name, pherank_1_category)], by.x = "name", by.y = "care_site_name")
  setnames(site_phe_links, "id", "source")
  
  site_phe_links <- merge(site_phe_links, nodes[,.(id,name)], by.x = "pherank_1_category", by.y = "name")
  setnames(site_phe_links, "id", "target")
  
  site_phe_links$value <- 10
  site_phe_links <- site_phe_links[,.(source, target, value)]
  
  links <- rbind(links, site_phe_links)
  
  ## create links between phecode nodes and specialties
  phe_spec_links <- merge(nodes, Map[,.(SpecialtyLong, pherank_1_category)], by.x = "name", by.y = "pherank_1_category")
  setnames(phe_spec_links, "id", "source")
  
  phe_spec_links <- merge(phe_spec_links, nodes[,.(id,name)], by.x = "SpecialtyLong", by.y = "name")
  setnames(phe_spec_links, "id", "target")
  
  phe_spec_links$value <- 50
  phe_spec_links <- phe_spec_links[,.(source, target, value)]
  
  links <- rbind(links, phe_spec_links)
  
  # capitalize phecode categories
  nodes[name %in% map$pherank_1_category, name := str_to_title(name)]
  
  # manual abbreviation of some clinic names to save space
  nodes[, name := gsub("OPHTHALMOLOGY","OPHTH",name)]
  nodes[, name := gsub("COMPREHENSIVE","COMP",name)]
  nodes[, name := gsub("ORTHOPAEDICS","ORTHO",name)]
  nodes[, name := gsub("ORTHOPEDICS","ORTHO",name)]
  nodes[, name := gsub("ZZZ-","",name)]
  nodes[, name := gsub("OTOLARYNGOLOGY","OTOLARYNG",name)]
  nodes[, name := gsub("_[[:digit:]]*","",name)]
  
  # cut off names at N characters
  char_max <- 25
  nodes[, name := substr(name,1,char_max)]
  nodes[nchar(name) == char_max, name := paste0(trimws(name),"...")]
  
  if (FALSE) {
    # change care site ID names to blank strings
    nodes[id %in% 0:(n_distinct(Map$care_site_id)-1), name := ""]
  }
  
  p <- sankeyNetwork(Links = rbind(site_phe_links,phe_spec_links), Nodes = nodes, Source = "source",
                     Target = "target", Value = "value", NodeID = "name",
                     units = "TWh", fontSize = 16, nodeWidth = 40, height = 500, width = 700)
  
  # p <- sankeyNetwork(Links = rbind(site_phe_links,phe_spec_links), Nodes = nodes, Source = "source",
  #                   Target = "target", Value = "value", NodeID = "name",
  #                   units = "TWh", fontSize = 12, nodeWidth = 20, height = 800, width = 1000)
  return(p)
}


#### Generate plots ####
sankey(map[visit_rank <= 50 & Group == "Medical"])
sankey(map[visit_rank <= 50 & Group == "Surgical"])
sankey(map[visit_rank <= 50 & Group == "Other"])
