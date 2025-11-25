## Care site Sankey plots step 2: make plots
## Run locally on Allie's macbook because of graphics issues on ACCRE

#### Set up and load data ####
setwd("~/Projects_local/care_sites/")
pacman::p_load(data.table, dplyr, foreach, PheWAS, ggplot2, plotly, ggpubr, scales, RColorBrewer, ggrepel, kableExtra, broom, networkD3, stringr)

## --- HELPER FUNCTION: R COLOR TO HEX ---
## Define this first so it is available for the code below
col2hex <- function(color_names) {
  # col2rgb converts names (e.g. "coral4") to a matrix of RGB values
  rgb_vals <- col2rgb(color_names)
  # rgb converts those RGB values to Hex strings (e.g. "#8B3E2F")
  hex_vals <- rgb(rgb_vals[1,], rgb_vals[2,], rgb_vals[3,], maxColorValue = 255)
  return(hex_vals)
}

## load data exported from ACCRE
map <- fread("data/map_formatted_for_sankey_plots_temp_112525.tsv")

## extract color groupings from PheWAS package
phecolors <- unique(data.table(PheWAS::pheinfo)[,.(group, color)])
phecolors[,group := gsub(" ", "", str_to_title(group))]
phecolors[, color := col2hex(color)] # CONVERT COLORS TO HEX

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
  nodes <- data.table(name = c(sites, specs), 
                      type = c(rep("site", length(sites)), rep("specialty", length(specs))))
  
  ## make sure there are no other duplicates
  stopifnot(n_distinct(nodes$name) == nrow(nodes))
  
  nodes[, id := 0:(.N-1)]

  ## create links between care site nodes and specialty nodes
  links <- merge(nodes[,.(id, name)], Map[,.(care_site_name, SpecialtyLong)], by.x = "name", by.y = "care_site_name")
  setnames(links, "id", "source")
  
  links <- merge(links, nodes[,.(id, name)], by.x = "SpecialtyLong", by.y = "name")
  setnames(links, "id", "target")
  
  links$value <- 10
  
  nodes <- nodes[,.(id, name, type)]
  links <- links[,.(source, target, value)]
  
  site_spec_links <- copy(links)
  
  ## create nodes for phecodes
  phenodes <- Map %>%
    select(name = pherank_1_category) %>% 
    data.table() %>% unique()
  phenodes[,id := nrow(nodes):(nrow(nodes)+.N-1)]
  phenodes[, type := "phecode"]
  
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
  
  phe_spec_links$value <- 40
  phe_spec_links <- phe_spec_links[,.(source, target, value)]
  
  links <- rbind(links, phe_spec_links)
  
  # manual abbreviation of some clinic names to save space or correct typos
  nodes[, name := gsub("OPHTHALMOLOGY","OPHTH",name)]
  nodes[, name := gsub("COMPREHENSIVE","COMP",name)]
  nodes[, name := gsub("ORTHOPAEDICS","ORTHO",name)]
  nodes[, name := gsub("ORTHOPEDICS","ORTHO",name)]
  nodes[, name := gsub("ZZZ-","",name)]
  nodes[, name := gsub("OTOLARYNGOLOGY","OTOLARYNG",name)]
  nodes[, name := gsub("_[[:digit:]]*","",name)]
  nodes[, name := gsub("CLINIE","CLINIC",name)]
  
  # cut off names at N characters for plotting
  char_max <- 25
  nodes[, name := substr(name,1,char_max)]
  nodes[nchar(name) == char_max, name := paste0(trimws(name),"...")]
  
  if (FALSE) {
    # change care site ID names to blank strings
    nodes[id %in% 0:(n_distinct(Map$care_site_id)-1), name := ""]
  }
  
  # capitalize phecode categories and remove spaces
  nodes[type == "phecode", name := gsub(" ", "", str_to_title(name))]
  
  # double check no duplicate node names
  stopifnot(nrow(nodes) == length(unique(nodes$name)))
  
  # --- NEW: GENERATE COLOR SCALE ---
  # This uses the 'type' column we added earlier to assign specific colors
  
  # 1. Start with the nodes table
  color_map <- copy(nodes)
  
  # 2. Assign default Greys
  color_map[, hex := "#bbbbbb"] # Default Light Grey (Specialties)
  color_map[type == "site", hex := "#555555"] # Dark Grey (Care Sites)
  
  # 3. Assign PheWAS colors to phecode category
  # Match phecode category name to the phecolors table
  color_map[type == "phecode", hex := phecolors$color[match(name, phecolors$group)]]
  
  # Fallback for any non-matching phecodes (default to black/dark grey)
  color_map[type == "phecode" & is.na(hex), hex := "#333333"]

  # 4. Build JS String
  # We combine just the nodes into the domain. Links will use default gray.
  final_domain <- color_map$name
  final_range  <- color_map$hex
  
  domain_js <- paste0('"', final_domain, '"', collapse = ", ")
  range_js  <- paste0('"', final_range, '"', collapse = ", ")
  
  custom_color_scale <- paste0(
    'd3.scaleOrdinal() .domain([', domain_js, ']) .range([', range_js, '])'
  )
  # --------------------------------- 
  
  # make plot
  p <- sankeyNetwork(Links = unique(rbind(site_phe_links,phe_spec_links)), Nodes = unique(nodes), Source = "source",
                     NodeGroup = "name", colourScale = custom_color_scale,
                     Target = "target", Value = "value", NodeID = "name",
                     fontFamily = "Arial", fontSize = 14,
                     nodeWidth = 14, nodePadding = 20,
                     units = "TWh", height = 600, width = 900)
  return(p)
}


#### Generate plots ####
sankey(map[visit_rank <= 20 & Group == "Medical"])

# sankey(map[visit_rank <= 50 & Group == "Medical"])
# sankey(map[visit_rank <= 50 & Group == "Surgical"])
# sankey(map[visit_rank <= 50 & Group == "Other"])
