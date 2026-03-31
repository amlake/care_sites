pacman::p_load(data.table, dplyr, ggplot2, ggrepel, scales)

## input: 
## dat = data frame with the following columns:
##  - care_site_id
##  - care_site_name
##  - specialty
##  - n_phe_per_site
##  - freq_phe_per_site

# dat <- copy(caresite_plot_df)

caresite_manhattan_bar <- function(dat, points = FALSE, flip = FALSE) {
    dat <- as.data.table(as.data.frame(dat))

    plot_df <- dat %>%
        select(care_site_id, care_site_name, specialty, n_phe_per_site, freq_phe_per_site) %>%
        group_by(specialty) %>%
        mutate(N = n()) %>% ## number of data points per specialty, for plotting purposes
        data.table()

    plot_df[, N := ifelse(N<3, N + 10, N + 5)]

    ## Group specialties into 3 groups: Medical, Surgical, Other
    Specialties_3group <- fread("/data/davis_lab/allie/care_sites/output/Figures/Descriptives/Specialties_NCareSites_Label_ANNOTATED.csv") %>%
        rename(specialty = MappedSpecialty) %>%
        filter(specialty %in% plot_df$specialty) %>%
        select(specialty, Group) %>%
        data.table()

    Specialties_3group <- merge(Specialties_3group, unique(plot_df[, .(specialty, N)]), by = "specialty")
    Specialties_3group$Group <- factor(Specialties_3group$Group, levels = c("Medical", "Surgical", "Other"))

    # Create group to alternate light/dark shades
    group_colors <- c(
        "Medical_light" = "#e64a358a", "Medical_dark" = "#E64B35",
        "Surgical_light" = "#3c548881", "Surgical_dark" = "#3C5488",
        "Other_light" = "#00a0887e", "Other_dark" = "#00A087"
    )

    # Create a base x-axis value for each specialty
    specialty_base <- Specialties_3group
    setorder(specialty_base, Group, specialty)
    specialty_base[, base_value := cumsum(N) - N]

    # Order the care sites by specialty and then by care site name
    plot_df <- merge(plot_df, Specialties_3group[, .(specialty, Group)], by = "specialty")
    plot_df <- merge(plot_df, specialty_base[, .(specialty, base_value)], by = "specialty")
    
    setorder(plot_df, specialty, care_site_name)
    plot_df[, id := seq_len(.N), by = specialty]
    plot_df[, id := rowid(specialty)]
    plot_df[, RowNumber := base_value + id]

    plot_df <- plot_df %>%
        group_by(Group, specialty) %>%
        mutate(cur_group_id = cur_group_id()) %>%
        mutate(Group_Shade = ifelse(cur_group_id %% 2 == 0, paste(Group, "light", sep = "_"), paste(Group, "dark", sep = "_"))) %>%
        mutate(Specialty_midpoint = median(RowNumber)) %>%
        ungroup() %>%
        data.table()

    midpoint_labels <- plot_df %>%
        group_by(specialty) %>%
        summarize(midpoint = median(RowNumber)) %>%
        ungroup()

    if (FALSE) {
        N_spec <- n_distinct(plot_df$specialty)
        pal <- rev(rep(brewer.pal(n = 8, name = "Set2"), 6)[1:N_spec])
        spec_order <- unique(plot_df[, .(specialty)])
        spec_order$Color <- pal
        plot_df <- merge(plot_df, spec_order, by = "specialty")
    }

    ## label the top care site in each specialty (after filtering out those with freq_phe_per_site < 0.01)
    plot_df_label <- plot_df %>%
        filter(abs(freq_phe_per_site) >= 0.01) %>%
        arrange(desc(abs(freq_phe_per_site))) %>%
        group_by(specialty) %>%
        slice(1:1)

    ## create the plot
    p <- ggplot(plot_df, aes(x = RowNumber, y = freq_phe_per_site*100, fill = Group_Shade))

    if (points == TRUE) { 
        p <- p + geom_point(shape = ifelse(flip == TRUE, 25, 24), size = 3)
    } else {
        p <- p + geom_bar(stat = "identity", color = "black")
    }

    p <- p +
        geom_label_repel(
            data = plot_df_label,
            aes(label = care_site_name),
            size = 4,
            box.padding = 0.5, point.padding = 0.5,
            vjust = ifelse(flip==TRUE, -0.75, 0.75), hjust = 0.5, max.overlaps = 10,
            min.segment.length = 0
        ) +
        scale_x_continuous(breaks = midpoint_labels$midpoint, labels = midpoint_labels$specialty) +
        scale_fill_manual(values = group_colors) +
        # scale_fill_identity()  +
        theme_minimal(base_family = "Helvetica") +
        theme(
            text = element_text(size = 11),
            axis.text.x = element_text(size = 9, angle = 45, hjust = 1, color = ifelse(flip == TRUE, "white", "black")),
            axis.text.y = element_text(size = 9),
            plot.title = element_text(hjust = 0.5),
            axis.title.y = element_text(hjust = 0.5),
            legend.position = "none",
            panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
            plot.margin =  margin(t = 0.2, r = 0, b = -0.5, l = 0.2, unit = "cm")
        )

    if (flip == TRUE) {
        p <- p + scale_y_reverse()
    }

    return(p)
}