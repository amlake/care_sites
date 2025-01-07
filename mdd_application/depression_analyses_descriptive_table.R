pacman::p_load(data.table, dplyr, table1, lubridate)
setwd("/data/davis_lab/allie/care_sites")

## Rounding function for table1
my.render.cont <- function(x) {
    with(
        stats.default(x),
        c("",
            "Mean (SD)" = sprintf(
                "%s (%s)",
                round_pad(MEAN, 1),
                round_pad(SD, 1)
            ),
            "Median (25%, 75%)" = sprintf(
                "%s (%s, %s)",
                round_pad(MEDIAN, 1),
                round_pad(q25, 1),
                round_pad(q75, 1)
            )
        )
    )
}

person <- fread("data/sd_data_qc/20230607_sd_pull_person_dates_cleaned_overlapping_grids.txt")

# collapse race categories
person <- person %>% 
    mutate(race = case_when(
        race_source_value == "W" ~ "White",
        race_source_value == "B" ~ "Black",
        race_source_value == "A" ~ "Asian",
        race_source_value %in% c("H","I","N") ~ "Other",
        grepl(",", race_source_value) ~ "Multiple",
        race_source_value %in% c("D","U") ~ "Unspecified"
    ))

## Depression code cohort
dep_cohort <- readRDS("/data/davis_lab/allie/care_sites/data/mdd_application/20230607_sd_pull_visit_occurrence_earliest_dep_icd_joined_by_date_010525.Rds") %>%
    group_by(GRID) %>%
    filter(visit_start_date == min(visit_start_date)) %>%
    ungroup() %>%
    left_join(person, by = "GRID") %>%
    mutate(DOB = as.Date(birth_datetime)) %>%
    mutate(visit_age = as.numeric(difftime(visit_start_date, DOB, units = "days")) / 365.25) %>%
    select(GRID, sex = gender_source_value, race, ethnicity = ethnicity_source_value, visit_age) %>%
    mutate(cohort = "Depression code cohort") %>%
    distinct()

stopifnot(n_distinct(dep_cohort$GRID) == nrow(dep_cohort))

## Antidepressant cohort
med_cohort <- readRDS("/data/davis_lab/allie/care_sites/data/mdd_application/20230607_sd_pull_visit_occurrence_earliest_med_joined_by_date_010525.Rds") %>%
    group_by(GRID) %>%
    filter(visit_start_date == min(visit_start_date)) %>%
    ungroup() %>%
    left_join(person, by = "GRID") %>%
    mutate(DOB = as.Date(birth_datetime)) %>%
    mutate(visit_age = as.numeric(difftime(visit_start_date, DOB, units = "days")) / 365.25) %>%
    select(GRID, sex = gender_source_value, race, ethnicity = ethnicity_source_value, visit_age) %>%
    mutate(cohort = "Antidepressant cohort") %>% 
    distinct()

stopifnot(n_distinct(med_cohort$GRID) == nrow(med_cohort))

## MDD PGS cohort
pgs_cohort <- fread("/data/davis_lab/allie/care_sites/data/mdd_application/mdd_pgs_regression_cohort_010525.csv") %>%
    select(GRID,median_age) %>% 
    left_join(person, by = "GRID") %>% 
    select(GRID, sex = gender_source_value, visit_age = median_age, race, ethnicity = ethnicity_source_value) %>%
    mutate(cohort = "Depression PGS cohort")

# NOTE- here, we are defining visit age as the pre-calculated median age across ALL ICD codes for each individual

## Combined table
length(intersect(dep_cohort$GRID, med_cohort$GRID)) # 78568 overlapping GRIDs
combined_cohort <- rbind(dep_cohort, med_cohort, pgs_cohort)
combined_cohort$cohort <- factor(combined_cohort$cohort, levels = c("Depression code cohort", "Antidepressant cohort", "Depression PGS cohort"))

# Variable factor levels and labels
combined_cohort$sex <- factor(combined_cohort$sex, levels = c("M", "F", "U"), labels = c("Male", "Female", "Unspecified")) 
label(combined_cohort$sex) <- "Sex"

combined_cohort$race <- factor(combined_cohort$race, levels = c("White", "Black", "Asian", "Multiple", "Other", "Unspecified"))
label(combined_cohort$race) <- "Race"

label(combined_cohort$visit_age) <- "Age at Visit"

## Make the table
table1(~ sex + race + visit_age | cohort, data = combined_cohort, overall = F, render.continuous = my.render.cont)
