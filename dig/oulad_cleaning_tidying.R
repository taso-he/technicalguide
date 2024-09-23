# Tidying and cleaning OULAD data ---------------------------------------------

# Tidying/cleaning script taken directly from OULAD Technical Report. 

# Loading in data and libraries -----------------------------------------------

# Load OULAD package
# devtools::install_github("jakubkuzilek/oulad")
library(oulad)

# Other packages

library(tidyverse)
library(scales) # Showing plots as percentages
library(broom) # For regression outputs
library(modelsummary) # For regression outputs
library(ggtext) # For charts
library(kableExtra) # For tables
library(easystats)
library(gghighlight)
library(sandwich) # For robust std errors
library(lmtest)
library(pROC)
library(pscl)
library(caret)
library(car) # For checking multicollinearity
# lmertest
library(kableExtra)
library(tibble)


# List of dataset names to load from the package
dataset_names <- c(
  "assessment", "course", "student", "student_assessment",
  "student_registration", "student_vle", "vle"
)

# Loop through the dataset names and load each one
for (dataset_name in dataset_names) {
  data(list = dataset_name, package = "oulad")
}

# Loading in module summary and domain information
# Not included in package, but included as a table in the OULAD paper
# DOI: 10.1038/sdata.2017.171

module_type <- data.frame(
  code_module = c("AAA", "BBB", "CCC", "DDD", "EEE", "FFF", "GGG"),
  domain = c(
    "Social Sciences", "Social Sciences", "STEM", "STEM", "STEM",
    "STEM", "Social Sciences"
  ),
  Presentations = c(2, 4, 2, 4, 3, 4, 3),
  Students = c(748, 7909, 4434, 6272, 2934, 7762, 2534)
)

# Note student_info is renamed to student in this package

# Cleaning data ---------------------------------------------------------------

# Filtering to exclude students with a HE qualification or post graduate qual

# Creating an unfiltered version of the dataframe we can refer back to
# Going forward 'student' will be a filtered dataframe
student_unfiltered <- student

# Only keeping distinct students to enable linear regression
# Some students takem more than one module. To enable simpler analysis (such as
# when conducting regressions), keep only one (the first occurrence of each
# unique value of student_id)
# This decreases number of students from 27550 to 24441
student <- student %>%
  distinct(id_student, .keep_all = TRUE)

# Storing id_student as factor across dataframes
student$id_student <- as.factor(student$id_student)
student_assessment$id_student <- as.factor(student_assessment$id_student)
student_registration$id_student <- as.factor(student_registration$id_student)
student_vle$id_student <- as.factor(student_vle$id_student)

# Checking for number of rows in each column that are empty or NA
# Function to count NA values and empty strings in each column
count_na_empty <- function(column) {
  na_count <- sum(is.na(column))
  empty_count <- sum(column == "", na.rm = TRUE)
  total_missing <- na_count + empty_count
  return(total_missing)
}

# Apply the function to each column and gather results into a data frame
missing_data_summary <- student %>%
  summarise(across(everything(), ~ count_na_empty(.x))) %>%
  pivot_longer(everything(), names_to = "Column", values_to = "Total_Missing")
# Only IMD band has missing values (621 missing)

# Filtering out those without IMD values
student <- student %>%
  filter(!is.na(imd_band))

# Filtering out GGG students
# Our model will use the students' scores up to day 56 as one of the covariates
# Module GGG doesn't have any assessments before this date, meaning all
# students had no scores.
# This lack of variability in the scores for GGG caused multicollinearity
# issues in the model.
# Therefore, to ensure the robustness and validity of the model, Module GGG was
# removed from the dataset.
student <- student %>%
  filter(code_module != "GGG")

## Tidying results data ----

### Withdrawn binary ----

student <- student %>%
  mutate(
    withdrawn = case_when(
      final_result == "Withdrawn" ~ "Withdrawn",
      final_result %in% c("Pass", "Fail", "Distinction") ~ "Not withdrawn",
      TRUE ~ NA_character_ # This handles any unexpected values
    )
  )

# Creating binary for outcome variable
student <- student %>%
  mutate(withdrawn_binary = ifelse(withdrawn == "Withdrawn", 1, 0))

### Withdrawn or fail binary ----

student <- student %>%
  mutate(
    withdrawn_fail = case_when(
      final_result %in% c("Withdrawn", "Fail") ~ "Withdrawn or Fail",
      final_result %in% c("Pass", "Distinction") ~ "Not Withdrawn or Fail",
      TRUE ~ NA_character_ # This handles any unexpected values
    )
  )

# Creating binary outcome variable for 'Withdrawn or Fail'
student <- student %>%
  mutate(withdrawn_fail_binary = ifelse(withdrawn_fail == "Withdrawn or Fail", 1, 0))

## Dropping those who withdrew before day 56 ----

# As in the OULAD example, drop from the analysis all who withdrew within
# the first month of the module

student_registration <- student_registration %>%
  filter(is.na(date_unregistration) | date_unregistration > 56)

# Merging in
student <- student %>%
  inner_join(
    student_registration %>%
      select(id_student, code_module, code_presentation, date_unregistration),
    by = c("id_student", "code_module", "code_presentation")
  )

## Tidying deprivation data ----

# Replacing deprivation bands with deciles

# This will make them easier to work with
student <- student %>%
  mutate(imd_decile = case_when(
    imd_band == "0-10%" ~ 1,
    imd_band == "10-20" ~ 2,
    imd_band == "20-30%" ~ 3,
    imd_band == "30-40%" ~ 4,
    imd_band == "40-50%" ~ 5,
    imd_band == "50-60%" ~ 6,
    imd_band == "60-70%" ~ 7,
    imd_band == "70-80%" ~ 8,
    imd_band == "80-90%" ~ 9,
    imd_band == "90-100%" ~ 10,
    TRUE ~ NA_integer_
  ))

# Storing this as factor
student$imd_decile <- as.factor(student$imd_decile)

# Creating binary deprivation measure
# Where bottom three deciles are coded as deprived
student <- student %>%
  mutate(deprivation_status = case_when(
    is.na(imd_decile) ~ NA_character_,
    imd_decile %in% 1:3 ~ "Deprived",
    TRUE ~ "Not deprived"
  ))

## Tidying disability data ----

# Replacing TRUE/FALSE with Disabled/Not disabled
student <- student %>%
  mutate(disability = case_when(
    disability == "FALSE" ~ "Not disabled",
    disability == "TRUE" ~ "Disabled",
    TRUE ~ NA_character_
  ))

## Tidying gender data ----

# Replacing TRUE/FALSE with Disabled/Not disabled
student <- student %>%
  mutate(gender = case_when(
    gender == "M" ~ "Male",
    gender == "F" ~ "Female",
    TRUE ~ NA_character_
  ))

## Tidying education level ----

# In filtered dataset,  create binary for those with A-levels, and those with
# a lower level of qualification and rename  qualification levels
student <- student %>%
  mutate(qualifications = case_when(
    highest_education == "A Level or Equivalent" ~ "A Level or equivalent",
    highest_education %in% c("Lower Than A Level", "No Formal quals") ~ "Lower than A Level or no formal qualifications",
    highest_education %in% c("HE Qualification", "Post Graduate Qualification") ~ "HE or post-graduate qualification",
    TRUE ~ NA_character_ # Handling potential unexpected values
  )) %>%
  mutate(highest_education = case_when(
    highest_education == "A Level or Equivalent" ~ "A Level or equivalent",
    highest_education == "Lower Than A Level" ~ "Lower than A Level", 
    highest_education == "No Formal quals" ~ "No formal qualifications",
    highest_education == "HE Qualification" ~ "Higher education qualification", 
    highest_education == "Post Graduate Qualification" ~ "Post-graduate qualification",
    TRUE ~ NA_character_ # Handling potential unexpected values
  ))

# Creating a binary for result, looking at withdrawn and not withdrawn

## Tidying age band ----

# Note there is overlap in age bands, so have assumed 0-35 is under 35.
student <- student %>%
  mutate(age_category = case_when(
    age_band == "0-35" ~ "Under 35",
    age_band %in% c("35-55", "55<=") ~ "35 or over",
    TRUE ~ NA_character_ # This handles any unexpected values
  ))

## Tidying number of previous attempts ----

student <- student %>%
  mutate(previous_attempts_binary = ifelse(num_of_prev_attempts == 0,
                                           "No previous attempts",
                                           "Has previous attempts"
  ))


## Creating a relative engagement metric ----

# Only looking at engagement before
# Calculate the sum_click for each id_student, code_module, and code_presentation
engagement_data <- student_vle %>%
  group_by(id_student, code_module, code_presentation) %>%
  summarise(total_clicks = sum(sum_click[date <= 56], na.rm = TRUE)) %>%
  ungroup()

# Calculate the percentile rank and scale it to 0-100
engagement_data <- engagement_data %>%
  group_by(code_module, code_presentation) %>%
  mutate(relative_module_engagement = percent_rank(total_clicks) * 100) %>%
  ungroup()

# Categorical variable
engagement_data <- engagement_data %>%
  group_by(code_module, code_presentation) %>%
  mutate(quartile = ntile(total_clicks, 4)) %>%
  mutate(engagement_category = case_when(
    quartile == 1 ~ "Very low engagement",
    quartile == 2 ~ "Low engagement",
    TRUE ~ "Average or above engagement"
  )) %>%
  ungroup()

# Merging in
student <- student %>%
  left_join(
    engagement_data %>% select(
      id_student, code_module,
      code_presentation,
      relative_module_engagement,
      engagement_category,
      total_clicks
    ),
    by = c("id_student", "code_module", "code_presentation")
  )

# Coding students who had no VLE data as 'No enagement'
student <- student %>%
  mutate(engagement_category = replace_na(
    engagement_category,
    "No engagement"
  ))

## Creating basic tables for student counts etc. ----

### Module counts ----

unique_students_per_module_presentation <- student %>%
  group_by(code_module, code_presentation) %>%
  summarise(unique_students = n_distinct(id_student))
# BBB and FFF have largest student numbers.

## Creating a numeric score: cleaning student_assessment ----

# Merging in the assessment type, presentation, and module code
student_assessment <- student_assessment %>%
  left_join(
    assessment %>% select(
      id_assessment, assessment_type,
      code_presentation, code_module,
      weight
    ),
    by = "id_assessment"
  )

## Creating assessment completion metric ----

student_assessment <- student_assessment %>%
  left_join(assessment %>% select(id_assessment, date), by = "id_assessment")

student_assessment_filtered <- student_assessment %>%
  filter(date <= 56)

student_assessment_filtered <- student_assessment_filtered %>%
  mutate(submission_status = if_else(date_submitted <= date, "on_time", "late"))

weighted_scores <- student_assessment_filtered %>%
  mutate(weighted_score = weight * score) %>%
  group_by(id_student, code_presentation, code_module) %>%
  summarise(total_weighted_score = sum(weighted_score, na.rm = TRUE), .groups="drop")

# Calculate the relative weighted score mark and quartile
weighted_scores <- weighted_scores %>%
  group_by(code_module, code_presentation) %>%
  mutate(
    relative_module_score = percent_rank(total_weighted_score) * 100,
    quartile = ntile(total_weighted_score, 4),
    score_category = case_when(
      quartile == 1 ~ "Very low scoring",
      quartile == 2 ~ "Low scoring",
      TRUE ~ "Average or above scoring"
    )
  ) %>%
  ungroup()

# Merge the scores into student
# Students who didn't submit any assessments won't have a match, and will be
# coded as "No score"

student <- student %>%
  left_join(weighted_scores, by = c(
    "id_student",
    "code_presentation", "code_module"
  )) %>%
  mutate(score_category = ifelse(is.na(score_category),
                                 "No score", as.character(score_category)
  ))


# Creating a dataframe suitable for straightforward regression ----

# As factor where necessary for regressions

student <- student %>%
  mutate(
    deprivation_status = factor(deprivation_status),
    disability = factor(disability),
    gender = factor(gender),
    code_presentation = factor(code_presentation),
    qualifications = factor(qualifications),
    previous_attempts_binary = factor(previous_attempts_binary),
    age_band = factor(age_band)
  )

# Changing reference level, so output shows the effect for the characteristic
# we might expect to show a lower score due to historic gaps

student$deprivation_status <- relevel(student$deprivation_status, ref = "Not deprived")
student$disability <- relevel(student$disability, ref = "Not disabled")
student$previous_attempts_binary <- relevel(student$previous_attempts_binary, ref = "No previous attempts")

# Setting order of presentations

# Adjusting the levels of code_presentation
student$code_presentation <- factor(student$code_presentation,
                                    levels = c(
                                      "2013B", "2013J",
                                      "2014B", "2014J"
                                    )
)
