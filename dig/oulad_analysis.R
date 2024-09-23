# OULAD analysis for Diagnose -------------------------------------------------

# Run analysis based off tidying and cleaning process in oulad_tidying_cleaning.R 
# Run this script first.

# Creating total weighted score -----------------------------------------------

# This metric isn't in the main OULAD analysis, as doesn't seem to be fully 
# robust. But usable for demonstration purposes in this analysis. 

# Creating a weighted score, by multiplying the score by the weight and dividing by 100
student_assessment <- student_assessment %>%
  mutate(weighted_score = (score * weight) / 100)

# Creating the total weighted score for each combination of student, module and presentation
score_table <- student_assessment %>%
  group_by(id_student, code_presentation, code_module) %>%
  summarise(total_weighted_score = sum(weighted_score),
            total_weight = sum(weight), .groups = "drop")

# Maximum weighted score that a student could get
# The modules vary in the total weighted score available.
max_weights <- score_table %>%
  group_by(code_module, code_presentation) %>%
  summarise(max_total_weight = max(total_weight, na.rm = TRUE), .groups = "drop")

# Adding maximum available weights into score table
# And adding their total weighted score
score_table <- score_table %>%
  left_join(max_weights, by = c("code_module", "code_presentation"))

# Standardising
score_table <- score_table %>%
  mutate(standardised_total_weighted_score = (total_weighted_score / max_total_weight) * 100)

# Adding a proportion completed column
score_table <- score_table %>%
  mutate(proportion_completed = (total_weight / max_total_weight))

# Creating column to see whether student completed all assessments
score_table <- score_table %>%
  mutate(incomplete_assessment = ifelse(total_weight < max_total_weight, 
                                        "Incomplete", "Complete"))

# Removing observations for module presentations where the total weighted score available was 0
score_table <- score_table %>%
  filter(max_total_weight != 0)

# 47% of observations didn't complete all of the assessments
incomplete_table <- score_table %>%
  group_by(incomplete_assessment) %>%
  summarize(count = n()) %>%
  mutate(percentage = round(count / sum(count), 2))

# Merging in score table 
student <- student %>%
  left_join(score_table %>% select(id_student, code_module, code_presentation, 
                                   standardised_total_weighted_score, incomplete_assessment),
            by = c("id_student", "code_module", "code_presentation"))

# Creating total_Vle_clicks ----

total_vle_clicks_thousands <- student_vle %>%
  group_by(code_module, code_presentation, id_student) %>%
  summarise(total_vle_clicks_thousands = sum(sum_click, na.rm = TRUE) / 1000, .groups = "drop")

student <- student %>%
  left_join(total_vle_clicks_thousands, by = c("code_module", "code_presentation", 
                                               "id_student"))

# Regression ------------------------------------------------------------------

# t-test ----

t_test <- t.test(standardised_total_weighted_score~deprivation_status, data=student)

# Create a data frame of results
t_test_results <- tibble(
  `Difference (pp)` = round(t_test$estimate[1]-t_test$estimate[2],2),
  `Lower confidence interval` = round(t_test$conf.int[1], 3),
  `Upper confidence interval` = round(t_test$conf.int[2], 3),
  `T statistic` = t_test$statistic,
  `Degrees of freedom` = t_test$parameter,
  `P value` = format.pval(t_test$p.value,eps = 0.001)
)

# ANOVA ---- 

anova <- aov(standardised_total_weighted_score ~ highest_education, data = student)


tidy_anova <- tidy(anova) %>%
  as_tibble() %>%
  mutate(
    `P value` = ifelse(p.value=="","",format.pval(round(p.value, 3), eps=0.001)),
    `Mean square` = round(meansq, 1),
    `statistic` = round(statistic, 1),
    across(everything(), ~ ifelse(is.na(.), "", .)),
    `Sum of squares` = sumsq
  ) %>%
  select(-p.value, -sumsq, -meansq) %>% # Remove the original p.value column
  rename(`F statistic` = statistic,
         `Term` = term,
         `Degrees of freedom` = df) %>%
  select(Term, `Sum of squares`, `Degrees of freedom`, `Mean square`, `F statistic`, `P value`)

# Pairwise t-tests 

# pairwise.t.test(student$standardised_total_weighted_score, student$highest_education)

tidy_tukey <- TukeyHSD(anova) %>%
  .$highest_education %>% # Extract the highest_education component
  as.data.frame() %>%     # Convert to a data frame
  rownames_to_column("comparison") %>% # Convert rownames to a column
  as_tibble() %>%
  mutate(Significance = ifelse(lwr < 0 & upr > 0, 
                               "Not significant at the 5% level", 
                               "Significant at the 5% level")) %>%
  mutate(
    `p adj` = format.pval(round(`p adj`,3),eps=0.001)
    ) %>%
  mutate(comparison = str_replace_all(comparison,"-"," - ")) %>% #put space around subtract
  mutate(comparison = str_replace(comparison,"t - g","t-g")) %>% #cope with hyphen in post-graduate
  rename(`Comparison` = comparison,
         `Difference` = diff,
         `Lower confidence interval` = lwr,
         `Upper confidence interval` = upr,
         `Adjusted p value` = "p adj") # Rename the statistic column to f_statistic

# Regression ----

# Linear regression with one variable
# Score and IMD

model_simple <- lm(standardised_total_weighted_score ~ deprivation_status,
                   data = student)

summary(model_simple)
# IMD has a significant effect on scores
# Being from a deprived area is associated with a decrease of 3.7 marks in the 
# score, compared to individuals who are not from deprived areas. 

# Interaction model ----

# Fit the simple interaction model
model_interaction <- lm(standardised_total_weighted_score ~ deprivation_status*gender,
                        data = student)

summary(model_interaction)

anova(model_interaction)

# Fit the full linear regression model
model <- lm(standardised_total_weighted_score ~ deprivation_status 
            + disability
            + gender 
            + code_presentation 
            + qualifications 
            + num_of_prev_attempts 
            + age_category
            #+ region # uncomment to include region 
            + total_vle_clicks_thousands,
            data = student)

# Region commented out to make output more readable, but does increase 
# model accuracy

# Summary of the model to see results
summary(model)

# Visualisations --------------------------------------------------------------

# Custom theme for plots ----
library(sysfonts)
font_add_google("Source Sans Pro")
library(showtext)
showtext_auto()

theme_taso <- function() {
  theme_minimal() +
    theme(
      text = element_text(size = 30, family = "Source Sans Pro"), # Default text size for all text elements
      plot.title.position = "plot", # Aligns plot title to whole plot
      plot.title = element_text(size = 30, face = "bold"),
      plot.subtitle = element_text(size = 25),
      plot.caption.position = "plot", # Aligns caption to the left of the plot
      plot.caption = element_text(hjust = 0, size = 20, face = "italic"),
      panel.grid.major = element_line(colour = "#CECABC", linewidth = 0.3), # Gridline colour
      plot.background = element_rect(fill = "white", color = NA), # Background colour
      plot.margin = margin(0.25, 0.25, 0.25, 0.25, "in"), # Adding margin
      panel.border = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      # axis.title = element_blank(),
      axis.line = element_line(colour = "#6E6E6E", linewidth = 0.5),
      axis.text.x = element_text(margin = margin(t = 7, unit = "pt")), # Increase top margin to move text down
      axis.ticks.length = unit(0.3, "cm"), # Increase the length of ticks
      axis.ticks.x = element_line(colour = "#6E6E6E", linewidth = 0.5)
    )
}

# Setting colours 

colors_deprivation <- c("Deprived" = "#3b66bc", "Not deprived" = "#07dbb3")
colors_disability <- c("Disabled" = "#3b66bc", "Not disabled" = "#07dbb3")
colors_gender <- c("Female" = "#3b66bc", "Male" = "#07dbb3")
colors_qualifications <- c("A Level or Equivalent" = "#3b66bc",
                           "Lower than A Level or no formal quals" = "#07dbb3")
colors_significance <- c("Significant at the 5% level" = "#3b66bc", 
                         "Not significant at the 5% level" = "#07dbb3")

## Overall gaps using box plot ----

overall_deprivation_gaps <- ggplot(student, aes(x = deprivation_status, y = standardised_total_weighted_score, color = deprivation_status, fill = deprivation_status)) +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.3) +
  #facet_wrap(~code_module, scales = "free_y", ncol = 2, nrow = 5) + # Facet wrap by 'variable'
  theme_taso() +  
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, by = 10),
                     expand = c(0,0)) +
  labs(title = "Average scores for module by deprivation status",
       subtitle = "",
       caption = "",
       y = "", x = "") +
  scale_color_manual(values = colors_deprivation) +
  scale_fill_manual(values = colors_deprivation) + 
  guides(fill = FALSE, colour = FALSE)

facet_overall_deprivation_gaps <- ggplot(student, aes(x = deprivation_status, y = standardised_total_weighted_score, color = deprivation_status, fill = deprivation_status)) +
  #geom_jitter(alpha = 0.1, width = 0.2, height = 0) +  # Add jitter to points, adjust width for horizontal jitter
  geom_boxplot(alpha = 0.2, width = 0.5, outlier.shape = NA) +
  facet_wrap(~code_module, scales = "free_y", ncol = 2, nrow = 5) + # Facet wrap by 'variable'
  theme_taso() +  
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, by = 20),
                     expand = c(0,0)) +
  labs(title = "Average scores for module by deprivation status and module",
       subtitle = "",
       caption = "",
       y = "", x = "") +
  scale_color_manual(values = colors_deprivation) + 
  scale_fill_manual(values = colors_deprivation) + 
  guides(fill = FALSE, colour = FALSE)

## Gaps over time using line chart ----

# Preparing data

average_scores_deprivation <- student %>%
  group_by(deprivation_status, code_presentation) %>%
  summarise(avg_score = mean(standardised_total_weighted_score, na.rm = TRUE), .groups = "drop") %>%
  arrange(code_presentation)

facet_average_scores_deprivation <- student %>%
  group_by(deprivation_status, code_presentation, code_module) %>%
  summarise(avg_score = mean(standardised_total_weighted_score, na.rm = TRUE), .groups = "drop") %>%
  arrange(code_presentation)

over_time_deprivation_gaps <- ggplot(average_scores_deprivation, aes(x = code_presentation, y = avg_score, color = deprivation_status, group = deprivation_status)) +
  geom_line() +
  geom_point() + # Optional: Add points to the line chart
  theme_taso() +
  labs(title = "Average scores over time by deprivation status",
       x = "",
       y = "",
       caption = "Note: Presentations of courses start in February and October - they are marked by “B” and “J” respectively.",
       color = "Deprivation Status") +
  scale_color_manual(values = colors_deprivation) +
  scale_y_continuous(limits = c(30, 80)) +
  # Adding additional theme elements
  theme(
    legend.position = "top",
    legend.text = element_text(size = 20), # Adjusting legend text size
    legend.title = element_text(size = 20, face = "bold")
  ) +
  coord_cartesian(clip = "off")

facet_over_time_deprivation_gaps <- ggplot(facet_average_scores_deprivation, aes(x = code_presentation, y = avg_score, color = deprivation_status, group = deprivation_status)) +
  geom_line() +
  geom_point() + # Optional: Add points to the line chart
  facet_wrap(~code_module, scales = "free_y", ncol = 2, nrow = 5) + # Facet wrap by 'variable'
  theme_taso() +
  labs(title = "Average scores over time by deprivation status and module",
       x = "",
       y = "",
       caption = "Note: Presentations of courses start in February and October - they are marked by “B” and “J” respectively.",
       color = "Deprivation Status") +
  scale_color_manual(values = colors_deprivation) +
  scale_y_continuous(limits = c(30, 80),
                     breaks = seq(0, 100, by = 20)) +
  # Adding additional theme elements
  theme(
    legend.position = "top",
    legend.text = element_text(size = 20), # Adjusting legend text size
    legend.title = element_text(size = 20, face = "bold")
  ) +
  coord_cartesian(clip = "off")

## ANOVA ----

### Chart to support ANOVA chart ----

student_sorted <- student %>%
  mutate(highest_education = reorder(highest_education, -standardised_total_weighted_score, 
                                     FUN = median, na.rm = TRUE))

highest_ed_chart <- ggplot(student_sorted, aes(x = highest_education, y = standardised_total_weighted_score, color = highest_education, fill = highest_education)) +
  geom_boxplot(width = 0.5, outlier.shape = NA, alpha = 0.3) +
  theme_taso() +  
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0, 100, by = 10),
                     expand = c(0,0)) +
  labs(title = "Average scores for module by highest education",
       subtitle = "",
       caption = "",
       y = "", x = "") +
  scale_fill_manual(values = c("#3b66bc", "#e14491", "#00a8da", "#835ebd", "#07dbb3")) +
  scale_color_manual(values = c("#3b66bc", "#e14491", "#00a8da", "#835ebd", "#07dbb3")) +
  guides(fill = FALSE, colour = FALSE) +
  coord_flip()  # Flips the chart to make bars horizontal


### ANOVA chart ----

tukey_chart <- ggplot(tidy_tukey, aes(y = reorder(Comparison, Difference), x = Difference, color = Significance)) +
  geom_point() + # Add the point estimates
  geom_vline(xintercept = 0, color = "#6E6E6E", linewidth = 0.5) + # Add the vertical line at x = 0
  geom_errorbar(aes(xmin = `Lower confidence interval`, xmax = `Upper confidence interval`), width = 0.2) + # Add the confidence intervals
  labs(title = "Tukey's HSD",
       subtitle = "Estimated difference in score (pp)",
       x = "",
       y = "",
       caption = "") +
  theme_taso() +
  scale_color_manual(values = colors_significance) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 18), # Adjusting legend text size
    legend.title = element_blank(),
    #axis.title.x = element_text(size = 8), 
    axis.line.y = element_blank(),
    axis.text.y = element_text(size = 18)
  ) +
  coord_cartesian(clip = "off")

## Simple regression ---- 

# Assuming summary_model_simple is the summary of your lm model object
summary_model_simple <- summary(model_simple)

# Directly extract coefficients and standard errors
intercept <- summary_model_simple$coefficients["(Intercept)", "Estimate"]
intercept_se <- summary_model_simple$coefficients["(Intercept)", "Std. Error"]
deprived_effect <- summary_model_simple$coefficients["deprivation_statusDeprived", "Estimate"]
deprived_effect_se <- summary_model_simple$coefficients["deprivation_statusDeprived", "Std. Error"]

# Calculate the 95% confidence intervals
# Multiply the standard error by 1.96 for the 95% CI
intercept_ci_lower <- intercept - (1.96 * intercept_se)
intercept_ci_upper <- intercept + (1.96 * intercept_se)
deprived_effect_ci_lower <- deprived_effect - (1.96 * deprived_effect_se)
deprived_effect_ci_upper <- deprived_effect + (1.96 * deprived_effect_se)

# Prepare the data for plotting
data_model_simple_chart <- data.frame(
  Group = c("Not deprived", "Deprived"),
  Value = c(intercept, intercept + deprived_effect),
  LowerCI = c(intercept_ci_lower, intercept_ci_lower + deprived_effect),
  UpperCI = c(intercept_ci_upper, intercept_ci_upper + deprived_effect)
)

# Ensure 'Group' is ordered as you wish in the plot
data_model_simple_chart$Group <- factor(data_model_simple_chart$Group, levels = c("Not deprived", "Deprived"))

# Plotting
simple_regression_chart <- ggplot(data_model_simple_chart, aes(x = Group, y = Value, color = Group)) +
  geom_point(size = 4) + # Adjust size as needed
  geom_errorbar(aes(ymin = LowerCI, ymax = UpperCI), width = 0.2) +
  theme_taso() +
  labs(title = "Average scores by deprivation status: regression results", 
       x = "", 
       y = "", 
       color = "Group",
       caption = "") +
  scale_color_manual(values = colors_deprivation) +
  scale_y_continuous(limits = c(30, 80),
                     expand = c(0,0)) +
  # Adding additional theme elements
  guides(fill = FALSE, colour = FALSE) +
  coord_cartesian(clip = "off")

### Multiple regression ----

summary_model <- summary(model)
p_values <- summary_model$coefficients[, 4]

# Extract Coefficients and Confidence Intervals
coefficients_model <- coef(model)
conf_intervals_model <- confint(model)

# Prepare Data for Plotting
data_for_model_chart <- data.frame(
  Variable = rownames(conf_intervals_model),
  Estimate = coefficients_model,
  Lower = conf_intervals_model[, 1],
  Upper = conf_intervals_model[, 2]
)

# Add p-values to the data frame
data_for_model_chart$P_Value <- p_values[rownames(data_for_model_chart)]

# Remove intercept
data_for_model_chart <- data_for_model_chart %>%
  filter(Variable != "(Intercept)")

# Sort the data frame by Estimate in descending order
data_for_model_chart <- data_for_model_chart[order(data_for_model_chart$Estimate, decreasing = TRUE), ]

data_for_model_chart <- data_for_model_chart %>%
  mutate(Significance = ifelse(Lower < 0 & Upper > 0, 
                               "Not significant at the 5% level", 
                               "Significant at the 5% level"))

# Define a named vector for mapping original names to readable names
readable_names <- c(
  "(Intercept)" = "Intercept",
  "deprivation_statusDeprived" = "Deprived",
  "disabilityDisabled" = "Disabled",
  "genderMale" = "Male",
  "code_presentation2013J" = "2013J",
  "code_presentation2014B" = "2014B",
  "code_presentation2014J" = "2014J",
  "qualificationsLower than A Level or no formal qualifications" = "Lower than A Level or no formal quals",
  "qualificationsHE or post-graduate qualification" = "HE or post graduate qual",
  "num_of_prev_attempts" = "Number of Previous Attempts",
  "age_categoryUnder 35" = "Under 35",
  "total_vle_clicks_thousands" = "Total VLE Clicks (Thousands)"
)

# Replace the original names with the readable names
data_for_model_chart$Variable <- readable_names[data_for_model_chart$Variable]

# Convert Variable to a factor for plotting in order
data_for_model_chart$Variable <- factor(data_for_model_chart$Variable, 
                                        levels = data_for_model_chart$Variable)

# Plot
full_regression_chart <- ggplot(data_for_model_chart, aes(x = Estimate, y = Variable, color = Significance)) +
  geom_point() + # Add the point estimates
  geom_vline(xintercept = 0, color = "#6E6E6E", linewidth = 0.5) + # Add the vertical line at x = 0
  geom_errorbar(aes(xmin = Lower, xmax = Upper), width = 0.2) + # Add the confidence intervals
  theme_minimal() + # Use a minimal theme for cleanliness
  labs(title = "Full regression model with 95% confidence intervals",
       subtitle = "Estimated effect on score",
       x = "",
       y = "",
       caption = "") +
  theme_taso() +
  scale_color_manual(values = colors_significance) +
  scale_x_continuous(limits = c(-10, 10),
                     expand = c(0,0),
                     breaks = seq(-10, 20, by = 5)) +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 20), # Adjusting legend text size
    legend.title = element_blank(),
    #axis.title.x = element_text(size = 8), 
    axis.line.y = element_blank()
  ) +
  coord_cartesian(clip = "off")

ggplot(tidy_tukey, aes(y = reorder(Comparison, Difference), x = Difference)) +
  geom_point() + # Add the point estimates
  geom_vline(xintercept = 0, color = "#6E6E6E", linewidth = 0.5) + # Add the vertical line at x = 0
  geom_errorbar(aes(xmin = `Lower confidence interval`, xmax = `Upper confidence interval`), width = 0.2) + # Add the confidence intervals
  theme_minimal() + # Use a minimal theme for cleanliness
  labs(title = "",
             subtitle = "Estimated effect on score",
             x = "",
             y = "",
             caption = "") +
  theme_taso() +
  coord_cartesian(clip = "off")


