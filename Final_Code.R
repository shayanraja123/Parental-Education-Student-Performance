library(ggplot2)
library(dplyr)

library(tidyr)
library(car)

setwd('/Users/shaya/Library/CloudStorage/OneDrive-HigherEducationCommission/ASU - Data Science, Analytics, and Engineering (M.Sc.)/Fall 2024/DSE 501 Statistics for Data Analysts/Project')
getwd()

data<-read.csv("StudentsPerformance.csv")
head(data)

rm(list = ls())

data <- data %>%
  mutate(parental.level.of.education = case_when(
    parental.level.of.education == "associate's degree" ~ "Assoc. Deg.",
    parental.level.of.education == "bachelor's degree" ~ "Bach. Deg.",
    parental.level.of.education == "master's degree" ~ "Mast. Deg.",
    parental.level.of.education == "some college" ~ "Some College",
    parental.level.of.education == "high school" ~ "High School",
    parental.level.of.education == "some high school" ~ "Some HS",
    TRUE ~ parental.level.of.education # Leave unchanged if no match
  ))


# Reshape data to a long format for separate histograms
data_long_categorical <- data %>%
  dplyr::select(parental.level.of.education) %>%
  pivot_longer(cols = everything(), names_to = "category", values_to = "value")

# Create a combined histogram figure with facets
ggplot(data_long_categorical, aes(x = value)) +
  geom_bar(fill = "#4B4B4B") +  # Unified color for all histograms
  facet_wrap(~ category, scales = "free_x", ncol = 3) +  # Layout with 3 columns to center bottom two
  labs(
    title = "Distribution of Education Levels",
    x = "Category Value",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    strip.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for readability
    legend.position = "none"
  )

# Select non-parental education factors
non_parental_factors <- data %>%
  dplyr::select(gender, race.ethnicity, lunch, test.preparation.course)

# Reshape data to long format for plotting
non_parental_long <- non_parental_factors %>%
  pivot_longer(cols = everything(), names_to = "Category", values_to = "Value")

# Create a bar chart for distribution of non-parental factors
ggplot(non_parental_long, aes(x = Value)) +
  geom_bar(fill = "#4B4B4B") +
  facet_wrap(~ Category, scales = "free", ncol = 2) +
  labs(
    title = "Distribution of Non-Parental Education Factors",
    x = "Categories",
    y = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    strip.text = element_text(size = 12),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Calculate the average score for each student
data <- data %>%
  dplyr::mutate(average.score = rowMeans(dplyr::select(., math.score, reading.score, writing.score)))

# Create a boxplot for average scores
ggplot(data, aes(x = parental.level.of.education, y = average.score)) +
  geom_boxplot(fill = "#4B4B4B", color = "black", alpha = 0.7) +
  labs(
    title = "Average Scores by Parental Education",
    x = "Parental Education Level",
    y = "Average Score"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Before continuing with Anova
levene_average<-leveneTest(average.score ~ parental.level.of.education, data = data)
levene_average

# We can now continue with Anova
anova_average <- aov(average.score ~ parental.level.of.education, data = data)
summary(anova_average)

# Box plots for all scores
scores <- c("math.score", "reading.score", "writing.score")
titles <- c("Math Scores by Parental Education", "Reading Scores by Parental Education", "Writing Scores by Parental Education")
# Loop through each score and display the boxplot
for (i in seq_along(scores)) {
  p <- ggplot(data, aes(x = parental.level.of.education, y = .data[[scores[i]]])) +
    geom_boxplot(fill = "#4B4B4B", color = "black", alpha = 0.7) +
    labs(
      title = titles[i],
      x = "Parental Education Level",
      y = "Score"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  print(p) # Correctly print the plot within the loop
}

levene_math<-leveneTest(math.score ~ parental.level.of.education, data = data)
levene_math

# Anova for math and education level
anova_math <- aov(math.score ~ parental.level.of.education, data = data)
summary(anova_math)

# check for homogeneity of variance across groups. Levene test
levene_read<-leveneTest(reading.score ~ parental.level.of.education, data = data)
levene_read

# Anova for Reading and education level
anova_reading <- aov(reading.score ~ parental.level.of.education, data = data)
summary(anova_reading)

# Check for homogenity of variances before continuing with anova
levene_writing<-leveneTest(writing.score ~ parental.level.of.education, data = data)
levene_writing

# Anova for Writing and education level
anova_write <- aov(writing.score ~ parental.level.of.education, data = data)
summary(anova_write)

# GLM Univariate
##factor data
data$parental.level.of.education <- factor(
  data$parental.level.of.education,
  levels = c("Some HS", "High School", "Some College", "Assoc. Deg.", "Bach. Deg.", "Mast. Deg.")
)


glm_average <- glm(
  average.score ~ parental.level.of.education,
  data = data,
  family = gaussian() # Assumes a normal distribution for the dependent variable
)
summary(glm_average)

# GLM Math
glm_math <- glm(
  math.score ~ parental.level.of.education,
  data = data,
  family = gaussian() # Assumes a normal distribution for the dependent variable
)
summary(glm_math)

# GLM Reading
glm_read <- glm(
  reading.score ~ parental.level.of.education,
  data = data,
  family = gaussian() # Assumes a normal distribution for the dependent variable
)
summary(glm_read)

# GLM Writing
glm_write <- glm(
  writing.score ~ parental.level.of.education,
  data = data,
  family = gaussian() # Assumes a normal distribution for the dependent variable
)
summary(glm_write)

# Bivariate Average results
bi_glm_average_gender <- glm(
  average.score ~ parental.level.of.education + gender,
  data = data,
  family = gaussian() # Assumes a normal distribution for the dependent variable
)
summary(bi_glm_average_gender)

bi_glm_average_race <- glm(
  average.score ~ parental.level.of.education + race.ethnicity,
  data = data,
  family = gaussian() # Assumes a normal distribution for the dependent variable
)
summary(bi_glm_average_race)

bi_glm_average_lunch <- glm(
  average.score ~ parental.level.of.education + lunch,
  data = data,
  family = gaussian() # Assumes a normal distribution for the dependent variable
)
summary(bi_glm_average_lunch)

bi_glm_average_test <- glm(
  average.score ~ parental.level.of.education + test.preparation.course,
  data = data,
  family = gaussian() # Assumes a normal distribution for the dependent variable
)
summary(bi_glm_average_test)

# Full Model
full_glm_average <- glm(
  average.score ~ parental.level.of.education + test.preparation.course+lunch + race.ethnicity+gender,
  data = data,
  family = gaussian() # Assumes a normal distribution for the dependent variable
)
summary(full_glm_average)

# vif
vif_full<-vif(full_glm_average)
vif_full

# No evidence of multicollinearity
# Now lets use stepwise model selection to find the best fit model

library(MASS)
best_model <- stepAIC(full_glm_average, direction = "both", trace = TRUE)
summary(best_model)

# Looks like parental level of education remains a significant predictor alongside other predictors of test score.

# Log regression of binary indicator
# average score

# Add binary indicator for passing (≥70%)
data <- data %>%
  mutate(pass_indicator = ifelse(average.score >= 70, 1, 0))

# Logistic regression function for a single bootstrap sample
bootstrap_mle <- function(data) {
  boot_sample <- data[sample(nrow(data), replace = TRUE), ]
  model <- glm(pass_indicator ~ parental.level.of.education, data = boot_sample, family = binomial())
  return(coef(model))
}

# Number of bootstrap iterations
n_boot <- 1000
set.seed(123) # For reproducibility

# Bootstrap MLE estimation
bootstrap_results <- replicate(n_boot, bootstrap_mle(data))

# Aggregate coefficients (mean across bootstrap samples)
mle_coefficients <- apply(bootstrap_results, 1, mean)

# Use the aggregated coefficients to create the best logistic regression model
best_log_reg <- glm(
  pass_indicator ~ parental.level.of.education,
  data = data,
  family = binomial(),
  start = mle_coefficients # Use bootstrap-aggregated coefficients as starting values
)

# Predict passing probabilities using the MLE-based model
data$predicted_pass_probability <- predict(best_log_reg, type = "response")

# Convert probabilities to binary predictions (1 if probability > 0.5, otherwise 0)
data$predicted_pass <- ifelse(data$predicted_pass_probability > 0.5, 1, 0)

# Summarize predictions by parental education level
pass_by_education <- data %>%
  group_by(parental.level.of.education) %>%
  summarise(
    total_students = n(),
    actual_passing = sum(pass_indicator),
    predicted_passing = sum(predicted_pass) # Sum of binary predictions
  )

# Display predictions
print(pass_by_education)



# average bootstrap
# Add binary indicator for passing (≥70%)
data <- data %>%
  mutate(pass_indicator = ifelse(average.score >= 70, 1, 0))

# Logistic regression function for a single bootstrap sample
bootstrap_mle <- function(data) {
  boot_sample <- data[sample(nrow(data), replace = TRUE), ]
  model <- glm(pass_indicator ~ parental.level.of.education, data = boot_sample, family = binomial())
  return(coef(model))
}

# Number of bootstrap iterations
n_boot <- 1000
set.seed(123) # For reproducibility

# Bootstrap MLE estimation
bootstrap_results <- replicate(n_boot, bootstrap_mle(data))

# Aggregate coefficients (mean across bootstrap samples)
mle_coefficients <- apply(bootstrap_results, 1, mean)

# Use the aggregated coefficients to create the best logistic regression model
best_log_reg <- glm(
  pass_indicator ~ parental.level.of.education,
  data = data,
  family = binomial(),
  start = mle_coefficients # Use bootstrap-aggregated coefficients as starting values
)

# Predict passing probabilities using the MLE-based model
data$predicted_pass_probability <- predict(best_log_reg, type = "response")

# Convert probabilities to binary predictions (1 if probability > 0.5, otherwise 0)
data$predicted_pass <- ifelse(data$predicted_pass_probability > 0.5, 1, 0)

# Summarize predictions by parental education level
pass_by_education <- data %>%
  group_by(parental.level.of.education) %>%
  summarise(
    total_students = n(),
    actual_passing = sum(pass_indicator),
    predicted_passing = sum(predicted_pass) # Sum of binary predictions
  )

# Display predictions
print(pass_by_education)



# math bootstrap
# Add binary indicator for passing math score (≥70%)
data <- data %>%
  mutate(pass_math = ifelse(math.score >= 70, 1, 0))

# Logistic regression function for a single bootstrap sample
bootstrap_mle_math <- function(data) {
  boot_sample <- data[sample(nrow(data), replace = TRUE), ]
  model <- glm(pass_math ~ parental.level.of.education, data = boot_sample, family = binomial())
  return(coef(model))
}

# Number of bootstrap iterations
n_boot <- 1000
set.seed(123) # For reproducibility

# Bootstrap MLE estimation for math score
bootstrap_results_math <- replicate(n_boot, bootstrap_mle_math(data))

# Aggregate coefficients (mean across bootstrap samples)
mle_coefficients_math <- apply(bootstrap_results_math, 1, mean)

# Use the aggregated coefficients to create the best logistic regression model
best_log_reg_math <- glm(
  pass_math ~ parental.level.of.education,
  data = data,
  family = binomial(),
  start = mle_coefficients_math # Use bootstrap-aggregated coefficients as starting values
)

# Predict passing probabilities for math scores using the MLE-based model
data$predicted_pass_math_probability <- predict(best_log_reg_math, type = "response")

# Convert probabilities to binary predictions (1 if probability > 0.5, otherwise 0)
data$predicted_pass_math <- ifelse(data$predicted_pass_math_probability > 0.5, 1, 0)

# Summarize predictions by parental education level
pass_by_education_math <- data %>%
  group_by(parental.level.of.education) %>%
  summarise(
    total_students = n(),
    actual_passing = sum(pass_indicator),
    predicted_passing_math = sum(predicted_pass_math) # Sum of binary predictions
  )

# Display predictions
print(pass_by_education_math)



# writing bootstrap
# Add binary indicator for passing writing score (≥70%)
data <- data %>%
  mutate(pass_writing = ifelse(writing.score >= 70, 1, 0))

# Logistic regression function for a single bootstrap sample
bootstrap_mle_writing <- function(data) {
  boot_sample <- data[sample(nrow(data), replace = TRUE), ]
  model <- glm(pass_writing ~ parental.level.of.education, data = boot_sample, family = binomial())
  return(coef(model))
}

# Number of bootstrap iterations
n_boot <- 1000
set.seed(123) # For reproducibility

# Bootstrap MLE estimation for writing score
bootstrap_results_writing <- replicate(n_boot, bootstrap_mle_writing(data))

# Aggregate coefficients (mean across bootstrap samples)
mle_coefficients_writing <- apply(bootstrap_results_writing, 1, mean)

# Use the aggregated coefficients to create the best logistic regression model
best_log_reg_writing <- glm(
  pass_writing ~ parental.level.of.education,
  data = data,
  family = binomial(),
  start = mle_coefficients_writing # Use bootstrap-aggregated coefficients as starting values
)

# Predict passing probabilities for writing scores using the MLE-based model
data$predicted_pass_writing_probability <- predict(best_log_reg_writing, type = "response")

# Convert probabilities to binary predictions (1 if probability > 0.5, otherwise 0)
data$predicted_pass_writing <- ifelse(data$predicted_pass_writing_probability > 0.5, 1, 0)

# Summarize predictions by parental education level
pass_by_education_writing <- data %>%
  group_by(parental.level.of.education) %>%
  summarise(
    total_students = n(),
    actual_passing = sum(pass_indicator),
    predicted_passing_writing = sum(predicted_pass_writing) # Sum of binary predictions
  )

# Display predictions
print(pass_by_education_writing)



# reading bootstrap
# Add binary indicator for passing reading score (≥70%)
data <- data %>%
  mutate(pass_reading = ifelse(reading.score >= 70, 1, 0))

# Logistic regression function for a single bootstrap sample
bootstrap_mle_reading <- function(data) {
  boot_sample <- data[sample(nrow(data), replace = TRUE), ]
  model <- glm(pass_reading ~ parental.level.of.education, data = boot_sample, family = binomial())
  return(coef(model))
}

# Number of bootstrap iterations
n_boot <- 1000
set.seed(123) # For reproducibility

# Bootstrap MLE estimation for reading score
bootstrap_results_reading <- replicate(n_boot, bootstrap_mle_reading(data))

# Aggregate coefficients (mean across bootstrap samples)
mle_coefficients_reading <- apply(bootstrap_results_reading, 1, mean)

# Use the aggregated coefficients to create the best logistic regression model
best_log_reg_reading <- glm(
  pass_reading ~ parental.level.of.education,
  data = data,
  family = binomial(),
  start = mle_coefficients_reading # Use bootstrap-aggregated coefficients as starting values
)

# Predict passing probabilities for reading scores using the MLE-based model
data$predicted_pass_reading_probability <- predict(best_log_reg_reading, type = "response")

# Convert probabilities to binary predictions (1 if probability > 0.5, otherwise 0)
data$predicted_pass_reading <- ifelse(data$predicted_pass_reading_probability > 0.5, 1, 0)

# Summarize predictions by parental education level
pass_by_education_reading <- data %>%
  group_by(parental.level.of.education) %>%
  summarise(
    total_students = n(),
    actual_passing = sum(pass_indicator),
    predicted_passing_reading = sum(predicted_pass_reading) # Sum of binary predictions
  )

# Display predictions
print(pass_by_education_reading)



# bootstrap all
# Add binary indicator for passing (≥70% average score)
data <- data %>%
  mutate(pass_indicator = ifelse(average.score >= 70, 1, 0))

# Logistic regression function for a single bootstrap sample with all predictors
bootstrap_mle_full <- function(data) {
  boot_sample <- data[sample(nrow(data), replace = TRUE), ]
  model <- glm(
    pass_indicator ~ parental.level.of.education + gender + race.ethnicity +
      lunch + test.preparation.course,
    data = boot_sample,
    family = binomial()
  )
  return(coef(model))
}

# Number of bootstrap iterations
n_boot <- 1000
set.seed(123) # For reproducibility

# Bootstrap MLE estimation for full model
bootstrap_results_full <- replicate(n_boot, bootstrap_mle_full(data))

# Aggregate coefficients (mean across bootstrap samples)
mle_coefficients_full <- apply(bootstrap_results_full, 1, mean)

# Use the aggregated coefficients to create the best logistic regression model
best_log_reg_full <- glm(
  pass_indicator ~ parental.level.of.education + gender + race.ethnicity +
    lunch + test.preparation.course,
  data = data,
  family = binomial(),
  start = mle_coefficients_full # Use bootstrap-aggregated coefficients as starting values
)

# Predict passing probabilities using the MLE-based model
data$predicted_pass_full_probability <- predict(best_log_reg_full, type = "response")

# Convert probabilities to binary predictions (1 if probability > 0.5, otherwise 0)
data$predicted_pass_full <- ifelse(data$predicted_pass_full_probability > 0.5, 1, 0)

pass_by_education_full <- data %>%
  group_by(parental.level.of.education) %>%
  summarise(
    total_students = n(),
    actual_passing = sum(pass_indicator), # Actual passing students
    predicted_passing = sum(predicted_pass_full) # Predicted passing students
  )

# Display predictions with actual passing
print(pass_by_education_full)



# bootstrap all writing
# Add binary indicator for passing (≥70% writing score)
data <- data %>%
  mutate(pass_writing = ifelse(writing.score >= 70, 1, 0))

# Logistic regression function for a single bootstrap sample with all predictors for writing scores
bootstrap_mle_writing <- function(data) {
  boot_sample <- data[sample(nrow(data), replace = TRUE), ] # Create bootstrap sample
  model <- glm(
    pass_writing ~ parental.level.of.education + gender + race.ethnicity +
      lunch + test.preparation.course,
    data = boot_sample,
    family = binomial()
  )
  return(coef(model)) # Return coefficients
}

# Number of bootstrap iterations
n_boot <- 1000
set.seed(123) # For reproducibility

# Bootstrap MLE estimation for writing scores
bootstrap_results_writing <- replicate(n_boot, bootstrap_mle_writing(data))

# Aggregate coefficients (mean across bootstrap samples)
mle_coefficients_writing <- apply(bootstrap_results_writing, 1, mean)

# Use the aggregated coefficients to create the best logistic regression model
best_log_reg_writing <- glm(
  pass_writing ~ parental.level.of.education + gender + race.ethnicity +
    lunch + test.preparation.course,
  data = data,
  family = binomial(),
  start = mle_coefficients_writing # Use bootstrap-aggregated coefficients as starting values
)

# Predict passing probabilities for writing scores using the MLE-based model
data$predicted_pass_writing_probability <- predict(best_log_reg_writing, type = "response")

# Convert probabilities to binary predictions (1 if probability > 0.5, otherwise 0)
data$predicted_pass_writing <- ifelse(data$predicted_pass_writing_probability > 0.5, 1, 0)

# Summarize predictions by parental education level
pass_by_education_writing <- data %>%
  group_by(parental.level.of.education) %>%
  summarise(
    total_students = n(), # Total students in each group
    actual_passing = sum(pass_writing), # Actual passing students
    predicted_passing = sum(predicted_pass_writing) # Predicted passing students
  )

# Display predictions with actual passing
print(pass_by_education_writing)



# bootstrap all math
# Add binary indicator for passing (≥70% math score)
data <- data %>%
  mutate(pass_math = ifelse(math.score >= 70, 1, 0))

# Logistic regression function for a single bootstrap sample with all predictors for math scores
bootstrap_mle_math <- function(data) {
  boot_sample <- data[sample(nrow(data), replace = TRUE), ] # Create bootstrap sample
  model <- glm(
    pass_math ~ parental.level.of.education + gender + race.ethnicity +
      lunch + test.preparation.course,
    data = boot_sample,
    family = binomial()
  )
  return(coef(model)) # Return coefficients
}

# Number of bootstrap iterations
n_boot <- 1000
set.seed(123) # For reproducibility

# Bootstrap MLE estimation for math scores
bootstrap_results_math <- replicate(n_boot, bootstrap_mle_math(data))

# Aggregate coefficients (mean across bootstrap samples)
mle_coefficients_math <- apply(bootstrap_results_math, 1, mean)

# Use the aggregated coefficients to create the best logistic regression model
best_log_reg_math <- glm(
  pass_math ~ parental.level.of.education + gender + race.ethnicity +
    lunch + test.preparation.course,
  data = data,
  family = binomial(),
  start = mle_coefficients_math # Use bootstrap-aggregated coefficients as starting values
)

# Predict passing probabilities for math scores using the MLE-based model
data$predicted_pass_math_probability <- predict(best_log_reg_math, type = "response")

# Convert probabilities to binary predictions (1 if probability > 0.5, otherwise 0)
data$predicted_pass_math <- ifelse(data$predicted_pass_math_probability > 0.5, 1, 0)

# Summarize predictions by parental education level
pass_by_education_math <- data %>%
  group_by(parental.level.of.education) %>%
  summarise(
    total_students = n(), # Total students in each group
    actual_passing = sum(pass_math), # Actual passing students
    predicted_passing = sum(predicted_pass_math) # Predicted passing students
  )

# Display predictions with actual passing
print(pass_by_education_math)



# bootstrap all reading
# Add binary indicator for passing (≥70% reading score)
data <- data %>%
  mutate(pass_reading = ifelse(reading.score >= 70, 1, 0))

# Logistic regression function for a single bootstrap sample with all predictors for reading scores
bootstrap_mle_reading <- function(data) {
  boot_sample <- data[sample(nrow(data), replace = TRUE), ] # Create bootstrap sample
  model <- glm(
    pass_reading ~ parental.level.of.education + gender + race.ethnicity +
      lunch + test.preparation.course,
    data = boot_sample,
    family = binomial()
  )
  return(coef(model)) # Return coefficients
}

# Number of bootstrap iterations
n_boot <- 1000
set.seed(123) # For reproducibility

# Bootstrap MLE estimation for reading scores
bootstrap_results_reading <- replicate(n_boot, bootstrap_mle_reading(data))

# Aggregate coefficients (mean across bootstrap samples)
mle_coefficients_reading <- apply(bootstrap_results_reading, 1, mean)

# Use the aggregated coefficients to create the best logistic regression model
best_log_reg_reading <- glm(
  pass_reading ~ parental.level.of.education + gender + race.ethnicity +
    lunch + test.preparation.course,
  data = data,
  family = binomial(),
  start = mle_coefficients_reading # Use bootstrap-aggregated coefficients as starting values
)

# Predict passing probabilities for reading scores using the MLE-based model
data$predicted_pass_reading_probability <- predict(best_log_reg_reading, type = "response")

# Convert probabilities to binary predictions (1 if probability > 0.5, otherwise 0)
data$predicted_pass_reading <- ifelse(data$predicted_pass_reading_probability > 0.5, 1, 0)

# Summarize predictions by parental education level
pass_by_education_reading <- data %>%
  group_by(parental.level.of.education) %>%
  summarise(
    total_students = n(), # Total students in each group
    actual_passing = sum(pass_reading), # Actual passing students
    predicted_passing = sum(predicted_pass_reading) # Predicted passing students
  )

# Display predictions with actual passing
print(pass_by_education_reading)



# Calculate accuracies for average, math, writing, and reading models
accuracy_average <- mean(data$pass_indicator == data$predicted_pass) # Average score model
accuracy_math <- mean(data$pass_math == data$predicted_pass_math) # Math score model
accuracy_writing <- mean(data$pass_writing == data$predicted_pass_writing) # Writing score model
accuracy_reading <- mean(data$pass_reading == data$predicted_pass_reading) # Reading score model

# Create a data frame for plotting
accuracy_data <- data.frame(
  Model = c("Average", "Math", "Writing", "Reading"),
  Accuracy = c(accuracy_average, accuracy_math, accuracy_writing, accuracy_reading) * 100 # Convert to percentages
)

# Plot the accuracies
library(ggplot2)

ggplot(accuracy_data, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Bootstrap Accuracy Comparison: Average, Math, Writing, and Reading Models",
    x = "Model",
    y = "Accuracy (%)",
    fill = "Model"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  ) +
  ylim(0, 100) # Set y-axis limits to 0-100%



# Load required libraries
library(broom)   # For tidying regression outputs
install.packages("kableExtra")
library(kableExtra) # For table formatting

# Function to extract regression summary and display as a table
generate_glm_table <- function(glm_model, title) {
  # Use broom to tidy the model output
  glm_summary <- broom::tidy(glm_model)
  
  # Format table
  glm_table <- glm_summary %>%
    mutate(
      estimate = round(estimate, 3),
      std.error = round(std.error, 3),
      statistic = round(statistic, 3),
      p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
    ) %>%
    rename(
      Term = term,
      Estimate = estimate,
      `Std. Error` = std.error,
      `t-Statistic` = statistic,
      `p-Value` = p.value
    )
  
  # Display as HTML table
  glm_table %>%
    kbl(
      caption = title,
      align = "lcccc",
      format = "html"
    ) %>%
    kable_styling(
      full_width = FALSE,
      bootstrap_options = c("striped", "hover", "condensed", "responsive")
    )
}

# Generate tables for each GLM model
generate_glm_table(glm_average, "GLM: Average Score by Parental Education")

generate_glm_table(glm_math, "GLM: Math Score by Parental Education")

generate_glm_table(glm_read, "GLM: Reading Score by Parental Education")

generate_glm_table(glm_write, "GLM: Writing Score by Parental Education")

# For bivariate GLM models
generate_glm_table(bi_glm_average_gender, "Bivariate GLM: Average Score by Parental Education & Gender")

generate_glm_table(bi_glm_average_race, "Bivariate GLM: Average Score by Parental Education & Race/Ethnicity")

generate_glm_table(bi_glm_average_lunch, "Bivariate GLM: Average Score by Parental Education & Lunch Status")

generate_glm_table(bi_glm_average_test, "Bivariate GLM: Average Score by Parental Education & Test Preparation")

# For full model
generate_glm_table(full_glm_average, "Full Model GLM: Average Score by Parental Education and Other Predictors")



# Corrected function with ordered columns
aggregate_ordered_glm_results <- function(..., model_names, factor_levels) {
  # Gather models into a list
  models <- list(...)
  
  # Extract summaries using broom::tidy
  summaries <- lapply(models, function(model) {
    broom::tidy(model) %>%
      dplyr::select(term, estimate, p.value) %>%
      dplyr::mutate(
        estimate = round(estimate, 3),
        p.value = ifelse(p.value < 0.001, "<0.001", round(p.value, 3))
      )
  })
  
  # Add model names to each summary and ensure order of terms matches factor levels
  for (i in seq_along(summaries)) {
    summaries[[i]] <- summaries[[i]] %>%
      dplyr::mutate(
        Model = model_names[i],
        term = factor(term, levels = c("(Intercept)", paste0("parental.level.of.education", factor_levels)))
      ) %>%
      dplyr::arrange(term)
  }
  
  # Combine all summaries into one table
  combined_summary <- dplyr::bind_rows(summaries) %>%
    dplyr::filter(!is.na(term)) %>% # Remove rows where terms are not in the factor levels
    tidyr::pivot_wider(
      names_from = Model,
      values_from = c(estimate, p.value),
      names_glue = "{Model}_{.value}"
    )
  
  # Reorder columns to alternate estimate and p.value
  ordered_cols <- c("term", unlist(lapply(model_names, function(name) c(paste0(name, "_estimate"), paste0(name, "_p.value")))))
  combined_summary <- combined_summary[, ordered_cols]
  
  # Return formatted table
  combined_summary %>%
    kableExtra::kbl(
      caption = "GLM Results: Estimates and p-Values Ordered by Factor Levels and Columns",
      align = "lcccccc",
      format = "html"
    ) %>%
    kableExtra::kable_styling(
      full_width = FALSE,
      bootstrap_options = c("striped", "hover", "condensed", "responsive")
    )
}

# Specify the factor levels in the desired order
factor_levels <- c("High School", "Some HS", "Some College", "Assoc. Deg.", "Bach. Deg.", "Mast. Deg.")

# Run the function with your models and their names
aggregate_ordered_glm_results(
  glm_average, glm_math, glm_read, glm_write,
  model_names = c("Average", "Math", "Reading", "Writing"),
  factor_levels = factor_levels
)


# Perform Chi-Square tests for passing indicators with parental education levels

# Chi-Square test for average passing indicator
chisq_avg <- table(data$pass_indicator, data$parental.level.of.education)
chisq_test_avg <- chisq.test(chisq_avg)
cat("Chi-Square Test for Average Score Passing Indicator:\n")
print(chisq_test_avg)

# Chi-Square test for math passing indicator
chisq_math <- table(data$pass_math, data$parental.level.of.education)
chisq_test_math <- chisq.test(chisq_math)
cat("\nChi-Square Test for Math Score Passing Indicator:\n")
print(chisq_test_math)

# Chi-Square test for reading passing indicator
chisq_reading <- table(data$pass_reading, data$parental.level.of.education)
chisq_test_reading <- chisq.test(chisq_reading)
cat("\nChi-Square Test for Reading Score Passing Indicator:\n")
print(chisq_test_reading)

# Chi-Square test for writing passing indicator
chisq_writing <- table(data$pass_writing, data$parental.level.of.education)
chisq_test_writing <- chisq.test(chisq_writing)
cat("\nChi-Square Test for Writing Score Passing Indicator:\n")
print(chisq_test_writing)