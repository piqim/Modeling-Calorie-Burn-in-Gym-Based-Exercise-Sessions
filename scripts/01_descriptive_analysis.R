# ==============================================================================
# Filename: 01_descriptive_analysis
# Purpose: To conduct descriptive analysis on the data set and produce plots, 
# graphs, bars and numerical values
# Data set: gym_members_exercise_tracking.csv (KAGGLE)
# Author: Mustaqim Bin Burhanuddin (Piqim)
# ==============================================================================

# Load packages
library(tidyverse)
library(psych)
library(skimr)
library(summarytools)

# Load data
gym_data <- read_csv("./dataset/gym_members_exercise_tracking.csv")
# quick view
skim(gym_data)

# ==============================================================================
# Part 1: Descriptive Analysis
# ==============================================================================

# Data preparation
## Convert Experience_Level to factor for better interpretation
gym_data <- gym_data %>%
  mutate(
    Experience_Level_Label = case_when(
      Experience_Level == 1 ~ "Beginner",
      Experience_Level == 2 ~ "Intermediate",
      Experience_Level == 3 ~ "Expert",
      TRUE ~ as.character(Experience_Level)
    ),
    Experience_Level_Label = factor(Experience_Level_Label, 
                                    levels = c("Beginner", "Intermediate", "Expert"))
  )


# ==============================================================================
## STEP 1: Summarize Each Variable
# ==============================================================================

# Continuous variables
continuous_vars <- c("Age", "Weight (kg)", "Height (m)", "BMI", 
                     "Max_BPM", "Avg_BPM", "Resting_BPM",
                     "Session_Duration (hours)", "Calories_Burned",
                     "Fat_Percentage", "Water_Intake (liters)", 
                     "Workout_Frequency (days/week)")

# Create summary statistics
continuous_summary <- gym_data %>%
  select(all_of(continuous_vars)) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", 
               values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~round(., 2)))

# Display
print(continuous_summary)

# Alternative: Use psych package for richer summaries
cat("\n=== Detailed Statistics (psych package) ===\n")
describe(gym_data %>% select(all_of(continuous_vars)))

# Categorical variables --------------------------------------------------------
categorical_vars <- c("Gender", "Workout_Type", "Experience_Level_Label")

categorical_summary <- gym_data %>%
  select(all_of(categorical_vars)) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", 
               values_to = "Category") %>%
  group_by(Variable, Category) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Variable) %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 2))

print(categorical_summary)

# ==============================================================================
## STEP 2: Explore Distributions
# ==============================================================================

# Create output directory if it doesn't exist
if (!dir.exists("output")) {
  dir.create("output")
}

# Histograms for key continuous variables

# Age Distribution
p1 <- ggplot(gym_data, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "#2E86AB", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = mean(Age)), 
             color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Distribution of Age",
       subtitle = paste("Mean:", round(mean(gym_data$Age), 2), "| SD:", round(sd(gym_data$Age), 2)),
       x = "Age (years)",
       y = "Count") +
  theme_minimal()

print(p1)
ggsave("output/age_distribution.png", p1, width = 8, height = 5)

# BMI Distribution
p2 <- ggplot(gym_data, aes(x = BMI)) +
  geom_histogram(bins = 30, fill = "#A23B72", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = mean(BMI)), 
             color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Distribution of BMI",
       subtitle = paste("Mean:", round(mean(gym_data$BMI), 2), "| SD:", round(sd(gym_data$BMI), 2)),
       x = "BMI",
       y = "Count") +
  theme_minimal()

print(p2)
ggsave("output/bmi_distribution.png", p2, width = 8, height = 5)

# Calories Burned Distribution
p3 <- ggplot(gym_data, aes(x = Calories_Burned)) +
  geom_histogram(bins = 30, fill = "#F18F01", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = mean(Calories_Burned)), 
             color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Distribution of Calories Burned",
       subtitle = paste("Mean:", round(mean(gym_data$Calories_Burned), 2), "| SD:", round(sd(gym_data$Calories_Burned), 2)),
       x = "Calories Burned",
       y = "Count") +
  theme_minimal()

print(p3)
ggsave("output/calories_distribution.png", p3, width = 8, height = 5)

# Session Duration Distribution
p4 <- ggplot(gym_data, aes(x = `Session_Duration (hours)`)) +
  geom_histogram(bins = 30, fill = "#6A994E", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = mean(`Session_Duration (hours)`)), 
             color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Distribution of Session Duration",
       subtitle = paste("Mean:", round(mean(gym_data$`Session_Duration (hours)`), 2), "hours | SD:", round(sd(gym_data$`Session_Duration (hours)`), 2)),
       x = "Session Duration (hours)",
       y = "Count") +
  theme_minimal()

print(p4)
ggsave("output/session_duration_distribution.png", p4, width = 8, height = 5)

# Bar Charts for categorical variables

# Gender Distribution
p5 <- ggplot(gym_data, aes(x = Gender, fill = Gender)) +
  geom_bar(alpha = 0.8) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  scale_fill_manual(values = c("#2E86AB", "#A23B72")) +
  labs(title = "Distribution of Gender",
       x = "Gender",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

print(p5)
ggsave("output/gender_distribution.png", p5, width = 8, height = 5)

# Workout Type Distribution
p6 <- ggplot(gym_data, aes(x = Workout_Type, fill = Workout_Type)) +
  geom_bar(alpha = 0.8) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Distribution of Workout Type",
       x = "Workout Type",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p6)
ggsave("output/workout_type_distribution.png", p6, width = 8, height = 5)

# Experience Level Distribution
p7 <- ggplot(gym_data, aes(x = Experience_Level_Label, fill = Experience_Level_Label)) +
  geom_bar(alpha = 0.8) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  scale_fill_manual(values = c("#6A994E", "#F18F01", "#BC4B51")) +
  labs(title = "Distribution of Experience Level",
       x = "Experience Level",
       y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

print(p7)
ggsave("output/experience_level_distribution.png", p7, width = 8, height = 5)

# ==============================================================================
## Additional exploratory plots
# ==============================================================================

# Workout Frequency Distribution
p8 <- ggplot(gym_data, aes(x = `Workout_Frequency (days/week)`)) +
  geom_bar(fill = "#577590", alpha = 0.8) +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = -0.5) +
  labs(title = "Distribution of Workout Frequency",
       x = "Workout Frequency (days/week)",
       y = "Count") +
  theme_minimal()

print(p8)
ggsave("output/workout_frequency_distribution.png", p8, width = 8, height = 5)

# Calories Burned by Workout Type (Boxplot)
p9 <- ggplot(gym_data, aes(x = Workout_Type, y = Calories_Burned, fill = Workout_Type)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Calories Burned by Workout Type",
       x = "Workout Type",
       y = "Calories Burned") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p9)
ggsave("output/calories_by_workout_type.png", p9, width = 8, height = 5)

# Calories Burned by Experience Level (Boxplot)
p10 <- ggplot(gym_data, aes(x = Experience_Level_Label, y = Calories_Burned, fill = Experience_Level_Label)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("#6A994E", "#F18F01", "#BC4B51")) +
  labs(title = "Calories Burned by Experience Level",
       x = "Experience Level",
       y = "Calories Burned") +
  theme_minimal() +
  theme(legend.position = "none")

print(p10)
ggsave("output/calories_by_experience.png", p10, width = 8, height = 5)

# ==============================================================================
## Export summaries to CSV
# ==============================================================================

write_csv(continuous_summary, "output/continuous_summary.csv")
write_csv(categorical_summary, "output/categorical_summary.csv")

# ==============================================================================
## Final message
# ==============================================================================

cat("\n✓ Descriptive analysis complete!\n")
cat("✓", length(list.files("outputs", pattern = ".png")), "plots saved to output/ folder\n")
cat("✓ Summary tables saved as CSV files\n")
cat("\nKey findings to explore:\n")
cat("- Average age:", round(mean(gym_data$Age), 1), "years\n")
cat("- Average calories burned:", round(mean(gym_data$Calories_Burned), 1), "\n")
cat("- Most common workout type:", names(sort(table(gym_data$Workout_Type), decreasing = TRUE))[1], "\n")
cat("- Gender split:", paste(round(prop.table(table(gym_data$Gender)) * 100, 1), collapse = "% / "), "%\n")