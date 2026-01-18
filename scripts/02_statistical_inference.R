# ==============================================================================
# Filename: 02_statistical_inference
# Purpose: To conduct statistical inference on the data set and produce plots, 
# graphs, bars and numerical values to validate hypothesis
# Data set: gym_members_exercise_tracking.csv (KAGGLE)
# Author: Mustaqim Bin Burhanuddin (Piqim)
# ==============================================================================

# Load packages
library(tidyverse)
library(car)           # VIF and diagnostics
library(broom)         # Tidy model outputs
library(performance)   # Model diagnostics
library(see)           # Visualizations for performance
library(emmeans)       # Estimated marginal means
library(ggeffects)     # Model predictions
library(patchwork)     # Combine plots
library(effectsize)    # Effect Size

# Load data
gym_data <- read_csv("./dataset/gym_members_exercise_tracking.csv")

# Data preparation
gym_data <- gym_data %>%
  mutate(
    # Convert to factors
    Gender = factor(Gender),
    Workout_Type = factor(Workout_Type),
    
    # Create labeled experience level
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
# Correlation Analysis
# ==============================================================================

cat("\n=== CORRELATION ANALYSIS ===\n")

# Select numeric predictors for correlation
numeric_vars <- gym_data %>%
  select(`Weight (kg)`, `Session_Duration (hours)`, Avg_BPM, Calories_Burned)

# Correlation matrix
cor_matrix <- cor(numeric_vars)
print(round(cor_matrix, 3))

# Visualize correlation
library(corrplot)
if (!requireNamespace("corrplot", quietly = TRUE)) {
  install.packages("corrplot")
  library(corrplot)
}

png("output/statistical_inference/correlation_matrix.png", width = 800, height = 800)
corrplot(cor_matrix, method = "color", type = "upper", 
         addCoef.col = "black", tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix: Predictors & Outcome",
         mar = c(0,0,2,0))
dev.off()

# ==============================================================================
# MODEL 1: Multiple Linear Regression (Base Model)
# Question: Can we predict calories burned from weight, 
#                    session duration, and Avg BPM?
# ==============================================================================

cat("\n=== MODEL 1: BASE MULTIPLE LINEAR REGRESSION ===\n")

# Fit the model
model1 <- lm(Calories_Burned ~ `Weight (kg)` + `Session_Duration (hours)` + Avg_BPM, 
             data = gym_data)

# Model summary
summary(model1)

# Tidy output
model1_tidy <- tidy(model1, conf.int = TRUE)
print(model1_tidy)

# Model performance metrics
model1_performance <- glance(model1)
print(model1_performance)

# Check for multicollinearity (VIF)
cat("\nVariance Inflation Factors (VIF):\n")
vif_values <- vif(model1)
print(vif_values)
cat("(VIF > 10 indicates problematic multicollinearity)\n")

# Diagnostic plots
png("output/statistical_inference/model1_diagnostics.png", width = 1200, height = 800)
par(mfrow = c(2, 3))
plot(model1, which = 1:6)
dev.off()

# Check assumptions with performance package
check_model1 <- check_model(model1, check = c("linearity", "homogeneity", "qq", "outliers"))
png("output/statistical_inference/model1_assumptions.png", width = 1400, height = 1000)
plot(check_model1)
dev.off()

# ==============================================================================
# MODEL 2: Multiple Linear Regression with Gender
# Question: Does gender affect calories burned when controlling 
#                    for weight, session duration, and Avg BPM?
# ==============================================================================

cat("\n=== MODEL 2: MULTIPLE LINEAR REGRESSION + GENDER ===\n")

# Fit the model
model2 <- lm(Calories_Burned ~ `Weight (kg)` + `Session_Duration (hours)` + 
               Avg_BPM + Gender, 
             data = gym_data)

# Model summary
summary(model2)

# Tidy output
model2_tidy <- tidy(model2, conf.int = TRUE)
print(model2_tidy)

# Compare Model 1 vs Model 2
cat("\n=== MODEL COMPARISON: Model 1 vs Model 2 ===\n")
anova_comparison <- anova(model1, model2)
print(anova_comparison)

# Performance comparison
comparison_table <- compare_performance(model1, model2, rank = TRUE)
print(comparison_table)

# Diagnostic plots for Model 2
png("output/statistical_inference/model2_diagnostics.png", width = 1200, height = 800)
par(mfrow = c(2, 3))
plot(model2, which = 1:6)
dev.off()

# ==============================================================================
# MODEL 3: Two-Way ANOVA (ANCOVA)
# Question: Which workout type burns the most calories, 
#                    controlling for session duration across genders?
# ==============================================================================

cat("\n=== MODEL 3: TWO-WAY ANOVA (ANCOVA) ===\n")

# Fit the ANCOVA model (with interaction)
model3 <- lm(Calories_Burned ~ Gender * Workout_Type + `Session_Duration (hours)`, 
             data = gym_data)

# ANOVA table
anova_table <- Anova(model3, type = "III")
print(anova_table)

# Model summary
summary(model3)

# Effect sizes (eta-squared)
cat("\nEffect Sizes (Eta-squared):\n")
eta_squared(model3)

# Diagnostic plots for Model 3
png("output/statistical_inference/model3_diagnostics.png", width = 1200, height = 800)
par(mfrow = c(2, 3))
plot(model3, which = 1:6)
dev.off()

# ==============================================================================
# POST-HOC TESTS for ANOVA
# ==============================================================================

cat("\n=== POST-HOC TESTS ===\n")

# Estimated marginal means for Workout Type
emm_workout <- emmeans(model3, ~ Workout_Type)
print(emm_workout)

# Pairwise comparisons for Workout Type
pairs_workout <- pairs(emm_workout, adjust = "tukey")
print(pairs_workout)

# Estimated marginal means for Gender × Workout Type interaction
emm_interaction <- emmeans(model3, ~ Gender * Workout_Type)
print(emm_interaction)

# Pairwise comparisons for interaction
pairs_interaction <- pairs(emm_interaction, adjust = "tukey")
print(pairs_interaction)

# ==============================================================================
# VISUALIZATIONS
# ==============================================================================

# Visualization 1: Model 1 - Predicted vs Actual
pred_data1 <- augment(model1)

v1 <- ggplot(pred_data1, aes(x = .fitted, y = Calories_Burned)) +
  geom_point(alpha = 0.5, color = "#2E86AB") +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Model 1: Predicted vs Actual Calories Burned",
       subtitle = paste("R² =", round(model1_performance$r.squared, 3)),
       x = "Predicted Calories Burned",
       y = "Actual Calories Burned") +
  theme_minimal()

ggsave("output/statistical_inference/model1_predicted_vs_actual.png", v1, width = 8, height = 6)

# Visualization 2: Model 2 - Gender effect
pred_data2 <- ggpredict(model2, terms = c("Weight (kg)", "Gender"))

v2 <- ggplot(pred_data2, aes(x = x, y = predicted, color = group, fill = group)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(title = "Model 2: Effect of Weight on Calories Burned by Gender",
       subtitle = "Controlling for Session Duration and Avg BPM",
       x = "Weight (kg)",
       y = "Predicted Calories Burned",
       color = "Gender",
       fill = "Gender") +
  theme_minimal() +
  scale_color_manual(values = c("#2E86AB", "#A23B72")) +
  scale_fill_manual(values = c("#2E86AB", "#A23B72"))

ggsave("output/statistical_inference/model2_gender_effect.png", v2, width = 10, height = 6)

# Visualization 3: ANOVA - Interaction plot
emm_plot_data <- as.data.frame(emm_interaction)

v3 <- ggplot(emm_plot_data, aes(x = Workout_Type, y = emmean, color = Gender, group = Gender)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2, linewidth = 1) +
  labs(title = "Estimated Marginal Means: Calories Burned",
       subtitle = "Gender × Workout Type Interaction (controlling for Session Duration)",
       x = "Workout Type",
       y = "Estimated Calories Burned",
       color = "Gender") +
  theme_minimal() +
  scale_color_manual(values = c("#2E86AB", "#A23B72")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("output/statistical_inference/model3_interaction_plot.png", v3, width = 10, height = 6)

# Visualization 4: Boxplot by Workout Type and Gender
v4 <- ggplot(gym_data, aes(x = Workout_Type, y = Calories_Burned, fill = Gender)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Calories Burned by Workout Type and Gender",
       x = "Workout Type",
       y = "Calories Burned") +
  theme_minimal() +
  scale_fill_manual(values = c("#2E86AB", "#A23B72")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("output/statistical_inference/calories_by_workout_gender.png", v4, width = 10, height = 6)

# ==============================================================================
# EXPORT RESULTS
# ==============================================================================

# Model 1 coefficients
write_csv(model1_tidy, "output/statistical_inference/model1_coefficients.csv")

# Model 2 coefficients
write_csv(model2_tidy, "output/statistical_inference/model2_coefficients.csv")

# Model comparison
write_csv(as.data.frame(comparison_table), "output/statistical_inference/models_comparison.csv")

# ANOVA table
write_csv(as.data.frame(anova_table), "output/statistical_inference/anova_table.csv")

# Post-hoc: Workout Type comparisons
write_csv(as.data.frame(pairs_workout), "output/statistical_inference/posthoc_workout_type.csv")

# Post-hoc: Interaction comparisons
write_csv(as.data.frame(pairs_interaction), "output/statistical_inference/posthoc_interaction.csv")

# Estimated marginal means
write_csv(emm_plot_data, "output/statistical_inference/estimated_marginal_means.csv")

# ==============================================================================
# SUMMARY REPORT
# ==============================================================================

cat("\n" , rep("=", 80), "\n", sep = "")
cat("STATISTICAL INFERENCE SUMMARY\n")
cat(rep("=", 80), "\n", sep = "")

cat("\n📊 MODEL 1: Base Regression\n")
cat("   R² = ", round(model1_performance$r.squared, 3), "\n")
cat("   Adjusted R² = ", round(model1_performance$adj.r.squared, 3), "\n")
cat("   F-statistic = ", round(model1_performance$statistic, 2), "\n")
cat("   p-value < 0.001\n")

cat("\n📊 MODEL 2: Regression + Gender\n")
cat("   R² = ", round(glance(model2)$r.squared, 3), "\n")
cat("   Adjusted R² = ", round(glance(model2)$adj.r.squared, 3), "\n")
gender_coef <- model2_tidy %>% filter(term == "GenderMale")
cat("   Gender coefficient = ", round(gender_coef$estimate, 2), "\n")
cat("   Gender p-value = ", format.pval(gender_coef$p.value, digits = 3), "\n")

cat("\n📊 MODEL 3: Two-Way ANOVA\n")
cat("   Main effect - Gender: p = ", format.pval(anova_table$`Pr(>F)`[2], digits = 3), "\n")
cat("   Main effect - Workout Type: p = ", format.pval(anova_table$`Pr(>F)`[3], digits = 3), "\n")
cat("   Interaction: p = ", format.pval(anova_table$`Pr(>F)`[4], digits = 3), "\n")

cat("\n✓ All results exported to output/statistical_inference/ folder\n")
cat("✓ Total files created: ")
cat(length(list.files("output", pattern = "model|anova|posthoc|emm|correlation")), "\n")

cat("\n", rep("=", 80), "\n", sep = "")
cat("Thank you for running the analysis! - Piqim :)")
