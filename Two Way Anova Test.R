# the Two-Way ANOVA test is used to determine the effect of two different categorical independent variables on one continuous dependent variable, as well as to test for any interaction effects between the two factors.

# H0 (fertilizer): there is no significant difference in mean yield between the fertilizer types
# H0 (water level): there is no significant difference in mean yield between the water levels
# H0 (interaction): there is no significant interaction between fertilizer type and water level affecting the mean yield
# HA (fertilizer): there is a significant difference in mean yield between the fertilizer types
# HA (water level): there is a significant difference in mean yield between the water levels
# HA (interaction): there is a significant interaction between fertilizer type and water level affecting the mean yield
# p-val < sig level reject H0
# p-val > sig level accept H0

# Load necessary library
library(ggplot2)

# Sample data for Two-Way ANOVA
crop_data <- data.frame(
  fertilizer = factor(rep(c("A", "B", "C"), each = 10, times = 2)),  # Factor 1: Fertilizer type
  water_level = factor(rep(c("Low", "High"), each = 30)),            # Factor 2: Water level
  yield = c(15, 16, 17, 20, 18, 22, 21, 19, 23, 24,  # Fertilizer A, Low water
            13, 15, 14, 18, 17, 16, 19, 21, 20, 18,  # Fertilizer B, Low water
            12, 13, 14, 15, 16, 17, 19, 18, 20, 21,  # Fertilizer C, Low water
            25, 26, 28, 27, 30, 29, 31, 32, 33, 34,  # Fertilizer A, High water
            22, 23, 25, 26, 27, 28, 29, 30, 31, 32,  # Fertilizer B, High water
            20, 21, 22, 24, 23, 25, 26, 28, 27, 29)  # Fertilizer C, High water
)

# Display the dataset
print(crop_data)

# Perform Two-Way ANOVA
anova_result <- aov(yield ~ fertilizer * water_level, data = crop_data)

# Print the summary of the ANOVA result
summary(anova_result)

# Define significance level
alpha <- 0.05

# Extract p-values from the ANOVA result
anova_summary <- summary(anova_result)
fertilizer_p <- anova_summary[[1]]["fertilizer", "Pr(>F)"]
water_level_p <- anova_summary[[1]]["water_level", "Pr(>F)"]
interaction_p <- anova_summary[[1]]["fertilizer:water_level", "Pr(>F)"]

# Interpret results based on significance level
cat("ANOVA Results Interpretation:\n")
cat("Fertilizer p-value:", fertilizer_p, "\n")
cat("Water Level p-value:", water_level_p, "\n")
cat("Interaction p-value:", interaction_p, "\n")

if(fertilizer_p < alpha) {
  cat("Conclusion: Reject H0 for fertilizer. There is a significant difference in mean yield between fertilizer types.\n")
} else {
  cat("Conclusion: Accept H0 for fertilizer. There is no significant difference in mean yield between fertilizer types.\n")
}

if(water_level_p < alpha) {
  cat("Conclusion: Reject H0 for water level. There is a significant difference in mean yield between water levels.\n")
} else {
  cat("Conclusion: Accept H0 for water level. There is no significant difference in mean yield between water levels.\n")
}

if(interaction_p < alpha) {
  cat("Conclusion: Reject H0 for interaction. There is a significant interaction between fertilizer type and water level affecting the mean yield.\n")
} else {
  cat("Conclusion: Accept H0 for interaction. There is no significant interaction between fertilizer type and water level affecting the mean yield.\n")
}

# Create a boxplot using ggplot2
ggplot(crop_data, aes(x = interaction(fertilizer, water_level), y = yield, fill = water_level)) +
  geom_boxplot() +
  labs(title = "Boxplot of Crop Yield by Fertilizer Type and Water Level",
       x = "Fertilizer Type and Water Level",
       y = "Yield") +
  theme_minimal() +
  scale_fill_manual(values = c("Low" = "skyblue", "High" = "orange"), 
                    name = "Water Level") +
  theme(plot.title = element_text(hjust = 0.5))
