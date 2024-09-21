# the One-Way Anova Test used to determine if there are statistically significant differences between the means of three or more independent (unrelated) groups.

# H0: the mean plant growth is the same across all fertilizer types
# HA: there is at least one fertilizer type that results in different mean plant growth compared to the others
# p-val < sig level reject H0
# p-val > sig level accept H0

# Load library packages
library(ggplot2)
library(reshape2)  # For the melt function

# Sample data for paired groups
diet_data <- data.frame(
  participant = 1:15,
  weight_before = c(78, 85, 92, 76, 88, 81, 90, 77, 84, 79, 86, 75, 82, 87, 80),
  weight_after = c(74, 82, 89, 73, 85, 78, 87, 74, 82, 76, 83, 72, 80, 84, 77)
)

# Display the dataset
print(diet_data)

# Perform paired sample t-test
paired_t_test_result <- t.test(diet_data$weight_before, diet_data$weight_after, paired = TRUE)

# Print the result
print(paired_t_test_result)

# Visualization: Base R Plot - Before and After Weights
plot(diet_data$participant, diet_data$weight_before, type = "o", col = "red", pch = 16, ylim = c(min(diet_data$weight_after) - 5, max(diet_data$weight_before) + 5),
     xlab = "Participant", ylab = "Weight (kg)", main = "Weight of Participants Before and After Diet Program")
lines(diet_data$participant, diet_data$weight_after, type = "o", col = "blue", pch = 16)
legend("topright", legend = c("Before Diet", "After Diet"), col = c("red", "blue"), pch = 16)

# Visualization: ggplot2 Plot - Before and After Weights
diet_data_long <- melt(diet_data, id.vars = 'participant', variable.name = 'Condition', value.name = 'Weight')

# Plot using ggplot2
ggplot(diet_data_long, aes(x = factor(participant), y = Weight, color = Condition, group = participant)) +
  geom_point(size = 3) +
  geom_line() +
  labs(title = "Weight of Participants Before and After Diet Program", x = "Participant", y = "Weight (kg)") +
  theme_minimal() +
  scale_color_manual(values = c("weight_before" = "red", "weight_after" = "blue")) +
  theme(legend.position = "top")

# Print test result with an interpretation
cat("Paired-Sample T-Test Results:\n")
cat("Test Statistic:", paired_t_test_result$statistic, "\n")
cat("p-value:", paired_t_test_result$p.value, "\n")

if(paired_t_test_result$p.value < alpha) {
  cat("Conclusion: Reject the null hypothesis (H0). There is a significant difference in the mean weight of participants before and after the diet program.\n")
} else {
  cat("Conclusion: Accept the null hypothesis (H0). There is no significant difference in the mean weight of participants before and after the diet program.\n")
}
