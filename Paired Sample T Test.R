# a Paired Sample T-Test is used to compare the means of two related groups that are measured under two different conditions, or when there is a natural pairing between the subjects in the two groups.

# H0: there is no significant difference in the mean test scores of students before and after the study program.
# HA: there is a significant difference in the mean test scores of students before and after the study program.
# p-val < sig level reject H0
# p-val > sig level accept H0

# Load necessary library packages
library(ggplot2)
library(reshape2)

# Sample data for paired groups
scores_data <- data.frame(
  student = 1:15,
  score_before = c(68, 75, 62, 71, 79, 74, 65, 73, 70, 68, 72, 69, 66, 78, 71),
  score_after = c(75, 80, 70, 78, 85, 80, 72, 78, 75, 74, 79, 73, 70, 84, 76)
)

# Display the dataset
print(scores_data)

# Perform paired sample t-test
paired_t_test_result <- t.test(scores_data$score_before, scores_data$score_after, paired = TRUE)

# Print the result
print(paired_t_test_result)

# Visualization: Base R Plot - Before and After Scores
plot(scores_data$student, scores_data$score_before, type = "o", col = "red", pch = 16, ylim = c(min(scores_data$score_after) - 5, max(scores_data$score_before) + 5),
     xlab = "Student", ylab = "Test Score", main = "Test Scores Before and After Study Program")
lines(scores_data$student, scores_data$score_after, type = "o", col = "blue", pch = 16)
legend("topright", legend = c("Before Study Program", "After Study Program"), col = c("red", "blue"), pch = 16)

# Visualization: ggplot2 Plot - Before and After Scores
scores_data_long <- melt(scores_data, id.vars = 'student', variable.name = 'Condition', value.name = 'Score')

# Plot using ggplot2
ggplot(scores_data_long, aes(x = factor(student), y = Score, color = Condition, group = student)) +
  geom_point(size = 3) +
  geom_line() +
  labs(title = "Test Scores Before and After Study Program", x = "Student", y = "Test Score") +
  theme_minimal() +
  scale_color_manual(values = c("score_before" = "red", "score_after" = "blue")) +
  theme(legend.position = "top")

# Define significance level
alpha <- 0.05

# Print test result with an interpretation
cat("Paired-Sample T-Test Results:\n")
cat("Test Statistic:", paired_t_test_result$statistic, "\n")
cat("p-value:", paired_t_test_result$p.value, "\n")

if(paired_t_test_result$p.value < alpha) {
  cat("Conclusion: Reject the null hypothesis (H0). There is a significant difference in the mean test scores of students before and after the study program.\n")
} else {
  cat("Conclusion: Accept the null hypothesis (H0). There is no significant difference in the mean test scores of students before and after the study program.\n")
}
