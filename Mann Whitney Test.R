# the Mann Whitney Test is used to compare two independent groups to determine whether their population distributions are different.
# H0: the distributions of customer satisfaction ratings are the same between Store A and Store B.
# HA: the distributions of customer satisfaction ratings are not the same between Store A and Store B.
# p-val < sig level reject H0
# p-val > sig level accept H0

# Load necessary library packages
library(ggplot2)

# Sample data for two independent groups (customer satisfaction ratings for Store A and Store B)
satisfaction_data <- data.frame(
  customer = 1:30,
  store = rep(c("Store A", "Store B"), each = 15),
  rating = c(7, 8, 6, 7, 9, 8, 6, 8, 7, 9, 7, 8, 6, 7, 8, # Store A
             6, 5, 7, 6, 5, 7, 6, 6, 5, 6, 7, 5, 6, 6, 7)  # Store B
)

# Display the dataset
print(satisfaction_data)

# Perform Mann-Whitney U test
mann_whitney_result <- wilcox.test(rating ~ store, data = satisfaction_data, exact = FALSE)

# Print the result
print(mann_whitney_result)

# Visualization: Boxplot of Ratings by Store
boxplot(rating ~ store, data = satisfaction_data, 
        col = c("lightblue", "lightgreen"),
        main = "Customer Satisfaction Ratings by Store",
        xlab = "Store", ylab = "Satisfaction Rating")

# Visualization: ggplot2 Boxplot of Ratings by Store
ggplot(satisfaction_data, aes(x = store, y = rating, fill = store)) +
  geom_boxplot() +
  labs(title = "Customer Satisfaction Ratings by Store", x = "Store", y = "Satisfaction Rating") +
  theme_minimal() +
  scale_fill_manual(values = c("Store A" = "lightblue", "Store B" = "lightgreen")) +
  theme(legend.position = "none")

# Define significance level
alpha <- 0.05

# Print test result with an interpretation
cat("Mann-Whitney U Test Results:\n")
cat("W statistic:", mann_whitney_result$statistic, "\n")
cat("p-value:", mann_whitney_result$p.value, "\n")

if(mann_whitney_result$p.value < alpha) {
  cat("Conclusion: Reject the null hypothesis (H0). There is a significant difference in the customer satisfaction ratings between Store A and Store B.\n")
} else {
  cat("Conclusion: Accept the null hypothesis (H0). There is no significant difference in the customer satisfaction ratings between Store A and Store B.\n")
}
