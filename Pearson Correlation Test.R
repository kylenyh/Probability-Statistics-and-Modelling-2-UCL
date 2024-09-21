# the Pearson Correlation Test is used to measure the strength and direction of the linear relationship between two continuous variables.

# H0: there is no linear correlation between girth and height
# HA: there is a linear correlation between girth and height
# p-val < sig level reject H0
# p-val > sig level accept H0

# load library package
library(ggplot2)

# loads trees data from R
data(trees)

# gives first 5 rows of data
head(trees)

# Set significance level
alpha <- 0.05

# Create a scatter plot with a linear trend line
ggplot(data = trees, aes(x = Girth, y = Height)) +
  geom_point(color = "blue", size = 2) +  # scatter plot points
  geom_smooth(method = "lm", se = TRUE, color = "red") +  # linear trend line
  labs(title = "Scatter Plot of Tree Girth vs Height",
       x = "Girth (inches)",
       y = "Height (feet)") +  # Close the labs() function properly
  theme_minimal()

# Perform Pearson Correlation Test
cor_test_result <- cor.test(trees$Girth, trees$Height)

# Show correlation test result
cor_test_result

# Print test result with an interpretation
cat("Pearson Correlation Test:\n")
cat("Correlation Coefficient:", cor_test_result$estimate, "\n")
cat("p-value:", cor_test_result$p.value, "\n")

if(cor_test_result$p.value < alpha) {
  cat("Conclusion: Reject the null hypothesis (H0). There is a significant linear correlation between Girth and Height.\n")
} else {
  cat("Conclusion: Accept the null hypothesis (H0). There is no significant linear correlation between Girth and Height.\n")
}
