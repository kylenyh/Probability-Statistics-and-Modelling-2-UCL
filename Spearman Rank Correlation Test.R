# the Spearman Rank Correlation Test is used to measure the strength and direction of the association between two ordinal or continuous variables.

# H0: there is no monotonic correlation between miles per galon and horsepower
# HA: there is a monotonic correlation between miles per galon and horsepower
# p-val < sig level reject H0
# p-val > sig level accept H0

# load library package
library(ggplot2)

# loads cars data from R
data(mtcars)

# gives first 5 rows of data
head(mtcars)

# Set significance level
alpha <- 0.05

# Perform Pearson Correlation Test
cor_test_result <- cor.test(mtcars$mpg, mtcars$hp, method = "spearman")

# shows correlation test result
cor_test_result

# Create a scatter plot with a linear trend line
ggplot(data = mtcars, aes(x = hp, y = mpg)) +
  geom_point(color = "blue", size = 2) +  # scatter plot points
  geom_smooth(method = "lm", se = TRUE, color = "red") +  # linear trend line
  labs(title = "Scatter Plot of Miles per Gallon vs. Horsepower",
       x = "Horsepower (hp)",
       y = "Miles per Gallon (mpg)") +
  theme_minimal()

# Print test result with an interpretation
cat("Pearson Correlation Test:\n")
cat("Correlation Coefficient:", cor_test_result$estimate, "\n")
cat("p-value:", cor_test_result$p.value, "\n")

if(cor_test_result$p.value < alpha) {
  cat("Conclusion: Reject the null hypothesis (H0). There is a monotonic correlation between miles per galon and horsepower.\n")
} else {
  cat("Conclusion: Accept the null hypothesis (H0). There is no monotonic correlation between miles per galon and horsepower.\n")
}
