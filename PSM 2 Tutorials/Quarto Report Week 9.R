UCL Probability, Statistics and Modelling 2 Week 9

{r include=FALSE}

# Load packages
library(easystats)
library(tidyverse)

{r echo=FALSE}

# Load the dataset
parking_violations <- read_csv("un_parking_violations.csv")

Question 1

What do you notice about this distribution? Is it closer to a normal distribution or the sort of distribution we would expect for a count variable?

{r echo=FALSE}

# Creates a histogram of the violations column in the data 
ggplot(parking_violations, aes(x = violations)) + geom_histogram()

Answer to Question 1

The histogram presented shows a highly right-skewed distribution, with the majority of observations concentrated near zero and a few extreme outliers extending toward very high values. 
This pattern aligns more closely with a count variable rather than a normal distribution. 
Count variables, especially when related to events like violations or occurrences, often exhibit this type of skewness due to their non-negative nature and the tendency for most entities to have low counts, while a small number may have disproportionately high counts. 
This indicates that this distribution is far from normal and reflects a typical count data pattern often modeled using Poisson or negative binomial distributions.

Question 2

Why is it important to account for the number of diplomats in each country’s UN delegation?

Answer to Question 2

It is crucial to account for the size of each country’s UN delegation to ensure fairness and comparability. 
Larger delegations would have more diplomats, which increases the potential for violations purely due to scale, independent of behavioral tendencies. 
By normalizing the violations count relative to the number of diplomats, a rate (e.g., violations per diplomat) could be derived to provide a more standardized and interpretable metric. 
This adjustment mitigates bias and allows for meaningful cross-country comparisons, ensuring that countries with larger delegations are not unfairly overrepresented in violation counts.

Question 3

How would you interpret these coefficients and the corresponding confidence intervals?

{r echo=FALSE}

# Using a Poission distribution to model count of parking violations
parking_model_poisson <- glm(
  violations ~ diplomats + corruption,
  data = parking_violations,
  family = "poisson"
)

Answer to Question 3

The results of the Poisson regression model indicate that both the number of diplomats and the corruption index significantly influence the expected number of parking violations. 
The coefficient for diplomats (0.02228) implies that for each additional diplomat in a country's UN delegation, the expected log count of parking violations increases by 0.02228. 
Exponentiating this coefficient reveals that each additional diplomat leads to approximately a 2.25% increase in parking violations, holding corruption constant. 
The confidence interval for this coefficient (0.02201 to 0.02256) is narrow and does not include zero, confirming the statistical significance of this relationship. 
Similarly, the coefficient for corruption (0.14117) suggests that a one-unit increase in the corruption index increases the expected log count of parking violations by 0.14117. 
This translates to a 15.16% increase in violations for each one-unit rise in corruption, with the confidence interval (0.13865 to 0.14369) also excluding zero, indicating strong statistical support for the effect. 
The intercept (5.64706) represents the log count of parking violations when both predictors are zero, though this value is primarily theoretical as zero corruption or zero diplomats may not be meaningful in this context.

{r echo=FALSE}

# Checking the equidispersion assumption
check_overdispersion(parking_model_poisson)

Question 4

Based on this test, is the equidispersion assumption true for our model? Is the outcome variable suitable for Poisson regression?

Answer to Question 4

The results of the overdispersion test indicate that the equidispersion assumption of the Poisson regression model is violated. 
In a Poisson model, the mean and variance of the outcome variable are assumed to be equal (equidispersion). 
However, the dispersion ratio of 3936.48 (far greater than 1) suggests that the variance of the outcome variable is significantly higher than its mean. 
The Pearson's chi-squared statistic of 562916.65 with a corresponding p-value of < 0.001 confirms that this overdispersion is statistically significant. 
This overdispersion suggests that the Poisson model is not a suitable fit for the outcome variable (violations).

Question 5

What is your interpretation of each coefficient and the associated p-value?

{r echo=FALSE}

# Using a negative binomial distribution to model count of parking violations
parking_model_nb <- MASS::glm.nb(
  violations ~ diplomats + corruption,
  data = parking_violations
)

{r echo=FALSE}

# Checking asumptions of regression model
check_collinearity(parking_model_nb)
check_outliers(parking_model_nb)
check_overdispersion(parking_model_nb)

{r echo=FALSE}

# Plot of influential observations
plot(check_outliers(parking_model_nb))

Answer to Question 5

The intercept (log-mean = 4.91) represents the baseline level of the outcome variable (on the log scale) when both predictors, diplomats and corruption, are zero. With a low p-value (< .001), this suggests that the intercept is statistically significant. 
The coefficient for diplomats (log-mean = 0.05, p < .001) indicates that for each additional diplomat, the outcome variable increases by approximately 5.1% holding all other variables constant on a statistically significant scale. 
Similarly, the corruption coefficient (log-mean = 0.19, p = 0.001) implies that each unit increase in corruption corresponds to a 20.9% increase in the outcome variable on a statistically significant scale, all else being equal.

Question 6

Which model fits the data better? Is that difference significant?

{r echo=FALSE}

# A likelihood-ratio test to see if the Poisson and negative-binomial models are different in how well they fit the data
compare_performance(parking_model_poisson, parking_model_nb, metrics = "common")
test_likelihoodratio(parking_model_poisson, parking_model_nb)

Answer to Question 6

The negative binomial model ((parking_model_nb)) fits the data significantly better than the Poisson model ((parking_model_poisson)), as demonstrated by multiple metrics. 
The AIC (2099.0) and BIC (2111.0) for the negative binomial model are substantially lower than the corresponding values for the Poisson model (2.8 x 10^5) for both criteria), indicating a superior fit. 
Additionally, the likelihood ratio test (LRT) comparing the two models produces a Chi-squared statistic of (2.8 x 10^5) with (p < .001), confirming that the improvement in fit is highly significant. 
While Nagelkerke’s (R^2) for the Poisson model is reported as 1.000, this likely reflects overfitting or a reporting anomaly given the other metrics, whereas the negative binomial model’s (R^2 = 0.116) is more realistic.

Question 7

Do the confidence intervals produced by each model for an explanatory variable overlap? Are the coefficients for diplomats significantly different from one another?

{r echo=FALSE}

# Comparing the coefficients across the two models 
compare_parameters(parking_model_poisson, parking_model_nb, exponentiate = TRUE)

Answer to Question 7

From a confidence interval perspective, diplomats and corruption are significant explanatory variables in both models, as their intervals do not include 1. 
The coefficients for diplomats are not significantly different between models as their confidence intervals overlap slightly, but the broader intervals in the negative binomial model indicate more uncertainty, aligning with its ability to handle overdispersion in the data.

Question 8

Are the assumptions of linear regression true in the case of this model? Can we trust the results produced by this model?

{r echo=FALSE}

# Fitting a linear model to model count of parking violations
parking_model_lm <- lm(
  violations ~ diplomats + corruption,
  data = parking_violations
)

{r echo=FALSE}

# Assumption checking

# Homoskedasticity	
check_heteroskedasticity(parking_model_lm)

# Normality of residuals	
check_normality(parking_model_lm)

# Multicollinearity	
check_collinearity(parking_model_lm)

# Influential outliers	
check_outliers(parking_model_lm)

# Independent residuals	
check_autocorrelation(parking_model_lm)

Answer to Question 8

The assumptions of linear regression are not fully satisfied in this model, as evidenced by the presence of heteroscedasticity, non-normality of residuals, autocorrelation, and an influential outlier. 
These violations undermine the reliability of standard errors, p-values, and confidence intervals, making the results less trustworthy for inferential purposes. 
However, multicollinearity is not a concern, as the VIF values are low, indicating stable relationships among predictors. 
As a result we cannot trust the model's results based on the reliability of the model's assumption results.
