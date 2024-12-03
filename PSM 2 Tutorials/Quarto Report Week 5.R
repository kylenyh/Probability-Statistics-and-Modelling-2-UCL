UCL Probability, Statistics and Modelling 2 Week 5

{r include=FALSE}

# Load packages
library(haven)
library(emmeans)
library(sjPlot)
library(sjmisc)
library(ggplot2)
library(psych)
library(car)
library(olsrr)

{r echo=FALSE}

# Set working directory
setwd("C:/Users/user/OneDrive/Desktop/UCL_PSM_2_R/Week_5")

# Reading the data
covpeak <- read_dta("Covid_19_PSMII.dta")

# Browsing through names in the dataset 
names(covpeak)

# Returns top and bottom of data
head(covpeak)
tail(covpeak)

# Opens data in a separate window
View(covpeak)

#If you are only working on a single dataset, it might make sense to use the "attach"
#command, which will allow you not to mention the name of your data for several commands
#(it saves you some time and typing)
attach(covpeak)

Question 1

The behavioural science unit is concerned whether the final model for expectations regarding the pandemic from last week satisfies the required modelling assumptions1. 
You are asked to consider each of the following:

a. Homoskedasticity (i.e., that the residual variance is constant)
b. Linearity (i.e., the explanatory and outcome variables have a linear relationship)
c. Normality of the residuals (i.e., the residual variance is normally distributed)
d. (Multi)collinearity (i.e., there are no highly correlated variables which would have an undue influence on the results)
e. Influential outliers (i.e., the model is not thrown off by outliers)
f. Considering all these aspects, what kind of assurance can you provide regarding each of these? Were any of these assumptions violated? 
If they were, how would you fix them?

{r echo=FALSE}

# Model 7 from previous week
covidexp7 <- lm(covidexp ~ keywork*female + female*covconc + gov + nhs +
                  age1 + age2 + age3)

# Summary of model 7
summary(covidexp7)

{r echo=FALSE}

# Homoskedasticity

# Create a residual vs. fitted values plot (as derived earlier)
plot(covidexp7$resid ~ covidexp7$fitted.values)

# Add horizental line at 0
abline(h = 0, lty = 2)

# This might be a little more helpful, or the one derived by plot_model earlier
residualPlots(covidexp7)

{r echo=FALSE}

# Linearity

# Linearity of the fitted values, appropriateness of the interactions
# We derive the same plot as for homoskedasticity but we also consider
# the various explanatory variables and their interactions

# In addition to the fitted values, consider each of the explanatory variables
residualPlot(covidexp7, variable = "covconc")
residualPlot(covidexp7, variable = "gov")
residualPlot(covidexp7, variable = "nhs")

{r echo=FALSE}

# Normality

# Normality of the error terms can be gauged using the Q-Q plots
qqnorm(covidexp7$residuals)
qqline(covidexp7$residuals)

{r echo=FALSE}

# Multicollinearity

# You can derive the variance inflation function (VIF) using the car package
car::vif(covidexp7) 

# You can also consider these by using the plot_model diagnostics figures

{r echo=FALSE}

# Influential outliers

# From the covidexp7 model, we save the standardised 
# residuals and store them in resstd (the name of this is arbitrary)
resstd <- rstandard(covidexp7)  

# Plot the standardized residuals in y axis. X axis will be the index or row names
plot(resstd, ylab="Standardised Residual", ylim=c(-3.5,3.5))

# Add horizontal lines 3 and -3 to identify extreme values
abline(h =c(-3,-2.5,0,2.5,3), lty = 2)

# Find out which data point is outside of 3 standard deviation cut-off
# Index corresponds to the row numbers of those point
index3 <- which(resstd > 3 | resstd < -3)
index3

# The number of cases can be checked with the describe function
describe(index3)

# Summary of model 7
summary(covidexp7)

# Consider the proportion of these cases in your data
1/1196
# Less than 0.1%

{r echo=FALSE}

# Following the same procedure, find out which data point 
# is outside of 2.5 standard deviation cut-off
index2.5 <- which(resstd > 2.5 | resstd < -2.5)
index2.5
describe(index2.5)

# 10 cases
10/1196
# Less than 1%

#An alternative way of plotting standardised residuals
ols_plot_resid_stand(covidexp7)


#Identifying outliers using Cook's distance
influencePlot(covidexp7, main="Influence Plot", 
              sub="Circle size is proportional to Cook's Distance" )
infIndexPlot(covidexp7)
#Thershold for Cook's Distance
4/1196

#Another way of plotting Cook's distance
ols_plot_cooksd_chart(covidexp7)

#You can get a better sense of the influential cases by deriving Cook's
#distance and having them reported
cooksD <- cooks.distance(covidexp7)
n <- nrow(covpeak)
highcooksD <- as.numeric(names(cooksD)[(cooksD >= (4/n))])
print(highcooksD)

#Again, it is worth considering the proportion of these cases in your data
describe(highcooksD)
56/1196
#4.7%

{r echo=FALSE}

# Model specification

# One way of checking whether the model has the right variables is to consider
# how it changes after adding each of the variables
avPlots(covidexp7)

Answer to Question 1

1a. In this plot, the residuals appear to follow a series of slanted, parallel lines, with the spread of residuals not remaining constant across the fitted values. This pattern suggests heteroskedasticity, meaning the variance of the residuals changes across different levels of fitted values. 
Specifically, as the fitted values increase, the residuals seem to show a slight funnel shape, which implies that the spread of residuals might be narrowing or widening systematically. 
This violation of homoskedasticity can affect the reliability of statistical tests and confidence intervals in a regression model, as it suggests that the residuals are not uniformly spread.

1b. In plot 1, each level of "covconc" (from 1 to 5) shows residuals scattered around the zero line, with a relatively even distribution. 
There is no discernible upward or downward trend in the residuals across different levels of covconc, suggesting that the relationship between covconc and the outcome variable does not deviate significantly from linearity. 
If a non-linear pattern were present, we might observe curved or systematic trends in the residuals across these levels. 
The residuals' approximate symmetry and the lack of systematic patterns here provide support for the assumption of linearity, indicating that a linear model is likely appropriate for describing the relationship between covconc and the outcome variable.

In plot 2, each level of "gov" (from 1 to 5) shows residuals spread around the zero line, with no evident trend or curve across these levels. 
This lack of pattern suggests that the relationship between "gov" and the outcome variable is linear, as there is no sign of non-linear behavior in the residuals. 
If the residuals showed a systematic pattern, such as a curved or sloped trend, it would indicate a possible non-linear relationship. 
Since the residuals here are symmetrically distributed around zero at each level and lack any clear structure, this provides evidence that the linearity assumption is satisfied for the variable "gov."

In plot 3, each level of "nhs" (from 1 to 5) shows residuals spread around the zero line, with no evident trend or curve across these levels. 
The lack of a curve pattern in the residuals across the plot suggests that the relationship between "nhs" and the outcome variable is linear, as there is no sign of non-linear behavior in the residuals. 
If the residuals showed a systematic pattern, such as a curved or sloped trend, it would indicate a possible non-linear relationship. 
Since the residuals here are symmetrically distributed around zero at each level and lack any clear structure, this indicates that the linearity assumption is satisfied for the variable "nhs."

1c. In this plot, most of the points lie close to this line, indicating that the residuals follows a normal distribution fairly well. 
Minor deviations at the ends (or "tails") suggest that the residuals may have some slight skew, but overall, the alignment along the diagonal indicates that normality is reasonably met. 
This implies that the residual variance is approximately normally distributed.

1d. In the plot, the residuals appear randomly scattered around zero with no obvious patterns, clusters, or trends, suggesting that the model’s assumptions of independence and constant variance are reasonably met. 
If multicollinearity were a significant issue, it would typically be diagnosed through other means, such as calculating the Variance Inflation Factor (VIF) or Condition Index. 
Hence, while this plot doesn’t directly confirm multicollinearity is absent, it suggests that there are no obvious residual-related issues that could indirectly hint at model misspecifications due to multicollinearity.

1e. In the plot, a few observations exceed this threshold, marked with their observation numbers, indicating that these points might have a strong influence on the regression results. 
However, most observations lie well below the threshold, suggesting that the model is generally robust to individual data points and is not unduly affected by outliers. 
Observations with high Cook’s D values provide further examination, as they might represent unusual or extreme cases that could skew the model’s estimates if left unchecked. 
Overall, this plot shows that, while there are some influential points, the majority of data points have minimal impact on the model, indicating that the model is not significantly thrown off by outliers.

1f. Homoscedasticity (Residuals vs Fitted Values plot) - The pattern of residuals along slanted, parallel lines with a funnel shape suggests heteroskedasticity, meaning the residual variance changes with the level of fitted values. 
This can lead to biased standard errors, affecting statistical tests and confidence intervals. To address heteroskedasticity, you could try a log transformation or other variance-stabilizing transformation on the dependent variable. 
Alternatively, using heteroskedasticity-robust standard errors (e.g., via White's correction) can make inference more reliable without altering the model structure.

Linearity for “covconc,” “gov,” and “nhs” (Residuals vs Fitted Values plot) - The residuals for each level of “covconc,” “gov,” and “nhs” appear symmetrically distributed around zero with no systematic trends, supporting the assumption of linearity. 
Since no apparent non-linear patterns are observed, the linear relationship assumption appears valid for these variables, and no corrective measures are necessary here.

Normality of Residuals (Q-Q plot) - The Q-Q plot indicates that the residuals are approximately normally distributed, with minor deviations at the tails. 
Although minor tail deviations suggest slight skewness, the overall alignment with the diagonal line implies that the assumption of normality is reasonably met. 
If normality were a concern, applying transformations such as a logarithmic or square-root transformation to the response variable could help. 
However, the minor deviations here are unlikely to pose substantial issues, particularly if the sample size is large, due to the robustness of regression methods to slight normality violations.

Independence and Multicollinearity (Residual plot): - The plot shows no discernible pattern, clusters, or trends in the residuals, which supports the assumption of independence. 
While this plot doesn’t directly confirm the absence of multicollinearity, the random distribution of residuals suggests no indirect evidence of model misspecification from collinear predictors. 
To formally assess multicollinearity, calculating Variance Inflation Factor (VIF) values for each predictor is recommended. 
A VIF above 5-10 would suggest multicollinearity, in which case you could consider removing or combining correlated predictors or using principal component analysis (PCA).

Influential Outliers (Cook’s D Chart) - The Cook’s D plot reveals a few observations with values exceeding the threshold, indicating potential outliers with strong influence on the model. 
Most points lie well below the threshold, suggesting overall model robustness. For influential points, it would be better to investigate each observation individually to understand if they represent valid extreme cases or data errors. 
Options to mitigate their influence include removing or down-weighting these points or applying robust regression methods that reduce the sensitivity to outliers.

Question 2

As the next step, carry out a similar analysis to what we did last week. 
This time, use confidence in the NHS’s handling of the pandemic as the outcome variable, and the confidence in the prime minister, government, and the police as the explanatory variables. 
What do you find? Were any of the assumptions violated? If they were, how would you address them?

{r echo=FALSE}

# New model 
nhspmgovpol <- lm(nhs~pm+gov+pol)
summary(nhspmgovpol)


# For an overview of multicollienarity, normality of residuals,
# and homoskedasticity run the following sjplot command (4 figures):
plot_model(nhspmgovpol, type = "diag")

{r echo=FALSE}

# Homoskedasticity

# Residual vs. fitted values plot
plot(nhspmgovpol$resid ~ nhspmgovpol$fitted.values)
abline(h = 0, lty = 2)

# Alternatively:
residualPlots(nhspmgovpol)

{r echo=FALSE}

# Linearity

# Residual vs. fitted values plot
residualPlots(nhspmgovpol)

# In addition to the fitted values, consider each of the explanatory variables
residualPlot(nhspmgovpol, variable = "gov")

# ResidualPlot(...)

{r echo=FALSE}

#3.Normality

#Q-Q plots
qqnorm(nhspmgovpol$residuals)
qqline(nhspmgovpol$residuals)

{r echo=FALSE}

# Multicollinearity

# Variance inflation function (VIF)
car::vif(nhspmgovpol) 

# Influential outliers

# Save the standardised residuals
resstd.2 <- rstandard(nhspmgovpol) 

# Plot the standardized residuals
plot(resstd.2, ylab="Standardised Residual", ylim=c(-3.5,3.5))
abline(h =c(-3,-2.5,0,2.5,3), lty = 2)

# 3 standard deviation cut-off
index3 <- which(resstd.2 > 3 | resstd.2 < -3)
index3
describe(index3)

# 13 cases
summary(nhspmgovpol)
13/1196
# Around 1.1%

# 2.5 standard deviation cut-off
index2.5 <- which(resstd.2 > 2.5 | resstd.2 < -2.5)
index2.5
describe(index2.5)

# 23 cases
23/1196
# Around 1.9%

# Plotting standardised residuals
ols_plot_resid_stand(nhspmgovpol)

{r echo=FALSE}

# Identifying outliers using Cook's D
influencePlot(nhspmgovpol, main="Influence Plot", 
              sub="Circle size is proportional to Cook's Distance" )
infIndexPlot(nhspmgovpol)

# Thershold for Cook's D
4/1196

# Plotting Cook's D
ols_plot_cooksd_chart(nhspmgovpol)

# Have Cook's Ds reported
n <- nrow(covpeak)
highcooksD <- as.numeric(names(cooksD)[(cooksD >= (4/n))])
print(highcooksD)
describe(highcooksD)
61/1196
# 5.1%

{r echo=FALSE}

# Model specification
avPlots(nhspmgovpol)

Answer to Question 2

Homoskedasticity: The plot of residuals against fitted values shows a slight funnel shape, where residuals are more spread out at lower fitted values and narrow somewhat as fitted values increase. 
This pattern suggests heteroskedasticity, where the variance of the residuals is not constant across all levels of the fitted values. 
Heteroskedasticity can lead to biased standard errors, which impacts the validity of confidence intervals and hypothesis tests. 
To address this, heteroskedasticity-robust standard errors could be used, which adjusts the standard errors to account for the non-constant variance. 
Alternatively, transforming the dependent variable (e.g., using a log or square root transformation) or adding/modifying predictors might help stabilize the variance of the residuals.

Linearity: The residual plots for individual predictors (e.g., pm, gov, and pol) suggest that the relationship between each predictor and the outcome variable is approximately linear, as residuals are evenly distributed around zero. 
However, the residuals versus fitted values plot shows a slight downward trend, which may indicate some non-linearity in the model. This suggests the model may not perfectly capture the true relationship. 
To address this, adding polynomial or interaction terms to the model would allow for a more flexible fit or considering a non-linear transformation of the predictors could help as well.

Normality: The Q-Q plot shows that the residuals follow the expected normal distribution reasonably well in the middle range but deviate at the tails, suggesting potential skewness or heavy tails. 
While minor deviations from normality are often not problematic in large samples, if the precise inference is critical, transforming the outcome variable could be used to improve normality. 
Another approach could involve using non-parametric methods or bootstrapping techniques, which do not rely on the normality assumption.

Multicollinearity: The Variance Inflation Factor (VIF) values for pm, gov, and pol are 3.34, 3.08, and 1.23, respectively. 
These values are well below the common threshold of 5 (or 10), indicating that multicollinearity is not a significant issue in your model. 
This means each predictor contributes unique information without excessive correlation with other predictors, so no specific addresses are required for multicollinearity.

Influential Outliers: The standardized residuals plot shows that most residuals lie within the ±2 or ±3 standard deviation range, with only a few points outside this range. 
This suggests that most data points do not exert undue influence on the model. Observations with residuals outside the ±3 range could be considered influential points, which may affect the model’s estimates. 
If any of these points have a high Cook’s distance, they could be further investigated to determine if they represent legitimate but extreme values or potential data errors. 
Removing or transforming these points might improve model stability if they unduly influence the regression estimates.

{r echo=FALSE}

# To empty the R environment
rm(list=ls())
