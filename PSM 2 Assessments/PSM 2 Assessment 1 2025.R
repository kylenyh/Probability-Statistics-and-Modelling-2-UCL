UCL PSM 2 Take Home Assessment 1 2025

{r include=FALSE}

# Load packages

library(haven)
library(psych)
library(sjmisc)
library(sjPlot)
library(PerformanceAnalytics)
library(car)
library(olsrr)

{r echo=FALSE, message=FALSE}

# Load the dataset 
PSM_data <- haven::read_dta("ESS9_THE.dta")

# Browse variable names
names(PSM_data)

# View top and bottom of the dataset
head(PSM_data)
tail(PSM_data)

# Convert to factor variable
ancestf <- to_factor(PSM_data$ancest)

{r echo=FALSE, message=FALSE}

# Multiple linear regression model
model <- lm(trstlgl ~ ancestf + atcherp + crmvct + gndr + agea, data = PSM_data)
 

# Model summary (blm)
summary(model)

# Plot model
plot_model(model, type = "diag")

{r echo=FALSE, message=FALSE}

# Q-Q plot for checking normality of residuals
# Helps to assess whether residuals follow a normal distribution.
qqnorm(model$residuals)
qqline(model$residuals)

# Residual plots for detecting non-linearity and unequal variance
# This will generate residuals vs fitted plots for each predictor.
residualPlots(model)

# (Optional) Specific residual plots for chosen variables – fill in variable names
residualPlot(model, variable = "atcherp")
residualPlot(model, variable = "agea")

# Check for multicollinearity among predictors using Variance Inflation Factors (VIF)
# VIF > 5–10 may suggest multicollinearity problems.
car::vif(model)

# Compute standardized residuals and plot them
# Points beyond ±2.5 or ±3 are potential outliers.
resstd <- rstandard(model)
plot(resstd, ylab = "Standardised Residual", ylim = c(-3.5, 3.5))
abline(h = c(-3, -2.5, 0, 2.5, 3), lty = 2)

# Identify extreme residuals beyond ±3
index3 <- which(resstd > 3 | resstd < -3)
index3
describe(index3)

# Identify moderate outliers beyond ±2.5
index2.5 <- which(resstd > 2.5 | resstd < -2.5)
index2.5
describe(index2.5)

# Added-variable plots (partial regression plots)
# Helps to assess the unique contribution of each predictor while controlling for others.
avPlots(model)
