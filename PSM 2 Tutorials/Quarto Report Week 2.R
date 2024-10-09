UCL Probability, Statistics and Modelling 2 Week 2

Question 1

Carry out all the preliminary steps: set your working directory, load in the packages and the 
data, and attach the data so you would not need to identify the dataset for the subsequent 
commands. If this is your first seminar, please install the packages that we used last week (they 
are all mentioned in the script).

{r include=FALSE}

# Load packages
library(haven)
library(questionr)
library(sjmisc)
library(sjlabelled)
library(sjPlot)
library(dplyr)
library(tidyverse)
library(Hmisc)
library(PerformanceAnalytics)
library(psych)
library(ggplot2)

{r echo=FALSE}

# Set working directory
setwd("C:/Users/user/OneDrive/Desktop/UCL_PSM_2_R/Week_2")

# Reading the data
covpeak <- read_dta("Covid_19_PSMII.dta")

# Browsing through names in the dataset 
names(covpeak)

# Returns top and bottom of data
head(covpeak)
tail(covpeak)

# Opens data in a separate window
View(covpeak)

# If you are only working on a single dataset, it might make sense to use the "attach"
# command, which will allow you not to mention the name of your data for several commands
# (it saves time and typing)
attach(covpeak)

Question 2

Today, we will have two outcome variables: concern about catching Covid-19 and  expectations regarding the length of the pandemic. Look at the descriptive statistics. 
What are your thoughts looking back at the results three and a half years after the start of the pandemic?

{r echo=FALSE}

# Gives descriptive statistics of the expected length of the pandemic and the concern regarding the pandemic

summary(covidexp)
frq(covidexp, out="v")
plot_frq(covidexp)

summary(covconc)
frq(covconc, out="v")
plot_frq(covconc)

Answer to Question 2

The table results show that the highest number of people had concerns about the pandemic lasting between 3 and 6 months and the lowest number had concerns about the pandemic lasting less than a month. 
Looking at the results of Cramer's and Fisher's p value, it can be concluded that there is a statistically significant association between the two categorical variables because the p value 
is lower than the significance level. However, Cramer's value of 0.12 suggests a weak association between the two categorical variables.

Question 3

Creating crosstabs and running correlation analysis, consider the association between concern 
about catching Covid-19 and expectations regarding the length of the pandemic. What is your takeaway?

{r echo=FALSE}

# Crosstabulation without percentages
sjt.xtab(covidexp, covconc)

# Crosstabulation with row percentages
sjt.xtab(covidexp, covconc, show.row.prc = TRUE)

{r echo=FALSE}

# Performs Pearson correlation test
cor_test_result <- cor.test(covidexp, covconc, method = "pearson", use = "complete.obs")

# Show correlation test result
cor_test_result

Answer to Question 3

Based on the Chi-Square test that was being run between concern about catching Covid-19 and expectations regarding the length of the pandemic, the p value returned based on the test run was 0.001, 
which indicates that there is a statistically significant association between the 2 variables due to the rejection of the null hypothesis. In the context of Pearson'c correlation test for the 
statistical analysis between the variables, the Pearson's product-moment correlation coefficient given was 0.169167 and the p value shown was 3.712 x 10^-9, suggesting a weak statistical correlation 
between the 2 variables.

Question 4

Let’s consider the association between the two variables, now with linear regression analysis, using them as explanatory and outcome variables one at a time. Please answer each of the questions below:

a) What is the association between these two variables?

b) To what extent are these models different/similar

c) What is the relationship between the R^2 statistics and the coefficient coefficient 

d) How would you interpret the results 

{r echo=FALSE}

# Fit simple linear regression model
model_1 <- lm(covconc ~ covidexp, data = covpeak)

# Print model summary
summary(model_1)

# Fit simple linear regression model
model_2 <- lm(covidexp ~ covconc, data = covpeak)

# Print model summary
summary(model_2)

Answer to Question 4a

After running a statistical linear regression analysis on both models, I can conclude that for the first model there is a statistically significant association between covidexp and covconc, due to the p value of the 
dependent variable being 3.71 x 10^-9. Similarly for the second model, I made the same on the second model saying that there is a statistically significant association between covconc and covidexp based on the exact given p value.

Answer to Question 4b

Both models in general share a lot of similarities, however, I would like to point out the differences that I have noticed in the summary results of both models. The first difference that I noticed was the categorization of 
the residuals of the error function based on the linear regression analysis between covidexp and covconc. The second difference that I would like to also point out is the value of the intercepts and the explanatory variables. 
For instance, in model 1, the analysis between covconc and covidexp gave an intercept value of 2.88704 and an explanatory variable value of 0.14623. 
In contrast, for model 2, the analysis between covidexp and covconc gave an intercept value of 3.81555 and an explanatory variable value of 0.19570. 
Finally, I want to mention the residual squared error values across both models. Since model 1 has a smaller RSE value, this suggests that the model's predictions are closer to the actual values. 
In contrast, model 2 has a larger RSE value, indicating that the model's predictions are farther from the actual values.

Answer to Question 4c

To examine the relationship between the R2 statistics and the correlation coefficient, I have looked at the summary statistics of both models and more specifically the multiple R-squared values and the adjusted R-squared values for each. 
Based on my observation of the summary results for both models, I can conclude that both models explain a 3% variability of the respective explanatory/independent variable depending on the given outcome/dependent variable. 
Speaking about the correlation coefficient, square rooting the R^2 value of both models gives a correlation coefficient value of 0.169167, leading to the conclusion of a weak statistical association.

Answer to Question 4d

The multiple R squared value of model 1 indicates that the independent variable can explain the 3% variance in the dependent variable. 
With the p value, since it falls lower than the significance level, this means that there is a significantly statistical relationship between the variables in model 1. 
Similarly, since model 2 contains the same multiple R squared value and the same p value, this means the same thing as my interpretation for model 1. 
This should not come as any surprise due to the same variables that are being used for both models, but just the interchangeability between them for linear regression and statistical interpretation purposes.

Question 5

Finally, plot the two simple linear regressions discussed today. How do the figures correspond 
to the outputs?

{r echo=FALSE}

regressplot <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "blue") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

# First model
regressplot(lm(covconc ~ covidexp))
# Compare the model to the output of the first model
summary(model_1)

# Second model
regressplot(lm(covidexp ~ covconc))
# Compare the model to the output of the first model
summary(model_2)

Answer to Question 5

For both models, the slope and intercept values are reflected in the linear regression plots. For instance, the coefficient value of the independent variable in model 1 was 0.14623, whereas the coefficient value 
of the independent variable in model 2 was 0.19570, suggesting not much difference between the dependent and independent variable. Likewise, the intercept values of both models can be referred to in the summary statistics 
and observed in the regression plots. Given the intercept value of around 2.88 in model 1 and around 3.81 in model 2, this indicates two distinct base intercept values of the regression equation assuming that the other variable is 0. 
Furthermore, due to the same multiple R squared value and p value provided across both models, there are no different conclusions based on the statistical relationships that can be made for each model. 
However, looking at the p value of both models, although it indicates a statistically significant relationship between the variables, it shows a weak relationship between the variables based on the regression fit in the graphs. 
Speaking about the adjusted R squared value, although the value can be used to explain how the independent variable can be taken into for the 3% variance in the dependent variable, this means that the models do not give much explanation
in the variability of the data and this is explicitly shown in the graphs where the points are shown to be spread quite far apart from the regression line.

{r echo=FALSE}

# To empty the R environment
rm(list=ls())

