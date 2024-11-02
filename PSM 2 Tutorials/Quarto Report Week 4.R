
UCL Probability, Statistics and Modelling 2 Week 4

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
library(texreg)
library(reghelper)

{r echo=FALSE}

# Set working directory
setwd("C:/Users/user/OneDrive/Desktop/UCL_PSM_2_R/Week_3")

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

We begin the tutorial by revisiting the models from last week. Compare these models to each other. Which (partial) associations are significant, and which are not? How would you interpret these coefficients? 
How do these models change from model 1 to model 5? What do these changes indicate? If you were helping the government’s behavioural science unit, what would be your advice?

{r echo=FALSE}

# The five linear regression models 
covidexp1 <- lm(covidexp ~ covconc)
covidexp2 <- lm(covidexp ~ covconc + gov + nhs)
covidexp3 <- lm(covidexp ~ covconc + gov + nhs + keywork)
covidexp4 <- lm(covidexp ~ covconc + gov + nhs + keywork + age2 + age3 + age4)
covidexp5 <- lm(covidexp ~ covconc + gov + nhs + keywork + age1 + age2 + age3)

# A nicer way to present the results and compare the models

# Juxtapose all the models with standard errors
screenreg(list(covidexp1, covidexp2, covidexp3, covidexp4, covidexp5),digits=3)

# Juxtapose all the models with confidence intervals
screenreg(list(covidexp1, covidexp2, covidexp3, covidexp4, covidexp5),digits=3, ci.force=TRUE)

Answer to Question 1

Model Comparisons

Model 1 - In Model 1, covconc (public concerns about COVID-19) has a 95% confidence interval of [0.131; 0.260]. Since this interval does not include zero, we conclude that covconc is statistically significant and positively 
associated with the outcome. This implies that higher public concern is likely associated with increased expectations related to the duration of COVID-19. Furthermore, the adjusted R^2 for model 1 is low at 0.028, 
which suggests that the model explains about 2.8% of the variance in the outcome variable.

Model 2 - In Model 2, the gov (trust in government) variable has a 95% confidence interval of [-0.182; -0.072], which does not include zero, indicating a statistically significant association. 
This negative interval suggests that increased trust in government is likely linked with lower expectations about the duration of COVID-19. In contrast, nhs (trust in NHS) has a confidence interval of [-0.046; 0.123], 
which includes zero, indicating that it is not statistically significant. The adjusted R^2 increases to 0.042 in this model, suggesting that the inclusion of gov marginally improves the model's explanatory power, 
now accounting for about 4.2% of the variance in the outcome.

Model 3 - In Model 3, keywork1 (keyworker status) has a confidence interval of [-0.088; 0.264], which includes zero, indicating that this variable is not statistically significant. 
This suggests that keyworker status does not meaningfully contribute to explaining variance in COVID-19 duration expectations in this model. The adjusted R^2 remains roughly the same at 0.042, 
showing no improvement in explanatory power with the addition of keywork1.

Model 4 - In Model 4, age2 and age3 (two of the age group indicators) show statistically significant associations, with confidence intervals of [0.055; 0.398] and [0.366; 0.806], respectively. 
These intervals indicate positive associations with the outcome, suggesting that individuals in these age groups perceive higher expectations about the duration of COVID-19. In contrast, age4 has a confidence interval 
of [-0.030; 1.019], which includes zero, indicating a lack of statistical significance. With an adjusted R^2 of 0.062, Model 4 explains approximately 6.2% of the variance, showing an improvement over previous models 
by accounting for differences in age groups.

Model 5 - In Model 5, age1 replaces age4 as the reference category. The confidence interval for age1 is [-1.019; 0.030], which includes zero, indicating that it is not statistically significant. 
Similarly, age2 and age3 are not statistically significant either in this model, with confidence intervals of [-0.779; 0.243] and [-0.436; 0.618], respectively. The adjusted R^2 remains unchanged at 0.062, 
indicating that the model does not explain more variance with age1 as the reference category.

Recommendations to the Government’s Behavioral Science Unit

Target Communication Strategies: The consistent significance of covconc suggests that public concern is an important predictor. Therefore, communication strategies should maintain a focus on 
increasing awareness of COVID-19 risks, especially among groups with lower levels of concern.

Address Trust in Government: The negative association between gov and the outcome variable in Model 2 implies that increasing public trust in government measures 
could positively influence compliance and possibly reduce perceived risk.

Consider Age Differences: The statistical significance of certain age groups (e.g., age2 and age3 in Model 4) suggests that age-tailored messaging might be effective. 
Younger individuals tend to feel less concerned or at risk, so targeted messaging to these groups could be beneficial.

Explore Other Predictors: Since these models collectively explain only a small proportion of the variance in COVID-19 expectations (low adjusted R^2 values across all models), 
other factors should be investigated. These could include media exposure, health literacy, or peer influences, which might better capture variance in public expectations regarding COVID-19.

Question 2

Now the behavioural science unit wants to understand which variables have the strongest association with the outcome variable (relative to each other). 
To answer this, estimate the standardised coefficients first, by revisiting the two different solutions for model 1 (from tutorial 2), and then by deriving the standardised coefficients for model 5. 
How would you interpret the results? What would you tell the behavioural science unit based on these results?

{r echo=FALSE}

# Simple linear regression for the expected length of the pandemic
summary(covidexp1)

# Simple linear regression for concern regarding catching COVID-19
covconc1 <- lm(covconc ~ covidexp)
summary(covconc1)

# Fitting the standardised models for each
scovidexp1 <- beta(covidexp1)
scovconc1 <- beta(covconc1)

# Print the results with the standardised beta coefficients
print(scovidexp1)
print(scovconc1)

# Standardise the variables
scovidexp <- (covidexp - mean(covidexp))/sd(covidexp)
scovconc <- (covconc - mean(covconc))/sd(covconc)

# Fit the linear regression to these standardised variables
st.covidexp1 <- lm(scovidexp ~ scovconc)
st.covconc1 <- lm(scovconc ~ scovidexp)
summary(st.covidexp1)
summary(st.covconc1)
print(scovidexp1)
print(scovconc1)

{r echo=FALSE}

# Use the standard deviation of the explanatory and outcome variable
ubeta1 <- summary(covconc1)$coef[2, 1]
sdy1 <- apply(covconc1$model[1], 2, sd)
sdx1 <- apply(covconc1$model[2], 2, sd)
sbeta1 <- ubeta1 * (sdx1/sdy1)
sbeta1

ubeta2 <- summary(covidexp1)$coef[2, 1]
sdy2 <- apply(covidexp1$model[1], 2, sd)
sdx2 <- apply(covidexp1$model[2], 2, sd)
sbeta2 <- ubeta2 * (sdx2/sdy2)
sbeta2

# Model fitted for expectations regarding the length of the pandemic
summary(covidexp5)

# The standardised effects
scovidexp5 <- beta(covidexp5)
print(scovidexp5)

Answer to Question 2

Based on the newly standardized results in model 5, many of the explanatory variables in the model appear to be non statistically significant due to their high p values. In contrast, 
two of the explanatory variables are statistically significant due to their low p values being lesser than the significance level (0.05). These variables are covconc (public concerns about COVID-19) 
and gov (trust in government). To statistically interpret the standardized point estimate for the covconc variable, a one standard deviation increase in public concerns about COVID-19 is associated 
with a 0.1350 increase in the public expectations of the length of COVID-19, all else being equal. Since the p value for this variable is lower than the significance level (0.05), this makes it 
statistically positive significant meaning that an increase about public concerns about COVID-19, would lead to an increase in public expectations on the length of COVID-19. To statistically intepret 
the standardized point estimate for the gov variable, a one standard deviation increase in public concerns about COVID-19 is associated with a 0.1427 decrease in the public expectations of the length 
of COVID-19, all else being equal. Since the p value for this variable is lower than the significance level (0.05), this makes it statistically negative significant meaning that an increase about public 
concerns about COVID-19, would lead to a decrease in public expectations on the length of COVID-19. Based on the adjusted R^2 value of the model (0.05246), this suggests that the model explains about 6.2% 
of the variance in the outcome variable. Looking at the F statistic of the model, since its p value is lesser than the significance level (0.05), this indicates that the model fits the data better than an empty model.

Recommendations to the Government’s Behavioral Science Unit

Consistent public concern about COVID-19- Since COVID-19 concern is a significant predictor variable across the model, public health related campaigns should continue to emphasize the severity of 
the pandemic and its potential risks. This could ensure more people to comply with public health measures.

Improving trust in government - The negative relationship between trust in the government and the the expected length of COVID-19 suggests that low government trust may lead to lower compliance. 
Therefore, it would be a good idea to increase transparency, improve communication and trust efforts.

Focusing on younger age groups - Given group age1 being the youngest age group and reference group for comparisons between the other older age groups, it would be a good idea for the government 
to tailor their policies to ensure that the younger population adhere to public health safety measure guidelines, since the younger population appears to be less concerned about COVID-19 in comparison to the older population.

Exploring other key factors - Given the low adjusted R^2 value in the model, it might be better to explore other additional factors (e.g. socioeconomic status, access to education, 
access to healthcare) to explore its impact on the variance with respect to the outcome variable in the model, which can be used to guide future behavioural science units.

Question 3

The behavioural science unit believes that women key workers were more likely to expect that the pandemic would last longer. Similarly, they also hypothesised that the association between concern 
about contracting the virus and the increased expectations regarding the length of the pandemic depended on one’s gender identity. Test these two hypotheses using moderation analysis. 
How would you interpret the results? What do the figures tell you?

{r echo=FALSE}

# Consider the 'keywork' and 'female' variables categorical
keywork <- to_factor(keywork)
female <- to_factor(female)

# An interaction between being a key worker and being female
# You can define an interaction between two variables by using the "*" symbol
# The "*" symbol will add the estimates for both the main and the interaction effects to the model
covidexp6 <- lm(covidexp ~ covconc + keywork * female + gov + nhs +
                  age1 + age2 + age3)

summary(covidexp6)

# The emmip command helps you with the graphical representation of the interaction
emmip(covidexp6, female ~ keywork, CIs=TRUE)

# Alternatively, you can use the sjmisc package and the "plot_model" command
plot_model(covidexp6, type = "int")

# Interaction between being female and being concerned about contracting the virus
covidexp7 <- lm(covidexp ~ keywork * female + female * covconc + gov + nhs +
                  age1 + age2 + age3)

summary(covidexp7)

{r echo=FALSE}

# Plot the interactions
emmip(covidexp7, female ~ keywork, CIs=TRUE)

# Alternatively, you can use the sjmisc package and the "plot_model" command
plot_model(covidexp7, type = "pred", terms = c("female", "keywork"))

# Notice, that in case of non-binary variables the two graphs differ
# The first one creates an average interaction effect, the second
# Plots the interaction effect for each level of the concern about Covid-19
emmip(covidexp7, female  ~ covconc, CIs=TRUE)
plot_model(covidexp7, type = "pred", terms = c("female", "covconc"))

Answer to Question 3

In the first model, the interaction that is being examined is between the key worker variable and the female gender variable on pandemic duration expectations. 
Based on the model summary, the keywork1 variable itself has a point estimate of approximately 0.08180 and a p value of 0.5768, suggesting that the effect of being a key worker 
during the pandemic duration expectations is not statistically significant. To statistically interpret the point estimate for the keywork1 variable, a one unit increase in key workers 
is associated with a 0.08180 increase in the public expectations of the length of COVID-19, assuming that all the other independent variables are equal to 0. Additionally, the female1 variable itself 
has a point estimate of approximately 0.05931 and a p value of 0.4451, suggesting that the effect of being a female during the pandemic duration expectations is statistically significant. 
To statistically interpret the point estimate for the female1 variable, a one unit increase in females is associated with a 0.05931 increase in the public expectations of the length of COVID-19, 
assuming that all the other independent variables are equal to 0. Furthermore, the intercation term between the two variables has a point estimate of -0.00845 and a p value of 0.9633, which means that 
the effect of being a female key worker during the pandemic duration expectations is not statistically significant. To statistically interpret the point estimate for the interaction variable, a one unit 
increase in female key workers is associated with a 0.00845 decrease in the public expectations of the length of COVID-19, assuming that all the other independent variables are equal to 0. 
Based on the adjusted R^2 value of 0.06141 for the first model, this means that the model explains about 6.1% of the variance in the outcome variable. Looking at the F statistic of the first model, 
since its p value is lesser than the significance level (0.05), this indicates that the model fits the data better than an empty model.

In the second model, the interaction that is being examined is between the concern about contracting COVID-19 variable and the female gender variable. Based on the model summary, 
the covconc variable itself has a point estimate of approximately 0.14723 and a p value of 0.0050, suggesting that the effect of being concerned about contracting COVID-19 during the 
pandemic duration expectations is not statistically significant. To statistically interpret the point estimate for the covconc variable, a one unit increase in concern about contracting COVID-19 
is associated with a 0.14723 increase in the public expectations of the length of COVID-19, assuming that all the other independent variables are equal to 0. Additionally, the female1 variable 
itself has a point estimate of approximately 0.02297 and a p value of 0.9250, suggesting that the effect of being a female during the pandemic duration expectations is not statistically significant. 
To statistically interpret the point estimate for the female1 variable, a one unit increase in females is associated with a 0.02297 increase in the public expectations of the length of COVID-19, 
assuming that all the other independent variables are equal to 0. Furthermore, the interaction term between the two variables has a point estimate of 0.01050 and a p value of 0.87520, 
which means that the effect of being concerned about COVID-19 as a female within the pandemic duration expectations is not statistically significant. 
To statistically interpret the point estimate for the interaction variable, a one unit increase in females is associated with a 0.01050 increase in the public expectations 
of the length of COVID-19, assuming that all the other independent variables are equal to 0. Based on the adjusted R^2 value of 0.06064 for the second model, 
this means that the model explains about 6.06% of the variance in the outcome variable. Looking at the F statistic of the first model, since its p value is lesser than 
the significance level (0.05), this indicates that the model fits the data better than an empty model.

Following the results of both models, the interaction plots provide insights into the interaction between two categorical variables across each model. In summary, for both interaction plots, 
while the data suggests a slight increase in pandemic duration expectations for female key workers compared to male key workers, the interaction effect between gender and key worker status 
does not appear to be strong. The gender effect on pandemic duration expectations is fairly consistent across both key workers and non-key workers.

Question 4

The behavioural science unit asks you to consider the fit of each of the models. Estimate all of the relevant model fit criteria. Purely based on these, which model is preferable and why? 
Why do simpler (i.e. more parsimonious) models have better fit?

{r echo=FALSE}


# Fitting all of the previous models in the same table
screenreg(list(covidexp1, covidexp2, covidexp3, covidexp4, covidexp5, covidexp6, covidexp7), digits=3)


# The penalised model selection criteria for the same models
AIC(covidexp1,covidexp2,covidexp3, covidexp4, covidexp5, covidexp6, covidexp7)
BIC(covidexp1,covidexp2,covidexp3, covidexp4, covidexp5, covidexp6, covidexp7)

Based on the model fit criteria, Model 4 appears to be the most preferable. The model summary indictates an R^2 value of 0.068 and an adjusted R^2 value of 0.062, which are the highest 
among the models without interaction terms. Introducing further complexity in Models 5, 6, and 7 by adding interaction terms (such as keywork: female and female: covconc) does not improve 
the R^2 value or adjusted R^2 value, which remain at the same levels as Model 4. This suggests that the additional terms do not contribute meaningful explanatory power and may introduce 
unnecessary complexity. Simpler, more parsimonious models like Model 4 are generally preferred because they capture the essential relationships in the data without overfitting,
making them more likely to generalize well to new data.

Question 5

To what extent should you make a decision based on model fit statistics? What can be the advantages and the disadvantages of maximising model fit? 
From the seven models fitted, which one would you pick and why?

Answer to Question 5

When evaluating models, relying solely on model fit statistics like the R^2 value and adjusted R^2 value can be limiting, as these metrics only capture the proportion of variance 
explained by the model but do not necessarily reflect predictive accuracy or model generalizability. While model fit statistics offer useful insights into the explanatory power of a model, 
they should not be the sole basis for decision-making. It's essential also to consider the model's purpose, interpretability, simplicity, and potential for overfitting. 
A higher R^2 value might indicate a better fit. Still, it could also mean that the model is overly complex, capturing noise in the data rather than underlying patterns, 
which can reduce its predictive power on new data.

Maximizing model fit can have advantages and disadvantages. On the positive side, higher model fit often means that the model captures more information from the data, potentially improving explanatory power. 
This can be particularly valuable when the goal is to understand complex relationships between variables. However, a major disadvantage of maximizing model fit is the risk of overfitting, where the model becomes 
too tailored to the specific dataset and performs poorly on new, unseen data. Furthermore, overly complex models with too many predictors or interaction terms can become difficult to interpret, 
reducing their practical value in decision-making contexts.

I would select Model 4 from the seven models fitted. This is because the model has the highest R^2 value and adjusted R^2 value among the simpler models without interaction terms, indicating it captures 
meaningful relationships in the data without unnecessary complexity. Adding more interaction terms in Models 5, 6, and 7 does not improve model fit, suggesting these terms may not provide additional explanatory 
power and could increase the risk of overfitting. Therefore, model 4 represents a good balance between explanatory power, simplicity and interpretability, making it a robust and generalizable choice based on 
the available model fit statistics and the need to avoid overfitting.

Question 6

Let’s consider a few alternative models with the variables used in the first week: confidence in the various institutions’ handling of the pandemic. Using the confidence in the prime minister’s handling of 
the pandemic as the outcome variable and the confidence in the NHS, the government, and the police as explanatory variables, how do each of the models fare? Which model has the best fit? 
Which model would you consider the best? Why?

Answer to Question 6

Model 3, which includes confidence in the NHS, government, and police as predictors, demonstrates the best overall fit for explaining confidence in the prime minister's handling of the pandemic, 
given its higher adjusted R^2 value compared to other models. This suggests that considering all three institutional confidence variables provides a fuller understanding of the factors influencing 
public trust in the prime minister’s response to COVID-19. However, if the added predictive power in Model 3 is marginal over Model 2, which includes only confidence in the NHS and government, 
Model 2 may be preferred for its simpler interpretation while still offering substantial explanatory power. Thus, while Model 3 maximizes fit, Model 2 could be selected 
  for practical applications where simplicity and interpretability are prioritized alongside predictive accuracy.
