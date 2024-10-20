UCL Probability, Statistics and Modelling 2 Week 3

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

The government’s behavioural science unit is interested in what shaped the expectations of Britons during the first peak of the pandemic and they asked you to investigate. 
First, they want you to revisit the model you fitted last week, with concern regarding catching the virus as the explanatory variable. Remind yourself of the results of this model.

{r echo=FALSE}

# linear regression model
covidexp1 <- lm(covidexp~covconc)
# Summary of model output 
summary(covidexp1)

Answer to Question 1

Based on the summary results, the point estimate value for the covconc variable is approximately 0.196, which indicates a weak positive relationship with covidexp. 
This suggests that as the public concern of catching COVID increases, expectations related to the pandemic will also increase, but at a small rate. 
Since the p value shown is less than the significance level (0.05), this confirms that the association between the two variables is statistically significant, leading to the conclusion 
that there is a statistical association between the two variables, but a weak one. However, while the relationship is statistically significant, the weak point estimate suggests that changes 
in covconc will not have much impact on covidexp.

Question 2

The behavioural science unit wants you to give them a sense regarding the uncertainty surrounding your estimates. 
To answer them, derive the confidence intervals and interpret the results. How did you calculate these?

{r echo=FALSE}

# 95% condidence interval 
confint(covidexp1)

# Deriving the 99% confidence intervals
confint(covidexp1, level = 0.99)

# Deriving the standard errors 
secovidexp1 <- sqrt(diag(vcov(covidexp1)))

secovidexp1

# SE for covonc in the covidexp1 model: 0.03294

# Deriving only the coefficients
coef(covidexp1)

{r echo=FALSE}

#95% confidence intervals:
0.1957+(1.96*0.03294)
0.1957-(1.96*0.03294)

#95% confidence intervals:
3.81555+(1.96*0.12157)
3.81555-(1.96*0.12157)

#99% confidence intervals
0.1957+(2.58*0.03294)
0.1957-(2.58*0.03294)

#99% confidence intervals
3.81555+(2.58*0.12157)
3.81555-(2.58*0.12157)

Answer to Question 2

The confidence intervals provide valuable insight into the precision of the estimates. For instance, the relatively narrow confidence intervals around the coefficients 0.1957 and 3.81555 suggest that the estimates are precise, 
with a relatively low level of uncertainty. However, as the confidence level increases from 95% to 99%, the intervals become wider, reflecting increased certainty that the true value lies within the interval but also accounting
for a greater range of possible values. The standard errors for both coefficients (B0 and B1) are small, indicating that the estimates are relatively stable and reliable.

Question 3

In addition to concern regarding the virus, the behavioural science unit are curious whether confidence in the government and the NHS were associated with people’s expectations regarding the length of the pandemic. 
Fit this model and answer the following questions:

a. How would you interpret each of the coefficients in the model?

b. What do the confidence intervals tell you and how do they correspond to the results?

c. To what extent are the results different/similar compared to the earlier model? Why might this be the case?

d. What would you tell the behavioural science unit (i.e. your substantive interpretation)?

{r echo=FALSE}

# Multiple linear regression model 
covidexp2 <- lm(covidexp~covconc+gov+nhs)

# Summary results of regression model 
summary(covidexp2)

# 95% confidence interval of regression model 
confint(covidexp2)

# Simple linear regression model 

coef(covidexp1)

# 95% confidence interval of regression model 
confint(covidexp1)

#Multiple linear regression:
coef(covidexp2)

# 95% confidence interval of regression model 
confint(covidexp2)


# Run a correlation analysis to further investigate
describe(covpeak)
corr <- covpeak[, c(3,6,9,10)]
chart.Correlation(corr, histogram=TRUE, pch=19)

Answer to Question 3a

The output results of the multiple linear regression model present the relationship between covidexp (dependent variable) and covconc, gov and nhs (independent variables). 
The intercept value of 3.97773 indicates the expected value of covidexp when all the independent variables are equal to 0. 
For every unit increase in covconc, the model predicts a 0.19227 increase in covidexp, indicating a weak statistically positive relationship, which can be confirmed since 
the p value of the variable is lower than the significance level (0.05). The variable gov has a negative coefficient value of -0.12686 suggesting that for every unit increase in gov, 
the model predicts a decrease in covidexp, which shows a weak statistically negative relationship due to the acceptance of the alternative hypothesis based on the low p value. 
Additionally, although the nhs variable has a coeffcient value of 0.03846, the p value of the variable is larger than the significance level (0.05), indicating that there is not enough 
evidence to conclude that there is a statistical relationship between nhs and covidexp.

Answer to Question 3b

The confidence intervals provide insights into the estimated range of values following the 95% confidence level. For the intercept, the estimated range of values lies between 3.54932058 and 4.40614427, 
indicating strong statistical significance and confidence that the covidexp value lies within this range assuming that all the other independent variables are equal to 0. 
For covconc, the estimated range of values lies between 0.12806705 and 0.25647750, which suggests high confidence that there is a statistically postive relationship between COVID-19 concern and COVID-19 exposure.
For gov, the confidence interval ranges from -0.1822 to -0.0715, however, since the confidence interval for nhs ranges from -0.0463 to 0.1233, this indicates that the effect of nhs perception on COVID-19 exposure 
is not statistically significant, as it crosses zero. This observation comes from the fact that the p-value for this variable exceeds the significance level (0.05), whilst both covconc and gov have p values 
that are lower than the significance level (0.05).

Answer to Question 3c

Comparing the model's current results to the previous model's results, the direction and significance of the coefficients appear to be the same. For instance, the independent variables covonc and gov 
continue to show a significant impact on covidexp, with similar coefficent values and confidence intervals. In contrast, the nhs variable remains insignificant across both models due to its confidence interval crossing 0. 
Slight differences in the point estimates may result from rounding or minor changes in the data, but on the whole, these have not impacted the results across both models.

Answer to Question 3d

For the behavioural science unit, the key takeaway following the results of the multiple linear regression model is that public concern about COVID-19 and trust in the government to address COVID-19 issues do 
have a significant impact on the public's perceptions of COVID-19. To explain further, higher concerns about getting COVID-19 were associated with higher public perceptions of getting increased exposure to COVID-19, 
while greater public trust in the government was associated with lower public perceptions of getting increased exposure to COVID-19. In contrast, since the perception of the NHS did not have any significant impact 
on people’s perceived exposure to COVID-19, it suggests that trust in the healthcare system alone is not a key driver of exposure perception. This could indicate that individuals rely more on government actions and 
policies to feel secure during a pandemic, rather than on the healthcare system's capabilities. Therefore, public health interventions should prioritize building trust in government measures and addressing public concerns 
about the virus to effectively reduce anxiety around exposure.

Question 4

The behavioural science unit is grateful for your help. Now, they ask you to consider two additional variables that are important to them: key workers (i.e. ‘essential workers’ for the functioning of the society and the state) 
and people in different age groups. They expect that key workers must have had a better sense of how long the pandemic might last. They also expect that the youngest people might have expected a shorter pandemic. 
Add these variables to the model, and answer the following questions:

a) Have the results changed? How would you explain the changes or the lack thereof?

b) Does changing the reference category matter?

c) What would you tell the behavioural science unit regarding their hypotheses

{r echo=FALSE}

# Adding dummy variables
# Adding demographic variables to the model
names(covpeak)

# Adding whether the person is a key worker or not
summary(keywork)
frq(keywork, out="v")
plot_frq(keywork)

# Adding keywork to model
covidexp3 <- lm(covidexp~covconc+gov+nhs+keywork)
summary(covidexp3)
confint(covidexp3)

# Adding age to the model
summary(age)
frq(age, out="v")
plot_frq(age)

# Adding age groups to model
covidexp4 <- lm(covidexp~covconc+gov+nhs+keywork+age2+age3+age4)
summary(covidexp4)
confint(covidexp4) 

{r echo=FALSE}

# Changing the reference category to the oldest age group
covidexp5 <- lm(covidexp~covconc+gov+nhs+keywork+age1+age2+age3)
summary(covidexp5)
confint(covidexp5)

# Coefficients of models 
coef(covidexp4)
coef(covidexp5)

# Intercept + Beta_age2

# covidexp4
3.84638883+0.22658619
# covidexp5
4.34098752-0.26801250

# Shows coefficients of each model
coef(covidexp2)
coef(covidexp3)
coef(covidexp4)

Answer to Question 4a

Yes, the results have changed based on adding the new variables to the model (key workers and age groups). The coefficients for covconc and government continue to remain significant although their p-values 
have slightly changed and similarly in the case of the nhs, it remains insignificant despite a minor change in its p-value. However, based on the behavioural science unit's hypothesis that the key workers 
variable does significantly influence perceived COVID-19 exposure, this does not seem to be the case as the key worker variable in the model is statistically insignificant due to its p-value being greater 
than the significance level (0.05). Regarding the age groups, age2 and age3 have statistically significant positive effects on perceived exposure to COVID-19, meaning that individuals within these age groups 
are more likely to feel more exposed to COVID-19 in comparison to the reference group (lower aged individuals). Additionally, the age4 group has a borderline effect, since its p value is 0.065, hinting that some 
of the oldest individuals may feel more exposed to COVID-19, though the effect is weaker than for the younger age groups.

Answer to Question 4b

Yes, changing the reference category could potentially affect the interpretation of the statistical results on the model. Currently, since the youngest age group (age1) serves as the reference group of the model, 
groups age2, age3 and age4 are being compared to the reference group to reflect how much more or how much less exposed those groups feel exposed to the youngest age group. If the reference category had to be changed, 
assuming that group age4 was now the reference group, this would shift the interpretation as the younger age groups would now be compared to the oldest age group. This could provide several different insights, depending 
on what perspective of the age group comparison the behavioural science unit is more interested in. 

Answer to Question 4c

I would inform the behavioral science unit that their hypotheses were only partially supported by the data. While they expected key workers to have a better sense of the pandemic's duration and lower perceived exposure, 
the results show that key worker status did not significantly affect perceived COVID-19 exposure. This could suggest that key workers, despite their societal role, did not feel more or less exposed than the general population.
As for the age groups, the unit's hypothesis that younger individuals might expect a shorter pandemic (and thus perceive lower exposure) was not fully confirmed. Instead, middle-aged and older individuals (age2 and age3) 
showed a stronger perception of exposure compared to the youngest group. The oldest age group (age4) showed a borderline effect.
