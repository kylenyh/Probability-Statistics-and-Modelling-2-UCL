UCL Probability, Statistics and Modelling 2 Week 1

Question 1

Create a folder for the seminars/tutorials and set up a working directory using RStudio.

Question 2

Install the packages needed for today’s class. Browse the dataset by looking at the top and 
bottom lines of the data as well as the whole dataset. What is your first impression?

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
setwd("C:/Users/user/OneDrive/Desktop/UCL_PSM_2_R/Week_1")

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

Answer to Question 2

After running the dataset and browsing through several rows of the data, I could observe the number of variables included, the type of variables included and the general structure or organization of the data. 

Question 3

Look at the descriptive statistics of all variables in the dataset. How do these statistics correspond to the description of each of the variables in Table 1?

{r echo=FALSE}

# Gives general descriptive statistics of data
describe(covpeak)

Answer to Question 3

The descriptive statistics revealed several insights about the dataset. For the variable age, the mean is 2.01, indicating that the average respondent falls within the 25-44 age range, while the median age is 2.0, confirming this trend. The standard deviation of 0.67 suggests a moderate spread of ages. In terms of gender, the mean is 1.64, implying a higher proportion of female respondents (mean of 0.63) compared to male (0.37). The area variable shows a mean of 6.12, indicating diverse metropolitan representation, with a maximum value of 12, highlighting the range of included areas. The low prevalence of non-binary individuals (mean of 0.00) suggests limited representation in the sample. These statistics correlate well with the variable descriptions, suggesting a need for further analysis of public confidence in institutions during the pandemic.

Question 4

Now consider only the questions regarding the respondents’ confidence in the various institutions’ handling of the pandemic. Based on the descriptive statistics, which institutions are trusted most/least? Why could this be the case?

{r echo=FALSE}

# Gives descriptive statistics of public confidence levels in how well the justice system, the police and the NHS handled the pandemic

summary(pm)
frq(pm, out="v")
plot_frq(pm)

summary(gov)
frq(gov, out="v")
plot_frq(gov)

summary(js)
frq(js, out="v")
plot_frq(js)

summary(pol)
frq(pol, out="v")
plot_frq(pol)

summary(nhs)
frq(nhs, out="v")
plot_frq(nhs)

{r echo=FALSE}

# Plots multiple variables next to each other

# Defines the first column of interest in the database
start <- which(colnames(covpeak) == "pm")

# Defines the last column of interest in the database
end <- which(colnames(covpeak) == "gov")

# Plots these next to each other
plot_stackfrq(covpeak[, start:end])

# Defines a new object which only contains the variables of interest
confidence <- select(covpeak, pm, gov, js, pol, nhs)

# Uses this to create the same plot as above
plot_stackfrq(confidence)

# Considers how these variables compare to each other
describe(confidence)

# Gives mean and standard deviation
describe(confidence, trim=FALSE, skew= FALSE, ranges=FALSE)

Answer to Question 4

Based on the statistics shown, the nhs variable provides data indictating the highest confidence level amongst the respondents with a rating of 4.26. 
In contrast, the gov variable provides data indictating the lowest confidence level amongst the respondents with a rating of 2.48. 
The reason for the nhs being the most trusted in the handling of the pandemic could be due to its high provision of health services and high quality of vaccination managements amongst patients. 
On the contrary, the gov being seen as the least trusted in the handling of the pandemic can be explained due to inconsistent polociies and poor communication.

Question 5

So far, we have only used descriptive statistics. Using crosstabs and the associated chi-square tests, we will carry out some inferential statistics. 
What is the null hypothesis for the Chi-square test? Is there an association between age/ethnicity and public confidence in the government? How would you interpret the results?

{r echo=FALSE}

# Looking at the Association between age and confidence using crosstabs
attach(covpeak)
summary(age)
frq(age, out="v")
plot_frq(age)

# A crosstabulation with row percentages
sjt.xtab(age, gov, show.row.prc = TRUE)

{r echo=FALSE}

# Looking at the association between ethnicity ('ethnic') and confidence
summary(ethnic)
frq(ethnic, out="v")
plot_frq(ethnic)

# Crosstabulation with row percentages
sjt.xtab(ethnic, gov, show.row.prc = TRUE)

Answer to Question 5

Hypotheses statements between age and public confidence 

H0: There is no association between age and public confidence in the government 

HA: There is an association between age and public confidence in the government

p val < 0.05 reject H0 (Fisher's p=0.009)

In this scenario, since the p value is lower than the significance level of 0.05, this leads to the conclusion that there is a statistically significant association between age and public confidence in the government. 

Hypotheses statements between ethnicity and public confidence 

H0: There is no association between ethnicity and public confidence in the government 

HA: There is an association between ethnicity and public confidence in the government

p val > 0.05 accept H0 (Fisher's p=0.821)

In this scenario, since the p value is higher than the significance level of 0.05, this leads to the conclusion that there is no statistically significant association between ethnicity and public confidence in the government. 

Question 6

Some scholars would argue that public confidence in the handling of the pandemic might be associated with a general trust in institutions. 
To examine this, run a correlation analysis with the public confidence variables. What is the null hypothesis? How would you interpret the results of these correlations? 
What does this tell you about public confidence during the first peak of the pandemic? What would you tell the scholars regarding institutional trust?

{r echo=FALSE}

# Association between confidence in different institutions using correlations

# Crosstabulation with row percentages
sjt.xtab(pm, gov, show.row.prc = TRUE)

# Pearson's correlation coefficient
r <- cor.test(pm, gov, method = "pearson", use = "complete.obs")
r

# Spearman's rank correlation coefficient
rs <- cor.test(pm, gov, method = "spearman", use = "complete.obs")
rs

# Gives description of data
describe(covpeak)

# Runs a multivariate correlation for institutional trust
# Variables in the 2-6 columns in the dataset
res2 <- rcorr(as.matrix(covpeak[,2:6]))

res2

# There is a visually more appealing way of displaying these associations
corr <- covpeak[, c(2:6)]
chart.Correlation(corr, histogram=TRUE, pch=19)

Answer to Question 6

Hypotheses statements between public confidence and general trust of instituitions 

H0: There is no association between public confidence and general trust in the different institutions HA: There is an association between public confidence and general trust in the different institutions

p val < 0.05 reject H0 (p value=2.2 x 10^-16)

Since the p value in this case is lesser than the significance level of 0.05, this suggests that there is a statistically significant relationship between public confidence and general trust in the different institutions. 
Based on the correlation matrix, the gov and pm have the highest correlation value of 0.82, suggesting a strongly positive correlation relationship. 
This indicates that individuals who trust the government are also highly likely to trust the prime minister. On the flip side, the gov and nhs have the lowest correlation value of 0.17. 
This indicates that individuals who trust the government are also highly unlikely to trust the national health service. Scholars should consider that trust in one institution may not universally translate to trust in all institutions. 
Factors influencing healthcare trust, for instance, could be more related to personal experiences or perceived competence, while political trust could be driven by communication, policy decisions and overall governance.

{r echo=FALSE}

# To empty the R environment
rm(list=ls())
