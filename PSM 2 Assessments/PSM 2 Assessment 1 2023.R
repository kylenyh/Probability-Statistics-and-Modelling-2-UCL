UCL PSM 2 Take Home Assessment 1 2023

For Question 4

Dependent Variable = Outcome Variable

Independent Variable = Explanatory Variable 




{r include=FALSE}

# Load packages

library(easystats)
library(labelled)
library(psych)
library(sjmisc)
library(tidyverse)

{r echo=FALSE, message=FALSE}

# Load the dataset 
data <- haven::read_dta("PAS_thexam_2023.dta")

# Browse variable names
names(data)

# View top and bottom of the dataset
head(data)
tail(data)






{r echo=FALSE, message=FALSE}

# Logistic regression (Log odds default) model between outcome variable and explanatory variables
model_logit <- glm(victim ~ crimeworry + vandalism + victsupp + fairtreat,
                   data = data, family = binomial)

# Model summary (blm)
summary(model_logit)





{r echo=FALSE, message=FALSE}

# Linear model between outcome variable and explanatory variables
model_lm <- lm(victim ~ crimeworry + vandalism + victsupp + fairtreat,
               data = data)

# Model summary (lm)
summary(model_lm)


p val < 0.05 reject H0 and accept H1

p val > 0.05 accept H0 and reject H1



