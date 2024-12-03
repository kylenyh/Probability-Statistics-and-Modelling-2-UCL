UCL Probability, Statistics and Modelling 2 Week 8

{r include=FALSE}

# Load libraries
library(easystats)
library(tidyverse)
library(brant)
library(MASS)

{r echo=FALSE}

# Load the dataset 
csew_data <- haven::read_dta("csew.dta")

Question 1

1a. what is the name of the column in csew_data that contains information about how worried people feel about having their car stolen?

1b. What values can this variable have?

Answer to Question 1a

wcarstol is the name of the column in the dataset that contains information about how worried people feel about having their car stolen. 

Answer to Question 1b

This variable has 7 different levels, with 7 being a dont know response and 1 being a very worried response. 

Question 2

What levels does the wcarstol column in the csew_data_for_model object have, and in what order?

{r echo=FALSE}

# Wrangling data for csew data model
csew_data_for_model <- csew_data |> 
  mutate(
    across(where(labelled::is.labelled), as_factor),
    wcarstol = fct_rev(wcarstol)
  ) |> 
  drop_na(wcarstol, sex, agegrp7, ethgrp2a, bcsvictim) |> 
  filter(wcarstol != "Not applicable") |> 
  droplevels()

csew_data_for_model

Answer to Question 2

The wcarstol column now shows 4 levels which are: "Not at all worried", "Not very worried", "Fairly worried", "Very worried". 
However, since the levels have been reversed from the original set of levels, the "Not at all worried" level is now number 1 and the "very worried" level is 4. 

Question 3

What is the null hypothesis for this model?

{r echo=FALSE}

# Model relating to wcarstol based on a set of explanatory variables
wcarstol_model <- MASS::polr(
  wcarstol ~ sex + agegrp7 + ethgrp2a + bcsvictim, 
  data = csew_data_for_model, 
  Hess = TRUE
)

wcarstol_model

Answer to Question 3

A person who has been a victim of crime in the past 12 months is not related to the likelihood that a person will be worried about their car being stolen.

Question 4

What do the Brant test results tell us about whether our model is compatible with the proportional odds assumption?

{r echo=FALSE

brant::brant(wcarstol_model)

Answer to Question 4

The Brant test evaluates whether the proportional odds assumption holds in an ordinal logistic regression model. This assumption requires that the relationship between each predictor and the log-odds of the outcome is consistent 
across all levels of the ordinal response. In this case, the omnibus test, which assesses the assumption for the model as a whole, produces a chi-squared statistic of 64.37 with 24 degrees of freedom and a p-value of 0.000. 
Since this p-value is less than the typical threshold of 0.05, the null hypothesis (that the proportional odds assumption holds globally) is rejected. This indicates that the model as a whole violates the proportional odds assumption.

When examining the variable-specific results, we observe that not all predictors violate the assumption. Variables such as agegrp745-54 (p = 0.02), agegrp755-64 (p = 0.01), and ethgrp2aAsian or Asian British (p = 0.00) show p-values 
below 0.05, indicating that these predictors do not satisfy the proportional odds assumption. In contrast, variables such as sexFemale (p = 0.07), agegrp725-34 (p = 0.11), and bcsvictimVictim of crime (p = 0.10) show p-values above 0.05, 
suggesting that the proportional odds assumption holds for these variables individually.

Overall, the results indicate that while some predictors meet the proportional odds assumption, others do not. The global violation suggests that an ordinal logistic regression model may not fully capture the relationships in the data.

Question 5

How would you interpret the odds ratios for the different levels of sex, agegrp7, ethgrp2a and bcsvictim, using the ‘Odds Ratio’ and ‘p’ columns in the table?

{r echo=FALSE}

# Looks at the odds ratios for the explanatory variables in this model
model_parameters(wcarstol_model, exponentiate = TRUE)

{r echo=FALSE}

# Creates a plot of these coefficients and the 95% confidence intervals
plot(model_parameters(wcarstol_model, exponentiate = TRUE), log_scale = TRUE)

Sex [Female] 
Holding all other variables constant, females have an odds ratio of 1.06 compared to males, indicating that they are 6% more likely to report a higher level of worry. However, this effect is not statistically significant (p = 0.503). 
This suggests that sex does not have a meaningful influence on worry levels when accounting for other factors in the model.

Age Group (agegrp7) 
Holding all other variables constant, individuals aged 25-34 have an odds ratio of 0.93 (p = 0.740), making them 7% less likely to report higher levels of worry compared to the youngest age group (reference). 
This effect is not significant, indicating no strong relationship between this age group and worry levels. 
Similarly, individuals aged 35-44 have an odds ratio of 1.11 (p = 0.641), meaning they are 11% more likely to report a higher worry level, but this is not statistically significant. 
Additionally, respondents aged 45-54 show an odds ratio of 1.14 (p = 0.537), suggesting they are 14% more likely to report higher worry levels, though this result is insignificant.

In contrast, individuals aged 55-64 have an odds ratio of 1.50 (p = 0.063), suggesting a 50% higher likelihood of reporting greater worry levels compared to the youngest group. 
While this result approaches significance, it does not provide conclusive evidence. 
Respondents aged 65-74 have an odds ratio of 1.13 (p = 0.576), showing a 13% higher likelihood of reporting higher worry levels, but this is not significant. 
For those aged 75+, the odds ratio is 0.77 (p = 0.286), suggesting they are 23% less likely to report higher worry levels, but this finding is not significant.

Ethnicity (ethgrp2a) 
Holding all other variables constant, individuals from a Mixed ethnic background have an odds ratio of 0.60 (p = 0.255), meaning they are 40% less likely to report higher worry levels than White respondents. 
However, this result is not statistically significant, suggesting no strong evidence for a relationship between this group and worry levels.

Conversely, individuals identifying as Asian or Asian British have an odds ratio of 2.28 (p < 0.001), indicating they are 128% more likely to report higher levels of worry compared to White respondents. 
This result is statistically significant, highlighting ethnicity as an important factor in worry levels. 
Similarly, respondents identifying as Chinese or Other have an odds ratio of 6.57 (p < 0.001), indicating a 557% greater likelihood of higher worry levels, a highly significant result. 
For Black or Black British respondents, the odds ratio is 1.71 (p = 0.089), suggesting a 71% increased likelihood of higher worry levels. 
Although this effect is only marginally significant, it indicates a potential association worth exploring further.

Victim of Crime (bcsvictim) 
Holding all other variables constant, individuals who were victims of crime have an odds ratio of 1.43 (p = 0.006), meaning they are 43% more likely to report higher levels of worry compared to those who were not victims. 
This result is statistically significant, indicating that experiencing victimization is strongly associated with increased worry levels.

Question 6

Which model best fits the data, the ordinal model or the linear model?

{r echo=FALSE}

# Linear model of the wcarstol model 
wcarstol_model_lm <- lm(
  as.numeric(wcarstol) ~ sex + agegrp7 + ethgrp2a + bcsvictim, 
  data = csew_data_for_model
)

wcarstol_model_lm

{r echo=FALSE}

# Compares current model to ordinal model 
compare_performance(wcarstol_model, wcarstol_model_lm)

Answer to Question 6

The ordinal model (polr) is a better fit for the data compared to the linear model (lm). The ordinal model has much lower values for AIC (3961.3), AICc (3961.6), and BIC (4043.2), compared to the linear model's AIC (4138.6), AICc (4138.9), and BIC (4215.0). 
These criteria measure the trade-off between model fit and complexity, with lower values indicating a better model.

Question 7

What does this tell us about which groups in society are most likely to be more worried about having their car stolen?

{r echo=FALSE}

# Step 1: specify what values of the explanatory variables we're interested in
# Remember the new data object we create must include values for **all** of the
# explanatory variables in our model. This week, we will use `unique()` to
# include every unique value in the original data in the new data we want to
# apply the model to.
csew_newdata <- expand_grid(
  sex = unique(pull(csew_data_for_model, "sex")),
  agegrp7 = unique(pull(csew_data_for_model, "agegrp7")),
  ethgrp2a = unique(pull(csew_data_for_model, "ethgrp2a")),
  bcsvictim = unique(pull(csew_data_for_model, "bcsvictim"))
)

csew_newdata

{r echo=FALSE}

# Step 2: produce predicted values of outcome variable
csew_predicted <- wcarstol_model |> 
  # Predict the probability of each outcome
  predict(newdata = csew_newdata, type = "probs") |> 
  # Join the values of the explanatory variables
  bind_cols(csew_newdata) |>
  # Convert the result to 'long' format because this is easier for plotting
  pivot_longer(
    cols = contains("worried"),
    names_to = "category",
    values_to = "prob"
  ) |> 
  # Match ordering of values between original data and new `category` variable
  mutate(
    category = factor(
      category, 
      levels = levels(pull(csew_data_for_model, "wcarstol"))
    )
  )

csew_predicted

{r echo=FALSE}

# Step 3: plot the predicted values
ggplot(
  csew_predicted, 
  aes(x = agegrp7, y = prob, fill = category, group = category)
) +
  geom_col(position = position_fill()) +
  facet_grid(cols = vars(ethgrp2a), rows = vars(sex, bcsvictim)) +
  scale_fill_ordinal() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = -55, hjust = 0))

Answer to Question 7

The chart reveals how worry about being a victim of car theft varies across ethnicity, gender, and age groups in society. Individuals identifying as "Black or Black British" and "Mixed" generally show higher levels of concern, with larger proportions in the "Fairly worried" and "Very worried" categories compared to "White" respondents, who are predominantly "Not at all worried" or "Not very worried." 
Gender differences are evident, as females across all ethnic groups consistently report higher levels of worry than males, with visibly larger yellow ("Very worried") and green ("Fairly worried") segments in the female rows. 
Age also plays a role, as younger and middle-aged groups tend to express slightly more worry than older adults, though this pattern varies across different ethnicities and genders. 
This indicates that societal perceptions of vulnerability to car theft are shaped by a combination of demographic factors, with women and minority ethnic groups exhibiting heightened concerns, potentially reflecting broader issues of inequality, lived experiences, or cultural perceptions of crime risk.
