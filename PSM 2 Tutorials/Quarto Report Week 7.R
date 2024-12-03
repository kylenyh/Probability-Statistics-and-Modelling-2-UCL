UCL Probability, Statistics and Modelling 2 Week 7

{r include=FALSE}

# Load libraries
library(easystats)
library(nnet)
library(tidyverse)
library(haven)

{r echo=FALSE}

# Load the dataset 
bcs_data <- read_dta("bcs_2007_8_teaching_data_unrestricted.dta")

Question 1

Look at the column complete_rate: what columns in the data have a substantial proportion of missing values?

Answer to Question 1

Looking at the complete_rate column in the summary output, the rows containing the variables that cross over with the column such as resyrago (17.2% complete, 82.8% missing), tcwmdiqu2 (8.96% complete, 91.04% missing), 
tcviolent (72.2% complete, 27.8% missing), tcsteal (72.2% complete, 27.8% missing), wcarstol (78.5% complete, 21.5% missing), wfromcar (78.3% complete, 21.7% missing), crimerat (81.8% complete, 18.2% missing) have a 
substantial proportion of missing values.

Question 2

What does the variable winsult stand for? How is this variable measured?

Answer to Question 2

The variable winsult refers to the concern of being insulted or pestered. This variable is a categorical ordinal variable, meaning it reflects ordered levels of worry but does not measure worry quantitatively. 
Additionally, it is also measured on a scale of 1 to 7, where 1 is the most extreme level, extremely worried participants (very worried) and 7 is the least extreme level, participants who are unable or unsure on how to give a response 
(dont know).

Question 3

How many participants said they thought crime was mainly caused by personal, social or other causes?

{r echo=FALSE}

bcs_data_for_model <- bcs_data |> 
  mutate(
    # Aggregate the outcome variable so it has fewer categories
    cause_crime = case_match(
      causem,
      c(3, 4, 8) ~ "personal",
      c(2, 5, 6, 7) ~ "social",
      .default = "other"
    ),
    # Set the reference value to be 'other'
    cause_crime = fct_relevel(cause_crime, "other"),
    # Convert categorical variables to factors
    across(c(ethgrp2, sex, work), as_factor)
  ) |> 
  # Remove 'refused' and 'don't know' values from variables we want to use
  filter(work %in% c("yes", "no")) |> 
  droplevels() |> 
  # Remove missing values from variables we want to use
  drop_na(age, ethgrp2, sex, work)

bcs_data_for_model

Answer to Question 3

2119 participants said they thought crime was caused by other causes, 4654 participants said they thought crime was caused by personal causes, 4850 participants said they thought crime was caused by social causes.

Question 4

Is this model better than a ‘null’ or ‘empty’ model containing no predictors? 

Question 5

Which explanatory variables are associated with significant increases or decreases in the likelihood of people saying crime is due to personal or social issues?

Question 6

What is the reference value of the variable ethgrp2?

{r echo=FALSE}

# Model 1 (cause crime and sex)
bcs_model_sex <- multinom(cause_crime ~ sex, data = bcs_data_for_model)

# Evaluates the performance of model 1
test_performance(bcs_model_sex)

Answer to Question 4

According to the test performance results of model 1, the chi squared value for the full model is 2.14 , with a p-value of 0.344. Since the p-value exceeds the standard threshold of 0.05 , we fail to reject the null hypothesis. 
This means there is not enough evidence indicating that the full model provides a significantly better fit than the null model.

{r echo=FALSE}

# Model 2 (cause crime, sex, age, work)
bcs_model_age_sex_work <- multinom(
  cause_crime ~ sex + age + work, 
  data = bcs_data_for_model
)

Answer to Question 5

We next ran a model with sex, age and whether the person had done any paid work in the past week as explanatory variables. This model was a better fit for the data than an empty model (chi-square=296.43, p<0.001). 
In this model, relative to a respondent saying crime was caused by an ‘other’ cause, respondents were significantly more likely to say crime had a personal cause with each additional year of age (OR=1.01, p<0.001), 
and more likely to say crime had a social cause if they had not done any paid work in the past week (OR=1.23, p<0.001). 
Respondents became significantly less likely to say crime had a social cause with each additional year of age (OR=0.99, p<0.001).

{r echo=FALSE}

# Model 3 (cause crime, sex, age, ethnic group)
bcs_model_age_sex_work_eth <- multinom(
  cause_crime ~ sex + age + work + ethgrp2, 
  data = bcs_data_for_model
)

Answer to Question 6

The reference category or variable in the ethnic group is the white category of people as it is numbered first on the list.

Question 7

What can we say from these charts about how the likelihood of a White person believing the main cause of crime is a social issue varies with age compared to a Black person?

{r echo=FALSE}

bcs_newdata <- expand_grid(
  sex = c("female", "male"),
  age = 18:64,
  work = unique(pull(bcs_data_for_model, "work")),
  ethgrp2 = unique(pull(bcs_data_for_model, "ethgrp2"))
)

head(bcs_newdata)

{r echo=FALSE}

bcs_predictions <- bcs_model_age_sex_work_eth |> 
  # Predict the probability of each outcome
  predict(newdata = bcs_newdata, type = "probs") |> 
  # Join the values of the explanatory variables
  bind_cols(bcs_newdata) |>
  # Convert the result to 'long' format because this is easier for plotting
  pivot_longer(
    cols = c("other", "personal", "social"),
    names_to = "category",
    values_to = "prob"
  )

{r echo=FALSE}

ggplot(
  bcs_predictions, 
  aes(x = age, y = prob, colour = category, linetype = sex)
) + 
  geom_line() + 
  facet_grid(rows = vars(work), cols = vars(ethgrp2)) + 
  theme_minimal()

Answer to Question 7

For White individuals, the likelihood of attributing crime to social causes declines with age. 
The downward slope of the blue line indicates that younger White individuals are significantly more likely to view crime as stemming from social issues, while older White individuals are less inclined to hold this view. 
This trend is consistent across both male and female subgroups and is more pronounced in the "yes" panels.

In contrast, Black individuals show an upward trend, where the likelihood of perceiving crime as a social issue increases with age. 
The blue line slopes upward, signifying that older Black individuals are more likely to associate crime with social causes than their younger counterparts. 
This positive correlation between age and the belief in social causes for crime is evident regardless of sex.

Comparing the two groups, there is a clear divergence: White individuals exhibit a negative relationship between age and the likelihood of attributing crime to social causes, whereas Black individuals display the opposite trend. 
This suggests that perceptions of the social origins of crime are shaped by both ethnic identity and age, with opposing trajectories for these two groups.
