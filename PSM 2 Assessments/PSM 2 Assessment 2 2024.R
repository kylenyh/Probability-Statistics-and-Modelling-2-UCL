UCL PSM 2 Take Home Assessment

{r include=FALSE}

# Load packages

library(easystats)
library(labelled)
library(psych)
library(sjmisc)
library(tidyverse)

{r echo=FALSE, message=FALSE}

# Load the dataset 
egypt_data <- haven::read_dta("Egypt_WVS.dta")

{r echo=FALSE, message=FALSE}

# NOTE: This code does some basic data wrangling. You will probably need to
# add to this code for your report. Note you will need to load the data for the
# country you are studying.
egypt <- egypt_data |>
  janitor::clean_names() |>
  select(
    a_year,
    w_weight,
    a_study,
    j_intdate,
    dont_carry_money = q139p,
    sex = q260,
    age = q262,
    age_6_cat = x003r,
    age_3_cat = x003r2,
    immigrant = q263,
    citizen = q269,
    birth_country = q266,
    main_language = q272,
    marital_status = q273,
    education = q275,
    employment = q279,
    occupation_group = q272,
    income = q288r,
    social_class = q287p,
    ethnic_group = q290,
    religion = q289,
    perception_corruption = q112
  ) |>
  mutate(across(where(is.labelled), as_factor)) |>
  filter(perception_corruption != "Not applicable") |>
  droplevels() 
  
View(egypt)

Introduction

{r echo=FALSE, message=FALSE}

skimr::skim(egypt)

count(egypt, income)

count(egypt, birth_country)

count(egypt, education)

count(egypt, sex)

count(egypt, ethnic_group)

Is there an association between the perception of security and subjective corruption in the country?

{r echo=FALSE, message=FALSE}

egypt_model_olm <- MASS::polr(
  perception_corruption ~ dont_carry_money, 
  data = egypt, 
  Hess = TRUE
)

{r echo=FALSE, message=FALSE}

brant::brant(egypt_model_olm)

{r echo=FALSE, message=FALSE}

model_parameters(egypt_model_olm, exponentiate = TRUE)

Is there an association between demographic characteristics, the perception of security and people’s subjective corruption?

{r echo=FALSE, message=FALSE}

# Linear regression model between demographic characteristics, the perception of security and people’s subjective corruption
egypt_model_lm <- lm(
  as.numeric(perception_corruption) ~ dont_carry_money + sex + social_class + age_3_cat, 
  data = egypt
)

summary(egypt_model_lm)

{r echo=FALSE, message=FALSE}

# Test for infuential outliers 
check_outliers(egypt_model_lm)

# Test for normality of residuals
check_normality(egypt_model_lm)

# Test for homoskedasticity
check_heteroskedasticity(egypt_model_lm)

# Test for multicollinearity
check_collinearity(egypt_model_lm)

# Test for independent residuals
check_autocorrelation(egypt_model_lm)

{r echo=FALSE, message=FALSE}

# Count regression model (Poisson) between demographic characteristics, the perception of security and people’s subjective corruption
egypt_model_pm <- glm(
  as.numeric(perception_corruption) ~ dont_carry_money + social_class + sex + age_3_cat, 
  data = egypt,
  family = "poisson"
)

summary(egypt_model_pm)

{r echo=FALSE, message=FALSE}

egypt_model_nbm <- MASS::glm.nb(
  as.numeric(perception_corruption) ~ dont_carry_money + social_class + sex + age_3_cat, 
  data = egypt
)

summary(egypt_model_nbm)

{r echo=FALSE, message=FALSE}

compare_performance(egypt_model_nbm, egypt_model_pm)

{r echo=FALSE, message=FALSE}

compare_performance(egypt_model_lm, egypt_model_pm)

{r echo=FALSE, message=FALSE}

check_collinearity(egypt_model_pm)

check_model(egypt_model_pm, check = "outliers", residual_type = "normal")

egypt_model_pm |> AER::dispersiontest() |> report()

{r echo=FALSE, message=FALSE}

model_parameters(egypt_model_pm, exponentiate = TRUE)

Is there an association between demographic characteristics, subjective corruption and people's perception of security?

{r echo=FALSE, message=FALSE}

egypt_model_blm <- glm(
  dont_carry_money ~ perception_corruption + social_class + sex + age_3_cat,
  data = egypt,
  family = "binomial"
)

{r echo=FALSE, message=FALSE}

check_model(egypt_model_blm)

{r echo=FALSE, message=FALSE}


check_model(egypt_model_blm, check = "vif", residual_type = "normal")

check_model(egypt_model_blm, check = "outliers", residual_type = "normal")

{r echo=FALSE, message=FALSE}

model_parameters(egypt_model_blm, exponentiate = TRUE)

{r echo=FALSE, message=FALSE}

test_performance(egypt_model_blm)
