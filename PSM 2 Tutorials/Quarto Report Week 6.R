UCL Probability, Statistics and Modelling 2 Week 6

Question 1

Congratulations! You have been hired by the Home Office as a government analyst. Your first task is to use the Crime Survey for England and Wales to estimate the probability of someone’s bike being stolen to develop an effective prevention strategy.

Familiarise yourself with the dataset and the outcome variable. What do you notice?

{r include=FALSE}

library(easystats)
library(haven)
library(tidyverse)
library(marginaleffects)
library(ResourceSelection)

{r echo=FALSE}

# Set working directory
setwd("C:/Users/user/OneDrive/Desktop/UCL_PSM_2_R/Week_6")

# Reading the data
CSEW <- read_dta("CSEW.dta")

# Browsing through names in the dataset 
names(CSEW)

# Returns top and bottom of data
head(CSEW)
tail(CSEW)

# Opens data in a separate window
View(CSEW)

#If you are only working on a single dataset, it might make sense to use the "attach"
#command, which will allow you not to mention the name of your data for several commands
#(it saves you some time and typing)
attach(CSEW)

The dataset stores whether or not a person has been a victim of bike theft. The most noticeable is that more than half of the values are missing and that almost all of the participants were not victims of bike theft so we only have a smaller number of victims.

Question 2

2. There are several working hypotheses in the Home Office regarding what might increase the probability of a bike being stolen. At each step, incorporate one more of these aspects into your model and interpret your findings by using the odds ratios and confidence intervals. 
What is the null-hypothesis being tested here?

a. Many people think that bike theft is primarily an “inner-city” problem.
b. Others suggest that the perpetrators might be emboldened in areas where there are clear signs of disorder (e.g. graffiti on the walls, high amount of rubbish on the street, as argued by the broken windows theory).
c. Some believe that living in an area for a longer period might be a protective factor (you learn where (not) to store your bike).
d. Other analysts point out that the number of years a person could have lived in a certain area is strongly associated with age, so controlling for that would be a good idea. In addition, young people are more likely to use bikes, which makes it more likely that they would have their bikes stolen.
e. Finally, there is some anecdotal evidence that people who go to pubs and clubs are more likely to have their bikes stolen.f. Compare these models to one another. How do they change? What kind of conclusion can you draw from these changes? What kind of policy-recommendation would you propose?

{r echo=FALSE}

# MODEL 1: Start with a basic model that just predicts a person is more/less 
# likely to be a victim of bike theft if they live in an inner-city area
bike1 <- glm(biktheft ~ inner, data = CSEW, family = "binomial")

# Let's see what the coefficients are for this model
model_parameters(bike1, exponentiate = TRUE)

{r echo=FALSE}

# MODEL 2: Now add in whether bike theft is more/less of a problem in areas with 
# more visible "signs of disorder"
bike2 <- glm(
  biktheft ~ inner + vandcomm + rubbcomm, 
  data = CSEW, 
  family = "binomial"
)

# Let's see what the coefficients are for this model
model_parameters(bike2, exponentiate = TRUE)

{r echo=FALSE}

# MODEL 3: How about if there is a relationship with how long a person has lived
# in an area?
bike3 <- glm(
  biktheft ~ inner + vandcomm + rubbcomm + yrsarea,
  data = CSEW, 
  family = "binomial"
)

# Let's see what the coefficients are for this model
model_parameters(bike3, exponentiate = TRUE)

{r echo=FALSE}

# MODEL 4: And does likelihood of having a bike stolen vary with a person's age?
bike4 <- glm(
  biktheft ~ inner + vandcomm + rubbcomm +  yrsarea + age,
  data = CSEW, 
  family = "binomial"
)

# Let's see what the coefficients are for this model
model_parameters(bike4, exponentiate = TRUE)

{r echo=FALSE}

# MODEL 5: Finally, does the frequency of going to pubs or clubs play a role?
bike5 <- glm(
  biktheft ~ inner + vandcomm + rubbcomm +  yrsarea + age + pubeve + club,
  data = CSEW, 
  family = "binomial"
)

# Let's see what the coefficients are for this model
model_parameters(bike5, exponentiate = TRUE)

Answer to Question 2a

2a. The null hypothesis for this would be that the location of the bike theft (inner-city vs. non-inner-city) does not affect the odds of a bike being stolen. 
To interpret the explanatory variable, based on the results provided, the odds ratio that is given is 2.46, suggesting that bikes in the inner city are 146% more likely to be stolen compared to non-inner-city areas. 
Since the provided confidence interval for the explanatory variable does not include a 1 and the p-value is less than 0.05 (p < 0.001), this suggests a rejection of the null hypothesis, meaning that the location of bike theft variable is statistically significant and has an effect on the odds of a bike being stolen.

2b. Firstly, the null hypothesis for the vandalism explanatory variable would be that the presence of vandalism in the community does not affect the odds of a bike being stolen. 
To interpret the vandalism explanatory variable, based on the results provided, the odds ratio that is given is 1.19, suggesting that bikes in the inner city are 19% more likely to be stolen compared to non-inner-city areas. 
Since the provided confidence interval for the explanatory variable includes a 1 and the p-value is more than 0.05 (p = 0.134), this suggests a rejection of the alternative hypothesis, meaning that the presence of the vandalism variable is not statistically significant and does not have an effect on the odds of a bike being stolen.

Secondly, the null hypothesis for the rubbish explanatory variable would be that the presence of rubbish in the community does not affect the odds of a bike being stolen. 
To interpret the rubbish explanatory variable, based on the results provided, the odds ratio that is given is 2.04, suggesting that bikes in the inner city are 104% more likely to be stolen relative to non-inner-city areas. 
Since the provided confidence interval for the explanatory variable does not include a 1 and the p-value is less than 0.05 (p < 0.001), this suggests a rejection of the null hypothesis, meaning that the presence of rubbish variable is statistically significant and has an effect on the odds of a bike being stolen.

2c. The null hypothesis for this would be that the number of years that someone has lived in the community does not affect the odds of a bike being stolen. 
To interpret this explanatory variable, based on the results provided, the odds ratio that is given is 0.87, suggesting that the people who have lived in the community for a certain number of years are 13% less likely to have their bikes stolen in the community relative to the people who have not lived in the community before. 
Since the provided confidence interval for the explanatory variable does not include a 1 and the p-value is less than 0.05 (p < 0.001), this suggests a rejection of the null hypothesis, meaning that the number of years that someone has lived in the community variable is statistically significant and has an effect on the odds of a bike being stolen.

2d. The null hypothesis for this would be that the age of a person in the community does not affect the odds of a bike being stolen. 
To interpret this explanatory variable, based on the results provided, the odds ratio that is given is 0.97, suggesting that the age of a person is 3% less likely to have his or her bike stolen in the community relative to an aged person who does not live in the community. 
Since the provided confidence interval for the explanatory variable does not include a 1 and the p-value is less than 0.05 (p < 0.001), this suggests a rejection of the null hypothesis, meaning that the age of a person in the community variable is statistically significant and has an effect on the odds of a bike being stolen.

2e. Firstly, the null hypothesis for the number of people who go to pubs explanatory variable would be that the number of people who go to pubs in the community does not affect the odds of a bike being stolen. 
To interpret this explanatory variable, based on the results provided, the odds ratio that is given is 0.97, suggesting that bikes in the community where people who go to pubs and clubs are 3% less likely to be stolen relative to the people who do not go to pubs in the community. 
Since the provided confidence interval for the explanatory variable includes a 1 and the p-value is more than 0.05 (p = 0.605), this suggests a rejection of the alternative hypothesis, meaning that the number of people who go to pubs in the community variable is not statistically significant and does not have an effect on the odds of a bike being stolen.

Secondly, the null hypothesis for the number of people who go to clubs explanatory variable would be that the number of people who go to clubs in the community does not affect the odds of a bike being stolen. 
To interpret this explanatory variable, based on the results provided, the odds ratio that is given is 1.44, suggesting that bikes in the community where people who go to clubs are 44% less likely to be stolen relative to the people who do not go to clubs in the community. 
Since the provided confidence interval for the explanatory variable does not include a 1 and the p-value is less than 0.05 (p = 0.006), this suggests a rejection of the null hypothesis, meaning that the number of people who go to clubs in the community variable is statistically significant and has an effect on the odds of a bike being stolen.

2f. The models evolve as additional variables are incorporated, reflecting the multifaceted nature of bike theft. 
The earlier models emphasize location, with "inner-city" areas showing higher odds of theft, while later models include environmental disorder (e.g., graffiti, rubbish) and personal factors like age or pub/club attendance. 
Including variables like age and years lived in the area reduces the significance of some earlier predictors, suggesting these individual-level factors are critical. 
The final model highlights clubs as a notable risk factor but not pubs, showing nuanced behavioral links to theft. 
Policy recommendations include targeted bike security in urban and disorderly areas, public awareness campaigns for young cyclists, and increased monitoring around nightlife clubs. 
These measures could help address bike theft without displacing the community.

Note

OR < 1: Indicates a negative association; the explanatory variable reduces the odds of the outcome.
OR = 1: Indicates no association; the explanatory variable has no effect on the odds of the outcome.
OR > 1: Indicates a positive association; the explanatory variable increases the odds of the outcome.

If CI includes 1, the explanatory variable is not statistically significantIf CI includes 0, the explanatory variable is statistically significant

Question 3

It is fairly difficult to interpret log-odds and odds-ratios, predicted probabilities are much more straightforward. Follow the instructions in the R-script and estimate the predicted probabilities of the model. 
How would you interpret these results?

{r echo=FALSE}

# Predicted probabilities for the existing dataset
bike_prob <- predict(bike5, type = "response")

# Summary of predicted probabilities
summary(bike_prob)

Answer to Question 3

Firstly, the lowest predicted probability of bike theft is 0.39%. This suggests that, in certain scenarios, the combination of factors (e.g., living outside an inner-city area, minimal signs of disorder, long residence time, older age and infrequent pub/club visits) makes bike theft very unlikely. 
Secondly, the median predicted probability is approximately 1.87%. This represents the midpoint risk across the dataset, indicating that in half the cases, the probability of bike theft is below 1.87%. Additionally, the average probability across all cases is 2.37%, slightly higher than the median. 
This suggests a slightly skewed distribution where a small number of individuals have notably higher risk levels. Furthermore, the highest predicted probability of bike theft is 32.07%, representing cases where the combination of predictors (e.g., inner-city residence, high signs of disorder, frequent pub/club visits) creates a high-risk scenario. 
Overall, most probabilities are clustered at the lower end, with the 75th percentile (third quartile) at 2.97%. 
This indicates that bike theft is generally a low-probability event in the population, but certain conditions elevate the risk significantly.

{r echo=FALSE}

# Check assumptions graphically
check_model(bike5)

{r echo=FALSE}

# Inspect model coefficients. How would you interpret each combination of odds 
# ratio and p-value
model_parameters(bike5, exponentiate = TRUE)

Question 4

The Home Office would like to know which variable is the most influential from the above list. Estimate the marginal effects to answer this question. 
What would be your response to the Home Office? How would you interpret the results?

Question 5

Finally, run the different tests of model fit (Hosmer-Lemeshow-test and various pseudo-R2 values). How much improvement do you notice comparing model 1 and model 5? How would you interpret these results?
