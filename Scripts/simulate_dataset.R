### Preamble ###
# Purpose: explore the effect of COVID shut-downs on restaurants in Greater Toronto Area (GTA)
# Author: Jia Jia Ji
# Email: jiajia.ji@mail.utoronto.ca
# Date: Feb.24, 2021
# To do: simluate the data for control group and treatement group, make graphs and tables to compare the responses from 2 groups


### install and load the packages ###
#install.packages('tidyverse')
#install.packages('ggplot2')
library(tidyverse)
library(ggplot2)


### R and R packages citations ###
citation()
citation('tidyverse')
citation('ggplot2')


### simulate data for the survey ###

# randomly sample 1000 restaurants from all restaurants in Toronto, 
# then randomly equally divide them into a control group and a treatment group, each group has 500 sample units

# since there exist non-response, we assume that the response rate is about 80%,
# so we actually get responses from 800 restaurants (simulate 800 responses) in total.

# we need to randomly decide how many non-responses in each group by using sample function 
# 0 represents control group, 1 represents treatment group 
set.seed(123)
nonresponse <- sample(x = c(0, 1), size = 200, replace = TRUE)
count(nonresponse) # control group has 103 non-responses, treatment group has 97 non-responses

# For the control group: randomly sample 397 responses (already ignored 103 non-responses)
set.seed(123)
# simulate data for each question
simulated_data_control <- tibble(group = sample(x = c('Open', 'Shutdoown'), size = 397, replace = TRUE, prob = c(1, 0)),
                                 business_hrs_change = sample(x = c('increase', 'decrease', 'remain the same'),
                                                              size = 397, replace = TRUE, prob = c(0.1, 0.5, 0.4)),
                                 employee_change = sample(x = c('increase', 'decrease', 'remain the same'),
                                                          size = 397, replace = TRUE, prob = c(0.1, 0.5, 0.4)),
                                 monthly_sales_change = rnorm(397, mean = -0.25, sd = 0.25) %>% round(digits = 4),
                                 profitability = sample(x = c('operate at a loss', 'break even', '0-5% profit', '5%-10% profit', '>10% profit'),
                                                        size = 397, replace = TRUE, prob = c(0.4, 0.25, 0.2, 0.1, 0.05)),
                                 favourite_number = rnorm(397, mean = 5, sd = 2) %>% round(digits = 2),
                                 subsidy = sample(x = c('Yes', 'No', 'I do not know'), 
                                                  size = 397, replace = TRUE, prob = c(0.75, 0.2, 0.05)),
                                 helpfulness_of_subsidy = sample(x = c('1', '2', '3', '4', '5'), size = 397, 
                                                                 replace = TRUE, prob = c(0.1, 0.1, 0.2, 0.3, 0.3)),
                                 effect_on_business = sample(x = c('1', '2', '3', '4', '5'), size = 397, 
                                                             replace = TRUE, prob = c(0.05, 0.2, 0.25, 0.3, 0.2)),
                                 confidence = sample(x = c('1', '2', '3', '4', '5'), size = 397, 
                                                     replace = TRUE, prob = c(0.1, 0.2, 0.3, 0.3, 0.1)))
# this is the simulated dataset for control group


# For the treatment group: randomly sample 403 responses (already ignored the 97 non-responses)
set.seed(123)
# simulate data for each question
simulated_data_trt <- tibble(group = sample(x = c('Open', 'Shutdoown'), size = 403, replace = TRUE, prob = c(0, 1)),
                             business_hrs_change = sample(x = c('increase', 'decrease', 'remain the same'),
                                                          size = 403, replace = TRUE, prob = c(0.05, 0.75, 0.2)),
                             employee_change = sample(x = c('increase', 'decrease', 'remain the same'),
                                                      size = 403, replace = TRUE, prob = c(0.05, 0.8, 0.15)),
                             monthly_sales_change = rnorm(403, mean = -0.5, sd = 0.18) %>% round(digits = 4),
                             profitability = sample(x = c('operate at a loss', 'break even', '0-5% profit', '5%-10% profit', '>10% profit'),
                                                    size = 403, replace = TRUE, prob = c(0.7, 0.1, 0.1, 0.06, 0.04)),
                             favourite_number = rnorm(403, mean = 5, sd = 2) %>% round(digits = 2),
                             subsidy = sample(x = c('Yes', 'No', 'I do not know'), 
                                              size = 403, replace = TRUE, prob = c(0.5, 0.45, 0.05)),
                             helpfulness_of_subsidy = sample(x = c('1', '2', '3', '4', '5'), size = 403, 
                                                             replace = TRUE, prob = c(0.1, 0.2, 0.4, 0.2, 0.1)),
                             effect_on_business = sample(x = c('1', '2', '3', '4', '5'), size = 403, 
                                                         replace = TRUE, prob = c(0.05, 0.15, 0.2, 0.3, 0.3)),
                             confidence = sample(x = c('1', '2', '3', '4', '5'), size = 403, 
                                                 replace = TRUE, prob = c(0.25, 0.35, 0.2, 0.1, 0.1)))
# this is the simulated dataset for treatment group


# combine the above 2 simulated datasets: 
# it has a column specifying the group type (control or treatment)
simulated_data <- rbind(simulated_data_control, simulated_data_trt)

# write the simulated raw data in a csv file and put it in the input folder
write_csv(simulated_data, 'Inputs/raw_data/raw_data.csv')
