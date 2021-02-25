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


### Summary statistics & graphs ###

#1. bar chart of business_hrs_change for 2 groups
# for each group, count how many restaurants in each level 
business_hrs_freq <- simulated_data %>%
  group_by(business_hrs_change, group) %>%
  select(business_hrs_change, group) %>%
  count()
# add a new column: proportions
business_hrs_freq$business_hrs_prop <- c(1,1,1,1,1,1)
# calculate the proportions of each level for the control group 
business_hrs_freq[c(1, 3, 5), 4] <- round((business_hrs_freq[c(1, 3, 5), 3] / 397) * 100, digits = 2)
# calculate the proportions of each level for the treatment group 
business_hrs_freq[c(2, 4, 6), 4] <- round((business_hrs_freq[c(2, 4, 6), 3] / 403) * 100, digits = 2)
# change the column names 
names(business_hrs_freq)[names(business_hrs_freq) == "n"] <- 'business_hrs_freq'

# make the bar chart
business_hrs_freq %>%
  ggplot(mapping = aes(x = business_hrs_change, y = business_hrs_freq, fill = group, label = business_hrs_prop)) + 
  geom_bar(stat = 'identity', position=position_dodge()) + 
  xlab('Change in daily business hours') + ylab('Proportion') +
  ggtitle('Change in daily business hours for the restaurants in control group and treatment group') +
  # also put the exact proportions in the bars
  geom_text(aes(x = business_hrs_change, y = business_hrs_freq, 
                label = paste0(business_hrs_prop, '%')), position = position_dodge(0.9), vjust = -0.5, size = 3) +
  theme_minimal()


#2. bar chart of employee_change for 2 groups
# for each group, count how many restaurants in each level 
employee_freq <- simulated_data %>%
  group_by(employee_change, group) %>%
  select(employee_change, group) %>%
  count()
# add a new column: proportions
employee_freq$employee_prop <- c(1,1,1,1,1,1)
# calculate the proportions of each level for the control group 
employee_freq[c(1, 3, 5), 4] <- round((employee_freq[c(1, 3, 5), 3] / 397) * 100, digits = 2)
# calculate the proportions of each level for the treatment group 
employee_freq[c(2, 4, 6), 4] <- round((employee_freq[c(2, 4, 6), 3] / 403) * 100, digits = 2)
# change the column names 
names(employee_freq)[names(employee_freq) == "n"] <- 'employee_frequency'

# make bar chart
employee_freq %>%
  ggplot(mapping = aes(x = employee_change, y = employee_frequency, fill = group, label = employee_prop)) + 
  geom_bar(stat = 'identity', position=position_dodge()) + 
  xlab('Change in the number of employees') + ylab('Proportion') +
  ggtitle('Change in the number of employees for restaurants in control group and treatment group') +
  geom_text(aes(x = employee_change, y = employee_frequency, 
                label = paste0(employee_prop, '%')), position = position_dodge(0.9), vjust = -0.5, size = 3) +
  theme_minimal()


# 3. histogram of avg_monthly_sales_change for 2 groups 
# convert the simulated data for this variable to be in % form 
simulated_data$monthly_sales_change <- simulated_data$monthly_sales_change * 100
# select the columns: group, and monthly_sales_change 
sa_change <- simulated_data %>%
  select(monthly_sales_change, group)

# make the histogram
sa_change %>%
  ggplot(mapping = aes(x = monthly_sales_change, fill = group)) +
  geom_histogram(position = 'dodge') +
  xlab('Change in average monthly sales') +
  ggtitle('Change in average monthly sales for restaurants in control group and treatment group') +
  theme_minimal()

# boxplot of avg_monthly_sales_change for 2 groups 
sa_change %>%
  ggplot(mapping = aes(y = monthly_sales_change, x = group)) +
  geom_boxplot() +
  xlab('Group') +
  ylab('Change in average monthly sales') +
  ggtitle('Boxplot of change in average monthly sales for restaurants in control group and treatment group') +
  theme_minimal()


# 4. bar chart of profitability for 2 groups
# for each group, count how many restaurants in each level 
profit <- simulated_data %>%
  group_by(profitability, group) %>%
  select( profitability, group) %>%
  count()
# add a new column: proportions
profit$profit_prop <- c(1,1,1,1,1,1,1,1,1,1)
# calculate the proportions of each level for the control group 
profit[c(1, 3, 5, 7, 9), 4] <- round((profit[c(1, 3, 5, 7, 9), 3] / 397) * 100, digits = 2)
# calculate the proportions of each level for the treatment group 
profit[c(2, 4, 6, 8, 10), 4] <- round((profit[c(2, 4, 6, 8, 10), 3] / 403) * 100, digits = 2)
# change column names
names(profit)[names(profit) == "n"] <- 'profit_freq'

# make bar chart
profit %>%
  ggplot(mapping = aes(x = profitability, y = profit_freq, fill = group, label = profit_prop)) + 
  geom_bar(stat = 'identity', position=position_dodge()) + 
  xlab('Profitability') + ylab('Proportion') +
  ggtitle('Profitability for restaurants in control group and treatment group') +
  geom_text(aes(x =  profitability, y = profit_freq, 
                label = paste0(x = profit_prop, '%')), position = position_dodge(0.9), vjust = -0.5, size = 3) +
  theme_minimal()


# 5. bar chart of government subsidy for 2 groups: same process as making the bar chart for business_hrs_change
subsidy_fre <- simulated_data %>%
  group_by(subsidy, group) %>%
  select(subsidy, group) %>%
  count()
# add a new column: proportions
subsidy_fre$subsidy_prop <- c(1,1,1,1,1,1)
subsidy_fre[c(1, 3, 5), 4] <- round((subsidy_fre[c(1, 3, 5), 3] / 397) * 100, digits = 2)
subsidy_fre[c(2, 4, 6), 4] <- round((subsidy_fre[c(2, 4, 6), 3] / 403) * 100, digits = 2)
names(subsidy_fre)[names(subsidy_fre) == "n"] <- 'subsidy_freq'

# make a bar chart
subsidy_fre %>%
  ggplot(mapping = aes(x = subsidy, y = subsidy_freq, fill = group, label = subsidy_prop)) + 
  geom_bar(stat = 'identity', position=position_dodge()) + 
  xlab('Whether the restaurant has received COVID-related government subsidies') + ylab('Proportion') +
  ggtitle('The bar chart of whether a restaurant has received COVID-related government subsidies for control group and treatment group') +
  geom_text(aes(x = subsidy, y = subsidy_freq, 
                label = paste0(x = subsidy_prop, '%')), position = position_dodge(0.9), vjust = -0.5, size = 3) +
  theme_minimal() 


# 6. bar chart of helpfulness of subsidy for 2 groups
# count the number of restaurants in each ranked linear scale number for each group
helpfulness <- simulated_data %>%
  group_by(helpfulness_of_subsidy, group) %>%
  select(helpfulness_of_subsidy, group) %>%
  count()
# add a new column: proportions
helpfulness$helpfulness_prop <- c(1,1,1,1,1,1,1,1,1,1)
# calculate the proportions 
helpfulness[c(1, 3, 5, 7, 9), 4] <- round((helpfulness[c(1, 3, 5, 7, 9), 3] / 397) * 100, digits = 2)
helpfulness[c(2, 4, 6, 8, 10), 4] <- round((helpfulness[c(2, 4, 6, 8, 10), 3] / 403) * 100, digits = 2)
names(helpfulness)[names(helpfulness) == "n"] <- 'helpfulness_freq'

# make bar chart 
helpfulness %>%
  ggplot(mapping = aes(x = helpfulness_of_subsidy, y = helpfulness_freq, fill = group, label = helpfulness_prop)) +
  geom_bar(stat = 'identity', position=position_dodge()) +
  xlab('Helpfulness of government subsidies') +
  ylab('Proportion') + 
  ggtitle('Helpfulness of COVID-related government subsidies for restaurants in control group and treatment group') +
  geom_text(aes(x = helpfulness_of_subsidy, y = helpfulness_freq, 
                label = paste0(x = helpfulness_prop, '%')), position = position_dodge(0.9), vjust = -0.5, size = 3) +
  theme_minimal()

# 7. bar chart of effect_on_business: same process as for helpfulness_of_subsidy
effect <- simulated_data %>%
  group_by(effect_on_business, group) %>%
  select(effect_on_business, group) %>%
  count()
# add a new column: proportions
effect$effect_prop <- c(1,1,1,1,1,1,1,1,1,1)
# calculate the proportions 
effect[c(1, 3, 5, 7, 9), 4] <- round((effect[c(1, 3, 5, 7, 9), 3] / 397) * 100, digits = 2)
effect[c(2, 4, 6, 8, 10), 4] <- round((effect[c(2, 4, 6, 8, 10), 3] / 403) * 100, digits = 2)
names(effect)[names(effect) == "n"] <- 'effect_freq'

# make bar chart 
effect %>%
  ggplot(mapping = aes(x = effect_on_business, y = effect_freq, fill = group, label = effect_prop)) +
  geom_bar(stat = 'identity', position=position_dodge()) +
  xlab('Effects of pandemic on restaurant') + ylab('Proportion') +
  ggtitle('How much the pandemic affects the restaurant businesses for control group and treatment group') +
  geom_text(aes(x = effect_on_business, y = effect_freq, 
                label = paste0(x = effect_prop, '%')), position = position_dodge(0.9), vjust = -0.5, size = 3) +
  theme_minimal()

# 8. bar chart of confidence 
conf <- simulated_data %>%
  group_by(confidence, group) %>%
  select(confidence, group) %>%
  count()
# add a new column: proportions
conf$confidence_prop <- c(1,1,1,1,1,1,1,1,1,1)
# calculate the proportions 
conf[c(1, 3, 5, 7, 9), 4] <- round((conf[c(1, 3, 5, 7, 9), 3] / 397) * 100, digits = 2)
conf[c(2, 4, 6, 8, 10), 4] <- round((conf[c(2, 4, 6, 8, 10), 3] / 403) * 100, digits = 2)
names(conf)[names(conf) == "n"] <- 'confidence_freq'

# make bar chart
conf %>%
  ggplot(mapping = aes(x = confidence, y = confidence_freq, fill = group, label = confidence_prop)) +
  geom_bar(stat = 'identity', position=position_dodge()) +
  xlab('Confidence in the operation of restaurant') + ylab('Proportion') +
  ggtitle('Confidence in operations of restaurants for control group and treatment group') +
  geom_text(aes(x = confidence, y = confidence_freq, 
                label = paste0(x = confidence_prop, '%')), position = position_dodge(0.9), vjust = -0.5, size = 3) +
  theme_minimal()
