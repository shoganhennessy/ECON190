# 10/02/2018 (begun) Senan Hogan-H.

# This file works on the March CPS dataset.
# http://ceprdata.org/cps-uniform-data-extracts/march-cps-supplement/march-cps-data/

# The code looks to observe and decompose wage inequality 1980-2016.
# See corresponding research paper for use of this work.
library(tidyverse)
library(rpart)
library(rpart.plot)
library(stargazer)
library(data.table)
set.seed(47)


############################
## Load combined March CPS dataframe
CPS.data <- fread('../Data/CPS_data.csv', header = T, sep = ',')



############################
## graph median and quantiles for wage inequality
# First using all income earnings, second hourly wages
# Inequality ratios used are 90-10, 90-50, 80-20

# Define years to loop over (skiping 2008 for data quality reasons)
years <- c(c(1980:2007), c(2009:2016))

# Define empty lists to add to, and get base year values for income ratios
year <- median <- quantile_90 <- quantile_10 <- ratio_9010 <- ratio_8020 <- c()

median_1980 <- CPS.data %>% subset(year==1980) %>% pull(rincp_ern) %>%
	quantile(probs = 0.5)

q_90_1980 <- CPS.data %>% subset(year==1980) %>% pull(rincp_ern) %>%
	quantile(probs = 0.9) 

q_10_1980 <- CPS.data %>% subset(year==1980) %>% pull(rincp_ern) %>%
	quantile(probs = 0.1)

for (i in years){
  year <- c(year, i)
  x <- subset(CPS.data, year==i)$rincp_ern
  med <- quantile(x, probs=0.5)
  median <- c(median, 100*med/median_1980)
  q_90 <- quantile(x, probs=0.9)
  q_10 <- quantile(x, probs=0.1)
  quantile_90 <- c(quantile_90, 100*(q_90/q_90_1980))
  quantile_10 <- c(quantile_10, 100*(q_10/q_10_1980))
  ratio_9010 <- c(ratio_9010, q_90/q_10)
  ratio_8020 <- c(ratio_8020, 
                  quantile(x, probs=0.8)/quantile(x, probs=0.2))
}
CPS_ratio.data <- data.frame(year, ratio_9010, median, quantile_90, quantile_10,
                             ratio_8020)

# Graph for median, 0.9 and 0.1 quantiles of income indexed to 1980.
CPS_ratio.data %>% 
	ggplot(aes(x=year)) +
  scale_x_continuous(breaks=seq(1980,2016,5)) +
  geom_hline(yintercept=100, linetype='dashed') +
  geom_point(aes(y=median, colour='Median')) +
  geom_line(aes(y=median, colour='Median')) +
  geom_point(aes(y=quantile_90, colour='90th Percentile')) +
  geom_line(aes(y=quantile_90, colour='90th Percentile')) +
  geom_point(aes(y=quantile_10, colour='10th Percentile')) +
  geom_line(aes(y=quantile_10, colour='10th Percentile')) +
  labs(x= 'Year', y='Real Annual Income, Indexed to 1980', 
       colour = '') + theme_classic()

# Graph for the income ratios
# Title in LaTeX: Ratio of Annual Income Between 90th and 10th Percentiles, 1980-2016
CPS_ratio.data %>% ggplot(aes(x=year))+
  scale_x_continuous(breaks=seq(1980, 2016, 5))+
  geom_point(aes(y=ratio_9010)) +
  geom_line(aes(y=ratio_9010))  +
  labs(x= 'Year', y='Ratio of Annual Income') +
  theme_classic()


# forming data frame for median and quantiles of hourly wage, by year
years <- c(c(1980:2007), c(2009:2016))
year <- median <- quantile_90 <- quantile_10 <- ratio_9010 <- ratio_8020 <- c()

median_1980 <- CPS.data %>% subset(year==1980) %>% pull(rhrwage) %>%
	quantile(probs = 0.5)

q_90_1980 <- CPS.data %>% subset(year==1980) %>% pull(rhrwage) %>%
	quantile(probs = 0.9) 

q_10_1980 <- CPS.data %>% subset(year==1980) %>% pull(rhrwage) %>%
	quantile(probs = 0.1)

for (i in years){
  year <- c(year, i)
  x <- subset(CPS.data, year==i)$rhrwage
  med <- quantile(x, probs=0.5)
  median <- c(median, 100*med/median_1980)
  q_90 <- quantile(x, probs=0.9)
  q_10 <- quantile(x, probs=0.1)
  quantile_90 <- c(quantile_90, 100*(q_90/q_90_1980))
  quantile_10 <- c(quantile_10, 100*(q_10/q_10_1980))
  ratio_9010 <- c(ratio_9010, q_90/q_10)
  ratio_8020 <- c(ratio_8020, 
                  quantile(x, probs=0.8)/quantile(x, probs=0.2))
}
CPS_ratio.data <- data.frame(year, ratio_9010, median, quantile_90, quantile_10,
                             ratio_8020)

# Graph for median, 0.9 and 0.1 quantiles of income indexed 1980 is 100 for all.
CPS_ratio.data %>% ggplot(aes(x=year)) +
  scale_x_continuous(breaks=seq(1980,2016,5)) +
  geom_hline(yintercept=100, linetype='dashed') +
  geom_point(aes(y=median, colour='Median')) +
  geom_line(aes(y=median, colour='Median')) +
  geom_point(aes(y=quantile_90, colour='90th Percentile')) +
  geom_line(aes(y=quantile_90, colour='90th Percentile')) +
  geom_point(aes(y=quantile_10, colour='10th Percentile')) +
  geom_line(aes(y=quantile_10, colour='10th Percentile')) +
  labs(x= 'Year', y='Real Hourly Wage, Indexed to 1980', 
       colour = '') +
  theme_classic()

# Title in LaTeX: Ratio of Hourly Wages Between 90th and 10th Percentiles, 1980-2016
CPS_ratio.data %>% ggplot(aes(x=year))+
  scale_x_continuous(breaks=seq(1980, 2016, 5))+
  geom_point(aes(y=ratio_9010)) +
  geom_line(aes(y=ratio_9010))  +
  labs(x= 'Year', y='Ratio of Hourly Wage') +
  theme_classic()



#######################################
## Regression tree, graphs for methods section.

# Log linear equation
# See paper for further explanation of the variable selection issue.
log_hourly_wage_equation <- log(rhrwage) ~ age + female + race  + 
  married + rural + suburb + centcity + selfemp + firmsz + education

# Tree model, early stage (1980-1985)
tree.model <- CPS.data %>% 
subset(year>1979 & year <1986) %>%
  rpart(log_hourly_wage_equation, data = ., model = T)
rpart.plot(tree.model, tweak=1.2) # Provide plot for methods section 

# Tree model, later stage (2010-2016)
tree.model <- CPS.data %>% 
  subset(year>2009 & year < 2017) %>%
  rpart(log_hourly_wage_equation, data = ., model = T)
rpart.plot(tree.model, tweak=1.2) # Provide plot for methods section



#####################################
## Graph of wage change by percentile and education level, 1980--2016
wage_educated <- wage_uneducated <- c()

wage_educated_1980 <- CPS.data %>% 
  filter(education >= 16 & year>=1980 & year <=1983) %>% 
  pull(rhrwage) %>% log()

wage_uneducated_1980 <- CPS.data %>% 
  filter(education < 16 & year>=1980 & year <=1983) %>% 
  pull(rhrwage) %>% log()

wage_educated_2016 <- CPS.data %>% 
  filter(education >= 16 & year>=2013 & year <=2016) %>% 
  pull(rhrwage) %>% log()

wage_uneducated_2016 <- CPS.data %>% 
  filter(education < 16 & year>=2013 & year <=2016) %>% 
  pull(rhrwage) %>% log()

### Go across every percentile, taking quantile change comparison
# newer period vs earlier period
percentile <- c(1:99)
for (i in percentile){
  print(i)
  educated_change_1980 <- quantile(wage_educated_1980, prob = i/100)
  educated_change_2016 <- quantile(wage_educated_2016, prob = i/100)
  wage_educated <- c(wage_educated, educated_change_2016 - educated_change_1980)
  
  uneducated_change_1980 <- quantile(wage_uneducated_1980, prob = i/100)
  uneducated_change_2016 <- quantile(wage_uneducated_2016, prob = i/100)
  wage_uneducated <- c(wage_uneducated, uneducated_change_2016 - uneducated_change_1980)
}
Wage_quantile.data <- data.frame(percentile, wage_educated, wage_uneducated)

# Draw a graph of quantile change by percentile.
Wage_quantile.data %>% 
  ggplot(aes(x = percentile)) + 
  geom_point(aes(y = wage_educated, colour = 'College graduates')) +
  geom_line(aes(y = wage_educated, colour = 'College graduates')) +
  geom_point(aes(y = wage_uneducated, colour = 'High school graduates')) +
  geom_line(aes(y = wage_uneducated, colour = 'High school graduates')) +
  geom_hline(yintercept=00, linetype='dashed') +
  labs(x= 'Percentile', y='Log Wage Change', colour = '') +
  theme_bw() +
  theme(legend.position=c(0.2, 0.8375), legend.background=element_blank()) +
  scale_x_continuous(breaks=seq(0, 100, 10)) +
  scale_y_continuous(breaks=seq(0, 0.8, 0.1))
