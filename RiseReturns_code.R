# 10/02/2018 Senan Hogan-H.

# This file works on the March CPS dataset.
# http://ceprdata.org/cps-uniform-data-extracts/march-cps-supplement/march-cps-data/

# The code looks to observe and decompose wage inequality 1980-2016.
# See corresponding research paper for use of this work.

set.seed(47)
library(tidyverse)
library(xtable)
library(caret)
library(rpart)
library(rpart.plot)
library(stargazer)
library(data.table)

# CPS.data <- fread('CPS__test_data.csv', header = T, sep = ',')
CPS.data <- fread('CPS_data.csv', header = T, sep = ',')


CPS.data <- CPS.data %>% subset(select = c(
  'year', # variable for year
  'fnlwgt', # weight variable
  'rhrwage', # real hourly wage, per person
  'inch_pct', # income percentile (20 is top, 1 bottom 5%) 
  'rincp_all', # real annual income, for person
  'rincp_ern', # real annual earnings (no unearned income), for person
  'rinch_all', # real annual income, for household
  'rinch_ern', # real annual earnings (no unearned income), for household
  'age', # age
  'female', # whether female
  'wbhao', # race
  'empl', # employment status, 1 is employed
  'educ92', # years education (only for after 92)
  'educ', # years education 
  'educ2', # years education 
  'citizen', # whether a citizen
  'married', # whether married
  'rural', # whether live in rural area
  'suburb', # whether live in suburbs
  'state', # which state they live in 
  'centcity', # whether they live in a central city
  'selfemp', # self-employed
  'unmem', # union membership
  'firmsz' # size of firm they work at  
  ))



# Normalise education variable for all years
CPS.data$education <- NA
# adjust educ2 variable for 1980-1991
CPS.data$education <- ifelse(CPS.data$year<1992 &
                               CPS.data$educ2=="", NA,
                             CPS.data$education)
CPS.data$education <- ifelse(CPS.data$year<1992 &
                               CPS.data$educ2=="Primary", 8,
                             CPS.data$education)
CPS.data$education <- ifelse(CPS.data$year<1992 &
                               CPS.data$educ2=="LTHS", 10,
                             CPS.data$education)
CPS.data$education <- ifelse(CPS.data$year<1992 &
                               CPS.data$educ2=="HS", 12,
                             CPS.data$education)
CPS.data$education <- ifelse(CPS.data$year<1992 &
                               CPS.data$educ2=="Some college", 14,
                             CPS.data$education)
CPS.data$education <- ifelse(CPS.data$year<1992 &
                               CPS.data$educ2=="College", 16,
                             CPS.data$education)
CPS.data$education <- ifelse(CPS.data$year<1992 &
                               CPS.data$educ2=="Advanced", 19,
                             CPS.data$education)


CPS.data$education <- ifelse(CPS.data$year>1991 &
                               CPS.data$educ92=="", NA,
                             CPS.data$education)
CPS.data$education <- ifelse(CPS.data$year>1991 &
                               CPS.data$educ92=="Less than 1st grade" , 0,
                             CPS.data$education)
CPS.data$education <- ifelse(CPS.data$year>1991 &
                               CPS.data$educ92=="1st-4th grade", 2.5,
                             CPS.data$education)
CPS.data$education <- ifelse(CPS.data$year>1991 &
                               CPS.data$educ92=="5th-6th grade", 5.5,
                             CPS.data$education)
CPS.data$education <- ifelse(CPS.data$year>1991 &
                               CPS.data$educ92=="7th-8th grade", 7.5,
                             CPS.data$education)
CPS.data$education <- ifelse(CPS.data$year>1991 &
                               CPS.data$educ92=="9th grade", 9,
                             CPS.data$education)
CPS.data$education <- ifelse(CPS.data$year>1991 &
                               CPS.data$educ92=="10th grade", 10,
                             CPS.data$education)
CPS.data$education <- ifelse(CPS.data$year>1991 &
                               CPS.data$educ92=="11th grade", 11,
                             CPS.data$education)
CPS.data$education <- ifelse(CPS.data$year>1991 &
                               CPS.data$educ92=="12th grade-no diploma", 11.5,
                             CPS.data$education)
CPS.data$education <- ifelse(CPS.data$year>1991 &
                               CPS.data$educ92=="HS graduate, GED", 12,
                             CPS.data$education)
CPS.data$education <- ifelse(CPS.data$year>1991 &
                               CPS.data$educ92=="Some college but no degree", 14,
                             CPS.data$education)
CPS.data$education <- ifelse(CPS.data$year>1991 &
                               CPS.data$educ92=="Associate degree-occupational/vocational", 14,
                             CPS.data$education)
CPS.data$education <- ifelse(CPS.data$year>1991 &
                               CPS.data$educ92=="Associate degree-academic program", 14,
                             CPS.data$education)
CPS.data$education <- ifelse(CPS.data$year>1991 &
                               CPS.data$educ92=="Bachelor's degree", 16,
                             CPS.data$education)
CPS.data$education <- ifelse(CPS.data$year>1991 &
                               CPS.data$educ92=="Professional school", 18,
                             CPS.data$education)
CPS.data$education <- ifelse(CPS.data$year>1991 &
                               CPS.data$educ92=="Master's degree", 18,
                             CPS.data$education)
CPS.data$education <- ifelse(CPS.data$year>1991 &
                               CPS.data$educ92=="Doctorate", 22,
                             CPS.data$education)

# Subset according to description in paper
CPS.data <- CPS.data %>% subset(empl > 0) # only employed people.
CPS.data <- CPS.data %>% subset(rhrwage >= 7.25) # people making more than minimum wage in 2011 $
CPS.data <- CPS.data %>% subset(rincp_all >= 0) # positive total yearly income
CPS.data <- CPS.data %>% subset(rincp_ern >= (7.25*40*14)) # year income above full time min wage, minimum 14 weeks
CPS.data <- CPS.data %>% subset(age >= 18 & age <= 65) # 18-65 years old

# Summary statistics table.

# Real wages are CPI-U-RS base 2015
CPS.data %>% subset(select = c(
  'rhrwage',
  'rincp_ern',
  'age',
  'education',
  'female')) %>% 
    stargazer(. , summary=TRUE,
              title="Summary Statistics, 1980-2016")

# CHANGE FOR POSITION IN INCOME DIST VARIABLE.

# forming data frame for median and indeed quantiles of hourly income.
# and for inequality ratios.  90-10 and 90-50 and 80-20
year <- median <- quantile_90 <- quantile_10 <- ratio_9010 <- c()
median_1980 <- median(subset(CPS.data, year==1980)$rincp_ern)
q_90_1980 <- quantile(subset(CPS.data, year==1980)$rincp_ern, 0.9)
q_10_1980 <- quantile(subset(CPS.data, year==1980)$rincp_ern, 0.1)
for (i in c(1980:2016)){
  year <- c(year, i)
  x <- subset(CPS.data, year==i)$rincp_ern
  median <- c(median, 100*(median(x)/median_1980))
  q_90 <- quantile(x, 0.9)
  q_10 <- quantile(x, 0.1)
  quantile_90 <- c(quantile_90, 100*(q_90/q_90_1980))
  quantile_10 <- c(quantile_10, 100*(q_10/q_10_1980))
  ratio_9010 <- c(ratio_9010, q_90/q_10)
}
CPS_ratio.data <- data.frame(year, ratio_9010, median, quantile_90, quantile_10)

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
  labs(x= 'Year', y='Real Annual Income, Indexed to 1980', 
       colour = '') +
  theme_classic()


# forming data frame for median and indeed quantiles of hourly wage.
# and for inequality ratios.  90-10 and 90-50 and 80-20
year <- median <- quantile_90 <- quantile_10 <- ratio_9010 <- c()
median_1980 <- median(subset(CPS.data, year==1980)$rhrwage)
q_90_1980 <- quantile(subset(CPS.data, year==1980)$rhrwage, 0.9)
q_10_1980 <- quantile(subset(CPS.data, year==1980)$rhrwage, 0.1)
for (i in c(1980:2016)){
  year <- c(year, i)
  x <- subset(CPS.data, year==i)$rhrwage
  median <- c(median, 100*(median(x)/median_1980))
  q_90 <- quantile(x, 0.9)
  q_10 <- quantile(x, 0.1)
  quantile_90 <- c(quantile_90, 100*(q_90/q_90_1980))
  quantile_10 <- c(quantile_10, 100*(q_10/q_10_1980))
  ratio_9010 <- c(ratio_9010, q_90/q_10)
}
CPS_ratio.data <- data.frame(year, ratio_9010, median, quantile_90, quantile_10)

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

# Graph for income ratios
CPS_ratio.data %>% ggplot(aes(x=year)) +
  geom_point(aes(y=ratio_9010)) +
  geom_line(aes(y=ratio_9010))  +
  theme_classic()

# ggplot theme + theme_classic()



# Prediction methods
# Mincer equation
CPS_mincer.reg <- CPS.data %>% lm(
  log(rhrwage) ~ education + I(age-education-6)+ I((age-education-6)^2), 
                                  data=.,
                                  weights=fnlwgt)

# regression tree
anova.model <- rpart(rhrwage~., data = CPS.data)
# , weights = fnlwgt
rpart.plot(anova.model, tweak=1.2)

rm(list = ls())
