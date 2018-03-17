# 10/02/2018 (begun) Senan Hogan-H.

# This file works on the March CPS dataset.
# http://ceprdata.org/cps-uniform-data-extracts/march-cps-supplement/march-cps-data/

# The code looks to observe and decompose wage inequality 1980-2016.
# See corresponding research paper for use of this work.

gc()
ls()
set.seed(47)
library(tidyverse)
library(xtable)
library(caret)
library(rpart)
library(rpart.plot)
library(stargazer)
library(data.table)

CPS.data <- fread('CPS__test_data.csv', header = T, sep = ',')
# CPS.data <- fread('CPS_data.csv', header = T, sep = ',')


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

# forming data frame for median and indeed quantiles of hourly income.
# and for inequality ratios.  90-10 and 90-50 and 80-20

years <- c(c(1980:2007), c(2009:2016))
year <- median <- quantile_90 <- quantile_10 <- ratio_9010 <- ratio_8020 <- c()

median_1980 <- quantile(subset(CPS.data, year==1980)$rincp_ern, 
                                     probs = 0.5)

q_90_1980 <- quantile(subset(CPS.data, year==1980)$rincp_ern, 
                      probs = 0.9) 

q_10_1980 <- quantile(subset(CPS.data, year==1980)$rincp_ern, 
                      probs = 0.1)

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



# Graph for income ratios

# Title in LaTeX:
# Ratio of Annual Income Between 90th and 10th Percentiles, 1980-2016
CPS_ratio.data %>% ggplot(aes(x=year))+
  scale_x_continuous(breaks=seq(1980, 2016, 5))+
  #scale_y_continuous(breaks=seq(3, 5, 0.25)) +
  geom_point(aes(y=ratio_9010)) +
  geom_line(aes(y=ratio_9010))  +
  #geom_point(aes(y=ratio_8020)) +
  #geom_line(aes(y=ratio_8020))  +
  labs(x= 'Year', y='Ratio of Annual Income') +
  theme_classic()


# forming data frame for median and indeed quantiles of hourly wage.

years <- c(c(1980:2007), c(2009:2016))
year <- median <- quantile_90 <- quantile_10 <- ratio_9010 <- ratio_8020 <- c()

median_1980 <- quantile(subset(CPS.data, year==1980)$rhrwage, 
                        probs = 0.5)

q_90_1980 <- quantile(subset(CPS.data, year==1980)$rhrwage, 
                      probs = 0.9) 

q_10_1980 <- quantile(subset(CPS.data, year==1980)$rhrwage, 
                      probs = 0.1)

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

# Title in LaTeX:
# Ratio of Wage Between 90th and 10th Percentiles, 1980-2016
CPS_ratio.data %>% ggplot(aes(x=year))+
  scale_x_continuous(breaks=seq(1980, 2016, 5))+
  scale_y_continuous(breaks=seq(3, 5, 0.25)) +
  geom_point(aes(y=ratio_9010)) +
  geom_line(aes(y=ratio_9010))  +
  # geom_point(aes(y=ratio_8020)) +
  # geom_line(aes(y=ratio_8020))  +
  labs(x= 'Year', y='Ratio of Hourly Wage') +
  theme_classic()


# Prediction methods 1.

# Mincer equation, by hourly wage
CPS_mincer1.reg <- CPS.data %>% lm(
  log(rhrwage) ~ education + I(age-education-6) + I((age-education-6)^2), 
  data=.)

# Mincer equation, by annual income
CPS_mincer2.reg <- CPS.data %>% lm(
  log(rincp_ern) ~ education + I(age-education-6) + I((age-education-6)^2), 
  data=.)
stargazer(CPS_mincer1.reg, CPS_mincer2.reg,
          title = 'Mincer Equation Results',
          covariate.labels = c('Years education', 'Potential experience',
                               '(Potential experience)$^2$'),
          #dep.var.caption  = 'Income Measure',
          dep.var.labels   = c('Log hourly wage', 'Log annual income'),
          omit.stat=c("LL","ser","f"),
          header = FALSE, float = FALSE, no.space = TRUE)


# Prediction methods 2.

years <- c(c(1980:2007), c(2009:2016))
year <- sd_wage <- sd_wage_unexplained <- c()

# Mincer equation adjusted, by hourly wage
for (i in years){
  year <- c(year, i)
  CPS_mincer.reg <- CPS.data %>% subset(year==i) %>%
    lm( log(rhrwage) ~ I(education) + I(education^2) +
          I(age-education-6) + I((age-education-6)^2) +
          I((age-education-6)^3) + I((age-education-6)^4), 
        data=.)
  sd_wage_unexplained <- c(sd_wage_unexplained,
                           sd(CPS_mincer.reg$residuals))
  sd_wage <- c(sd_wage, sd(CPS_mincer.reg$fitted.values))
}
data_frame(year, sd_wage) %>% ggplot(aes(x = year)) +
  geom_point(aes(y =  sd_wage, colour = 'SD predicted wage')) +
  geom_smooth(aes(y =  sd_wage, colour = 'SD predicted wage'), method='lm') +
  geom_point(aes(y =  sd_wage_unexplained, colour = 'SD residuals')) +
  geom_smooth(aes(y =  sd_wage_unexplained, colour = 'SD residuals'), method='lm') +
  theme_classic()



# Mincer equation adjusted, by annual income
CPS_mincer2.reg <- CPS.data %>% lm(
  log(rhrwage) ~ ., data=.)

# regression tree, graphs for data section.  
# This is to remove 'self-prediction' in ML regresison, i.e. income predicting income.
# See paper for further explanation of the variable selection issue.
anova.model <- CPS.data %>% subset(select = 
                                     -c(inch_pct, rincp_all,
                                        rincp_ern, rinch_all, rinch_ern)) %>%
  subset(year>1979 & year <1981) %>%
  rpart(log(rhrwage) ~ ., data = .)
rpart.plot(anova.model, tweak=1.2) # Provide plots 
