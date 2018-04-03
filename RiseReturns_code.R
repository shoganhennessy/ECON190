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

# CPS.data <- fread('CPS__test_data.csv', header = T, sep = ',')
CPS.data <- fread('CPS_data.csv', header = T, sep = ',')


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

# regression tree, graphs for data section.  
# This is to remove 'self-prediction' in ML regresison, i.e. income predicting income.
# See paper for further explanation of the variable selection issue.
log_wage_equation <- log(rhrwage) ~ age + female + race  + 
  married + rural + suburb + centcity + selfemp + firmsz + education

tree.model <- CPS.data %>% subset(year>1979 & year <1986) %>%
  rpart(log_wage_equation , data = .)
rpart.plot(tree.model, tweak=1.2) # Provide plot for methods section 

tree.model <- CPS.data %>% subset(select = 
                                    -c(inch_pct, rincp_all,
                                       rincp_ern, rinch_all, rinch_ern)) %>%
  subset(year>2009 & year < 2017) %>%
  rpart(log(rhrwage) ~ ., data = .)
rpart.plot(tree.model, tweak=1.2) # Provide plot for methods section





# Graph of estimated wage change by percentile and education, 1980--2016
wage_educated <- wage_uneducated <- c()

wage_educated_1980 <- log(subset(CPS.data, 
                             education >= 16 & year>=1980 & year <=1983)$rhrwage)
wage_uneducated_1980 <- log(subset(CPS.data, education >= 12 & 
                                   education < 16 & year>=1980 & year <=1983)$rhrwage)
wage_educated_2016 <- log(subset(CPS.data, 
                               education >= 16 & year>=2013 & year <=2016)$rhrwage)
wage_uneducated_2016 <- log(subset(CPS.data, education >= 12 & 
                               education < 16 & year>=2013 & year <=2016)$rhrwage)
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
data.frame(percentile, wage_educated, wage_uneducated) %>% 
  ggplot(aes(x = percentile)) + 
  geom_point(aes(y = wage_educated, colour = 'College graduates')) +
  geom_line(aes(y = wage_educated, colour = 'College graduates')) +
  geom_point(aes(y = wage_uneducated, colour = 'High school graduates')) +
  geom_line(aes(y = wage_uneducated, colour = 'High school graduates')) +
  geom_hline(yintercept=00, linetype='dashed') +
  labs(x= 'Percentile', y='Log Wage Change', colour = '') +
  theme_bw() +
  theme(legend.position=c( 0.2, 0.8375), legend.background=element_blank()) +
  scale_x_continuous(breaks=seq(0, 100, 10)) +
  scale_y_continuous(breaks=seq(0, 0.8, 0.1)) + 
  #coord_cartesian(xlim=c(10, 90))
ggsave('hour_wage_change.png')






# Graph of estimated wage change by percentile and education, 1980--2016
wage_educated <- wage_uneducated <- c()

wage_educated_1980 <- log(subset(CPS.data, 
                                 education >= 16 & year>=1980 & year <=1983)$rincp_ern)
wage_uneducated_1980 <- log(subset(CPS.data, education >= 12 & 
                                     education < 16 & year>=1980 & year <=1983)$rincp_ern)
wage_educated_2016 <- log(subset(CPS.data, 
                                 education >= 16 & year>=2013 & year <=2016)$rincp_ern)
wage_uneducated_2016 <- log(subset(CPS.data, education >= 12 & 
                                     education < 16 & year>=2013 & year <=2016)$rincp_ern)
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
data.frame(percentile, wage_educated, wage_uneducated) %>% 
  ggplot(aes(x = percentile)) + 
  geom_point(aes(y = wage_educated, colour = 'College graduates')) +
  geom_line(aes(y = wage_educated, colour = 'College graduates')) +
  geom_point(aes(y = wage_uneducated, colour = 'High school graduates')) +
  geom_line(aes(y = wage_uneducated, colour = 'High school graduates')) +
  geom_hline(yintercept=00, linetype='dashed') +
  labs(x= 'Percentile', y='Log Wage Change', colour = '') +
  theme_bw() +
  theme(legend.position=c( 0.2, 0.8375), legend.background=element_blank()) +
  scale_x_continuous(breaks=seq(0, 100, 10)) +
  scale_y_continuous(breaks=seq(0, 0.8, 0.1))  

ggsave('annual_wage_change.png')
  



# Residuals table
# Title : Inequality Measures Based on Regression Model Residuals for Hourly Wage
years <- c(1980, 1985, 1990, 1995, 2000, 2005, 2010)

# storer to make a matrix for a LaTeX table
residual_store <- c()

log_wage_equation <- log(rhrwage) ~ age + female + race + 
  married + rural + suburb + centcity + selfemp + education

set.seed(47)
for (i in years){
  print(i)
  CPS_subset.data <- subset(CPS.data, year>=i & year<(i+5))
  if (i==2010){
    CPS_subset.data <- subset(CPS.data, year>=i & year<(i+7))
  }
  
  # Model 1. Mincer Equation
  CPS_mincer.reg <- CPS_subset.data %>% 
    lm(log(rhrwage) ~ education + I(age-education-6) + I((age-education-6)^2), 
    data=.)
  errors <- CPS_mincer.reg$residuals
  sd <- sd(CPS_subset.data$Mincer_log_predictions)
  q_90 <- quantile(errors, probs = 0.9)  
  q_50 <- quantile(errors, probs = 0.5)  
  q_10 <- quantile(errors, probs = 0.1)  
  residual_store <- c(residual_store, sd, 
                      (q_90 - q_10), (q_90 - q_50), (q_50 - q_10))
    
  # Model 2. Adjusted Mincer
  CPS_mincer.reg <- CPS_subset.data %>%
    lm( log(rhrwage) ~ I(education) + I(education^2) +
          I(age-education-6) + I((age-education-6)^2) +
          I((age-education-6)^3) + I((age-education-6)^4), 
        data=.)
  errors <- CPS_mincer.reg$residuals
  sd <- sd(CPS_mincer.reg$fitted.values)
  q_90 <- quantile(errors, probs = 0.9)  
  q_50 <- quantile(errors, probs = 0.5)  
  q_10 <- quantile(errors, probs = 0.1)  
  residual_store <- c(residual_store, sd, 
                      (q_90 - q_10), (q_90 - q_50), (q_50 - q_10))

  # Model 3. Random Forest
  CPS_forest.reg <- CPS_subset.data %>%
    train(log_wage_equation ,
          preProcess=c('center', 'scale'),
          data = . , 
          method = 'rf' , 
          trControl = trainControl(method='oob'), 
          tuneGrid = data.frame(mtry = c(3)),
          na.action = na.pass, importance = T,
          metric='RMSE')
  predictions <- predict(CPS_forest.reg)
  errors <- log(CPS_subset.data$rhrwage) - predictions
  sd <- sd(predictions)
  sd <- sd(CPS_mincer.reg$fitted.values)
  q_90 <- quantile(errors, probs = 0.9)  
  q_50 <- quantile(errors, probs = 0.5)  
  q_10 <- quantile(errors, probs = 0.1)  
  residual_store <- c(residual_store, sd, 
                      (q_90 - q_10), (q_90 - q_50), (q_50 - q_10))  
  #store observation numbers
  residual_store <- c(residual_store, nrow(CPS_subset.data))  
}

residual_store <- format(round(residual_store, 2), nsmall = 2)

residual_store <- c('Model 1.', '', '', '',
  'Model 2.', '', '', '',
  'Model 3.', '', '', '', '\multicolumn{2}{l}{Observations:}',
  'S.d.', '90-10', '90-50', '50-10',
  'S.d.', '90-10', '90-50', '50-10',
  'S.d.', '90-10', '90-50', '50-10','', residual_store)

residual_store <- matrix(residual_store, nrow=13, ncol=9)
tab <- xtable(residual_store, comment=FALSE)
print(tab, type="latex", include.rownames = FALSE, include.colnames = FALSE)



#  First regression table 

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


# Appendix SUmmary Table.
stargazer(CPS.data , summary=TRUE,
            title="Extended Summary Statistics, 1980-2016")

