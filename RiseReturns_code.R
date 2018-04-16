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

# Residuals table
# Title : Inequality Measures Based on Regression Model Residuals for Hourly Wage


# Prediction methods 1. Mincer
CPS.data <- fread('CPS_data.csv', header = T, sep = ',')

Variable_residuals <- id <- c()

#Form model with
years <- c(c(1980:2007), c(2009:2016))
for (i in years){
  print(i)
  year <- c(year, i)
  CPS_mincer.reg <- CPS.data %>% subset(year==i) %>%
    lm( log(rhrwage) ~ education + I(age-education-6) + I((age-education-6)^2), 
        data=.)
  Variable_residuals <- c(Variable_residuals, 
                          CPS_mincer.reg$residuals)
  id <- c(id, subset(CPS.data, year==i)$id)
}

CPS.data <- dplyr::right_join(data_frame(Variable_residuals, id), 
                              CPS.data, by = 'id')
rm(Mincer_predicted_rhrwage, Variable_residuals, id)

# Form fixed residual distribution
CPS.data$Mincer_resid_percentile <- ecdf(
  CPS.data$Variable_residuals)(CPS.data$Variable_residuals)

CPS.data$Mincer_fixed_resid_rhrwage <- quantile(Mincer_residuals_rhrwage, 
                                                probs = CPS.data$Mincer_resid_percentile, 
                                                na.rm = TRUE,
                                                names = FALSE)

# Save Y1, Y2, Y3
CPS.data$Mincer_Y1_rhrwage <- predict(CPS_mincer1.reg, CPS.data)
#CPS.data$Mincer_fixed_resid_rhrwage

CPS.data$Mincer_Y2_rhrwage <- CPS.data$Mincer_predicted_rhrwage
#CPS.data$Mincer_fixed_resid_rhrwage

CPS.data$Mincer_Y3_rhrwage <- CPS.data$Mincer_predicted_rhrwage + 
  CPS.data$Variable_residuals

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
  errors <- CPS_subset.data$Variable_residuals
  sd <- sd(errors)
  q_90 <- quantile(errors, probs = 0.9)  
  q_50 <- quantile(errors, probs = 0.5)  
  q_10 <- quantile(errors, probs = 0.1)  
  residual_store <- c(residual_store, sd, 
                      (q_90 - q_10), (q_90 - q_50), (q_50 - q_10))
}

residual_store <- format(round(residual_store, 2), nsmall = 2)

residual_store1 <- c('Model 1.', '', '', '', 
                     'S.d.', '90-10', '90-50', '50-10',
                     residual_store)

residual_store1 <- matrix(residual_store1, nrow=4, ncol=9)
tab <- xtable(residual_store1, comment=FALSE)
print(tab, type="latex", include.rownames = FALSE, include.colnames = FALSE)









# Prediction methods 2. Adjusted Mincer
CPS.data <- fread('CPS_data.csv', header = T, sep = ',')

Variable_residuals <- id <- c()

#Form model with
years <- c(c(1980:2007), c(2009:2016))
for (i in years){
  print(i)
  year <- c(year, i)
  CPS_mincer.reg <- CPS.data %>% subset(year==i) %>%
    lm( log(rhrwage) ~ I(education) + I(education^2) +
          I(age-education-6) + I((age-education-6)^2) +
          I((age-education-6)^3) + I((age-education-6)^4), 
        data=.)
  Variable_residuals <- c(Variable_residuals, 
                          CPS_mincer.reg$residuals)
  id <- c(id, subset(CPS.data, year==i)$id)
}

CPS.data <- dplyr::right_join(data_frame(Variable_residuals, id), 
                              CPS.data, by = 'id')
rm(Variable_residuals, id)

years <- c(1980, 1985, 1990, 1995, 2000, 2005, 2010)

# storer to make a matrix for a LaTeX table
residual_store <- id <- c()

log_wage_equation <- log(rhrwage) ~ age + female + race + 
  married + rural + suburb + centcity + selfemp + education

set.seed(47)
for (i in years){
  print(i)
  CPS_subset.data <- subset(CPS.data, year>=i & year<(i+5))
  if (i==2010){
    CPS_subset.data <- subset(CPS.data, year>=i & year<(i+7))
  }
  errors <- CPS_subset.data$Variable_residuals
  sd <- sd(errors)
  q_90 <- quantile(errors, probs = 0.9)  
  q_50 <- quantile(errors, probs = 0.5)  
  q_10 <- quantile(errors, probs = 0.1)  
  residual_store <- c(residual_store, sd, 
                      (q_90 - q_10), (q_90 - q_50), (q_50 - q_10))
}

residual_store <- format(round(residual_store, 2), nsmall = 2)

residual_store1 <- c('Model 2.', '', '', '', 
                     'S.d.', '90-10', '90-50', '50-10',
                     residual_store)

residual_store1 <- matrix(residual_store1, nrow=4, ncol=9)
tab <- xtable(residual_store1, comment=FALSE)
print(tab, type="latex", include.rownames = FALSE, include.colnames = FALSE)







# Prediction methods 3. Random Forest
set.seed(47)
CPS.data <- fread('CPS_data.csv', header = T, sep = ',')
#CPS.data <- sample_n(CPS.data, 1000)

Variable_residuals <- id <- c()

log_wage_equation <- log(rhrwage) ~ age + female + race + 
  married + rural + suburb + centcity + selfemp + education

#Form model with
years <- c(c(1980:2007), c(2009:2016))
for (i in years){
  print(i)
  year <- c(year, i)
  CPS_mincer.reg <- CPS.data %>% subset(year==i) %>%
    train(log_wage_equation ,
          preProcess=c('center', 'scale'),
          data = . , 
          method = 'rf' , 
          trControl = trainControl(method='oob'), 
          tuneGrid = data.frame(mtry = c(4)),
          na.action = na.pass, importance = F,
          metric='RMSE')
  Variable_residuals <- c(Variable_residuals, 
                          log(subset(CPS.data, year==i)$rhrwage) - predict(CPS_mincer.reg))
  id <- c(id, subset(CPS.data, year==i)$id)
}

CPS.data <- dplyr::right_join(data_frame(Variable_residuals, id), 
                              CPS.data, by = 'id')
rm(Variable_residuals, id)

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
  errors <- CPS_subset.data$Variable_residuals
  sd <- sd(errors)
  q_90 <- quantile(errors, probs = 0.9)  
  q_50 <- quantile(errors, probs = 0.5)  
  q_10 <- quantile(errors, probs = 0.1)  
  residual_store <- c(residual_store, sd, 
                      (q_90 - q_10), (q_90 - q_50), (q_50 - q_10),
                      length(errors))
}

residual_store <- format(round(residual_store, 2), nsmall = 2)

residual_store1 <- c('Model 3.', '', '', '', 'Observations',
                     'S.d.', '90-10', '90-50', '50-10', '',
                     residual_store)

residual_store1 <- matrix(residual_store1, nrow=5, ncol=9)
tab <- xtable(residual_store1, comment=FALSE)
print(tab, type="latex", include.rownames = FALSE, include.colnames = FALSE)



This file works on the March CPS dataset.
# http://ceprdata.org/cps-uniform-data-extracts/march-cps-supplement/march-cps-data/

# The code looks to observe and decompose wage inequality 1980-2016.
# This is the lengthy decomposition phase,
# elaborating on JMP 1993, updated with different prediction methods


gc()
ls()
set.seed(47)
library(tidyverse)
library(xtable)
library(caret)
library(stargazer)
library(data.table)

# Prediction methods 1. Mincer
CPS.data <- fread('CPS_data.csv', header = T, sep = ',')

# Form static model for time period 1980-1984
CPS_mincer1.reg <- CPS.data %>%
  lm( log(rhrwage) ~ education + I(age-education-6) + I((age-education-6)^2),
      data=.)

Mincer_residuals_rhrwage <- CPS_mincer1.reg$residuals

Mincer_predicted_rhrwage <- id <- Variable_residuals <- c()

#Form model with
years <- c(c(1980:2007), c(2009:2016))
for (i in years){
  print(i)
  year <- c(year, i)
  CPS_mincer.reg <- CPS.data %>% subset(year==i) %>%
    lm( log(rhrwage) ~ education + I(age-education-6) + I((age-education-6)^2), 
        data=.)
  Mincer_predicted_rhrwage <- c(Mincer_predicted_rhrwage,
                                CPS_mincer.reg$fitted.values)
  Variable_residuals <- c(Variable_residuals, 
                          CPS_mincer.reg$residuals)
  id <- c(id, subset(CPS.data, year==i)$id)
}

CPS.data <- dplyr::right_join(data_frame(Mincer_predicted_rhrwage, 
                                         Variable_residuals, id), 
                              CPS.data, by = 'id')
rm(Mincer_predicted_rhrwage, Variable_residuals, id)

# Form fixed residual distribution
CPS.data$Mincer_resid_percentile <- ecdf(
  CPS.data$Variable_residuals)(CPS.data$Variable_residuals)

CPS.data$Mincer_fixed_resid_rhrwage <- quantile(Mincer_residuals_rhrwage, 
                                                probs = CPS.data$Mincer_resid_percentile, 
                                                na.rm = TRUE,
                                                names = FALSE)

# Save Y1, Y2, Y3
CPS.data$Mincer_Y1_rhrwage <- predict(CPS_mincer1.reg, CPS.data)
#CPS.data$Mincer_fixed_resid_rhrwage

CPS.data$Mincer_Y2_rhrwage <- CPS.data$Mincer_predicted_rhrwage
#CPS.data$Mincer_fixed_resid_rhrwage

CPS.data$Mincer_Y3_rhrwage <- CPS.data$Mincer_predicted_rhrwage + 
  CPS.data$Variable_residuals

# Graph of components
years <- c(c(1980:2007), c(2009:2016))
year <- quantity_diff_9010 <- returns_diff_9010 <- Y3_diff_9010 <- errors_diff_9010 <- c()

for (i in years){
  print(i)
  year <- c(year, i)
  Y3 <- subset(CPS.data, year==i)$Mincer_Y3_rhrwage
  Y3_90 <- quantile(Y3, probs=0.9)
  Y3_10 <- quantile(Y3, probs=0.1)
  Y3_diff_9010 <- c(Y3_diff_9010, Y3_90 - Y3_10)            
  
  quantity <- subset(CPS.data, year==i)$Mincer_Y1_rhrwage
  quantity_90 <- quantile(quantity, probs=0.9)
  quantity_10 <- quantile(quantity, probs=0.1)
  quantity_diff_9010 <- c(quantity_diff_9010, (quantity_90 - quantity_10)) 
  
  returns <- subset(CPS.data, year==i)$Mincer_Y2_rhrwage
  returns_90 <- quantile(returns, probs=0.9)
  returns_10 <- quantile(returns, probs=0.1)
  returns_diff_9010 <- c(returns_diff_9010, (returns_90 - returns_10) - 
                           (quantity_90 - quantity_10))
  
  errors <- subset(CPS.data, year==i)$Mincer_Y3_rhrwage -
    subset(CPS.data, year==i)$Mincer_Y2_rhrwage
  errors_90 <- quantile(errors, probs=0.9)
  errors_10 <- quantile(errors, probs=0.1)
  errors_diff_9010 <- c(errors_diff_9010, (errors_90 - errors_10))
}

quantity_diff_9010 <- quantity_diff_9010 - mean(quantity_diff_9010)
returns_diff_9010 <- returns_diff_9010 - mean(returns_diff_9010)
errors_diff_9010 <- errors_diff_9010 - mean(errors_diff_9010)

CPS_Y.data <- data.frame(year, Y3_diff_9010, quantity_diff_9010, 
                         returns_diff_9010, errors_diff_9010)

CPS_Y.data %>% ggplot(aes(x=year)) +
  geom_point(aes(y=Y3_diff_9010)) +
  geom_line(aes(y=Y3_diff_9010)) +
  labs(x= 'Year', y='90-10 Log Wage Differential', 
       colour = '') +
  theme_bw() +
  theme(legend.position="top") +
  scale_x_continuous(breaks=seq(1980,2016,5)) +
  scale_y_continuous(breaks=seq(1.3,1.6,0.05), minor_breaks=seq(1.3,1.6,0.025)) + 
  coord_cartesian(ylim=c(1.3,1.6))

ggsave('graph1.png')

CPS_Y.data %>% ggplot(aes(x=year)) +
  geom_point(aes(y=quantity_diff_9010, colour='Characteristics')) +
  geom_line(aes(y=quantity_diff_9010, colour='Characteristics')) +
  geom_point(aes(y=returns_diff_9010, colour='Returns')) +
  geom_line(aes(y=returns_diff_9010, colour='Returns')) +
  geom_point(aes(y=errors_diff_9010, colour='Unobserved')) +
  geom_line(aes(y=errors_diff_9010, colour='Unobserved')) +
  geom_hline(yintercept=00, linetype='dashed') +
  labs(x= 'Year', y='Contribution to 90-10 Log Wage Differential', 
       colour = '') +
  theme_bw() +
  theme(legend.position=c( 0.8375, 0.2), legend.background=element_blank()) +
  scale_x_continuous(breaks=seq(1980,2016,5)) +
  scale_y_continuous(breaks=seq(-0.12, 0.12, 0.04)) + coord_cartesian(ylim=c(-0.12, 0.12))
ggsave('edit1.png')




























# Prediction methods 2. Adjusted Mincer
CPS.data <- fread('CPS_data.csv', header = T, sep = ',')

# Form static model
CPS_mincer1.reg <- CPS.data %>%
  lm( log(rhrwage) ~ I(education) + I(education^2) +
        I(age-education-6) + I((age-education-6)^2) +
        I((age-education-6)^3) + I((age-education-6)^4), 
      data=.)
Mincer_residuals_rhrwage <- CPS_mincer1.reg$residuals

Mincer_predicted_rhrwage <- id <- Variable_residuals <- c()

#Form model with
years <- c(c(1980:2007), c(2009:2016))
for (i in years){
  print(i)
  year <- c(year, i)
  CPS_mincer.reg <- CPS.data %>% subset(year==i) %>%
    lm( log(rhrwage) ~ I(education) + I(education^2) +
          I(age-education-6) + I((age-education-6)^2) +
          I((age-education-6)^3) + I((age-education-6)^4), 
        data=.)
  Mincer_predicted_rhrwage <- c(Mincer_predicted_rhrwage,
                                CPS_mincer.reg$fitted.values)
  Variable_residuals <- c(Variable_residuals, 
                          CPS_mincer.reg$residuals)
  id <- c(id, subset(CPS.data, year==i)$id)
}

CPS.data <- dplyr::right_join(data_frame(Mincer_predicted_rhrwage, 
                                         Variable_residuals, id), 
                              CPS.data, by = 'id')
rm(Mincer_predicted_rhrwage, Variable_residuals, id)

# Form fixed residual distribution
CPS.data$Mincer_resid_percentile <- ecdf(
  CPS.data$Variable_residuals)(CPS.data$Variable_residuals)

CPS.data$Mincer_fixed_resid_rhrwage <- quantile(Mincer_residuals_rhrwage, 
                                                probs = CPS.data$Mincer_resid_percentile, 
                                                na.rm = TRUE,
                                                names = FALSE)

# Save Y1, Y2, Y3
CPS.data$Mincer_Y1_rhrwage <- predict(CPS_mincer1.reg, CPS.data)
#CPS.data$Mincer_fixed_resid_rhrwage

CPS.data$Mincer_Y2_rhrwage <- CPS.data$Mincer_predicted_rhrwage
#CPS.data$Mincer_fixed_resid_rhrwage

CPS.data$Mincer_Y3_rhrwage <- CPS.data$Mincer_predicted_rhrwage + 
  CPS.data$Variable_residuals

# Graph of components
years <- c(c(1980:2007), c(2009:2016))
year <- quantity_diff_9010 <- returns_diff_9010 <- Y3_diff_9010 <- errors_diff_9010 <- c()

for (i in years){
  print(i)
  year <- c(year, i)
  Y3 <- subset(CPS.data, year==i)$Mincer_Y3_rhrwage
  Y3_90 <- quantile(Y3, probs=0.9)
  Y3_10 <- quantile(Y3, probs=0.1)
  Y3_diff_9010 <- c(Y3_diff_9010, Y3_90 - Y3_10)            
  
  quantity <- subset(CPS.data, year==i)$Mincer_Y1_rhrwage
  quantity_90 <- quantile(quantity, probs=0.9)
  quantity_10 <- quantile(quantity, probs=0.1)
  quantity_diff_9010 <- c(quantity_diff_9010, (quantity_90 - quantity_10)) 
  
  returns <- subset(CPS.data, year==i)$Mincer_Y2_rhrwage
  returns_90 <- quantile(returns, probs=0.9)
  returns_10 <- quantile(returns, probs=0.1)
  returns_diff_9010 <- c(returns_diff_9010, (returns_90 - returns_10) - 
                           (quantity_90 - quantity_10))
  
  errors <- subset(CPS.data, year==i)$Mincer_Y3_rhrwage -
    subset(CPS.data, year==i)$Mincer_Y2_rhrwage
  errors_90 <- quantile(errors, probs=0.9)
  errors_10 <- quantile(errors, probs=0.1)
  errors_diff_9010 <- c(errors_diff_9010, (errors_90 - errors_10))
}

quantity_diff_9010 <- quantity_diff_9010 - mean(quantity_diff_9010)
returns_diff_9010 <- returns_diff_9010 - mean(returns_diff_9010)
errors_diff_9010 <- errors_diff_9010 - mean(errors_diff_9010)

CPS_Y.data <- data.frame(year, Y3_diff_9010, quantity_diff_9010, 
                         returns_diff_9010, errors_diff_9010)

CPS_Y.data %>% ggplot(aes(x=year)) +
  geom_point(aes(y=quantity_diff_9010, colour='Characteristics')) +
  geom_line(aes(y=quantity_diff_9010, colour='Characteristics')) +
  geom_point(aes(y=returns_diff_9010, colour='Returns')) +
  geom_line(aes(y=returns_diff_9010, colour='Returns')) +
  geom_point(aes(y=errors_diff_9010, colour='Unobserved')) +
  geom_line(aes(y=errors_diff_9010, colour='Unobserved')) +
  geom_hline(yintercept=00, linetype='dashed') +
  labs(x= 'Year', y='Contribution to 90-10 Log Wage Differential', 
       colour = '') +
  theme_bw() +
  theme(legend.position=c( 0.8375, 0.2), legend.background=element_blank()) +
  scale_x_continuous(breaks=seq(1980,2016,5)) +
  scale_y_continuous(breaks=seq(-0.12, 0.12, 0.04)) + coord_cartesian(ylim=c(-0.12, 0.12))
ggsave('edit2.png')















# Prediction methods 3. Random Forest
rm(list=ls())
gc()
set.seed(47)

CPS.data <- fread('CPS_data.csv', header = T, sep = ',')
CPS.data <- sample_n(CPS.data, 1500000) 
component_store3 <- c()

log_wage_equation <- log(rhrwage) ~ age + female + race +
  married + rural + suburb + centcity + selfemp + education

# Form static model
CPS_mincer1.reg <- CPS.data %>% 
  train(log_wage_equation ,
        preProcess=c('center', 'scale'),
        data = . , 
        method = 'rf' , 
        trControl = trainControl(method='oob'), 
        tuneGrid = data.frame(mtry = c(3)),
        importance = F,
        metric='RMSE')

CPS.data <- fread('CPS_data.csv', header = T, sep = ',')

Mincer_residuals_rhrwage <- log(CPS.data$rhrwage) - predict(CPS_mincer1.reg, 
                                                            newdata = CPS.data)

Mincer_predicted_rhrwage <- id <- Variable_residuals <- c()

#Form model with varying returns
years <- c(c(1980:2007), c(2009:2016))
for (i in years){
  print(i)
  year <- c(year, i)
  CPS_mincer.reg <- CPS.data %>% subset(year==i) %>%
    train(log_wage_equation ,
          preProcess=c('center', 'scale'),
          data = . , 
          method = 'rf' , 
          trControl = trainControl(method='oob'), 
          tuneGrid = data.frame(mtry = c(4)),
          na.action = na.pass, importance = F,
          metric='RMSE')
  Mincer_predicted_rhrwage <- c(Mincer_predicted_rhrwage,
                                predict(CPS_mincer.reg))
  Variable_residuals <- c(Variable_residuals, 
                          log(subset(CPS.data, year==i)$rhrwage) - predict(CPS_mincer.reg))
  id <- c(id, subset(CPS.data, year==i)$id)
  rm(CPS_mincer.reg)
  gc()
}

CPS.data <- dplyr::right_join(data_frame(Mincer_predicted_rhrwage, 
                                         Variable_residuals, id), 
                              CPS.data, by = 'id')
rm(Mincer_predicted_rhrwage, Variable_residuals, id)

# Form fixed residual distribution
CPS.data$Mincer_resid_percentile <- ecdf(
  CPS.data$Variable_residuals)(CPS.data$Variable_residuals)

CPS.data$Mincer_fixed_resid_rhrwage <- quantile(Mincer_residuals_rhrwage, 
                                                probs = CPS.data$Mincer_resid_percentile, 
                                                na.rm = TRUE,
                                                names = FALSE)

# Save Y1, Y2, Y3
CPS.data$Mincer_Y1_rhrwage <- predict(CPS_mincer1.reg, CPS.data)
#CPS.data$Mincer_fixed_resid_rhrwage

CPS.data$Mincer_Y2_rhrwage <- CPS.data$Mincer_predicted_rhrwage
#CPS.data$Mincer_fixed_resid_rhrwage

CPS.data$Mincer_Y3_rhrwage <- CPS.data$Mincer_predicted_rhrwage + 
  CPS.data$Variable_residuals

# Graph of components
years <- c(c(1980:2007), c(2009:2016))
year <- quantity_diff_9010 <- returns_diff_9010 <- Y3_diff_9010 <- errors_diff_9010 <- c()

for (i in years){
  print(i)
  year <- c(year, i)
  Y3 <- subset(CPS.data, year==i)$Mincer_Y3_rhrwage
  Y3_90 <- quantile(Y3, probs=0.9)
  Y3_10 <- quantile(Y3, probs=0.1)
  Y3_diff_9010 <- c(Y3_diff_9010, Y3_90 - Y3_10)            
  
  quantity <- subset(CPS.data, year==i)$Mincer_Y1_rhrwage
  quantity_90 <- quantile(quantity, probs=0.9)
  quantity_10 <- quantile(quantity, probs=0.1)
  quantity_diff_9010 <- c(quantity_diff_9010, (quantity_90 - quantity_10)) 
  
  returns <- subset(CPS.data, year==i)$Mincer_Y2_rhrwage
  returns_90 <- quantile(returns, probs=0.9)
  returns_10 <- quantile(returns, probs=0.1)
  returns_diff_9010 <- c(returns_diff_9010, (returns_90 - returns_10) - 
                           (quantity_90 - quantity_10))
  
  errors <- subset(CPS.data, year==i)$Mincer_Y3_rhrwage -
    subset(CPS.data, year==i)$Mincer_Y2_rhrwage
  errors_90 <- quantile(errors, probs=0.9)
  errors_10 <- quantile(errors, probs=0.1)
  errors_diff_9010 <- c(errors_diff_9010, (errors_90 - errors_10))
}

quantity_diff_9010 <- quantity_diff_9010 - mean(quantity_diff_9010)
returns_diff_9010 <- returns_diff_9010 - mean(returns_diff_9010)
errors_diff_9010 <- errors_diff_9010 - mean(errors_diff_9010)

CPS_Y.data <- data.frame(year, Y3_diff_9010, quantity_diff_9010, 
                         returns_diff_9010, errors_diff_9010)

CPS_Y.data %>% ggplot(aes(x=year)) +
  geom_point(aes(y=quantity_diff_9010, colour='Characteristics')) +
  geom_line(aes(y=quantity_diff_9010, colour='Characteristics')) +
  geom_point(aes(y=returns_diff_9010, colour='Returns')) +
  geom_line(aes(y=returns_diff_9010, colour='Returns')) +
  geom_point(aes(y=errors_diff_9010, colour='Unobserved')) +
  geom_line(aes(y=errors_diff_9010, colour='Unobserved')) +
  geom_hline(yintercept=00, linetype='dashed') +
  labs(x= 'Year', y='Contribution to 90-10 Log Wage Differential', 
       colour = '') +
  theme_bw() +
  theme(legend.position=c( 0.8375, 0.2), legend.background=element_blank()) +
  scale_x_continuous(breaks=seq(1980,2016,5)) +
  scale_y_continuous(breaks=seq(-0.12, 0.12, 0.04)) + coord_cartesian(ylim=c(-0.12, 0.12))
ggsave('edit3.png')

# This makes a table of components in inequality by prediction method

# Prediction methods 3. Random Forest
rm(list=ls())
gc()
set.seed(47)

CPS.data <- fread('CPS_data.csv', header = T, sep = ',')
CPS.data <- sample_n(CPS.data, 1500000) 
component_store3 <- c()

log_wage_equation <- log(rhrwage) ~ age + female + race +
  married + rural + suburb + centcity + selfemp + education

# Form static model
CPS_mincer1.reg <- CPS.data %>% 
  train(log_wage_equation ,
        preProcess=c('center', 'scale'),
        data = . , 
        method = 'rf' , 
        trControl = trainControl(method='oob'), 
        tuneGrid = data.frame(mtry = c(3)),
        importance = F,
        metric='RMSE')

CPS.data <- fread('CPS_data.csv', header = T, sep = ',')

Mincer_residuals_rhrwage <- log(CPS.data$rhrwage) - predict(CPS_mincer1.reg, 
                                                            newdata = CPS.data)

Mincer_predicted_rhrwage <- id <- Variable_residuals <- c()

#Form model with varying returns
set.seed(47)
years <- c(c(1980:2007), c(2009:2016))
for (i in years){
  print(i)
  year <- c(year, i)
  CPS_mincer.reg <- CPS.data %>% subset(year==i) %>%
    train(log_wage_equation ,
          preProcess=c('center', 'scale'),
          data = . , 
          method = 'rf' , 
          trControl = trainControl(method='oob'), 
          tuneGrid = data.frame(mtry = c(4)),
          na.action = na.pass, importance = F,
          metric='RMSE')
  Mincer_predicted_rhrwage <- c(Mincer_predicted_rhrwage,
                                predict(CPS_mincer.reg))
  Variable_residuals <- c(Variable_residuals, 
                          log(subset(CPS.data, year==i)$rhrwage) - predict(CPS_mincer.reg))
  id <- c(id, subset(CPS.data, year==i)$id)
  rm(CPS_mincer.reg)
  gc()
}

CPS.data <- dplyr::right_join(data_frame(Mincer_predicted_rhrwage, 
                                         Variable_residuals, id), 
                              CPS.data, by = 'id')
rm(Mincer_predicted_rhrwage, Variable_residuals, id)

# Form fixed residual distribution
CPS.data$Mincer_resid_percentile <- ecdf(
  CPS.data$Variable_residuals)(CPS.data$Variable_residuals)

CPS.data$Mincer_fixed_resid_rhrwage <- quantile(Mincer_residuals_rhrwage, 
                                                probs = CPS.data$Mincer_resid_percentile, 
                                                na.rm = TRUE,
                                                names = FALSE)

# Save Y1, Y2, Y3
CPS.data$Mincer_Y1_rhrwage <- predict(CPS_mincer1.reg, CPS.data)
#CPS.data$Mincer_fixed_resid_rhrwage

CPS.data$Mincer_Y2_rhrwage <- CPS.data$Mincer_predicted_rhrwage
#CPS.data$Mincer_fixed_resid_rhrwage

CPS.data$Mincer_Y3_rhrwage <- CPS.data$Mincer_predicted_rhrwage + 
  CPS.data$Variable_residuals

q_90_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y3_rhrwage, 
                      probs = 0.9) 

q_10_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y3_rhrwage, 
                      probs = 0.1)

q_50_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y3_rhrwage, 
                      probs = 0.5)

q_90_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y3_rhrwage, 
                      probs = 0.9) 

q_10_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y3_rhrwage, 
                      probs = 0.1)

q_50_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y3_rhrwage, 
                      probs = 0.5)


component_store3 <- c(component_store3, 
                      (q_90_2016 - q_10_2016) - (q_90_1980 - q_10_1980), 
                      (q_90_2016 - q_50_2016) - (q_90_1980 - q_50_1980),
                      (q_50_2016 - q_10_2016) - (q_50_1980 - q_10_1980))

q_90_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y1_rhrwage, 
                      probs = 0.9) 

q_10_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y1_rhrwage, 
                      probs = 0.1)

q_50_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y1_rhrwage, 
                      probs = 0.5)

q_90_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y1_rhrwage, 
                      probs = 0.9) 

q_10_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y1_rhrwage, 
                      probs = 0.1)

q_50_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y1_rhrwage, 
                      probs = 0.5)


component_store3 <- c(component_store3, 
                      (q_90_2016 - q_10_2016) - (q_90_1980 - q_10_1980), 
                      (q_90_2016 - q_50_2016) - (q_90_1980 - q_50_1980),
                      (q_50_2016 - q_10_2016) - (q_50_1980 - q_10_1980))

q_90_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y2_rhrwage - 
                        subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y1_rhrwage, 
                      probs = 0.9) 

q_10_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y2_rhrwage - 
                        subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y1_rhrwage, 
                      probs = 0.1)

q_50_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y2_rhrwage - 
                        subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y1_rhrwage, 
                      probs = 0.5)

q_90_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y2_rhrwage - 
                        subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y1_rhrwage,
                      probs = 0.9) 

q_10_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y2_rhrwage - 
                        subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y1_rhrwage, 
                      probs = 0.1)

q_50_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y2_rhrwage - 
                        subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y1_rhrwage, 
                      probs = 0.5)


component_store3 <- c(component_store3, 
                      (q_90_2016 - q_10_2016) - (q_90_1980 - q_10_1980), 
                      (q_90_2016 - q_50_2016) - (q_90_1980 - q_50_1980),
                      (q_50_2016 - q_10_2016) - (q_50_1980 - q_10_1980))

q_90_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y3_rhrwage - 
                        subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y2_rhrwage, 
                      probs = 0.9) 

q_10_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y3_rhrwage - 
                        subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y2_rhrwage, 
                      probs = 0.1)

q_50_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y3_rhrwage - 
                        subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y2_rhrwage, 
                      probs = 0.5)

q_90_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y3_rhrwage - 
                        subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y2_rhrwage,
                      probs = 0.9) 

q_10_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y3_rhrwage - 
                        subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y2_rhrwage, 
                      probs = 0.1)

q_50_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y3_rhrwage - 
                        subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y2_rhrwage, 
                      probs = 0.5)


component_store3 <- c(component_store3, 
                      (q_90_2016 - q_10_2016) - (q_90_1980 - q_10_1980), 
                      (q_90_2016 - q_50_2016) - (q_90_1980 - q_50_1980),
                      (q_50_2016 - q_10_2016) - (q_50_1980 - q_10_1980))

component_store3 <- format(round(component_store3, 3), nsmall = 3)

component_store3 <- c('Model 3.', '', '',
                      '90-10', '90-50', '50-10', component_store3)

component_store3  <- matrix(component_store3, nrow=3, ncol=6)
component_store3 

#[1,] "Model 3." "90-10" " 0.264" " 0.091" "-0.043" " 0.229"
#[2,] ""         "90-50" " 0.193" " 0.044" "-0.018" " 0.148"
#[3,] ""         "50-10" " 0.070" " 0.047" "-0.024" " 0.081"


# Prediction methods 1. Mincer
CPS.data <- fread('CPS_data.csv', header = T, sep = ',')
component_store1 <- c()

# Form static model for time period 1980-1984
CPS_mincer1.reg <- CPS.data %>% lm(
  log(rhrwage) ~ education + I(age-education-6) + I((age-education-6)^2), 
  data=.)

Mincer_residuals_rhrwage <- CPS_mincer1.reg$residuals

Mincer_predicted_rhrwage <- id <- Variable_residuals <- c()

#Form varying model
years <- c(c(1980:2007), c(2009:2016))
for (i in years){
  print(i)
  year <- c(year, i)
  CPS_mincer.reg <- CPS.data %>% subset(year==i) %>%
    lm( log(rhrwage) ~ education + I(age-education-6) + I((age-education-6)^2), 
        data=.)
  Mincer_predicted_rhrwage <- c(Mincer_predicted_rhrwage,
                                CPS_mincer.reg$fitted.values)
  Variable_residuals <- c(Variable_residuals, 
                          CPS_mincer.reg$residuals)
  id <- c(id, subset(CPS.data, year==i)$id)
}

CPS.data <- dplyr::right_join(data_frame(Mincer_predicted_rhrwage, 
                                         Variable_residuals, id), 
                              CPS.data, by = 'id')
rm(Mincer_predicted_rhrwage, Variable_residuals, id)

# Form fixed residual distribution
CPS.data$Mincer_resid_percentile <- ecdf(
  CPS.data$Variable_residuals)(CPS.data$Variable_residuals)

CPS.data$Mincer_fixed_resid_rhrwage <- quantile(Mincer_residuals_rhrwage, 
                                                probs = CPS.data$Mincer_resid_percentile, 
                                                na.rm = TRUE,
                                                names = FALSE)

# Save Y1, Y2, Y3
CPS.data$Mincer_Y1_rhrwage <- predict(CPS_mincer1.reg, CPS.data)

CPS.data$Mincer_Y2_rhrwage <- CPS.data$Mincer_predicted_rhrwage

CPS.data$Mincer_Y3_rhrwage <- CPS.data$Mincer_predicted_rhrwage + 
  CPS.data$Variable_residuals


q_90_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y3_rhrwage, 
                      probs = 0.9) 

q_10_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y3_rhrwage, 
                      probs = 0.1)

q_50_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y3_rhrwage, 
                      probs = 0.5)

q_90_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y3_rhrwage, 
                      probs = 0.9) 

q_10_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y3_rhrwage, 
                      probs = 0.1)

q_50_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y3_rhrwage, 
                      probs = 0.5)


component_store1 <- c(component_store1, 
                      (q_90_2016 - q_10_2016) - (q_90_1980 - q_10_1980), 
                      (q_90_2016 - q_50_2016) - (q_90_1980 - q_50_1980),
                      (q_50_2016 - q_10_2016) - (q_50_1980 - q_10_1980))

q_90_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y1_rhrwage, 
                      probs = 0.9) 

q_10_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y1_rhrwage, 
                      probs = 0.1)

q_50_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y1_rhrwage, 
                      probs = 0.5)

q_90_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y1_rhrwage, 
                      probs = 0.9) 

q_10_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y1_rhrwage, 
                      probs = 0.1)

q_50_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y1_rhrwage, 
                      probs = 0.5)


component_store1 <- c(component_store1, 
                      (q_90_2016 - q_10_2016) - (q_90_1980 - q_10_1980), 
                      (q_90_2016 - q_50_2016) - (q_90_1980 - q_50_1980),
                      (q_50_2016 - q_10_2016) - (q_50_1980 - q_10_1980))

q_90_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y2_rhrwage - 
                        subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y1_rhrwage, 
                      probs = 0.9) 

q_10_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y2_rhrwage - 
                        subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y1_rhrwage, 
                      probs = 0.1)

q_50_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y2_rhrwage - 
                        subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y1_rhrwage, 
                      probs = 0.5)

q_90_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y2_rhrwage - 
                        subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y1_rhrwage,
                      probs = 0.9) 

q_10_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y2_rhrwage - 
                        subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y1_rhrwage, 
                      probs = 0.1)

q_50_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y2_rhrwage - 
                        subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y1_rhrwage, 
                      probs = 0.5)


component_store1 <- c(component_store1, 
                      (q_90_2016 - q_10_2016) - (q_90_1980 - q_10_1980), 
                      (q_90_2016 - q_50_2016) - (q_90_1980 - q_50_1980),
                      (q_50_2016 - q_10_2016) - (q_50_1980 - q_10_1980))

q_90_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y3_rhrwage - 
                        subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y2_rhrwage, 
                      probs = 0.9) 

q_10_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y3_rhrwage - 
                        subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y2_rhrwage, 
                      probs = 0.1)

q_50_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y3_rhrwage - 
                        subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y2_rhrwage, 
                      probs = 0.5)

q_90_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y3_rhrwage - 
                        subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y2_rhrwage,
                      probs = 0.9) 

q_10_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y3_rhrwage - 
                        subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y2_rhrwage, 
                      probs = 0.1)

q_50_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y3_rhrwage - 
                        subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y2_rhrwage, 
                      probs = 0.5)


component_store1 <- c(component_store1, 
                      (q_90_2016 - q_10_2016) - (q_90_1980 - q_10_1980), 
                      (q_90_2016 - q_50_2016) - (q_90_1980 - q_50_1980),
                      (q_50_2016 - q_10_2016) - (q_50_1980 - q_10_1980))

component_store1 <- format(round(component_store1, 3), nsmall = 3)

component_store1 <- c('Model 1.', '', '',
                      '90-10', '90-50', '50-10', component_store1)

component_store1  <- matrix(component_store1, nrow=3, ncol=6)
component_store1









# Prediction methods 2. Mincer, adjusted
CPS.data <- fread('CPS_data.csv', header = T, sep = ',')
component_store2 <- c()

# Form static model for time period 1980-1984
CPS_mincer1.reg <- CPS.data %>% 
  lm( log(rhrwage) ~ I(education) + I(education^2) +
        I(age-education-6) + I((age-education-6)^2) +
        I((age-education-6)^3) + I((age-education-6)^4), 
      data=.)

Mincer_residuals_rhrwage <- CPS_mincer1.reg$residuals

Mincer_predicted_rhrwage <- id <- Variable_residuals <- c()

#Form varying model
years <- c(c(1980:2007), c(2009:2016))
for (i in years){
  print(i)
  year <- c(year, i)
  CPS_mincer.reg <- CPS.data %>% subset(year==i) %>%
    lm( log(rhrwage) ~ I(education) + I(education^2) +
          I(age-education-6) + I((age-education-6)^2) +
          I((age-education-6)^3) + I((age-education-6)^4), 
        data=.)
  Mincer_predicted_rhrwage <- c(Mincer_predicted_rhrwage,
                                CPS_mincer.reg$fitted.values)
  Variable_residuals <- c(Variable_residuals, 
                          CPS_mincer.reg$residuals)
  id <- c(id, subset(CPS.data, year==i)$id)
}

CPS.data <- dplyr::right_join(data_frame(Mincer_predicted_rhrwage, 
                                         Variable_residuals, id), 
                              CPS.data, by = 'id')
rm(Mincer_predicted_rhrwage, Variable_residuals, id)

# Form fixed residual distribution
CPS.data$Mincer_resid_percentile <- ecdf(
  CPS.data$Variable_residuals)(CPS.data$Variable_residuals)

CPS.data$Mincer_fixed_resid_rhrwage <- quantile(Mincer_residuals_rhrwage, 
                                                probs = CPS.data$Mincer_resid_percentile, 
                                                na.rm = TRUE,
                                                names = FALSE)

# Save Y1, Y2, Y3
CPS.data$Mincer_Y1_rhrwage <- predict(CPS_mincer1.reg, CPS.data)

CPS.data$Mincer_Y2_rhrwage <- CPS.data$Mincer_predicted_rhrwage

CPS.data$Mincer_Y3_rhrwage <- CPS.data$Mincer_predicted_rhrwage + 
  CPS.data$Variable_residuals


q_90_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y3_rhrwage, 
                      probs = 0.9) 

q_10_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y3_rhrwage, 
                      probs = 0.1)

q_50_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y3_rhrwage, 
                      probs = 0.5)

q_90_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y3_rhrwage, 
                      probs = 0.9) 

q_10_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y3_rhrwage, 
                      probs = 0.1)

q_50_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y3_rhrwage, 
                      probs = 0.5)


component_store2 <- c(component_store2, 
                      (q_90_2016 - q_10_2016) - (q_90_1980 - q_10_1980), 
                      (q_90_2016 - q_50_2016) - (q_90_1980 - q_50_1980),
                      (q_50_2016 - q_10_2016) - (q_50_1980 - q_10_1980))

q_90_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y1_rhrwage, 
                      probs = 0.9) 

q_10_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y1_rhrwage, 
                      probs = 0.1)

q_50_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y1_rhrwage, 
                      probs = 0.5)

q_90_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y1_rhrwage, 
                      probs = 0.9) 

q_10_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y1_rhrwage, 
                      probs = 0.1)

q_50_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y1_rhrwage, 
                      probs = 0.5)


component_store2 <- c(component_store2, 
                      (q_90_2016 - q_10_2016) - (q_90_1980 - q_10_1980), 
                      (q_90_2016 - q_50_2016) - (q_90_1980 - q_50_1980),
                      (q_50_2016 - q_10_2016) - (q_50_1980 - q_10_1980))

q_90_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y2_rhrwage - 
                        subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y1_rhrwage, 
                      probs = 0.9) 

q_10_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y2_rhrwage - 
                        subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y1_rhrwage, 
                      probs = 0.1)

q_50_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y2_rhrwage - 
                        subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y1_rhrwage, 
                      probs = 0.5)

q_90_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y2_rhrwage - 
                        subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y1_rhrwage,
                      probs = 0.9) 

q_10_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y2_rhrwage - 
                        subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y1_rhrwage, 
                      probs = 0.1)

q_50_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y2_rhrwage - 
                        subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y1_rhrwage, 
                      probs = 0.5)


component_store2 <- c(component_store2, 
                      (q_90_2016 - q_10_2016) - (q_90_1980 - q_10_1980), 
                      (q_90_2016 - q_50_2016) - (q_90_1980 - q_50_1980),
                      (q_50_2016 - q_10_2016) - (q_50_1980 - q_10_1980))

q_90_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y3_rhrwage - 
                        subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y2_rhrwage, 
                      probs = 0.9) 

q_10_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y3_rhrwage - 
                        subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y2_rhrwage, 
                      probs = 0.1)

q_50_1980 <- quantile(subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y3_rhrwage - 
                        subset(CPS.data, year>=1980 & year<=1983)$Mincer_Y2_rhrwage, 
                      probs = 0.5)

q_90_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y3_rhrwage - 
                        subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y2_rhrwage,
                      probs = 0.9) 

q_10_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y3_rhrwage - 
                        subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y2_rhrwage, 
                      probs = 0.1)

q_50_2016 <- quantile(subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y3_rhrwage - 
                        subset(CPS.data, year>=2013 & year<=2016)$Mincer_Y2_rhrwage, 
                      probs = 0.5)


component_store2 <- c(component_store2, 
                      (q_90_2016 - q_10_2016) - (q_90_1980 - q_10_1980), 
                      (q_90_2016 - q_50_2016) - (q_90_1980 - q_50_1980),
                      (q_50_2016 - q_10_2016) - (q_50_1980 - q_10_1980))

component_store2 <- format(round(component_store2, 3), nsmall = 3)

component_store2 <- c('Model 2.', '', '',
                      '90-10', '90-50', '50-10', component_store2)

component_store2  <- matrix(component_store2, nrow=3, ncol=6)

















# Form LaTeX table
component_store <- rbind(component_store1, component_store2, component_store3)

tab <- xtable(component_store, comment=FALSE)
print(tab, type="latex", include.rownames = FALSE, include.colnames = FALSE)

# Random Forest Variable Importance, by hourly wage
CPS.data <- fread('CPS_data.csv', header = T, sep = ',')
#CPS.data <- sample_n(CPS.data, 1000)
set.seed(47)

years <- c(c(1980:2007), c(2009:2016))
year <- r_squared_forest <- 
  education_importance_scaled <- age_importance_scaled <- female_importance_scaled <- 
  education_importance <- age_importance <- female_importance <- 
  race_importance <- race_importance_scaled <- c()

log_wage_equation <- log(rhrwage) ~ age + female + race + 
  married + rural + suburb + centcity + selfemp + education

for (i in years){
  print(i)
  year <- c(year, i)
  CPS_forest.reg <- CPS.data %>% subset(year==i) %>% 
    train(log_wage_equation ,
          preProcess=c('center', 'scale'),
          data = . , 
          method = 'rf' , 
          trControl = trainControl(method='oob'), 
          tuneGrid = data.frame(mtry = c(3)),
          na.action = na.pass, importance = T,
          metric='RMSE')
  
  r_squared_forest <- c(r_squared_forest, 
                        as.numeric(CPS_forest.reg$results[2][1]))
  
  CPS_forest.Imp <- varImp(CPS_forest.reg, scale = FALSE)
  
  education_importance <- 
    c(education_importance, CPS_forest.Imp$importance$Overall[9])
  age_importance <- 
    c(age_importance, CPS_forest.Imp$importance$Overall[1])
  female_importance <- 
    c(female_importance, CPS_forest.Imp$importance$Overall[2])
  race_importance <-
    c(race_importance, CPS_forest.Imp$importance$Overall[3])
  
  CPS_forest.Imp <- varImp(CPS_forest.reg, scale = TRUE)
  
  education_importance_scaled <- 
    c(education_importance_scaled, CPS_forest.Imp$importance$Overall[9])
  age_importance_scaled <- 
    c(age_importance_scaled, CPS_forest.Imp$importance$Overall[1])
  female_importance_scaled <- 
    c(female_importance_scaled, CPS_forest.Imp$importance$Overall[2])
  race_importance_scaled <-
    c(race_importance_scaled, CPS_forest.Imp$importance$Overall[3])
}
data_frame(year, education_importance, 
           age_importance, female_importance, race_importance) %>% 
  ggplot(aes(x = year)) +
  geom_point(aes(y = education_importance, 
                 colour = 'Education')) +
  geom_smooth(aes(y = education_importance, 
                  colour = 'Education'), method='lm', se=F) +
  geom_point(aes(y = age_importance, 
                 colour = 'Age')) +
  geom_smooth(aes(y = age_importance, 
                  colour = 'Age'), method='lm', se=F) +
  geom_point(aes(y = female_importance, 
                 colour = 'Gender')) +
  geom_smooth(aes(y = female_importance, 
                  colour = 'Gender'), method='lm', se=F) +
  geom_point(aes(y = race_importance, 
                 colour = 'Race')) +
  geom_smooth(aes(y = race_importance, 
                  colour = 'Race'), method='lm', se=F) +
  labs(x= 'Year', y='Variable Importance', colour = '') +
  theme_classic()  + 
  theme(legend.position='bottom')
ggsave('importance_graph.png')

data_frame(year, education_importance_scaled , 
           age_importance_scaled , female_importance_scaled ) %>% 
  ggplot(aes(x = year)) +
  geom_point(aes(y = education_importance_scaled , 
                 colour = 'Education')) +
  geom_smooth(aes(y = education_importance_scaled , 
                  colour = 'Education'), method='lm', se=F) +
  geom_point(aes(y = age_importance_scaled , 
                 colour = 'Age')) +
  geom_smooth(aes(y = age_importance_scaled , 
                  colour = 'Age'), method='lm', se=F) +
  geom_point(aes(y = female_importance_scaled , 
                 colour = 'Gender')) +
  geom_smooth(aes(y = female_importance_scaled , 
                  colour = 'Gender'), method='lm', se=F) +
  geom_point(aes(y = race_importance_scaled , 
                 colour = 'Race')) +
  geom_smooth(aes(y = race_importance_scaled , 
                  colour = 'Race'), method='lm', se=F) +
  labs(x= 'Year', y=' Scaled Variable Importance', colour = '') +
  theme_classic()  + 
  theme(legend.position='bottom')
ggsave('importance_graph_scaled.png')





# Comparing R-squared values.
years <- c(c(1980:2007), c(2009:2016))
year <- r_squared_mincer <- r_squared_mincer_adj <- c()
for (i in years){
  print(i)
  year <- c(year, i)
  
  CPS_mincer.reg <- CPS.data %>% subset(year==i) %>%
    lm( log(rhrwage) ~ education + I(age-education-6) + I((age-education-6)^2), 
        data=.)
  
  r_squared_mincer <- c(r_squared_mincer, summary(CPS_mincer.reg)$r.squared)
  
  CPS_mincer_adj.reg <- CPS.data %>% subset(year==i) %>%
    lm( log(rhrwage) ~ I(education) + I(education^2) +
          I(age-education-6) + I((age-education-6)^2) +
          I((age-education-6)^3) + I((age-education-6)^4), 
        data=.)
  
  r_squared_mincer_adj <- c(r_squared_mincer_adj, summary(CPS_mincer_adj.reg)$r.squared)
  
}
data_frame(year, r_squared_mincer, r_squared_mincer_adj, r_squared_forest) %>% 
  ggplot(aes(x = year)) +
  geom_point(aes(y = r_squared_mincer, colour = 'Mincer')) +
  geom_smooth(aes(y = r_squared_mincer , 
                  colour = 'Mincer'), method='lm', se=F) +
  geom_point(aes(y = r_squared_mincer_adj, colour = 'Mincer, adjusted')) +
  geom_smooth(aes(y = r_squared_mincer_adj , 
                  colour = 'Mincer, adjusted'), method='lm', se=F) +
  geom_point(aes(y = r_squared_forest, colour = 'Random forest')) +
  geom_smooth(aes(y = r_squared_forest , 
                  colour = 'Random forest'), method='lm', se=F) +
  labs(x= 'Year', y='R-Squared', 
       colour = '') +
  theme_bw() +
  theme(legend.position=c( 0.8375, 0.2), legend.background=element_blank()) +
  scale_x_continuous(breaks=seq(1980,2016,5))
ggsave('rsquared.png')


