
# This file works on the March CPS dataset.
# http://ceprdata.org/cps-uniform-data-extracts/march-cps-supplement/march-cps-data/

# The code looks to observe and decompose wage inequality 1980-2016.
# This is the lengthy decomposition JMP 1993, updated with different rediction methods

# Prediction methods 1. Mincer
CPS.data <- fread('CPS_data.csv', header = T, sep = ',')

# Form static model for time period 1980-1984
CPS_mincer1.reg <- CPS.data %>% subset(year >=1980 & year<1985)%>% lm(
  log(rhrwage) ~ education + I(age-education-6) + I((age-education-6)^2), 
  data=.)

CPS.data$Mincer_Ybar_rhrwage <- 
  predict(CPS_mincer1.reg)

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
CPS.data$Mincer_Y1_rhrwage <- predict(CPS_mincer1.reg, CPS.data) + 
  CPS.data$Mincer_fixed_resid_rhrwage

CPS.data$Mincer_Y2_rhrwage <- CPS.data$Mincer_predicted_rhrwage + 
  CPS.data$Mincer_fixed_resid_rhrwage

CPS.data$Mincer_Y3_rhrwage <- CPS.data$Mincer_predicted_rhrwage + 
  CPS.data$Variable_residuals

# Graph of components
years <- c(c(1985:2007), c(2009:2016))
year <- quantity_diff_9010 <- returns_diff_9010 <- Y3_diff_9010 <- errors_diff_9010 <- c()

for (i in years){
  print(i)
  year <- c(year, i)
  Y3 <- subset(CPS.data, year==i)$Mincer_Y3_rhrwage
  Y3_90 <- quantile(Y3, probs=0.9)
  Y3_10 <- quantile(Y3, probs=0.1)
  Y3_diff_9010 <- c(Y3_diff_9010, Y3_90 - Y3_10)            
  
  quantity <- subset(CPS.data, year==i)$Mincer_Y1_rhrwage -
    subset(CPS.data, year==i)$Mincer_Ybar_rhrwage
  quantity_90 <- quantile(quantity, probs=0.9)
  quantity_10 <- quantile(quantity, probs=0.1)
  quantity_diff_9010 <- c(quantity_diff_9010, (quantity_90 - quantity_10)) 
  
  returns <- subset(CPS.data, year==i)$Mincer_Y2_rhrwage -
    (subset(CPS.data, year==i)$Mincer_Y1_rhrwage -
       subset(CPS.data, year==i)$Mincer_Ybar_rhrwage)
  returns_90 <- quantile(returns, probs=0.9)
  returns_10 <- quantile(returns, probs=0.1)
  returns_diff_9010 <- c(returns_diff_9010, (returns_90 - returns_10))
  
  errors <- subset(CPS.data, year==i)$Mincer_Y3_rhrwage - 
    subset(CPS.data, year==i)$Mincer_Y2_rhrwage
  errors_90 <- quantile(errors, probs=0.9)
  errors_10 <- quantile(errors, probs=0.1)
  errors_diff_9010 <- c(errors_diff_9010, (errors_90 - errors_10) )
}
quantity_diff_9010
returns_diff_9010
errors_diff_9010
mean(quantity_diff_9010)
mean(returns_diff_9010)
mean(errors_diff_9010)


#quantity_diff_9010 <- quantity_diff_9010 - mean(quantity_diff_9010)
#returns_diff_9010 <- returns_diff_9010 - mean(returns_diff_9010)
#errors_diff_9010 <- errors_diff_9010 - mean(errors_diff_9010)
mean(Y3_diff_9010 - quantity_diff_9010 - returns_diff_9010 - errors_diff_9010 )

CPS_Y.data <- data.frame(year, Y3_diff_9010, quantity_diff_9010, 
                         returns_diff_9010, errors_diff_9010)

CPS_Y.data %>% ggplot(aes(x=year)) +
  scale_x_continuous(breaks=seq(1985,2016,5)) +
  geom_point(aes(y=quantity_diff_9010, colour='Charcteristics')) +
  geom_line(aes(y=quantity_diff_9010, colour='Charcteristics')) +
  geom_point(aes(y=returns_diff_9010, colour='Returns')) +
  geom_line(aes(y=returns_diff_9010, colour='Returns')) +
  geom_point(aes(y=errors_diff_9010, colour='Unobserved')) +
  geom_line(aes(y=errors_diff_9010, colour='Unobserved')) +
  #geom_point(aes(y=Y3_diff_9010, colour='Total')) +
  #geom_line(aes(y=Y3_diff_9010, colour='Total')) +  
  labs(x= 'Year', y='Relative Component', 
       colour = '') +
  theme_classic() + 
  theme(legend.position="top")





# Prediction methods 2. Adjusted Mincer
rm(list=ls())
CPS.data <- fread('CPS__test_data.csv', header = T, sep = ',')

CPS_mincer1.reg <- CPS.data %>% subset(year >=1980 & year<=1985) %>%
  lm( log(rhrwage) ~ I(education) + I(education^2) +
        I(age-education-6) + I((age-education-6)^2) +
        I((age-education-6)^3) + I((age-education-6)^4), 
      data=.)

CPS.data$Mincer_Ybar_rhrwage <- mean(log(subset(CPS.data, year >=1980 & year<=1985)$rhrwage))

Mincer_residuals_rhrwage <- CPS_mincer1.reg$residuals

Mincer_predicted_rhrwage <- id <- Variable_residuals <- c()

#Form varying model
years <- c(c(1980:2007), c(2009:2016))
for (i in years){
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

predictions.data <- data_frame(Mincer_predicted_rhrwage, Variable_residuals, id)

CPS.data <- dplyr::right_join(predictions.data, CPS.data, by = 'id')

variable_residuals <- Variable_residuals

# Form fixed residual distribution
CPS.data <- CPS.data %>% mutate(Mincer_resid_precentile = 
                                  ecdf(Variable_residuals)(variable_residuals))

CPS.data$Mincer_residuals_rhrwage <- quantile(Mincer_residuals_rhrwage, 
                                              probs = CPS.data$Mincer_resid_precentile)

# Save Ybar, Y1, Y2, Y3
CPS.data$Mincer_Y1_rhrwage <- predict(CPS_mincer1.reg, CPS.data) + 
  CPS.data$Mincer_residuals_rhrwage

CPS.data$Mincer_Y2_rhrwage <- CPS.data$Mincer_predicted_rhrwage + 
  CPS.data$Mincer_residuals_rhrwage

CPS.data$Mincer_Y3_rhrwage <- CPS.data$Mincer_predicted_rhrwage + 
  CPS.data$Variable_residuals

# Graph of components
years <- c(c(1980:2007), c(2009:2016))
year <- quantity_diff_9010 <- returns_diff_9010 <- Y3_diff_9010 <- errors_diff_9010 <- c()

for (i in years){
  year <- c(year, i)
  Y3 <- subset(CPS.data, year==i)$Mincer_Y3_rhrwage
  Y3_90 <- quantile(Y3, probs=0.9)
  Y3_10 <- quantile(Y3, probs=0.1)
  Y3_diff_9010 <- c(Y3_diff_9010, Y3_90 - Y3_10)            
  
  quantity <- subset(CPS.data, year==i)$Mincer_Y1_rhrwage
  quantity_90 <- quantile(quantity, probs=0.9)
  quantity_10 <- quantile(quantity, probs=0.1)
  quantity_diff_9010 <- c(quantity_diff_9010, quantity_90 - quantity_10 - 1 ) 
  
  returns <- subset(CPS.data, year==i)$Mincer_Y2_rhrwage - 
    subset(CPS.data, year==i)$Mincer_Y1_rhrwage
  returns_90 <- quantile(returns, probs=0.9)
  returns_10 <- quantile(returns, probs=0.1)
  returns_diff_9010 <- c(returns_diff_9010, returns_90 - returns_10)
  
  errors <- subset(CPS.data, year==i)$Mincer_Y3_rhrwage - 
    subset(CPS.data, year==i)$Mincer_Y2_rhrwage
  errors_90 <- quantile(errors, probs=0.9)
  errors_10 <- quantile(errors, probs=0.1)
  errors_diff_9010 <- c(errors_diff_9010, errors_90 - errors_10  - 1 )
}
CPS_Y.data <- data.frame(year, quantity_diff_9010, 
                         returns_diff_9010, errors_diff_9010)

CPS_Y.data %>% ggplot(aes(x=year)) +
  scale_x_continuous(breaks=seq(1980,2016,5)) +
  scale_y_continuous(breaks=seq(0,1,0.2)) +
  geom_point(aes(y=quantity_diff_9010, colour='Charcteristics')) +
  geom_line(aes(y=quantity_diff_9010, colour='Charcteristics')) +
  geom_point(aes(y=returns_diff_9010, colour='Returns')) +
  geom_line(aes(y=returns_diff_9010, colour='Returns')) +
  geom_point(aes(y=errors_diff_9010, colour='Unobserved')) +
  geom_line(aes(y=errors_diff_9010, colour='Unobserved')) +  
  labs(x= 'Year', y='Relative Component', 
       colour = '') +
  theme_classic() + 
  theme(legend.position="top")



# Prediction methods 3. Random Forest
rm(list=ls())
CPS.data <- fread('CPS__test_data.csv', header = T, sep = ',')
log_wage_equation <- log(rhrwage) ~ age + female + race + citizen + 
  married + rural + suburb + centcity + selfemp + unmem + education


CPS_mincer1.reg <- CPS.data %>% subset(year >=1980 & year<=1985) %>%
  train(log_wage_equation ,
        preProcess=c('center', 'scale'),
        data = . , 
        method = 'rf' , 
        trControl = trainControl(method='oob'), 
        tuneGrid = data.frame(mtry = c(1:21)),
        na.action = na.pass, importance = T,
        metric='RMSE')

CPS.data$Mincer_Ybar_rhrwage <- mean(log(subset(CPS.data, year >=1980 & year<=1985)$rhrwage))

Mincer_residuals_rhrwage <- log(subset(CPS.data, year >=1980 & year<=1985)$rhrwage) - 
  predict(CPS_mincer1.reg)

Mincer_predicted_rhrwage <- id <- Variable_residuals <- c()

#Form varying model
years <- c(c(1980:2007), c(2009:2016))
for (i in years){
  print(i)
  year <- c(year, i)
  CPS_mincer.reg <- CPS.data %>% subset(year ==i) %>%
    train(log_wage_equation ,
          preProcess=c('center', 'scale'),
          data = . , 
          method = 'rf' , 
          trControl = trainControl(method='oob'), 
          tuneGrid = data.frame(mtry = c(1:21)),
          na.action = na.pass, importance = T,
          metric='RMSE')
  Mincer_predicted_rhrwage <- c(Mincer_predicted_rhrwage,
                                predict(CPS_mincer.reg))
  Variable_residuals <- c(Variable_residuals, 
                          log(subset(CPS.data, year==i)$rhrwage) - 
                            predict(CPS_mincer.reg))
  id <- c(id, subset(CPS.data, year==i)$id)
}

predictions.data <- data_frame(Mincer_predicted_rhrwage, Variable_residuals, id)

CPS.data <- dplyr::right_join(predictions.data, CPS.data, by = 'id')

variable_residuals <- Variable_residuals

# Form fixed residual distribution
CPS.data <- CPS.data %>% mutate(Mincer_resid_precentile = 
                                  ecdf(Variable_residuals)(variable_residuals))

CPS.data$Mincer_residuals_rhrwage <- quantile(Mincer_residuals_rhrwage, 
                                              probs = CPS.data$Mincer_resid_precentile)

# Save Ybar, Y1, Y2, Y3
CPS.data$Mincer_Y1_rhrwage <- predict(CPS_mincer1.reg, CPS.data) + 
  CPS.data$Mincer_residuals_rhrwage

CPS.data$Mincer_Y2_rhrwage <- CPS.data$Mincer_predicted_rhrwage + 
  CPS.data$Mincer_residuals_rhrwage

CPS.data$Mincer_Y3_rhrwage <- CPS.data$Mincer_predicted_rhrwage + 
  CPS.data$Variable_residuals

# Graph of components
years <- c(c(1980:2007), c(2009:2016))
year <- quantity_diff_9010 <- returns_diff_9010 <- Y3_diff_9010 <- errors_diff_9010 <- c()

for (i in years){
  year <- c(year, i)
  Y3 <- subset(CPS.data, year==i)$Mincer_Y3_rhrwage
  Y3_90 <- quantile(Y3, probs=0.9)
  Y3_10 <- quantile(Y3, probs=0.1)
  Y3_diff_9010 <- c(Y3_diff_9010, Y3_90 - Y3_10)            
  
  quantity <- subset(CPS.data, year==i)$Mincer_Y1_rhrwage
  quantity_90 <- quantile(quantity, probs=0.9)
  quantity_10 <- quantile(quantity, probs=0.1)
  quantity_diff_9010 <- c(quantity_diff_9010, quantity_90 - quantity_10 - 1 ) 
  
  returns <- subset(CPS.data, year==i)$Mincer_Y2_rhrwage - 
    subset(CPS.data, year==i)$Mincer_Y1_rhrwage
  returns_90 <- quantile(returns, probs=0.9)
  returns_10 <- quantile(returns, probs=0.1)
  returns_diff_9010 <- c(returns_diff_9010, returns_90 - returns_10)
  
  errors <- subset(CPS.data, year==i)$Mincer_Y3_rhrwage - 
    subset(CPS.data, year==i)$Mincer_Y2_rhrwage
  errors_90 <- quantile(errors, probs=0.9)
  errors_10 <- quantile(errors, probs=0.1)
  errors_diff_9010 <- c(errors_diff_9010, errors_90 - errors_10  - 1 )
}
quantity_diff_9010 <- quantity_diff_9010 - mean(quantity_diff_9010)
returns_diff_9010 <- returns_diff_9010 - mean(returns_diff_9010)
errors_diff_9010 <- errors_diff_9010 - mean(errors_diff_9010)

CPS_Y.data <- data.frame(year, quantity_diff_9010, 
                         returns_diff_9010, errors_diff_9010)

CPS_Y.data %>% ggplot(aes(x=year)) +
  scale_x_continuous(breaks=seq(1980,2016,5)) +
  scale_y_continuous(breaks=seq(0,1,0.2)) +
  geom_point(aes(y=quantity_diff_9010, colour='Charcteristics')) +
  geom_line(aes(y=quantity_diff_9010, colour='Charcteristics')) +
  geom_point(aes(y=returns_diff_9010, colour='Returns')) +
  geom_line(aes(y=returns_diff_9010, colour='Returns')) +
  geom_point(aes(y=errors_diff_9010, colour='Unobserved')) +
  geom_line(aes(y=errors_diff_9010, colour='Unobserved')) +  
  labs(x= 'Year', y='Relative Component', 
       colour = '') +
  theme_classic() + 
  theme(legend.position="top")