# This makes a table of components in inequality by prediction method




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










# Prediction methods 3. Random Forest
set.seed(47)

CPS.data <- fread('CPS_data.csv', header = T, sep = ',')

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
        na.action = na.pass, importance = T,
        metric='RMSE')

Mincer_residuals_rhrwage <- log(CPS.data$rhrwage) - predict(CPS_mincer1.reg)

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






# Form LaTeX table
component_store <- rbind(component_store1, component_store2, component_store3)

tab <- xtable(component_store, comment=FALSE)
print(tab, type="latex", include.rownames = FALSE, include.colnames = FALSE)
