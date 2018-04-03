# Random Forest Variable Importance, by hourly wage
CPS.data <- fread('CPS_data.csv', header = T, sep = ',')
set.seed(47)

years <- c(c(1980:2007), c(2009:2016))
year <- r_squared_forest <- 
  education_importance <- age_importance <- female_importance <- race_importance <- c()

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
          tuneGrid = data.frame(mtry = c(4)),
          na.action = na.pass, importance = T,
          metric='RMSE')
  
  r_squared_forest <- c(r_squared_forest, 
                       as.numeric(CPS_forest.reg$results[2][1]))
  
  CPS_forest.Imp <- varImp(CPS_forest.reg, scale = FALSE)
  
  education_importance <- 
    c(education_importance, CPS_forest.Imp$importance$Overall[11])
  age_importance <- 
    c(age_importance, CPS_forest.Imp$importance$Overall[1])
  female_importance <- 
    c(female_importance, CPS_forest.Imp$importance$Overall[2])
  race_importance <- 
    c(race_importance, CPS_forest.Imp$importance$Overall[3])
}
data_frame(year, education_importance, 
           age_importance, female_importance) %>% 
  ggplot(aes(x = year)) +
  geom_point(aes(y = education_importance, 
                 colour = 'Education')) +
  geom_smooth(aes(y = education_importance, 
                  colour = 'Education'), method='lm') +
  geom_point(aes(y = age_importance, 
                 colour = 'Age')) +
  geom_smooth(aes(y = age_importance, 
                  colour = 'Age'), method='lm') +
  geom_point(aes(y = female_importance, 
                 colour = 'Gender')) +
  geom_smooth(aes(y = female_importance, 
                  colour = 'Gender'), method='lm') +
  geom_point(aes(y = race_importance, 
                 colour = 'Race')) +
  geom_smooth(aes(y = race_importance, 
                  colour = 'Race'), method='lm')
  labs(x= 'Year', y='Variable Importance', colour = '') +
  theme_classic()  + 
  theme(legend.position='bottom')
ggsave('importance_graph.png')







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
  geom_point(aes(y = r_squared_mincer_adj, colour = 'Mincer, adjusted')) +
  geom_point(aes(y = r_squared_forest, colour = 'Random forest'))  +
  labs(x= 'Year', y='R Squared', 
       colour = '') +
  theme_bw() +
  theme(legend.position=c( 0.8375, 0.2), legend.background=element_blank()) +
  scale_x_continuous(breaks=seq(1980,2016,5))
ggsave('rsquared.png')

