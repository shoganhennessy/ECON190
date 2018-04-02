# Random Forest Variable Importance, by hourly wage
set.seed(47)

years <- c(c(1980:2007), c(2009:2016))
year <- r_squared <- 
  education_importance <- age_importance <- female_importance <- c()

log_wage_equation <- log(rhrwage) ~ age + female + race + citizen + 
  married + rural + suburb + centcity + selfemp + unmem + education

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
  
  r_squared_forest <- (r_squared_forest, )
  
  CPS_forest.Imp <- varImp(CPS_forest.reg, scale = FALSE)
  
  education_importance <- 
    c(education_importance, CPS_forest.Imp$importance$Overall[11])
  age_importance <- 
    c(age_importance, CPS_forest.Imp$importance$Overall[1])
  female_importance <- 
    c(female_importance, CPS_forest.Imp$importance$Overall[2])
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
  labs(x= 'Year', y='Variable Importance', colour = '') +
  theme_classic()  + 
  theme(legend.position='bottom')
ggsave('importance_graph4.png')