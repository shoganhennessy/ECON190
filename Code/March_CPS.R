# 26/01/2018 Senan Hogan-H.
# This file builds and works on the March CPS dataset.
# http://ceprdata.org/cps-uniform-data-extracts/march-cps-supplement/march-cps-data/
library(tidyverse) 
library(data.table)
library(haven)

# The March CPS files are in a directory named '../Data/March_CPS'
# The following combines all files 1980-2016 to a csv file to be worked on in a later script 

# Find the individual files (in dta form) here:
# http://ceprdata.org/cps-uniform-data-extracts/march-cps-supplement/march-cps-data/


#################################################
## Convert all to csv, removing old stata file
variables_to_keep <- c(
  'id', # variable to identify individuals
  'year', # variable for year
  'rhrwage', # real hourly wage, per person
  'inch_pct', # income percentile (20 is top, 1 bottom 5%) 
  'rincp_all', # real annual income, for person
  'rincp_ern', # real annual earnings (no unearned income), for person
  'rinch_all', # real annual income, for household
  'rinch_ern', # real annual earnings (no unearned income), for household
  'age', # age
  'female', # whether female
  'wbho', # race variables
  'empl', # employment status, 1 is employed
  'married', # whether married
  'rural', # whether live in rural area
  'suburb', # whether live in suburbs
  'centcity', # whether they live in a central city
  'selfemp', # self-employed
  'firmsz', # size of firm they work at  
  'educ2', 'educ92', 'educ' #education variables
)

## Convert each file for 1980-2016
for (year in 1980:2016){
  old_file <- '../Data/March_CPS/cepr_march_' %>% paste0(year) %>% paste0('.dta')
  new_file <- '../Data/March_CPS/CPS' %>% paste0(year) %>% paste0('.csv')
  old_file %>% paste0(' -> ') %>% paste0(new_file) %>% print()
  ## Read Stata data, keeping relevant variables
  Stata.data <- read_dta(old_file) %>% select(variables_to_keep)
  # Deal with Stata labels
  Stata.data$wbho <- Stata.data$wbho %>% as_factor() %>% as.character()
  Stata.data$educ <- Stata.data$educ %>% as_factor() %>% as.character()
  Stata.data$educ2 <- Stata.data$educ2 %>% as_factor() %>% as.character()
  Stata.data$educ92 <- Stata.data$educ92 %>% as_factor() %>% as.character()
  Stata.data$firmsz <- Stata.data$firmsz %>% as_factor() %>% as.character()
  # Write new csv
  Stata.data %>% fwrite(new_file)
}


#########################################
### Combine all CSV files.
#Initialise dataframe
CPS.data <- fread('../Data/March_CPS/CPS1980.csv') %>%
  select(variables_to_keep) %>%
  top_n(0)
# Append the rest
for (year in 1980:2016){
  print(year)
  old_file <- '../Data/March_CPS/cepr_march_' %>% paste0(year) %>% paste0('.dta')
  new_file <- '../Data/March_CPS/CPS' %>% paste0(year) %>% paste0('.csv')
  new.data <- fread(new_file)
  CPS.data <- CPS.data %>% bind_rows(new.data)
  ## Delete redundant dta and csv
  #file.remove(old_file)
  #file.remove(new_file)
}


#########################################
### Clean data file inneeded information.

# Make race a numeric variable:
# race = 1 if white, non-Hispanic, =2 if black, =3 if Hispanic
# = 0 if unavailable.  This is best coding for prediction package caret.
CPS.data <- CPS.data %>% mutate(race = ifelse(
  wbho == 'White', 1 ,
  ifelse(wbho == 'Black', 2,
         ifelse(
           wbho == 'Hispanic', 3, 0 ))))

# CHange NAs in some variables to 0 for prediction package caret.
CPS.data <- CPS.data %>% mutate(race = ifelse(is.na(race), 0, race))

# Create variables for summary table
CPS.data <- CPS.data %>% 
  mutate(Race_white = ifelse(wbho == 'White', 1 , 0)) %>% 
           mutate(Race_black = ifelse(wbho == 'Black', 1 , 0)) %>% 
                    mutate(Race_hispanic = ifelse(wbho == 'Hispanic', 1 , 0)) %>% 
                    mutate(Race_other = ifelse(Race_white == 0 &
                                                 Race_black == 0 &
                                                 Race_hispanic == 0 , 1, 0))


# Drop redundant race variable 
CPS.data <- CPS.data %>% subset(select = -c(wbho))

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
                               CPS.data$educ2=="Advanced", 18,
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

# Drop redundant education variables 
CPS.data <- CPS.data %>% subset(select = -c(educ92, educ, educ2))

# Subset according to description in paper
CPS.data <- CPS.data %>% subset(empl > 0) # only employed people.
CPS.data <- CPS.data %>% subset(rhrwage >= 7.25) # people making more than minimum wage in 2011 $
CPS.data <- CPS.data %>% subset(rincp_all >= 0) # positive total yearly income
CPS.data <- CPS.data %>% subset(rincp_ern >= (7.25*40*14)) # year income above full time min wage, minimum 14 weeks
CPS.data <- CPS.data %>% subset(age >= 18 & age <= 65) # 18-65 years old

# Drop redundant variables 
CPS.data <- CPS.data %>% subset(select = -c(empl))


### SHow yearly variable number
print(nrow(subset(CPS.data, year==2007)))
# extremely low amount of observations.
print(nrow(subset(CPS.data, year==2008)))
# justification for removing 2008
print(nrow(subset(CPS.data, year==2009)))

CPS.data <- CPS.data %>% subset(year != 2008) 
# 2008 extremely low number of observations.

# Create a combined id/year variable, for matching of predictions later
CPS.data$id <- c(1:nrow(CPS.data)) 

# Remove observation with absurdly hour hourly wage
CPS.data <- CPS.data %>% subset(rhrwage < 100000) 


# Export csv of data frame
fwrite(CPS.data, "../Data/CPS_data.csv")

# Export subsample of data frame, to test analysis.
set.seed(47)
CPS.data %>%
  sample_n(10000) %>%
  fwrite("../Data/CPS__test_data.csv")
