# 26/01/2018 Senan Hogan-H.

# This file builds and works on the March CPS dataset.
# http://ceprdata.org/cps-uniform-data-extracts/march-cps-supplement/march-cps-data/

gc()
ls()
library(tidyverse)
library(data.table)

# The March CPS files are in a directory named 'March_CPS'
# The following combines all files 1980-2016 to a VERY large csv files to be worked on 
# This is not to be tried on a standard computer, not enough memory to host.
# Use on a remote instance or powerful server.

# Find the individual files (in dta form) here:
# http://ceprdata.org/cps-uniform-data-extracts/march-cps-supplement/march-cps-data/

CPS.data <- fread('March_CPS/CPS1980.csv', header = T, sep = ',')
CPS.data <- dplyr::bind_rows(CPS.data,
                             fread('March_CPS/CPS1981.csv', header = T, sep = ','))
CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS1982.csv', header = T, sep = ','))
CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS1983.csv', header = T, sep = ','))
CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS1984.csv', header = T, sep = ','))
CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS1985.csv', header = T, sep = ','))
CPS.data <- dplyr::bind_rows(CPS.data,
                             fread('March_CPS/CPS1986.csv', header = T, sep = ','))
CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS1987.csv', header = T, sep = ','))
CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS1988.csv', header = T, sep = ','))

# month, age variable changes from character/string type to factor type in 1989
CPS.data$month <- as.factor(as.numeric(CPS.data$month))
CPS.data$age <- as.factor(as.numeric(CPS.data$age))

CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS1989.csv', header = T, sep = ','))

# make previous files have 3 for month.

CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS1990.csv', header = T, sep = ','))
CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS1991.csv', header = T, sep = ','))
CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS1992.csv', header = T, sep = ','))
CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS1993.csv', header = T, sep = ','))
CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS1994.csv', header = T, sep = ','))
CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS1995.csv', header = T, sep = ','))

CPS.data$mig_flag <- as.factor(CPS.data$mig_flag)

CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS1995.csv', header = T, sep = ','))
CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS1996.csv', header = T, sep = ','))
CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS1997.csv', header = T, sep = ','))
CPS.data <- dplyr::bind_rows(CPS.data,
                             fread('March_CPS/CPS1998.csv', header = T, sep = ','))
CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS1999.csv', header = T, sep = ','))

CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS2000.csv', header = T, sep = ','))
CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS2001.csv', header = T, sep = ','))
CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS2002.csv', header = T, sep = ','))
CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS2003.csv', header = T, sep = ','))
CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS2004.csv', header = T, sep = ','))
CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS2005.csv', header = T, sep = ','))
CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS2006.csv', header = T, sep = ','))
CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS2007.csv', header = T, sep = ','))
CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS2008.csv', header = T, sep = ','))
CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS2009.csv', header = T, sep = ','))

CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS2010.csv', header = T, sep = ','))
CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS2010.csv', header = T, sep = ','))
CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS2012.csv', header = T, sep = ','))
CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS2013.csv', header = T, sep = ','))

CPS.data$month <- match('March', month.name)
CPS.data$parno <- as.factor(CPS.data$parno)
CPS.data$spouseno <- as.factor(CPS.data$spouseno)
CPS.data$famno <- as.factor(CPS.data$famno)
CPS.data$age <- as.numeric(CPS.data$age)
CPS.data$unmem <- as.numeric(CPS.data$unmem)
CPS.data$uncov <- as.numeric(CPS.data$uncov)
CPS.data$agi <- as.factor(CPS.data$agi)
CPS.data$fica <- as.factor(CPS.data$fica)
CPS.data$fmoop <- as.factor(CPS.data$fmoop)
CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS2014.csv', header = T, sep = ','))
CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS2015.csv', header = T, sep = ','))
CPS.data <- dplyr::bind_rows(CPS.data, 
                             fread('March_CPS/CPS2016.csv', header = T, sep = ','))

# Make race a numeric variable:
# race = 1 if white, non-Hispanic, =2 if black, =3 if Hispanic
CPS.data <- CPS.data %>% mutate(race = ifelse(
  wbhaom == 'White', 1 ,
  ifelse(wbhaom == 'Black', 2,
         ifelse(
           wbhaom == 'Hispanic', 3, NA ))))
# Drop redundant race variable 
CPS.data <- CPS.data %>% subset(select = -c(wbhaom))


# Variable selection.  Drops most of the 475 variables, by selecting 21
CPS.data <- CPS.data %>% subset(select = c(
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
  'race', # race variable
  'empl', # employment status, 1 is employed
  'education', # years education 
  'citizen', # whether a citizen
  'married', # whether married
  'rural', # whether live in rural area
  'suburb', # whether live in suburbs
  'centcity', # whether they live in a central city
  'selfemp', # self-employed
  'unmem', # union membership
  'firmsz', # size of firm they work at  
  'educ2', 'educ92', 'educ' #education variables
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

nrow(subset(CPS.data, year==2007))
# extremely low amount of observations.
nrow(subset(CPS.data, year==2008)) 
# justification for removing 2008
nrow(subset(CPS.data, year==2009))

CPS.data <- CPS.data %>% subset(year != 2008) 
# 2008 extremely low number of observations.

# Export large data frame
fwrite(CPS.data, "CPS_data.csv")

# Export subsample of data frame, to test analysis.
set.seed(47)
fwrite(sample_n(CPS.data, 100000), "CPS__test_data.csv")



# Create a combined id/year variable, for matching of predictions later
# This takes a LONG time, can't find a more efficient method
CPS.data %>% mutate(id2 =  toString(id))
CPS.data %>% mutate(id3 =  toString(year))
CPS.data %>% mutate(id1 =  paste(id2, id3, sep=''))

CPS.data <- CPS.data %>% subset(select = -c(id2, id3) )


# Export large data frame
fwrite(CPS.data, "CPS_data.csv")

# Export subsample of data frame, to test analysis.
set.seed(47)
fwrite(sample_n(CPS.data, 100000), "CPS__test_data.csv")
