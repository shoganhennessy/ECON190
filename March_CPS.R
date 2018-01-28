# 26/01/2018 Senan Hogan-H.

# This file builds and works on the March CPS dataset.
# http://ceprdata.org/cps-uniform-data-extracts/march-cps-supplement/march-cps-data/

library(tidyverse)

# The March CPS files are in a respository named 'March_CPS'
# The following combines all files 1980-2016 to a VERY large csv files to be worked on 
# Find the individual files (already converted to csv from dta) here:
# https://drive.google.com/open?id=1HfKOjKay5bBEDc0HcyEMGMiRoK18x8Xe

CPS.data <- read.csv('cepr2015.csv')
for (i in c(2:length(list.files('March_CPS')))){
  CPS.data <- dplyr::bind_rows(CPS.data, read.csv(list.files('March_CPS')[i]))
}

write.csv(CPS.data, file = "CPS_data.csv", row.names=FALSE)