# 26/01/2018 Senan Hogan-H.

# This file builds and works on the March CPS dataset.
# http://ceprdata.org/cps-uniform-data-extracts/march-cps-supplement/march-cps-data/

library(tidyverse)

# The March CPS files are in a directory named 'March_CPS'
# The following combines all files 1980-2016 to a VERY large csv files to be worked on 
# This is not to be tried on a standard computer, not enough memory to host.
# Use on a remote instance or powerful server.

# Find the individual files (in dta form) here:
# http://ceprdata.org/cps-uniform-data-extracts/march-cps-supplement/march-cps-data/

CPS.data <- read.csv('March_CPS/CPS1980.csv')
CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS1981.csv'))
CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS1982.csv'))
CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS1983.csv'))
CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS1984.csv'))
CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS1985.csv'))
CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS1986.csv'))
CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS1987.csv'))
CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS1988.csv'))

# month, age variable changes from character/string type to factor type in 1989
CPS.data$month <- as.factor(as.numeric(CPS.data$month))
CPS.data$age <- as.factor(as.numeric(CPS.data$age))

CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS1989.csv'))

# make previous files have 3 for month.

CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS1990.csv'))
CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS1991.csv'))
CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS1992.csv'))
CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS1993.csv'))
CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS1994.csv'))
CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS1995.csv'))

CPS.data$mig_flag <- as.factor(CPS.data$mig_flag)

CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS1996.csv'))
CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS1997.csv'))
CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS1998.csv'))
CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS1999.csv'))

CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS2000.csv'))
CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS2001.csv'))
CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS2002.csv'))
CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS2003.csv'))
CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS2004.csv'))
CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS2005.csv'))
CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS2006.csv'))
CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS2007.csv'))
CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS2008.csv'))
CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS2009.csv'))

CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS2010.csv'))
CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS2011.csv'))
CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS2012.csv'))
CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS2013.csv'))

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

CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS2014.csv'))
CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS2015.csv'))
CPS.data <- dplyr::bind_rows(CPS.data, read.csv('March_CPS/CPS2016.csv'))

# Export large data frame
write.csv(CPS.data, file = "CPS_data.csv", row.names=FALSE)

# Export subsample of data frame, to test analysis.
set.seed(47)
write.csv(sample_n(CPS.data, 100000),
          file = "CPS__test_data.csv", row.names=FALSE)

# Remove dataframe (and all) from environment to restore use of memory.
rm(list=ls())