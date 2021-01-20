#!/bin/bash
## Get March CPS zip files from CEPR
for year in {1980..2016..1}
do
  echo "http://ceprdata.org/wp-content/cps/data/cepr_march_$year.zip"
  wget -O "../Data/March_CPS/cepr_march_$year.zip" -N "http://ceprdata.org/wp-content/cps/data/cepr_march_$year.zip"
  #unzip -o "cepr_march_$year.zip"
  #rm "cepr_march_$year.zip"
done
Rscript March_CPS.R
