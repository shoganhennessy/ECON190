#!/bin/bash
## Get March CPS zip files from CEPR
cd Data/March_CPS
for year in {1980..2016..1}
do
  echo "http://ceprdata.org/wp-content/cps/data/cepr_march_$year.zip"
  wget -N "http://ceprdata.org/wp-content/cps/data/cepr_march_$year.zip"
  unzip -o "cepr_march_$year.zip"
  #rm "cepr_march_$year.zip"
done
cd ../..

cd Code
Rscript March_CPS.R
