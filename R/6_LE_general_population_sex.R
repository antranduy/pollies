#-----------------------------------------------------------------------------------------------------------------------
# Purpose: Create long- and wide-format data sets for life expectancies of males and females from the general populations
#          using data compiled by Adrian Barnett. See https://github.com/agbarnett/pollies/tree/master/R       
# Author : An Tran-Duy
# Date   : 1 April 2020
# Place  : Melbourne, Australia
#-----------------------------------------------------------------------------------------------------------------------

library(dplyr)

setwd('C:/Users/adtran/OneDrive - The University of Melbourne/Politicians longevity/GitHub/Data')

expectancy_sex_long <- read.csv('life_expectancy_population_sex.csv', stringsAsFactors = FALSE)
save(expectancy_sex_long, file = 'LifeExpectancyGeneralPopulation_sex_long.RData')

expectancy_sex_wide <- reshape(expectancy_sex_long, idvar = c('country', 'years'), timevar = 'sex', v.name = 'expect', direction = 'wide' )
save(expectancy_sex_wide, file = 'LifeExpectancyGeneralPopulation_sex_wide.RData')
