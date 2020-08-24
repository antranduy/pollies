#-----------------------------------------------------------------------------------------------------------------------
# Purpose: Create survival data for all politicians in 11 countries using data compiled by Adrian Barnett. See R code
#          for data compilation at https://github.com/agbarnett/pollies/tree/master/R
# Author : An Tran-Duy
# Date   : 24 January 2020
# Place  : Melbourne, Australia
#-----------------------------------------------------------------------------------------------------------------------

library(dplyr)
library(lubridate)
library(stringr)

remove(list = ls())

# Create survival variables --------------------------------------------------------------------------------------------
masterpath <- 'C:/Users/adtran/OneDrive - The University of Melbourne/Politicians longevity/GitHub'
setwd(masterpath)
# list of countries
source('0_Country_list.R')
for (country in countries) {   
   # Load politicians and life table data
   setwd(paste('Data/', country, sep =''))
   filenames = dir(pattern = country)
   filenames = filenames[str_detect(filenames, 'RData')]
   file = ifelse(any(str_detect(filenames, 'imputed')), paste(country, '.imputed.RData',sep=''), paste(country, '.RData',sep=''))
   load(file)  
   remove(life.table)
   # Create new variables for survival data summary and analysis
   new_name_pol <- paste(country, 'Politicians', sep = '') 
   assign(
      new_name_pol,									
      politicians %>% 
         mutate(fu_days = as.numeric(DOD - Date_entered),                  # in days
                fu_years = as.numeric(DOD - Date_entered)/365.25,          # in years
                age_entered_days = as.numeric(Date_entered - DOB),         # in days
                age_entered_years = as.numeric(Date_entered - DOB)/365.25, # in years
                age_end_days = as.numeric(DOD - DOB),
                age_end_years = as.numeric(DOD - DOB)/365.25,
                dead = if_else(Status == 'Living', 0, 1),
                sex = if_else(Sex == 'Male', 'male', 'female'),
                sex = factor(sex, levels = c('male', 'female')),	
                YOB = year(DOB),					   
                year_entered = year(Date_entered),
                YOD = year(DOD),
                country = country
         ) %>%
         dplyr::filter(!is.na(fu_years), 
                       year_entered >= 1700 & year_entered <= 2020
         )	
   )
   save(list = new_name_pol, file = paste(new_name_pol, '.RData', sep = ''))
   setwd('../..')
}

# Combine politicians from 11 countries --------------------------------------------------------------------------------
allPoliticians <- vector(mode = 'list', length = length(countries))
mainpath <- 'C:/Users/adtran/OneDrive - The University of Melbourne/Politicians longevity/GitHub/Data/'
for (i in 1:length(countries)) {	
   subpath <- paste(mainpath, countries[i], '/', sep = '')
   file <- paste(countries[i], 'Politicians.RData', sep = '')
   load(paste(subpath, file, sep = ''))
   pol_data <- get(paste(countries[i], 'Politicians', sep = ''))
   pol_data <- select(pol_data, DOB, DOD, Date_entered,
                      Status, fu_days, fu_years, 
                      age_entered_days, age_entered_years, age_end_days, age_end_years,
                      dead, sex, YOB, year_entered, YOD, country)
   allPoliticians[[i]] <- pol_data
}

allPoliticians <- do.call(rbind, allPoliticians)
save(allPoliticians, file = 'C:/Users/adtran/OneDrive - The University of Melbourne/Politicians longevity/GitHub/Data/allPoliticians.RData')
