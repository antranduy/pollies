#-----------------------------------------------------------------------------------------------------------------------
# Purpose: Estimate life expectancies and their 95% CIs of politicians in different countries over consecutive 
#          10-year periods, for males and females separately
# Author : An Tran-Duy - University of Melbourne
# Date   : 5 February 2020
# Place  : Melbourne
#-----------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(lubridate)
library(stringr)
library(flexsurv)

masterpath <- 'C:/Users/adtran/OneDrive - The University of Melbourne/Politicians longevity/GitHub/'
setwd(masterpath)

source('0_Country_list.R')         # Load a vector of 11 countries
source('1_Functions.R')            # Load functions for estimating life expectancy
load('data/allPoliticians.Rdata')  # Load survival data of all politicians from 11 countries

allPoliticians$male <- ifelse(allPoliticians$sex == 'male', 1, 0)
allPoliticians$male <- as.factor(allPoliticians$male)

num_countries <- length(countries)
LE_Gompertz_PH_45_boot <- vector(mode = 'list', length = num_countries) # Allocate memory for the output
N_boot <- 1000                                                          # Number of boostrap replications

# Loop through countries
for (i in 1:num_countries) {   
   ctry <- countries[i] 
   politicians <- dplyr::filter(allPoliticians, country == ctry)
   interval <- 10
   Eyear <- floor(max(politicians$year_entered))
   Syear <- floor(min(politicians$year_entered))
   mod <- (Eyear - Syear) %% interval
   Syear <- Syear + mod
   
   # Extract day and month of the last year entered
   Eday <- format(max(politicians$Date_entered), format = '%d')
   Emonth <- format(max(politicians$Date_entered), format = '%m')
   
   # Middle year of the intervals to loop over
   start.intervals <- seq(Syear, Eyear - interval, interval)
   # Allocate memory for the output data to speed up computation
   country_ex <- vector(mode = 'list', length = length(start.intervals)*N_boot)
   
   # Loop over time with intervals equal to year.window
   for (j in 1:length(start.intervals)) {      
      start.year <- start.intervals[j]
      start.date <- ymd(paste0(start.year, Emonth, Eday))
      end.date <- ymd(paste0(start.year + interval, Emonth, Eday))      
      
      fitting_data <- dplyr::filter(politicians, Date_entered <= end.date)
      fitting_data$Date_entered <- if_else(fitting_data$Date_entered < start.date, start.date, fitting_data$Date_entered)
      fitting_data$age_entered_years <- as.numeric(fitting_data$Date_entered  - fitting_data$DOB)/365.25
      fitting_data <- dplyr::filter(fitting_data, age_entered_years < age_end_years)
      
      # Sample politicians with replacement and fit the model to the bootstrapped sample
      for (b in 1:N_boot) {
         idc <- sample(1:nrow(fitting_data), replace = TRUE)
         fitting_data_boot <- fitting_data[idc,]
         
         sample_size <- nrow(fitting_data_boot)
         num_deaths <- sum(fitting_data_boot$dead == 1)
         num_female <- sum(fitting_data_boot$sex == 'female')
         
         origin <- 0
         if (num_female >= 20) {
            # Fit Gompert proportional hazard model using age as time scale
            surv <- with(fitting_data_boot, Surv(time = age_entered_years, time2 = age_end_years, event = dead, origin = origin))
            fit <- flexsurvreg(surv ~ male, dist = 'gompertz', data = fitting_data_boot)
            par <- fit$coefficients
            # Estimate life expectancy at age 45 based on the fitted model
            ex_male <- le_sex(origin, par, 45, 1)
            ex_female <- le_sex(origin, par, 45, 0)
            ex.dat <- data.frame(country = ctry, boot = b, year = start.year + interval/2, age = 45, expect_male = ex_male, expect_female = ex_female, N = sample_size, n_death = num_deaths, n_female = num_female)
         } else {
            ex.dat <- data.frame(country = ctry, boot = b, year = start.year + interval/2, age = 45, expect_male = NA, expect_female = NA, N = sample_size, n_death = num_deaths, n_female = num_female)	  
         }
         country_ex[[(j - 1)*N_boot + b]] <- ex.dat
      } 
   } 
   country_ex <- do.call(rbind, country_ex)
   LE_Gompertz_PH_45_boot[[i]] <- country_ex
}   
LE_Gompertz_PH_45_boot <- do.call(rbind, LE_Gompertz_PH_45_boot)

save(LE_Gompertz_PH_45_boot, file='data/LifeExpectancyPoliticiansGompertzPH_shifting_sex_10years_BOOT.RData')
write.csv(LE_Gompertz_PH_45_boot, 'LifeExpectancyPoliticiansGompertzPH_shifting_sex_10years_boot.csv', row.names = FALSE)

# Estimate lower and upper bounds of the 95% CI of the life expectancy -------------------------------------------------
LE_Gompertz_PH_45 <- LE_Gompertz_PH_45_boot %>% 
   group_by(country, year) %>%
   summarise(ll_male = quantile(expect_male, 0.025, na.rm = TRUE), 
             mean_male = mean(expect_male, na.rm = TRUE),
             ul_male = quantile(expect_male, 0.975, na.rm = TRUE),
             ll_female = quantile(expect_female, 0.025, na.rm = TRUE),
             mean_female = mean(expect_female, na.rm = TRUE),
             ul_female = quantile(expect_female, 0.975, na.rm = TRUE)
   ) %>%
   arrange(country, year)

save(LE_Gompertz_PH_45, file='data/LifeExpectancyPoliticiansGompertzPH_shifting_sex_10years_CI.RData')
