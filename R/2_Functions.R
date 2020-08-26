#-----------------------------------------------------------------------------------------------------------------------
# Purpose: Estimate life expectancy and 95% CI of politicians in different countries over consecutive 10-year periods
# Author : An Tran-Duy - University of Melbourne
# Date   : 1 February 2020
# Place  : Melbourne
#-----------------------------------------------------------------------------------------------------------------------

# Survival rate function, pooling males and females----------------------------------------------------------------------
# Compute survival rate at time t after baseline age
# Arguments:
#   origin: the origin argument in Surv() function in the package survival
#   par: a vector of shape and constant parameters of the Gompert proportional hazard model 
#   age: baseline age, i.e. age at which the survival rate equals 1
#   t: time at which the survival rate is calculated
surv_pooled <- function(origin, par, age, t) {	
   shape <- par[1]
   cons <- par[2]
   tx <- age - origin
   # Survival rate at tx
   Sx <- exp(-(1/shape)*(exp(shape*tx) - 1)*exp(cons))
   # Survival rate at time t, conditional on living up to time tx
   St <- exp(-(1/shape)*(exp(shape*(tx + t)) - 1)*exp(cons)) / Sx
   return(St) 
}

# Life expectancy function, pooling males and females -------------------------------------------------------------------
# Compute life expectancy conditional on having lived to a specific age
# Arguments: 
#   origin: the origin argument in Surv() function in the package survival
#   par: a vector of shape and constant parameters of the Gompert proportional hazard model 
#   age: baseline age, i.e. age at which the survival rate equals 1
le_pooled <- function(origin, par, age) {
   area <- 0	
   st <- rep(NA, 101)	
   st[1] <- 1	# Year 0
   for (year in 1:100) {		
      st[year + 1] <- surv_pooled(origin, par, age, year)		
      area <- area + (st[year] + st[year + 1])/2	
   }	
   return(area)
}

# Survival rate function, sex specific----------------------------------------------------------------------------------
# Compute survival rate at time t after baseline age
# Arguments:
#   origin: the origin argument in Surv() function in the package survival
#   par: a vector of shape and constant parameters and coefficient for male of the Gompert proportional hazard model 
#   age: baseline age, i.e. age at which the survival rate equals 1
#   male: 1 if male and 0 if female
#   t: time at which the survival rate is calculated
surv_sex <- function(origin, par, age, male, t) {	
   shape <- par[1]
   cons <- par[2]
   male_coef <- par[3]
   tx <- age - origin
   bX <- cons + male_coef*male
   # Survival rate at tx
   Sx <- exp(-(1/shape)*(exp(shape*tx) - 1)*exp(bX))
   # Survival rate at year t, conditional on living up to year tx
   St <- exp(-(1/shape)*(exp(shape*(tx + t)) - 1)*exp(bX)) / Sx
   return(St) # Survival rate
}

# Life expectancy function, sex specific --------------------------------------------------------------------------------
# Compute life expectancy conditional on having lived to a specific age
# Arguments: 
#   origin: the origin argument in Surv() function in the package survival
#   par: a vector of shape and constant parameters and coefficient for male of the Gompert proportional hazard model 
#   age: baseline age, i.e. age at which the survival rate equals 1
#   male: 1 if male and 0 if female
le_sex <- function(origin, par, age, male) {
   area <- 0	
   st <- rep(NA, 101)	
   st[1] <- 1	# Year 0
   for (year in 1:100) {		
      st[year + 1] <- surv_sex(origin, par, age, male, year)		
      area <- area + (st[year] + st[year + 1])/2	
   }	
   return(area)
}
