#-----------------------------------------------------------------------------------------------------------------------
# Purpose: Plot absolute life expectancies and gaps between politicians and general population for males
# Author : An Tran-Duy
# Date   : 26 February 2020
# Place  : Melbourne, Australia
#-----------------------------------------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(reshape2)
remove(list = ls())

setwd('C:/Users/adtran/OneDrive - The University of Melbourne/Politicians longevity/GitHub/Data')

# Plot absolute life expectancies for males ----------------------------------------------------------------------------

# Load life expectancy data from the general population. Data compiled by Adrian Barnett
load('ConditionalLifeExpectancyPopulation.RData')
# Load life expectancy data from politicians
load('LifeExpectancyPoliticiansGompertzPH_shifting_sex_10years_CI.RData') 

# Exclude rows with NA for politicians life expectancy (time window when sample size = 0)
LE_Gompertz_PH_45 <- dplyr::filter(LE_Gompertz_PH_45, !is.na(mean_male)) %>%
   select(country, year, ll_male, mean_male, ul_male) %>%
   rename(ll_pol = ll_male, expect_pol = mean_male, ul_pol = ul_male) %>%
   mutate(country = as.character(country), country = replace(country, country == 'New_Zealand', 'New Zealand'))

expectancy <- expectancy.sex %>%
   dplyr::filter(sex == 'Male') %>%
   rename(expect_gen = expect) %>%
   select(-sex) %>%
   mutate(country = replace(country, country == 'NZ', 'New Zealand'))

# Restricting the general population to the years in the politicians data set
expectancy1 <- merge(expectancy, LE_Gompertz_PH_45, 
                     by.x = c('country', 'years'), by.y = c('country', 'year'), 
                     all.x = FALSE)

expectancy1$ll_gen <- expectancy1$ul_gen <-  NA

# Change to long shape
plot.data <- reshape(expectancy1, 
                     varying = list(c('expect_gen', 'expect_pol'), 
                                    c('ll_gen', 'll_pol'),
                                    c('ul_gen', 'ul_pol')
                     ), 
                     v.names = c('expect', 'LL', 'UL'),
                     times = c('General population', 'Politicians'),
                     direction = 'long'
)

head(plot.data)

plot.data <- dplyr::rename(plot.data, Population = time)
plot.data$Population <- factor(plot.data$Population, levels = c('Politicians', 'General population'))

# Get life expectancies in the last years for reporting in the manuscript
dlast <- arrange(plot.data, country, years) %>% group_by(country) %>% slice(n())

# Data for max, min
stats.extremes <- group_by(plot.data, Population, country) %>%
   summarise(min = sprintf('%0.1f', min(expect)),              
             max = sprintf('%0.1f', max(expect)),
             LL.min = sprintf('%0.1f', LL[which.min(expect)]),
             UL.min = sprintf('%0.1f', UL[which.min(expect)]),
             LL.max = sprintf('%0.1f', LL[which.max(expect)]),
             UL.max = sprintf('%0.1f', UL[which.max(expect)])
   ) %>%
   ungroup()
stats = stats.extremes %>%
   mutate(
      UL = 0, # Needed for plot, as this is used for mapping in aes() in ggplot()
      LL = 0  # Needed for plot, as this is used for mapping in aes() in ggplot()
   )
stats.pol <- dplyr::filter(stats, Population == 'Politicians') %>%
   mutate(year = 1920, expect = 45, # Coordinates of the max-min box for politicians
          label = paste(max, ' (', LL.max, ', ', UL.max, ')', '\n', 
                        min, ' (', LL.min, ', ', UL.min, ')',
                        sep = '') 
   )

stats.gen <- dplyr::filter(stats, Population == 'General population') %>%
   mutate(year = 2003, expect = 26, # Coordinates of the max-min box for general population
          label = paste(max, '\n', 
                        min,
                        sep = '') 
   )

# Create data for legend on the first plot (no country)
legend <- slice(plot.data, 1:4) %>% mutate(country = '')
legend$Population[1:2] <- 'Politicians'
start.pol <- 1954                                            # x coordinate of the start of the politician legend line
start.gen <- 1990                                            # x coordinate of the start of the general population legend line
line.length <- 10
legend$years[1:2] <- c(start.pol, start.pol + line.length)   # x coordinates of politician legend
legend$expect[1:2] <- 41                                     # y coordinates for politician legend
legend$years[3:4] <- c(start.gen, start.gen + line.length)   # x coordinates for general population legend lines
legend$expect[3:4] <- 22                                     # y coordinates for general population legend lines

plot.data.legend <- bind_rows(legend, plot.data)

legend.fs <- 3                                               # Font size for legend
label.fs <- 11                                               # Font size for axis and facet labels
p <- ggplot(data = plot.data.legend, aes(x = years, y = expect, ymax = UL, ymin = LL, color = Population), size = 0.01) +
   geom_ribbon(alpha = 0.2, linetype = 0) + 
   geom_line(size = 0.3) +
   geom_point(size = 1) +
   scale_color_manual(values = c('blue', 'dark red')) +
   scale_x_continuous(name = 'Year', limits = c(1920, 2014)) +
   scale_y_continuous(name = 'Difference in life expectancy at age 45', limits = c(20, 45)) +
   facet_wrap(~country, ncol = 3, scales = 'free_x')+
   theme_bw() + 
   theme(axis.text = element_text(size = label.fs), 
         strip.text = element_text(size = label.fs + 2),
         axis.title = element_text(size = label.fs + 2),
         legend.position = 'none') +
   
   # Add max and min values for each country ------------------------------
   geom_label(data = stats.pol, aes(x = year, y = expect, label = label), size = legend.fs, hjust = 0, vjust = 1) +
   geom_label(data = stats.gen, aes(x = year, y = expect, label = label), size = legend.fs, hjust = 0, vjust = 1) +
   
   # Add legend to the first plot (no country) ----------------------------
   geom_label(data = legend, aes(x = 1920, y = 45, label='Max (95% CI)\nMin (95% CI)'), size = legend.fs, color = 'blue', hjust = 0, vjust = 1) + # labels
   geom_text(data = legend, aes(x = start.pol - 1, y = 43, label = 'Politicians'), size = 1.1*legend.fs, color = 'blue', hjust = 0, vjust = 0) +
   
   geom_label(data = legend, aes(x = 2003, y = 26, label='Max\nMin'), size = legend.fs, hjust = 0, vjust = 1) + # labels
   geom_text(data = legend, aes(x = 1960, y = 24, label = 'General population'), size = 1.1*legend.fs, hjust = 0, vjust = 0)

p

jpeg('LE_at_45_male.jpg', width = 2800, height = 2800, res = 300, quality = 100)
print(p)
dev.off()  

# Plot difference in life expectancies for males------------------------------------------------------------------------
remove(list = ls())
setwd('C:/Users/adtran/OneDrive - The University of Melbourne/Politicians longevity/GitHub/Data')

# Load data
load('ConditionalLifeExpectancyPopulation.RData') # 
load('LifeExpectancyPoliticiansGompertzPH_shifting_sex_10years_BOOT.RData') # 

LE_Gompertz_PH_45_boot$country <- as.character(LE_Gompertz_PH_45_boot$country)
LE_Gompertz_PH_45_boot <- LE_Gompertz_PH_45_boot %>%
   select(year, expect_male, country) %>%
   rename(expect_pol = expect_male) %>%
   mutate(country = as.character(country), country = replace(country, country == 'New_Zealand', 'New Zealand'))

expectancy <- expectancy.sex %>%
   dplyr::filter(sex == 'Male') %>%
   rename(expect_gen = expect) %>%
   select(-sex) %>%
   mutate(country = replace(country, country == 'NZ', 'New Zealand'))

expectancy_boot <- inner_join(LE_Gompertz_PH_45_boot, expectancy,
                              by = c('country' = 'country', 'year' = 'years')) %>%
   mutate(expect_dif = expect_pol - expect_gen)

# 95% CI of the difference in LE
plot.data <- expectancy_boot %>% group_by(country, year) %>%
   summarise(ll_dif = quantile(expect_dif, 0.025, na.rm = TRUE), 
             dif = mean(expect_dif, na.rm = TRUE),
             ul_dif = quantile(expect_dif, 0.975, na.rm = TRUE)
   ) %>%
   arrange(country, year) %>%
   ungroup()

# Data for max, min and 95% CI
stats.extremes <- group_by(plot.data, country) %>%
   summarise(min = sprintf('%0.1f', min(dif, na.rm = TRUE)),               
             max = sprintf('%0.1f', max(dif, na.rm = TRUE)),
             LL.min = sprintf('%0.1f', ll_dif[which.min(dif)]),
             UL.min = sprintf('%0.1f', ul_dif[which.min(dif)]),
             LL.max = sprintf('%0.1f', ll_dif[which.max(dif)]),
             UL.max = sprintf('%0.1f', ul_dif[which.max(dif)])
   ) %>%
   mutate(
      ll_dif = 0,              # Needed for plot, as this is used for mapping in aes() in ggplot()
      ul_dif = 0,              # Needed for plot, as this is used for mapping in aes() in ggplot()
      year = 1920, dif = 9,    # Coordinates of the max-min box
      label = paste(max, ' (', LL.max, ', ', UL.max, ')', '\n', 
                    min, ' (', LL.min, ', ', UL.min, ')',
                    sep = '') 
   )

#Create data for legend on the first plot (no country)
legend <- slice(plot.data, 1) %>% mutate(country = '', year = NA, dif = 0, ll_dif = 0, ul_dif = 0)

plot.data.legend <- bind_rows(legend, plot.data)   

# Get the difference in the last year for describing in the paper
dlast <- arrange(plot.data, country, year) %>% group_by(country) %>% slice

legend.fs <- 3                                               # Font size for legend
label.fs <- 11                                               # Font size for axis and facet labels
p <- ggplot(data = plot.data.legend, aes(x = year, y = dif, ymin = ll_dif, ymax = ul_dif)) +
   geom_hline(yintercept = 0, lty = 1, col = 'dark red', size = 0.6)+
   geom_line(size = 0.3, color = 'blue') +
   geom_point(size = 1, color = 'blue') +
   geom_ribbon(alpha = 0.2, linetype = 0) +
   scale_x_continuous(name = 'Year', limits = c(1920, 2014)) +
   scale_y_continuous(name = 'Difference in life expectancy at age 45', limits = c(-4, 9), breaks = seq(-3, 9, 3)) +
   facet_wrap(~country, ncol = 3, scales = 'free_x') +
   theme_bw() +
   theme(axis.text = element_text(size = label.fs), 
         strip.text = element_text(size = label.fs + 2),
         axis.title = element_text(size = label.fs + 2),
         legend.position = 'none') +
   
   # Add max and min values for each country ---------------------------------------
   geom_label(data = stats.extremes, aes(x = year, y = dif, label = label), color = 'blue', size = legend.fs, hjust = 0, vjust = 1) +
   
   # Add legend to the first plot (no country) -------------------------------------
   geom_label(data = legend, aes(x = 1920, y = 9, label='Max (95% CI)\nMin (95% CI)'), size = legend.fs, color = 'blue', hjust = 0, vjust = 1) +
   geom_text(data=legend, aes(x = 1967, y = 3.5, label ='Politicians living longer'), size = legend.fs) +
   geom_text(data=legend, aes(x = 1967, y = 0.15, label='Politicians equal to\n general population'), col = 'dark red', size = legend.fs) +
   geom_text(data=legend, aes(x = 1967, y = -3.2, label='Politicians living shorter'), size = legend.fs)

p

jpeg('LE_GAP_at_45_male.jpg', width = 2800, height = 2800, res = 300, quality = 100)
print(p)
dev.off()
