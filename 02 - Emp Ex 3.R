#===============================================================================
# Program   :  01 - Emp Ex 3
# Date      :  November 30, 2023
# Project   :  ECONOM 4371
# Author    :  Lucas Hayes
#===============================================================================

#-------------------------------------------------------------------------------
# 1. Set up packages and working directory
#-------------------------------------------------------------------------------

# 1.1 Load packages
library(openxlsx)
library(dplyr)
library(readr)
library(tidyverse)
library(stargazer)
library(plm)
library(multiwayvcov)

# 1.2 Set working directory 
setwd("C:/Users/4luca/OneDrive - University of Missouri/Coursework/2023 - 2024/Fall 2023/ECONOM 4371/Empirical Ex 3")

#-------------------------------------------------------------------------------
# 2. Load in Excel files
#-------------------------------------------------------------------------------

# 2.1 Load in income_democracy
income_democracy <- read.xlsx("income_democracy.xlsx")

# 2.2 Turn income_deocracy into a panel data set
income_democracy <- plm.data(income_democracy, index=c("country"))

#-------------------------------------------------------------------------------
# 3. Find summary statistics for income_democracy$dem_ind
#-------------------------------------------------------------------------------

# 3.1 Find mean, median, min, max, 25th, and 75th percentile of income_democracy$dem_ind
describe(income_democracy$dem_ind)

# 3.2 Find the SD of income_democracy$dem_ind
sd(income_democracy$dem_ind, na.rm = TRUE)

#-------------------------------------------------------------------------------
# 4. Find value of dem_ind in United States & Libya in 2000 and averaged over all years
#-------------------------------------------------------------------------------

# 4.1 Find value of dem_ind in United States
income_democracy_dem_ind_US <- income_democracy %>%
  filter(income_democracy$country == "United States")

# 4.2 Find value of dem_ind in United States averaged over all years
summary(income_democracy_dem_ind_US$dem_ind)

# 4.3 Find value of dem_ind in Libya
income_democracy_dem_ind_Libya <- income_democracy %>%
  filter(income_democracy$country == "Libya")

# 4.4 Find value of dem_ind in Libya averaged over all years
summary(income_democracy_dem_ind_Libya$dem_ind)

# 4.5 Find value of dem_ind in United States in 2000
income_democracy_dem_ind_US_2000 <- income_democracy_dem_ind_US %>%
  filter(income_democracy_dem_ind_US$year == 2000)

# 4.6 Find value of dem_ind in Libya in 2000
income_democracy_dem_ind_Libya_2000 <- income_democracy_dem_ind_Libya %>%
  filter(income_democracy_dem_ind_Libya$year == 2000)

#-------------------------------------------------------------------------------
# 5. Find countries with an average value of Dem_ind greater than 0.95; less than 0.10; and between 0.3 and 0.7.
#-------------------------------------------------------------------------------

# 5.1 Find countries with an average value of Dem_ind for all countries
income_democracy_averages <- income_democracy %>%
  group_by(country) %>%
  summarise(mean(dem_ind))

# 5.1 Change mean(dem_ind) variable to a normal one
income_democracy_averages <- income_democracy_averages %>% 
  mutate(mean_dem_ind = `mean(dem_ind)`)

# 5.3 Remove mean(dem_ind) variable
income_democracy_averages = subset(income_democracy_averages, select = c(country, mean_dem_ind))

# 5.4 Find countries with an average value of Dem_ind greater than 0.95
income_democracy_dem_ind_more_than_0.95 <- income_democracy_averages %>%
  filter(mean_dem_ind > 0.95)

# 5.5 Find countries with an average value of Dem_ind less than 0.10
income_democracy_dem_ind_less_than_0.10 <- income_democracy_averages %>%
  filter(mean_dem_ind < 0.1)

# 5.6 Find countries with an average value of Dem_ind between 0.3 and 0.7
income_democracy_dem_ind_between_0.3_0.7 <- income_democracy_averages %>%
  filter(mean_dem_ind <= 0.7 & mean_dem_ind >= 0.3)

#-------------------------------------------------------------------------------
# 6. Regress Dem_ind on Log_GDPPC using standard errors that are clustered by country
#-------------------------------------------------------------------------------

# 6.1 Regress Dem_ind on Log_GDPPC using standard errors that are clustered by country
plm_dem_ind_log_gdppc_fixed <- plm(dem_ind ~ log_gdppc, data=income_democracy, model="between")

# 6.2 Export regression output list for plm_dem_ind_log_gdppc_fixed
stargazer(plm_dem_ind_log_gdppc_fixed, title = "plm_dem_ind_log_gdppc_fixed", type = "text", out = "plm_dem_ind_log_gdppc_fixed")

# 6.3 Find 95% confidence interval for plm_dem_ind_log_gdppc_fixed
confint(plm_dem_ind_log_gdppc_fixed)

# 6.4 Export regression output list for plm_dem_ind_log_gdppc_fixed
stargazer(plm_dem_ind_log_gdppc_fixed, title = "plm_dem_ind_log_gdppc_fixed", type = "text", out = "plm_dem_ind_log_gdppc_fixed")

#-------------------------------------------------------------------------------
# 7. Turn income_democracy into a non-panel data set to explore if the regression would change
#-------------------------------------------------------------------------------

# 7.1 Turn income_democracy into income_democracy_nonpanel
income_democracy_nonpanel <- as.data.frame(income_democracy)

# 7.2 Regress Dem_ind on Log_GDPPC using standard errors that are clustered by country
lm_dem_ind_log_gdppc <- lm(dem_ind ~ log_gdppc, data=income_democracy_nonpanel)

# 7.3 Export regression output list for lm_dem_ind_log_gdppc
stargazer(lm_dem_ind_log_gdppc, title = "lm_dem_ind_log_gdppc", type = "text", out = "lm_dem_ind_log_gdppc")

#-------------------------------------------------------------------------------
# 8. Remove extra data
#-------------------------------------------------------------------------------

# 8.1 Fully clean work space
rm(income_democracy_dem_ind_Libya, income_democracy, income_democracy_averages, income_democracy_dem_ind_between_0.3_0.7, income_democracy_dem_ind_less_than_0.10, income_democracy_dem_ind_Libya_2000, income_democracy_dem_ind_more_than_0.95, income_democracy_dem_ind_US, income_democracy_dem_ind_US_2000, income_democracy_nonpanel)
rm(lm_dem_ind_log_gdppc, plm_dem_ind_log_gdppc_fixed, plm_log_gdppc_dem_ind_fixed)
gc()

### END PROGRAM ###