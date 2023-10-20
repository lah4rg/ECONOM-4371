#===============================================================================
# Program   :  01 - Emp Ex 2
# Date      :  October 19, 2023
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

# 1.2 Set working directory 
setwd("C:/Users/4luca/OneDrive - University of Missouri/Coursework/2023 - 2024/Fall 2023/ECONOM 4371/Empirical Ex 2")

#-------------------------------------------------------------------------------
# 2. Load in Excel files
#-------------------------------------------------------------------------------

# 2.1 Load in birthweight_smoking
birthweight_smoking <- read.xlsx("birthweight_smoking.xlsx")

# 2.2 Load in Growth
Growth <- read.xlsx("Growth.xlsx")

#-------------------------------------------------------------------------------
# 3. Run regression of smoking on birthweight
#-------------------------------------------------------------------------------

# 3.1 Run regression of smoker on birthweight
lm_smoker_birtweight <- lm(birthweight ~ smoker, data = birthweight_smoking)

# 3.2 Export regression output list for lm_smoker_birtweight
stargazer(lm_smoker_birtweight, title = "lm_smoker_birtweight", type = "text", out = "lm_smoker_birtweight")

#-------------------------------------------------------------------------------
# 4. Run regression of smoking, alcohol, and nprevist on birthweight
#-------------------------------------------------------------------------------

# 4.1 Run regression of smoking, alcohol, and nprevist on birthweight
lm_smoker_alcohol_nprevist_birtweight <- lm(birthweight ~ smoker + alcohol + nprevist, data = birthweight_smoking)

# 4.2 Export regression output list for lm_smoker_alcohol_nprevist_birtweight
stargazer(lm_smoker_alcohol_nprevist_birtweight, title = "lm_smoker_alcohol_nprevist_birtweight", type = "text", out = "lm_smoker_alcohol_nprevist_birtweight")

#-------------------------------------------------------------------------------
# 5. Run regression of smoking, alcohol, and Tripre0, Tripre2, and Tripre3 on birthweight
#-------------------------------------------------------------------------------

# 5.1 Run regression of smoking, alcohol, and Tripre0, Tripre2, and Tripre3 on birthweight
lm_smoker_alcohol_tripre0_tripre2_tripre3_birtweight <- lm(birthweight ~ smoker + alcohol + tripre0 + tripre2 + tripre3, data = birthweight_smoking)

# 5.2 Export regression output list for lm_smoker_alcohol_tripre0_tripre2_tripre3_birtweight
stargazer(lm_smoker_alcohol_tripre0_tripre2_tripre3_birtweight, title = "lm_smoker_alcohol_tripre0_tripre2_tripre3_birtweight", type = "text", out = "lm_smoker_alcohol_tripre0_tripre2_tripre3_birtweight")

#-------------------------------------------------------------------------------
# 6. Remove extra data
#-------------------------------------------------------------------------------

# 6.1 Fully clean work space
rm(birthweight_smoking, Growth, lm_smoker_birtweight, lm_smoker_alcohol_nprevist_birtweight, lm_smoker_alcohol_tripre0_tripre2_tripre3_birtweight)
gc()

### END PROGRAM ###