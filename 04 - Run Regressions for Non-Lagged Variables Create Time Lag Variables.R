#===============================================================================
# Program   :  04 - Run Regressions for Non-Lagged Variables Create Time Lag Variables
# Date      :  September 18, 2023
# Project   :  Econ Honors Thesis
# Author    :  Lucas Hayes
#===============================================================================

#-------------------------------------------------------------------------------
# 1. Set up packages and working directory 
#-------------------------------------------------------------------------------

# 1.1 Load packages
library(tidyverse)
library(dplyr)
library(openxlsx)
library(writexl)
library(ggplot2)
library(stargazer)

# 1.2 Set working directory
setwd("C:/Users/4luca/OneDrive - University of Missouri/Coursework/2023 - 2024/Fall 2023/ECONOM 4995H/Data/Econ Honors Thesis")

#-------------------------------------------------------------------------------
# 2. Load Master_Dataset
#-------------------------------------------------------------------------------

# 2.1 Load in permanent RDS file
Master_Dataset <- readRDS("Master_Dataset")

#-------------------------------------------------------------------------------
# 3. Run regressions for non-lagged variables
#-------------------------------------------------------------------------------

# 3.1 Run regression and create list for BC.Total.DV_Q.from.World...2 relationship
lm_BC.Total.DV_Q.from.World...2 <- lm(Q.from.World...2 ~ BC.Total.DV, data = Master_Dataset)

# 3.2 Run regression and create list for BC.Total.DV_Q.from.Brazil...3 relationship
lm_BC.Total.DV_Q.from.Brazil...3 <- lm(Q.from.Brazil...3 ~ BC.Total.DV, data = Master_Dataset)

# 3.3 Run regression and create list for BC.Total.DV_Q.from.USA...4 relationship
lm_BC.Total.DV_Q.from.USA...4 <- lm(Q.from.USA...4 ~ BC.Total.DV, data = Master_Dataset)

# 3.4 Run regression and create list for UC.Total.DV_Q.from.World...2 relationship
lm_UC.Total.DV_Q.from.World...2 <- lm(Q.from.World...2 ~ UC.Total.DV, data = Master_Dataset)

# 3.5 Run regression and create list for UC.Total.DV_Q.from.Brazil...3 relationship
lm_UC.Total.DV_Q.from.Brazil...3 <- lm(Q.from.Brazil...3 ~ UC.Total.DV, data = Master_Dataset)

# 3.6 Run regression and create list for UC.Total.DV_Q.from.USA...4 relationship
lm_UC.Total.DV_Q.from.USA...4 <- lm(Q.from.USA...4 ~ UC.Total.DV, data = Master_Dataset)

#-------------------------------------------------------------------------------
# 4. Export regression outputs into text files
#-------------------------------------------------------------------------------

# 4.1 Export regression output list for lm_BC.Total.DV_Q.from.World...2 relationship
stargazer(lm_BC.Total.DV_Q.from.World...2, title = "lm_BC.Total.DV_Q.from.World...2", type = "text", out = "lm_BC.Total.DV_Q.from.World...2.text")

# 4.2 Export regression output list for lm_BC.Total.DV_Q.from.Brazil...3 relationship
stargazer(lm_BC.Total.DV_Q.from.Brazil...3, title = "lm_BC.Total.DV_Q.from.Brazil...3", type = "text", out = "lm_BC.Total.DV_Q.from.Brazil...3.text")

# 4.3 Export regression output list for lm_BC.Total.DV_Q.from.USA...4 relationship
stargazer(lm_BC.Total.DV_Q.from.USA...4, title = "lm_BC.Total.DV_Q.from.USA...4", type = "text", out = "lm_BC.Total.DV_Q.from.USA...4.text")

# 4.4 Export regression output list for lm_UC.Total.DV_Q.from.World...2 relationship
stargazer(lm_UC.Total.DV_Q.from.World...2, title = "lm_UC.Total.DV_Q.from.World...2", type = "text", out = "lm_UC.Total.DV_Q.from.World...2.text")

# 4.5 Export regression output list for lm_UC.Total.DV_Q.from.Brazil...3 relationship
stargazer(lm_UC.Total.DV_Q.from.Brazil...3, title = "lm_UC.Total.DV_Q.from.Brazil...3", type = "text", out = "lm_UC.Total.DV_Q.from.Brazil...3.text")

# 4.6 Export regression output list for lm_UC.Total.DV_Q.from.USA...4 relationship
stargazer(lm_UC.Total.DV_Q.from.USA...4, title = "lm_UC.Total.DV_Q.from.USA...4", type = "text", out = "lm_UC.Total.DV_Q.from.USA...4.text")

#-------------------------------------------------------------------------------
# 5. Create lag variables
#-------------------------------------------------------------------------------

# 5.1 Create one-year lag variables for BC.Total.DV and UC.Total.DV
Master_Dataset_Lag1 <- Master_Dataset %>%
  dplyr::mutate(BC.Total.DV_Lag1 = dplyr::lag(BC.Total.DV, n=1)) %>%
  dplyr::mutate(UC.Total.DV_Lag1 = dplyr::lag(UC.Total.DV, n=1))

# 5.2 Create two-year lag variables for Q.from.World...2, Q.from.Brazil...3, and Q.from.US...4
Master_Dataset_Lag2 <- Master_Dataset %>%
  dplyr::mutate(BC.Total.DV_Lag2 = dplyr::lag(BC.Total.DV, n=2)) %>%
  dplyr::mutate(UC.Total.DV_Lag2 = dplyr::lag(UC.Total.DV, n=2))

#-------------------------------------------------------------------------------
# 6. Remove empty years in Master_Dataset_Lag1 and Master_Dataset_Lag2 and nonlagged trade flows
#-------------------------------------------------------------------------------

# 6.1 Remove NA observations in Master_Dataset_Lag1
Master_Dataset_Lag1 <- na.omit(Master_Dataset_Lag1)

# 6.2 Remove NA observations in Master_Dataset_Lag2
Master_Dataset_Lag2 <- na.omit(Master_Dataset_Lag2)

# 6.3 Remove non-lagged trade flows in Master_Dataset_Lag1
Master_Dataset_Lag1 <- Master_Dataset_Lag1[-c(8,20)]

# 6.4 Remove non-lagged trade flows in Master_Dataset_Lag2
Master_Dataset_Lag2 <- Master_Dataset_Lag2[-c(8,20)]

#-------------------------------------------------------------------------------
# 7. Save Master_Dataset_Lag1 and Master_Dataset_Lag2 in a permanent RDS and Excel format
#-------------------------------------------------------------------------------

# 7.1 Save Master_Dataset_Lag1 as RDS file
write_rds(Master_Dataset_Lag1, "Master_Dataset_Lag1", compress = "gz")

# 7.2 Save Master_Dataset_Lag1 as Excel
write_xlsx(Master_Dataset_Lag1,"Master_Dataset_Lag1.xlsx")

# 7.3 Save Master_Dataset_Lag2 as RDS file
write_rds(Master_Dataset_Lag2, "Master_Dataset_Lag2", compress = "gz")

# 7.4 Save Master_Dataset_Lag2 as Excel
write_xlsx(Master_Dataset_Lag2,"Master_Dataset_Lag2.xlsx")

#-------------------------------------------------------------------------------
# 8. Remove extra data
#-------------------------------------------------------------------------------

# 8.1 Fully clean work space
rm(Master_Dataset, Master_Dataset_Lag1, Master_Dataset_Lag2)
rm(lm_BC.Total.DV_Q.from.World...2, lm_BC.Total.DV_Q.from.Brazil...3, lm_BC.Total.DV_Q.from.USA...4, lm_UC.Total.DV_Q.from.World...2, lm_UC.Total.DV_Q.from.Brazil...3, lm_UC.Total.DV_Q.from.USA...4)
gc()

### END PROGRAM ###