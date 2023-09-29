#===============================================================================
# Program   :  00 - Import Excel Data
# Date      :  September 8, 2023
# Project   :  Econ Honors Thesis
# Author    :  Lucas Hayes
#===============================================================================

#-------------------------------------------------------------------------------
# 1. Set up packages and working directory
#-------------------------------------------------------------------------------

# 1.1 Load packages
library(openxlsx)
library(writexl)
library(dplyr)
library(readr)
library(tidyverse)

# 1.2 Set working directory 
setwd("C:/Users/4luca/OneDrive - University of Missouri/Coursework/2023 - 2024/Fall 2023/ECONOM 4995H/Data/Econ Honors Thesis")

#-------------------------------------------------------------------------------
# 2. Load in Excel files
#-------------------------------------------------------------------------------

# 2.1 Load in Brazil_China_Master_Dataset
Brazil_China_Master_Dataset <- read.xlsx("Brazil-China Master Dataset.xlsx")

# 2.2 Load in Brazil_USA_Master_Dataset
Brazil_USA_Master_Dataset <- read.xlsx("Brazil-USA Master Dataset.xlsx")

# 2.3 Load in USA_China_Master_Dataset
USA_China_Master_Dataset <- read.xlsx("USA-China Master Dataset.xlsx")

#-------------------------------------------------------------------------------
# 3. Combine Excel files into one Master_Dataset
#-------------------------------------------------------------------------------

# 3.1 Bind together the three separate data sets
Master_Dataset <- bind_cols(Brazil_China_Master_Dataset, Brazil_USA_Master_Dataset, USA_China_Master_Dataset)

#-------------------------------------------------------------------------------
# 4. Remove repeating and unused variables in Master_Dataset
#-------------------------------------------------------------------------------

# 4.1 Remove NA observations
Master_Dataset <- na.omit(Master_Dataset)

# 4.2 Remove repeated columns
Master_Dataset <- Master_Dataset[-c(11:14, 21:24)]

#-------------------------------------------------------------------------------
# 5. Save Master_Dataset in a permanent RDS format
#-------------------------------------------------------------------------------

# 5.1 Save to permanent data set
write_rds(Master_Dataset, "Master_Dataset", compress = "gz")

#-------------------------------------------------------------------------------
# 6. Save Master_Dataset in a permanent Excel format
#-------------------------------------------------------------------------------

# 6.1 Save Master_Dataset in Excel format
write_xlsx(Master_Dataset, "Master_Dataset.xlsx")

#-------------------------------------------------------------------------------
# 7. Remove extra data
#-------------------------------------------------------------------------------

# 7.2 Fully clean work space
rm(Brazil_China_Master_Dataset, Brazil_USA_Master_Dataset, USA_China_Master_Dataset, Master_Dataset)
gc()

### END PROGRAM ###