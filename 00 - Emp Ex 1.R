#===============================================================================
# Program   :  00 - Emp Ex 1
# Date      :  September 28, 2023
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
setwd("C:/Users/4luca/OneDrive - University of Missouri/Coursework/2023 - 2024/Fall 2023/ECONOM 4371/Empirical Ex 1")

#-------------------------------------------------------------------------------
# 2. Load in Excel files
#-------------------------------------------------------------------------------

# 2.1 Load in Earnings_and_Height
Earnings_and_Height <- read.xlsx("Earnings_and_Height.xlsx")

# 2.2 Load in Growth
Growth <- read.xlsx("Growth.xlsx")

#-------------------------------------------------------------------------------
# 3. Run regressions of growth on tradeshare
#-------------------------------------------------------------------------------

# 3.1 Run regression of growth on tradeshare
lm_growth_tradeshare <- lm(growth ~ tradeshare, data = Growth)

# 3.2 Export regression output list for lm_growth_tradeshare
stargazer(lm_growth_tradeshare, title = "lm_growth_tradeshare", type = "text", out = "lm_growth_tradeshare")

#-------------------------------------------------------------------------------
# 4. Create a scatter plot for growth and tradeshare with regression line
#-------------------------------------------------------------------------------

# 4.1 Plot growth and tradeshare with regression line
jpeg(file="Growth & Tradeshare Scatterplot.jpeg")
plot(Growth$growth, Growth$tradeshare, main="Growth & Tradesahre Scatterplot",
     xlab="Growth", ylab="Tradeshare", xlim=c(-4,10), ylim=c(0,2))
abline(lm(growth ~ tradeshare, data = Growth), col = "blue")
dev.off()

#-------------------------------------------------------------------------------
# 5. Create new Growth_v2 dataframe without Malta observation
#-------------------------------------------------------------------------------

# 5.1 Remove Malta observation
Growth_v2 <- Growth[-c(65),]

#-------------------------------------------------------------------------------
# 6. Run regressions of growth on tradeshare w/o Malta
#-------------------------------------------------------------------------------

# 6.1 Run regression of growth on tradeshare w/o Malta
lm_growth_tradeshare_v2 <- lm(growth ~ tradeshare, data = Growth_v2)

# 6.2 Export regression output list for lm_growth_tradeshare_v2
stargazer(lm_growth_tradeshare_v2, title = "lm_growth_tradeshare_v2", type = "text", out = "lm_growth_tradeshare_v2")

#-------------------------------------------------------------------------------
# 7. Create a scatter plot for growth and tradeshare w/o Malta
#-------------------------------------------------------------------------------

# 7.1 Plot growth and tradeshare w/o Malta
jpeg(file="Growth & Tradeshare Scatterplot without Malta.jpeg")
plot(Growth_v2$growth, Growth_v2$tradeshare, main="Growth & Tradesahre Scatterplot w/o Malta",
     xlab="Growth", ylab="Tradeshare", xlim=c(-4,10), ylim=c(0,2))
abline(lm(growth ~ tradeshare, data = Growth_v2), col = "blue")
dev.off()

#-------------------------------------------------------------------------------
# 8. Remove extra data
#-------------------------------------------------------------------------------

# 8.1 Fully clean work space
rm(Earnings_and_Height, Growth, Growth_v2, lm_growth_tradeshare, lm_growth_tradeshare_v2)
gc()

### END PROGRAM ###