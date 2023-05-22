
###########################################################################
###########################################################################
###                                                                     ###
###                              READ DATA                              ###
###                                                                     ###
###########################################################################
###########################################################################
# Libraries
#library(readxl)
library(bannerCommenter)
#library(dplyr)
#library(tidyr)
library(readr)

# Set WD 
setwd("C:/Users/juanc/Desktop/Papers/logic1800/")

# Read data
df <- read_csv("input/deathsweekly.csv", 
                         col_names = FALSE, skip = 9)

# Change colnames
colnames(df)<-c("place","period","year","both_sex","males","females")

# Remove empty rows
df <- df[5:nrow(df), ]
