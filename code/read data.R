
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
#library(tidyr)
library(readr)
library(dplyr)

# Set WD 
setwd("C:/Users/juanc/Desktop/Papers/logic1800/")

# Read data
df <- read_csv("input/deathsweekly.csv", 
                         col_names = FALSE, skip = 9)

# Change colnames
colnames(df)<-c("place","period","both_sex","males","females")

# Remove empty rows
df <- df[5:nrow(df), ]

# Replace empty with 0
df[is.na(df$place)==TRUE, ]$place<-"NA"

# Inserting places
for (row in 1:nrow(df)) {
  place <- df[row, "place"]
  if(place!="NA") {
    next
  } else{
    df[row,"place"]$place<-df[row-1,"place"]$place
  }
  
}

# Change week date for week numbers
df <- df %>%
  group_by(place) %>%
  mutate(week_number = row_number())
