
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
setwd("E:/articulos/logic1800/")


##################################################################
##                          Death data                          ##
##################################################################



# Read data
df <- read_csv("input/deathsweekly.csv", 
                         col_names = FALSE, skip = 9)

# Change colnames
colnames(df)<-c("place","period","both_sex","males","females")

# Remove empty rows
df <- df[5:nrow(df), ]

# Remove last rows (empty)
df <- df[1:(nrow(df)-27),]


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

# Count how many weeks
df %>% 
  group_by(place) %>% 
  summarise(num_valores_unicos = n_distinct(week_number), .groups = "drop")

unique(df$both_sex)

# Remove commas from variable
df$both_sex<-gsub(",", "", df$both_sex)

# Correct column type
df$both_sex<-as.numeric(df$both_sex)


# Create total deaths 
df <- df %>%
  group_by(period) %>%
  mutate(canada_deaths = sum(both_sex, na.rm = TRUE) )


#dataset of NAs:
nas<-df[is.na(df$both_sex),]

# Remove yukon: so many NAS
df<-df[df$place!="Yukon, place of occurrence",]

#dataset of NAs:
nas<-df[is.na(df$both_sex),]


# New dataframe for portugal
dfcanada <- df[, c("canada_deaths", "week_number")]

# Count NA's
sum(is.na(dfcanada$canada_deaths))

# Remove NAs
dfcanada<-dfcanada[!(is.na(dfcanada$canada_deaths)),]

#remove duplicated rows
dfcanada<-unique(dfcanada)
# GRAPH
# Song released April 27, 2017, week number: 381 (April 22, 2017)
#2017 start at 366, finish at 418
start_date<-366
finish_date<-418
dfcanada_filtered <- dfcanada[dfcanada$week_number >= start_date & dfcanada$week_number <= finish_date, ]
library(ggplot2)
graph <- ggplot(dfcanada_filtered, aes(x=week_number, y=canada_deaths)) +
  geom_line() +   theme_bw()   +
  xlab("")
graph

graph + geom_vline(xintercept = 381)



#2016 start at 313, finish at 365
start_date<-313
finish_date<-365
dfcanada_filtered <- dfcanada[dfcanada$week_number >= start_date & dfcanada$week_number <= finish_date, ]
library(ggplot2)
graph2 <- ggplot(dfcanada_filtered, aes(x=week_number, y=canada_deaths)) +
  geom_line() +   theme_bw()   +
  xlab("")
 graph2

graph2 + geom_vline(xintercept = 329)



##################################################################
##                         Suicide data                         ##
##################################################################



# Read data
df <- read_csv("input/canada-suicide-data.csv", 
               col_names = FALSE, skip = 9)

# Change colnames
colnames(df)<-c("place","period","suicides")

# Remove empty rows
df <- df[4:nrow(df), ]

# Remove last rows (empty)
df <- df[1:(nrow(df)-29),]


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

# Count how many weeks
df %>% 
  group_by(place) %>% 
  summarise(num_valores_unicos = n_distinct(week_number), .groups = "drop")

unique(df$suicides)

# Correct column type
df$suicides<-as.numeric(df$suicides)

#keep only canada
df<-df[df$place=="Canada, place of occurrence",]

# Create total deaths 
#df <- df %>%
#  group_by(period) %>%
#  mutate(canada_deaths = sum(suicides, na.rm = TRUE) )

# Remove NAs
dfcanada<-df[!(is.na(df$suicides)),]

# Remove duplicated rows
dfcanada<-unique(dfcanada)
# GRAPH
# Song released April 27, 2017, week number: 381 (April 22, 2017)
# MTV video music award: august 27 2017: week number: 400
# Performance at grammy awards on 28 january 2018, week 422
#2017 start at 366, finish at 418
start_date<-313
finish_date<-450
dfcanada_filtered <- dfcanada[dfcanada$week_number >= start_date & dfcanada$week_number <= finish_date, ]
library(ggplot2)
graph <- ggplot(dfcanada_filtered, aes(x=week_number, y=suicides)) +
  geom_line() +   theme_bw()   +
  xlab("")
graph

graph + geom_vline(xintercept = 381) +geom_vline(xintercept = 400) +
  geom_vline(xintercept= 422)+ geom_vline(xintercept = 329) +geom_vline(xintercept = 348)+ geom_vline(xintercept = 369)



#2016 start at 313, finish at 365
start_date<-313
finish_date<-365
dfcanada_filtered <- dfcanada[dfcanada$week_number >= start_date & dfcanada$week_number <= finish_date, ]
library(ggplot2)
graph2 <- ggplot(dfcanada_filtered, aes(x=week_number, y=suicides)) +
  geom_line() +   theme_bw()   +
  xlab("")
graph2

graph2 + geom_vline(xintercept = 329) +geom_vline(xintercept = 348)



