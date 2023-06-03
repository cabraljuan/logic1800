
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
library(ggplot2)


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



############################################################################
############################################################################
###                                                                      ###
###                             SUICIDE DATA                             ###
###                                                                      ###
############################################################################
############################################################################




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
# Places like YUKON doesn't have so much data, most NA's are introduced there

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



##################################################################
##                            Graphs                            ##
##################################################################



# Song released April 27, 2017, week number: 381 (April 22, 2017)
# MTV video music award: august 27 2017: week number: 400
# Performance at grammy awards on 28 january 2018, week 422
#2017 start at 366, finish at 418
start_date<-313
finish_date<-450
dfcanada_filtered <- dfcanada[dfcanada$week_number >= start_date & dfcanada$week_number <= finish_date, ]
graph <- ggplot(dfcanada_filtered, aes(x=week_number, y=suicides)) +
  geom_line() +   theme_bw()   +
  xlab("")
graph

graph + geom_vline(xintercept = 381) +geom_vline(xintercept = 400) +
  geom_vline(xintercept= 422)+ geom_vline(xintercept = 329) +geom_vline(xintercept = 348)+ geom_vline(xintercept = 369)



#2016 start at 313, finish at 365
start_date<-366
finish_date<-450
dfcanada_filtered <- dfcanada[dfcanada$week_number >= start_date & dfcanada$week_number <= finish_date, ]
library(ggplot2)
graph2 <- ggplot(dfcanada_filtered, aes(x=week_number, y=suicides)) +
      theme_minimal()  +  labs(title = "Suicides in Canada per Week (2017 to 2018)", 
                                            x = "Week Number", 
                                            y = "Number of Suicides") +
  geom_line(color = 'steelblue', size = 1.2, linetype = 'solid') +
  geom_point(color = 'darkred', size = 2) 
graph2

# Adding marks
graph2<-graph2 + geom_vline(xintercept = 381,  color="darkred",size=1.0) +
  geom_vline(xintercept = 400,  color="darkred",size=1.0)+ 
  geom_vline(xintercept = 422,  color="darkred",size=1.0)+ 
  annotate("text", x = 381, y = max(dfcanada_filtered$suicides), label = "Song released", hjust = -0.1) +
  annotate("text", x = 400, y = max(dfcanada_filtered$suicides), label = "MTV video music award", hjust = -0.1) +
  annotate("text", x = 422, y = max(dfcanada_filtered$suicides), label = "Performance at grammy awards", hjust = -0.1) 

graph2
ggsave("output/graphs/suicide_weekly_data_2017_2018.jpg", plot = graph2, width = 10, height = 6, dpi = 300)





##################################################################
##                         OLS analysis                         ##
##################################################################
library(stargazer)

# Song released April 27, 2017, week number: 381 (April 22, 2017)
# Youtube video released august 17 2017: week number: 397 (aug 12, 2017)
# MTV video music award: august 27 2017: week number: 400 (didn't get so much attention)
# Performance at grammy awards on 28 january 2018, week 422
#2017 start at 366, finish at 418


# New variable
dfcanada$treatment<-0
dfcanada[dfcanada$week_number==382,]$treatment<-1
dfcanada[dfcanada$week_number==398,]$treatment<-1
dfcanada[dfcanada$week_number==423,]$treatment<-1

# Month dummies
dfcanada$jan <- as.integer(grepl("Jan", dfcanada$period, ignore.case = TRUE))
dfcanada$feb <- as.integer(grepl("Feb", dfcanada$period, ignore.case = TRUE))
dfcanada$mar <- as.integer(grepl("March", dfcanada$period, ignore.case = TRUE))
dfcanada$apr <- as.integer(grepl("April", dfcanada$period, ignore.case = TRUE))
dfcanada$may <- as.integer(grepl("May", dfcanada$period, ignore.case = TRUE))
dfcanada$jun <- as.integer(grepl("June", dfcanada$period, ignore.case = TRUE))
dfcanada$jul <- as.integer(grepl("July", dfcanada$period, ignore.case = TRUE))
dfcanada$aug <- as.integer(grepl("August", dfcanada$period, ignore.case = TRUE))
dfcanada$sept <- as.integer(grepl("September", dfcanada$period, ignore.case = TRUE))
dfcanada$oct <- as.integer(grepl("October", dfcanada$period, ignore.case = TRUE))
dfcanada$nov <- as.integer(grepl("November", dfcanada$period, ignore.case = TRUE))
dfcanada$dec <- as.integer(grepl("December", dfcanada$period, ignore.case = TRUE))

# Quadratic time
dfcanada$week_numberq<-(dfcanada$week_number)*(dfcanada$week_number)

# First model
model1 <- lm(suicides ~ treatment + jan +feb+mar+apr+may+jun+
               jul+aug+sept+oct+nov+week_number+week_numberq, data = dfcanada)
summary(model1)

# Second model
#Restrict sample to week 




# Puedes usar stargazer para convertir el resumen del modelo en una tabla de LaTeX
stargazer(model1, title="Resultados de la Regresión", label="tab:model1", 
          header=FALSE, type = "latex")

