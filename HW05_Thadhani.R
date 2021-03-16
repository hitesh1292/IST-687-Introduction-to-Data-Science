
################################################
# IST 687, Standard Homework Heading
#
# Student name:Thadhani Hitesh Chandrakumar
# Homework number:HW05
# Date due:30th September 2019 11:59PM
#
# Attribution statement: (choose the statements that are true)
# 2. I did this work with help from the book and the professor and these Internet sources: < https://www.dummies.com/programming/r/how-to-round-off-numbers-in-r/ > 

install.packages("dplyr")
install.packages("sqldf")

library(RCurl) # library command to include the package in code after it is downloaded
library(jsonlite)
library(tidyverse)
library(dplyr)
library(sqldf)

dataset <- getURL("http://opendata.maryland.gov/resource/pdvh-tf2u.json") # Loading JSON File 

df <- jsonlite::fromJSON(dataset) # fromJSON Command to convert the JSON to R dataframe format 

View(df)

df$vehicle_count <- as.numeric(df$vehicle_count) # Making Vehicle_count numeric as there are NA's in few places

value <- df %>%
  filter(str_trim(day_of_week)=="THURSDAY") %>%
  pull(vehicle_count) %>%
  mean(na.rm=TRUE) # Mean of vehicles involved in accidents on Thursday with NA's 

value <- mean( df$vehicle_count[str_trim(df$day_of_week)=="THURSDAY"], na.rm=TRUE) # Mean of vehicles involved in accidents on Thursday with NA's in 1 step without Pipe command

sum(is.na(df$injury)) # Checking for NA's in Injury Column

# Total number of accidents with injuries

accinj <- df %>% filter((injury)=="YES") %>% group_by(injury) %>% count(injury)  

accinj

# Accidents happen on Friday

 df %>% filter(str_trim(day_of_week)=="FRIDAY") %>% group_by() %>% count("FRIDAYACC")
 
 
# Total number of accidents on Friday where injury='YES'

injfriyes <- df %>% filter(str_trim(day_of_week)=="FRIDAY" & str_trim(injury)=="YES") %>% group_by() %>% count("FRIDAYYES")

injfriyes

# Total number of accidents on Friday where injury='NO'

injfrino <- df %>% filter(str_trim(day_of_week)=="FRIDAY" & str_trim(injury)=="NO") %>% group_by() %>% count("FRIDAYNO")

injfrino

# Injuries occurred each day of the week

injeachday <- df %>% filter((injury)=="YES") %>% group_by(day_of_week)  %>% count(day_of_week) 

injeachday


# Accidents on Friday in a new dataframe

accfri <- df %>% filter(str_trim(day_of_week)=="FRIDAY")

View(accfri)

# Replacing NA's in vehicle_count with the mean 

meanvehicle <- df %>% pull(vehicle_count) %>% mean(na.rm=TRUE) %>% round()

which(is.na(df$vehicle_count))

df$vehicle_count[which(is.na(df$vehicle_count))] <- injfrimean

# Histogram of the number of vehicles in accidents on Friday

injfrimeanhist <- accfri %>% pull(vehicle_count) %>%  hist()


# Distribution of the number of vehicles in accidents on Sunday 

injsunmean <- df %>% filter(str_trim(day_of_week)=="SUNDAY") %>% pull(vehicle_count) 

hist(injsunmean,breaks=20) # Histogram of accidents on Sunday

quantile(injsunmean) #  Quantile distribution of accidents on Sunday

# Sunday and Friday Comparison

# On friday the accidents with vehicle_count=2(2 vehicles invloved in accident) is more than vehicles invloved on Sunday with vehicle_count=2.
# single vehicle invloved in accidents on Sunday is more than the single vehicle invloved in accident on Friday.
# More number of vehicles meet with accidents on FRIDAY than on SUNDAY.




