################################################
# IST 687, Standard Homework Heading
#
# Student name: Thadhani Hitesh Chandrakumar
# Homework number:HW07
# Date due: 14th October 2019 11:59 PM
#
# Attribution statement: (choose the statements that are true)
# 1. I did this work by myself, with help from the book and the professor

library("ggplot2") # Including package ggplot2 in code
library("tidyverse") # Including package tidyverse in code

install.packages("maps") # Install Maps Package

library("maps") # Including package Maps2 in code

setwd("C:/Syracuse Fall 2019/Introduction to Data Science IST687 Saltz/ISTX87/Homework") # Set working directory


mydata <- read_csv("MedianZIP.csv") # Reading the MedianZIP.csv in mydata dataframe

View(mydata) # View mydata dataframe

which(is.na(mydata$Mean)) # Finding if there are NA's in Mean Column and which row number


mydata$Mean[which(is.na(mydata$Mean))] <- mydata$Median[which(is.na(mydata$Mean))] # Replacing NA's with Median

View(mydata)

install.packages("zipcode") # Install zipcode package

library(zipcode) # Include zipcode package in the code

mydata$zip # Value of zip column of mydata dataframe

mydata$zip <- clean.zipcodes(mydata$zip) # clean.zipcodes cleans the zipcodes making 4 digit zip 
# codes 5 digit zip codes by placing 0 in front of them.

data(zipcode) # Pre-loaded detailed information about the dataframe zipcode in R 

View(zipcode) # View zipcode dataframe

data() # All the pre-loaded datasets in R


dfNew <- merge(mydata, zipcode, by="zip") # Merge mydata & zipcode dataframes using zip column

View(dfNew) # View merged dfNew dataset

dfNew$Mean <- as.numeric(dfNew$Mean) # Converting Mean column values to numeric values

# Creates a new dataframe with state , stateName and centre columns from the predefined dataset in R named
# US State Facts & Figures 

stateNameDF <- data.frame(state=state.abb, stateName=state.name, center=state.center)


stateNameDF$stateName <- tolower(stateNameDF$stateName) # Converting all statenames into lowercase for ggplot


dfLatest <- merge(dfNew, stateNameDF, by="state") # Merging the stateNameDf & dfNew dataframes using state column


View(dfLatest) # View the latest merged dataframe


#Step 1:  Plot points for each zipcode (don't forget to library ggplot2 and ggmap

# A.

us <- map_data("state") # Getting map of us states in us dataframe

dotmap<- ggplot(dfLatest, aes(map_id = stateName)) # Using dfLatest with its stateName 

dotmap<- dotmap + geom_map(map = us) # Using us map for the map

# Keeping X-Axis as Longitude and Y-Axis as Latitude with color of points in map being mean
dotmap<- dotmap + geom_point(aes(x=longitude,y=latitude,color=Mean)) 

dotmap # Plotting the map of us 

# C. The map has a color scheme which is very similar and we cannot properly distinguish
# the states depending on colors. 

#Step 2:  Use Tidyverse to create a Data Frame of State-by-State Income

#A.Library the tidyverse() package (if you have not already done so), and then run the following command to create a new data frame:

summaryDF <- dfLatest %>% # Using dfLatest Dataframe 
  group_by(stateName) %>% # Grouping by stateName on dfLatest Dataframe we used above
  summarize(totalPop=sum(Pop), Income=sum(Mean*Pop)) # For every state we have totalPop column
# which is sum of population and Income Column sum of Mean * Population  

View(summaryDF) 

# B. 50 rows in the new dataframe with 3 observations grouped on stateName with totalPop and 
# Income which is sum of Mean * Population.

# C. Create a new variable on this data frame called meanIncome. Divide totalPop by Income to find the average income for each state.

summaryDF$meanIncome <- summaryDF$Income/summaryDF$totalPop
# Dividing TotalIncome by TotalPop to get meanIncome

View(summaryDF)

#Step 3:  Create a map of the U.S. showing average income

# A.	Create a map visualization, where the color of each state represents the mean income for that state.

dotmap<- ggplot(summaryDF, aes(map_id = stateName)) # Using summaryDf as dataframe & statename as aesthetics

dotmap<- dotmap + geom_map(map = us, aes(fill=meanIncome)) # Using US map and fill the map with meanIncome

dotmap<- dotmap + expand_limits(x=us$long,y=us$lat) # Using Longitude on X-Axis & Latitude on Y-Axis of US Map 

dotmap <- dotmap + ggtitle(" State filled with Mean Income ") # ggtitle to give the title of map

dotmap # Plot the Map where color of state represents mean Income for the state

# If NA's are there then only the state appears grey, here I did not have any NA so No state is in Grey 

#Step 4:  Show the population of each state on the map

# A. Merging stateNameDF & summaryDF dataframes using stateName column to get centre.x and centre.y in new dataframe

dfmerged <- merge(stateNameDF, summaryDF, by="stateName")

View(dfmerged)

# B. New dataframe was created 1st with lower case stateNames and X-Axis & Y-Axis as centre.x, centre.y resp


dotmapNew <- dotmap # Using the previous dotmap plotted in Step 3

# Point at the centre of each state using x & y and the size of the point is total population
# of that state using size 

dotmapNew <- dotmapNew + geom_point(data = dfmerged, aes(x=center.x, y=center.y, size=dfmerged$totalPop))

dotmapNew # Plotted dotmapNew with point at the centre of each state & its size equal to total
# population of that state

  