
################################################
# IST 687, Standard Homework Heading
#
# Student name:Thadhani Hitesh Chandrakumar
# Homework number:HW09
# Date due: 4th November 2019 11:59PM
#
# Attribution statement: (choose the statements that are true)
# 1. I did this work by myself, with help from the book and the professor

library("tidyverse")
library("RCurl")
library("jsonlite")

setwd("C:\\Syracuse Fall 2019\\Introduction to Data Science IST687 Saltz\\ISTX87\\")
#	Getting Ready: Loading and Verifying the Titanic Dataset

# Loading the Titanic Dataset into RStudio

load("titanic.raw.rdata")

getwd()

#  Imported data into a dataframe called "badboat"

badboat <- titanic.raw

# Using View to view the loaded dataset

View(badboat)


install.packages("arules") # Install package arules

install.packages("arulesViz") # Install package arulesViz

library("arules") # Include package arules in the code

library("arulesViz") # Include package arulesViz in the code

table(badboat) #Making a contingency table using table function


#Step 1:  Explore the Data Set.

# A.

View(badboat)

# It gives a information about all of the passengers their age, gender, class of travel along with survival if they survived.

# This is not a sparse matrix as most of the entries are non-zero.

# B.

survive <- table(badboat$Survived) # Counting the number of people survived and those who did not using table function

survive 

# Here we see that there are 711 people who survived on the titanic and 1490 people did not survive. More number of people 
# didn't survive on the deck.

# C. 

Per_survive <- prop.table(survive) # Using prop.table to give the percentage of survival i.e. % of people who survived
# and % of people who did not survive.

Per_survive # Gives % of survival approximately 68% people did not survive and only 32% people survived on titanic.

# D. 

Per_class <- prop.table(table(badboat$Class)) # % of Class of journey like how many % of people took 1st class, 2nd class, 3rd class & crew

Per_class 


Per_sex <- prop.table(table(badboat$Sex)) # % of gender like male and female passengers % 

Per_sex


Per_age <- prop.table(table(badboat$Age)) # % of age group broadly 2 groups child & Adult

Per_age


# E. contingency table of percentages for the Age and Sex variables together

Per_agesex <- prop.table(table(badboat$Age,badboat$Sex)) # Contingency table for Age and Sex together  

Per_agesex  

# The observation tells % of male & female based on adult & child. There are 19.3% of female who are adults
# & only 2% of females children. There are 76% of the males who are adults and remaining 3% children among males.


#Step 2:  Coercing the data frame into transactions.


# A.

badboatX <- as(badboat,"transactions") # Converting the badboat Dataframe into transactions matrix using as function

#B. 

Inspect_titanic <- inspect(badboatX) # It gives detailed and summarized information about the transactions.

Inspect_titanic


Item_Freq_titanic <- itemFrequency(badboatX) # It gives the freuqency/percentage of all the variables by their types. Like class 1st,2nd,3rd,crew each frequency and similar for all other 3 variables based on the categories present in each class.


Item_Freq_titanic


Item_FreqPlot_titanic <- itemFrequencyPlot(badboatX,support=0.1) # It gives a frequency plot for all the 4 variables based on frequencies of invidual categories within each variable i.e. frequecy of different items in the matrix. 

# C.

View(badboatX)

# It has 3 elements Name, Type and Value.
# We have data, itemInfo, itemSetInfo info are present in Name. 
# Data is the storage of all points in the binary matrix.
# ItemInfo is used to store the labels within the variables like different categories inside 1 variable.
# ItemSetInfo contains transactional information where each row of transaction is an itemset.
# Type gives type of the data like list, matrix , dataframe along with its dimension like 14*4 etc.
# Value is more detailed information about the dimension with its data type.


# D. The difference between badboat and bodboatX is that badboat is a normal dataframe whereas badboatX is a trasactional matrix which we got by coercing the badboat dataframe where we can use apriori , confidence, support and compare with other variables to get more detailed information/relation.

#Step 3:  Discovering patterns using associated rules mining.

# A.


ruleset <- apriori(badboatX, # apriopri rules to be created for the transaction matrix badboatX being used here 
                   parameter = list(support=0.005,confidence=0.5), # Specifying minimum support and confidence of half % & 50% respectively
                   appearance = list(default="lhs",rhs=("Survived=Yes"))) # Specifying what comes on Left Hand Side all variables and Right hand side taken 1 variable survived yes explicitly to make deductions about this variable in particular. 

#B.

inspect(ruleset) # Gives confidence, support and lift also for the ruleset as we have used apriori 

# C.

inspectDT(ruleset) # Gives more interactive view of the data in hand.

# D.

#I would want to be a female child who boarded on a 2nd class.
