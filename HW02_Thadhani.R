################################################
# IST 687, Standard Homework Heading
#
# Student name: Thadhani Hitesh Chandrakumar
# Homework number:HW02
# Date due: 9th September 2019 11:59 PM
#
# Attribution statement: (choose the statements that are true)
# 1. I did this work by myself, with help from the book and the professor

#Step A:  Explore the myArrests dataframe from PE02.

# Loading the USArrests into the myArrests Dataframe

myArrests <- USArrests 
myArrests

View(myArrests)

# Summary of myArrests Dataframe:

summary(myArrests)
str(myArrests)

# Step B Explore the Assault Rate 

# 1.# A Write a comment: Is a higher or lower assault rate best?

# Lower Assault rate is better

#2.	What is the mean assault rate?

# mean assault rate

avgassrate <- mean(myArrests$Assault)
avgassrate

#3.	Which state has the best assault rate?

# Best assault rate is the least assualt rate among all the states

# gives INDEX of Row having the best assault rate

lowestassrate <- which.min(myArrests$Assault)
lowestassrate 

# Gives the observation for best assault rate from the index which was found
# using which.min function above

myArrests[lowestassrate,]

# Step C Explore the Murder Rate 

#1.	Which state has the highest murder rate?

maxmurrate <- which.max(myArrests$Murder)
maxmurrate

myArrests[maxmurrate,]

rownames(myArrests[maxmurrate,])

#2.	Create a sorted dataframe, based on descending murder rate

mydescmurframe <- myArrests[order(-myArrests$Murder),]
mydescmurframe

#3.	Show the 10 states with the highest murder rate

         
head(mydescmurframe,10)

# Rownames only(metadata)

rownames(mydescmurframe[1:10,])

#Step D: Which state is the safest?

#1.	Write a comment: explaining which attributes in your myArrests dataframe are appropriate for determining the safest state

# Murder, Rape and Assault are the 3 attributes to find the safest state

#2.	Write a comment: What are two different ways that you can arithmetically combine these attributes

# By suming and averaging the attributes Murder, Rape and Assault

#3.	Write the R code to combine these attributes and determine the safest state

#Sum 

myArrests$Safe <- (myArrests$Murder)+(myArrests$Assault)+(myArrests$Rape)

myArrests$Safe

str(myArrests)

View(myArrests)

safesta <- myArrests[order(myArrests$Safe),]

head(safesta,5)

rownames(safesta[1:5,])

#Average 

myArrests$Safeavg <- ((myArrests$Murder)+(myArrests$Assault)+(myArrests$Rape))/3

myArrests$Safeavg 

View(myArrests)

safestaavg <- myArrests[order(myArrests$Safeavg),]

head(safestaavg,5)

rownames(safestaavg[1:5,])

#Step E:  In depth look at the state with "best" combination of the arrest attributes.

#1.	What are the 5 safest states, when "safest" is defined as of rape and murder counting twice as much as any other attribute used in the calculation? Hint: use scale( ) to create a new standardized version of the attributes.

myArrests$safe2 <- 2*scale(myArrests$Murder)+scale(myArrests$Assault)+2*scale(myArrests$Rape)

View(myArrests)

safestates <- myArrests[order(myArrests$safe2),]

head (safestates,5)

rownames(safestates[1:5,])

#2.	Given your findings in Step 4, is your answer in Step 3 still supported? Why or why not? Why is using scale important?
#  Yes both the answers are correct and here as we have used scale function which standardizes the data and scales it so that it gives more exact value normalizing any unusual value.
        