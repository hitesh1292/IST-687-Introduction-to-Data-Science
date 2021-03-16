
################################################
# IST 687, Standard Homework Heading
#
# Student name:Thadhani Hitesh Chandrakumar
# Homework number:HW04
# Date due:23rd September 2019 11:59PM
#
# Attribution statement: (choose the statements that are true)
# 1. I did this work by myself, with help from the book and the professor


#Step 1:  Cleaning up NAs from the airquality dataframe

myAQdata <- airquality # Loading the pre defined dataset airquality into myAQdata 

View(myAQdata) # Viewing the myAQdata dataframe in tabular form using view() command

?airquality # ?command gives the detailed information about the pre loaded dataset like rowmanes,colnames,dimension of dataset and what the dataset means & is used for 

myAQdata$Ozone[is.na(myAQdata$Ozone)] # Gives the NA's in the Ozone Column

is.na(myAQdata$Ozone) # Gives NA Row Number TRUE means NA in that Row Number
# The above command gives the TRUE or FALSE Result for all rows where the value of column Ozone
# is NA. For Example, TRUE at 5 means row index 5 has NA in OZONE Column.


mean(myAQdata$Ozone,na.rm=TRUE) # Calculating mean of all Ozone Column instances

myAQdata$Ozone[is.na(myAQdata$Ozone)] <- mean(myAQdata$Ozone,na.rm=TRUE)

# Step 1 D Replacing the NA in Ozone Column with the mean in the above command 


myAQdata$Solar.R[is.na(myAQdata$Solar.R)] <- mean(myAQdata$Solar.R,na.rm=TRUE) 

# Step 1 E Replacing the NA in Solar.R Column with the mean in the above command

View(myAQdata) # Viewing the myAQdata after mean substitution


# Step 2: using a package to replace NAs

# Step 2 Part A 

install.packages("imputeTS") # Install package imputeTs

library(imputeTS) # Include it in the library everytime before running the code to include package in code


myAQdata1 <- airquality # Load airquality dataset in myAQdata1 Create a new dataframe

View(myAQdata1)

# Step 2 Part C

myAQdata1$Ozone <- na.interpolation(myAQdata1$Ozone) # Replacing NA in Ozone Column by interpolation

myAQdata1$Solar.R <- na.interpolation(myAQdata1$Solar.R) # Replacing NA in Solar.R Column by interpolation

# Step 2 Part D

# First 5 rows of myAQdata & myAQdata1 differ with column value in Ozone & Solar.R in 5th row first time
# using mean and second time using interpolation function to replace all NA's in dataset.

#Step 3: Sampling

printVecInfo <- sample(myAQdata$Wind,size=10) # Sampling the Wind Column 10 samples

View(printVecInfo)

hist(printVecInfo,col = "red") # Displaying the samples by using histogram in Red colour


printVecInfo <- sample(myAQdata$Wind,size=10,replace=TRUE)  # Using replace=TRUE

hist(printVecInfo)

printVecInfo <- sample(myAQdata$Wind,size=10,replace=FALSE) # using replace=FALSE

hist(printVecInfo)

# Set 3 Part B 3 samples to test with histogram

printVecInfo <- sample(myAQdata$Wind,size=10,replace=TRUE)

hist(printVecInfo,col = "blue")

printVecInfo <- sample(myAQdata$Wind,size=10,replace=TRUE)

hist(printVecInfo,col = "green")

printVecInfo <- sample(myAQdata$Wind,size=10,replace=TRUE)

hist(printVecInfo,col = "yellow")

# Histograms generated are different as samples are randomly picked and this randomness causes different
# result each time which all are correct.

#Step 2:  Replicating our samples

# Using replicate function on Wind Column

replicatewind1 <- replicate(200,mean(sample(mean(myAQdata$Wind,na.rm=TRUE),size=10,replace=TRUE)),simplify=TRUE)

hist(replicatewind1) # 1st Histogram

# Repeating the above 2 more times with histogram plotting

replicatewind2 <- replicate(200,mean(sample(mean(myAQdata$Wind,na.rm=TRUE),size=10,replace=TRUE)),simplify=TRUE)

hist(replicatewind2) # 2nd Histogram

replicatewind3 <- replicate(200,mean(sample(mean(myAQdata$Wind,na.rm=TRUE),size=10,replace=TRUE)),simplify=TRUE)

hist(replicatewind3) # 3rd Histogram

# Difference between the Histograms above

# Histograms generated in Step 1 & Step 2 differ as the samples taken everytime are random and rnadom
# sample from the dataset would generate a different sample values resulting in different histograms which
# are correct with a different data in each one.