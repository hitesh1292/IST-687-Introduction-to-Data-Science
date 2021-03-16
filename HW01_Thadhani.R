
################################################
# IST 687, Standard Homework Heading
#
# Student name: Thadhani Hitesh Chandrakumar
# Homework number:HW01
# Date due: 09/02/2019 11:59PM
#
# Attribution statement: (choose the statements that are true)
# 1. I did this homework by myself, with help from the book and the professor


dev.off() 
cat('\014')  
rm(list=ls())

#Vectors and Variable Defined 
height <- c(59, 60, 61, 58, 67, 72, 70)
weight <- c(150, 140, 180, 220, 160,  140,130)
a <- 150

# Mean Height

height.mean <- mean(height)
print(height.mean)

# Mean Weight

weight.mean <- mean(weight)
print(weight.mean)


#Length of Height Vector

length.height <- length(height)
print(length.height)


#Length of Height Vector

length.weight <- length(weight)
print(length.weight)

# Sum Height
sum.height <- sum(height)
print(sum.height)

#Sum Weight

sum.weight <- sum(weight)
print(sum.weight)

#Average Height of B & D
d <- sum.height
print(d)

e <- length.height
print(e)

averageheightbd <- c(d,e)
average.heightbd <- mean(averageheightbd)
print(average.heightbd)


#Average Height of B & D

average <- mean(c(sum.height,length.weight))
print(average)

#Average Weight of C & E

averagece <- mean(c(length.weight,sum.weight))
print(averagece)

# Max Function usage

maxH <- max(height)
maxH

# Min function usage

minW <- min(weight)
minW

#Vector Math

# Vector Math Part A

extraWeight <- weight + 25
extraWeight

# Vector Math Part B

aveextraWeight <- mean(extraWeight)
aveextraWeight

# Using Conditional if statements

if  ( 100 < 150 ) "100 is less than 150" else "100 is greater than 150"

# Conditional if Part A

if( maxH > 70) "yes" else "no"

# Conditional if Part B

if( minW > a) "yes" else "no"

#Practice with Vectors

#Practice with Vectors Part A

bigHT <- height[ height > 60]
bigHT

#Practice with Vectors Part B

smallWT <- weight[c(2,4)]
smallWT

#Practice with Vectors Part C

weight <- weight[-3]
weight

#Practice with Vectors Part D

# Yes height(3) will generate error as height is not pre-defined function and to access the element in height vector [] bracket needs to be used instead of () 
