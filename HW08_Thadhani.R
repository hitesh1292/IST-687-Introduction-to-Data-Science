################################################
# IST 687, Standard Homework Heading
#
# Student name: Thadhani Hitesh Chandrakumar
# Homework number:HW08
# Date due: 21st October 2019 11:59 PM
#
# Attribution statement: (choose the statements that are true)
# 1. I did this work by myself, with help from the book and the professor


# a.	Read in data from the following URL into a data frame
#URL <- "http://college.cengage.com/mathematics/brase/understandable_statistics/7e/students/datasets/mlr/excel/mlr01.xls"

setwd("C:/Syracuse Fall 2019/Introduction to Data Science IST687 Saltz/ISTX87/Homework") # Set working directory

#B.Use the 'download.file' command to download the excel spreadsheet, and store it in a temporary local file
#tempfile <- download.file(URL, dest = "mlr01.xlsx")

# c.	Use the read_excel command from the readxl library. Store the data in the dataframe 'df'
library(tidyverse) # Include tidyverse package in the code

install.packages("readxl")
library ("readxl") # Include readxl package in the code

getwd()

df <- read_excel("C://Users//hitesh//Downloads//mlr01.xls") # Rading mlr01.xls into df dataframe

View(df)

str(df)


colnames(df) # Colnames of df originally before renaming

library(dplyr) # Include library dplyr in the code

#Renaming the columns within the dataframe

names(df)[1:4] <- c("spring_fawn_count", "size_antelope_population", "annual_precipitation_inches","winter_severity_index")


#Step 1:  Visualizing a Linear Model.

library("ggplot2") # Include library ggplot2 in the code


#a. Bivariate plot of the number of baby fawns versus adult antelope population


# Providing dataframe df and specifying X & Y Axis Variables to be plotted using scatter plots using geom_point and method to fit the curve with best line of blue color

fawnante <- ggplot(df, aes(x = size_antelope_population, y = spring_fawn_count)) + geom_point() + stat_smooth(method = "lm", col = "blue")

# Giving names/labels to X & Y Axis

fawnante <- fawnante + xlab("size_antelope_population")+ ylab("spring_fawn_count")

fawnante # Plotting the bivariate plot 

# The bivariate plot shows that spring_fawn_count are linearly positively related to each other. The best fit line has 
# only 1 point exactly through which the line passes.

# b. Bivariate plot of the number of baby fawns versus precipitation that year 

# Providing dataframe df and specifying X & Y Axis Variables to be plotted using scatter plots using geom_point and method to fit the curve with best line of green color

fawnpreci <- ggplot(df, aes(x = annual_precipitation_inches, y = spring_fawn_count)) + geom_point() + stat_smooth(method = "lm", col = "green")

# Giving names/labels to X & Y Axis

fawnpreci <- fawnpreci + xlab("annual_precipitation_inches")+ ylab("spring_fawn_count")

fawnpreci # Plotting the bivariate plot

# The bivariate plot shows that spring_fawn_count are linearly positively related to each other. The best fit line has 
# 3 points through which the line passes and few of them have very less prediction error.


# c.Bivariate plot that examines the number of baby fawns versus severity of the winter


# Providing dataframe df and specifying X & Y Axis Variables to be plotted using scatter plots using geom_point and method to fit the curve with best line of green color

fawnseve <- ggplot(df, aes(x = winter_severity_index, y = spring_fawn_count)) + geom_point() + stat_smooth(method = "lm", col = "purple")

# Giving names/labels to X & Y Axis

fawnseve <- fawnseve + xlab("winter_severity_index")+ ylab("spring_fawn_count")


fawnseve # Plotting the bivariate plot

# The bivariate plot shows that spring_fawn_count are linearly negatively correlated to each other. The best fit line has 
# has no points through which passes.

#Step 2:  Creating a regression model.

# a. 

# Multiple linear regression in a single regression 

# Running single linear regression for predicting spring_fawn_count using the 3 variables as independent 

regout <- lm(formula=spring_fawn_count ~ size_antelope_population + annual_precipitation_inches + winter_severity_index , data = df )

# Running single linear regression for predicting spring_fawn_count using the 3 variables at once with . rather than specifying all the variable names as used above 

regout1 <- lm(data=df,spring_fawn_count ~ .)  

summary(regout) # Summary of the model
 
summary(regout1) # Summary of the model


#b. Adjusted R-Square value is 0.955. The 3 variables account for about 95.5%
# of the variability for the spring_fawn_count. The model has one of the best predictability as it is very close to 1.

#c. p-value for each of the predictors is below cut-off point 0.05 so all the explanatory
# variables are important to do the multiple linear regression. They all contribute to the 
# regression being important in determining predicted value of Y.
# The most significant independent variable is annual_precipitation_inches for predicting fawn count as its value is the 
# least among the 3 independent variable's p-value.


#Step 3:  Interpreting the model.

#a. Linear model has a excellent predictive power of 95.5%.This means that the model can predict almost 95.5% of the values
# correctly based on 3 variables for the spring fawn count.As the adjusted R-squared value is close to 1 it is going to predict the values
# as accurate as possible based on 3 independent variables.


#b.Full multiple regression equation and then explain how to interpret the equation 

# Predicted spring_fawn_count = -5.92201+0.33822*size_antelope_population+0.40150*annual_precipitation_inches+0.26295*winter_severity_index

# The equation has 1 intercept and 3 coefficients each for 3 independent variables called as slopes.

# Intercept(-5.92201) : The predicted value of spring_fawn_count is -5.92201 when all the independent variables are kept to 0.

# Slopes :

# 1. 0.33822(size_antelope_population) : The predicted value of spring fawn increases by 0.33822 when size of antelope popu
# increases by 1 unit meaning if it increases by 1 then predicted value of fawn increases by 0.33822.

# 2. 0.40150(annual_precipitation_inches) : The predicted value of spring fawn increases by 0.40150 when size of annual_precipitation_inches
# increases by 1 unit meaning if it increases by 1 then predicted value of fawn increases by 0.40150.

# 3. 0.26295(winter_severity_index) : The predicted value of spring fawn increases by 0.26295 when size of winter_severity_index
# increases by 1 unit meaning if it increases by 1 then predicted value of fawn increases by 0.26295.