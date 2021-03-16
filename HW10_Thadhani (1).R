
################################################
# IST 687, Standard Homework Heading
#
# Student name: Thadhani Hitesh Chandrakumar
# Homework number:HW10
# Date due: 11th November 2019 11:59 PM
#
# Attribution statement: (choose the statements that are true)
# 1. I did this work by myself, with help from the book and the professor

install.packages("kernlab") # Install Package kernlab

install.packages("ggplot2") # Install Package ggplot2 

library("kernlab") # Include kernlab package in the code

library("ggplot2") # Include ggplot2 package in the code


diamonds_df <- diamonds # loading diamonds dataset into diamonds_df dataframe


View(diamonds_df)


#	Cleaning the data.

#a. Making a subset from the original diamonds_df dataframe having only 2 cuts Ideal & Preminum

goodDiamonds <- diamonds_df[which(diamonds_df$cut %in% c('Premium','Ideal')),]

View(goodDiamonds)

table(goodDiamonds$cut)

table(diamonds_df$cut) # Calculating the number of cuts for each category and verifying it with View Record entries count

# b. Converting the clarity and color variables into numbers

goodDiamonds$clarity <- as.numeric(goodDiamonds$clarity)

goodDiamonds$color <- as.numeric(goodDiamonds$color)

str(goodDiamonds)

# c.Making the factor come down to only 2 cut's ideal & premium which was 5 previously in the diamonds_df dataframe

goodDiamonds$cut <- as.factor(as.character(goodDiamonds$cut))


View(goodDiamonds) # Viewing the goodDiamonds Dataframe after cleaning 

?diamonds

# Step 1:  Re-execute the Code Created in PE10

table(goodDiamonds$cut)

# There are 21551 Ideal Diamonds in the dimonds dataset whereas 13791 Preminum cut dimonds in the dataset


#Step 2:  Creating training and test data sets. 

# A. 

randIndex <- sample(1:dim(goodDiamonds)[1]) # Generating random sample from 1st element to last element index(35342)

summary(randIndex) 

length(randIndex) # gives the length of the randomIndex sample generated above

# B.
cutPoint2_3 <- floor(2 * dim(goodDiamonds)[1]/3) # Getting the cutoff-point for training & test data
# Calculating the 2/3rd point for dividing the dataset into training data and use remaining 1/3rd for test data

cutPoint2_3 # 2/3rd cutoff point value

# Generating the training dataset from the first 2/3rd cutoff point

trainData_Diamonds <- goodDiamonds[randIndex[1:cutPoint2_3],]

# Generating the test dataset from the remaining 1/3rd dataset after 2/3rd is kept for training

testData_Diamonds <- goodDiamonds[randIndex[(cutPoint2_3+1):dim(goodDiamonds)[1]],]

# C.

dim(trainData_Diamonds) # Getting the dimension of training dataset(23561 obs and 10 variables) 

dim(testData_Diamonds) # Getting the dimension of test dataset(11781 obs and 10 variables)

# We have 23561 observations and 10 columns for the training data and 11781 observations and 10 columns for test data.

# Step 3:  Build a support vector model.

# A. Created SVM model with training data using ksvm function 

install.packages("caret") # Installing caret package

library("caret") # Include caret function in the code

svmOutput_trainData <- ksvm(cut ~ ., data = trainData_Diamonds, kernel ="rbfdot",kpar="automatic",
                            C = 5,cross = 3,prob.model = TRUE)


#B. The 1st parameter is the cut which is the variable we want the model to predict the outcome of based on all other parameters which is specified by . after tilde character(~) which separates left-side from right-side of the expression.
# The 2nd parameter is the data meaning which data is to be used for analysis here i used traindata_Diamonds training data
# 3rd Parameter is the kernel which is the type of algorithm used to enable projection of lower-dimensional to higher-dimension data.
# Here Radial Basis Function is used to do that mapping. This function takes several inputs from each row in a dataset and calculates a distance value based on different combinations of the variables.
# The weights of the different combinations of variables are adjusted by algorithm to get maximum distance separating 2 groups like ideal & premium cut diamonds kind of maximum distance for a hyperplane to do classification in th best possible way with least number of mistakes.
# 4th variable is the kpar which is the variety of parameters that can be used to control the function of radial basis function.
# C refers to cost of constraints which can be used to influence the margin of separation and the degree of classification mistakes.
# We can have small value of C where the model is more generalized but at the cost of making more classification mistakes having large margin of separation.
# Large values of C would result in small mistakes in classification but a small margin of separation causing very delicate separation between the groups.
# Cross refers to cross validation model that this algorithm is using. It is important to avoid overfitting.
# prob.model= TRUE ,gives threefold crossvalidation to generate the probabilities associated with if we have a ideal or premium diamond.



# C. 
svmOutput <- svmOutput_trainData # Assigning the training output at the end of ksvm to svmOutput

svmOutput


#Step 4:  Predicting values in the test data. 

# A.

# Predicting the output using predict function taking into account test data for model validation 

svmPred <- predict(svmOutput, testData_Diamonds, type="votes")

# B. 
str(svmPred)


# C.

str(svmPred) # Structure of the predicted output

head(svmPred) 

# Step 5:  Create a confusion matrix. 

# A. Confusion matrix for second row of svmPred to the contents of testData$cut

View(testData_Diamonds)

# Creating confusion matrix for accuracy and error prediction of the model

compTable <- data.frame(testData_Diamonds[,2],svmPred[2,]) # testData of diamonds's 2nd column because it contains cut of the diamonds to be compared to predicted output using testdata's 2nd row 

dia_conmat<- table(compTable) # Confusion matrix created 

dia_conmat

# Calculating Accracy Rate for the diamonds confusion matrix

Acc_Rate <- sum(dia_conmat[1,1]+dia_conmat[2,2])/sum(dia_conmat[1,1]+dia_conmat[1,2]+dia_conmat[2,1]+dia_conmat[2,2])*100

Acc_Rate

# Calculating Error Rate for the diamonds confusion matrix

Error_Rate <- sum(dia_conmat[1,2]+dia_conmat[2,1])/sum(dia_conmat[1,1]+dia_conmat[1,2]+dia_conmat[2,1]+dia_conmat[2,2])*100

Error_Rate

# B.

# Error 8.51% which we calculated above in the variable Error_rate

# C. This model has error rate which is 8.51% and the accuracy of 91.49% which is very good considering the model,it might vary slightly. 

# Step 6:  Understanding the reasoning behind the practice.

#It is always needed to have separate test and training dataset because we need to train the model
# 1st, make it learn based on this data as the machine does not learn like humans do so we give large training data
# which enables it learn and then use test data to predict the outcome based on its learning. This testdata
# helps to evaluate if the model is working fine doing predictions after training for unknown data so that 
# we can use this model as a generalized model to do predictions. Also we will be able to avoid overfitting.