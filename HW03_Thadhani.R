# IST 687, Standard Homework Heading
#
# Student name: Thadhani Hitesh Chandrakumar 
# Homework number: HW03
# Date due: 16th September 2019 11:59PM
#
# Attribution statement: (choose the statements that are true)
# 1. I did this work by myself, with help from the book and the professor

#Getting the data from the web using read.csv command

urltoRead <- "https://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv" 
#dfStates<- read.csv("https://www2.census.gov/programs-surveys/popest/tables/2010-2011/state/totals/nst-est2011-01.csv",stringsAsFactors=FALSE)

dfStates <- read.csv("Nst-est2011.csv")

# View,Head,tail commands for viewing the dataset
View(dfStates)

head(dfStates)

tail(dfStates)

readStates <- function() { # Start of the readStates Function

#Removing Unneded rows by row selector

dfStates <- dfStates[c(-60:-66,-1:-8),]


dim(dfStates)

#Removing Unneded columns by column selector

dfStates <- dfStates[,-6:-10]

dim(dfStates)

rownames(dfStates) <- NULL # To Make index start again from 1 as upper and lower rows are deleted index has changed

# Renaming column names using colnames function 

colnames(dfStates)

colnames(dfStates)[1] <- "stateName"

colnames(dfStates)[2] <- "Census"

colnames(dfStates)[3] <- "Estimated"

colnames(dfStates)[4] <- "Pop2010"

colnames(dfStates)[5] <- "Pop2011"


dim(dfStates)

# Using gsub command to remove dot from stateName, comma from other columns 

dfStates$stateName <- gsub("\\.","",dfStates$stateName)

dfStates$Census <- gsub(",","",dfStates$Census)

dfStates$Estimated <- gsub(",","",dfStates$Estimated)

dfStates$Pop2010 <- gsub(",","",dfStates$Pop2010)

dfStates$Pop2011 <- gsub(",","",dfStates$Pop2011)

# Using as.numeric command

dfStates$Census <- as.numeric(gsub(" ","",dfStates$Census))

dfStates$Estimated <- as.numeric(gsub(" ","",dfStates$Estimated))

dfStates$Pop2010 <- as.numeric(gsub(" ","",dfStates$Pop2010))

dfStates$Pop2011 <- as.numeric(gsub(" ","",dfStates$Pop2011))

return(dfStates) # Return the data after munging to the function

}

dfStates <- readStates() # Calling the readStates function

View(dfStates)


# Min , Max and Mean of Pop2011

minPop2011 <- min(dfStates$Pop2011)

minPop2011 # Min of Pop2011

meanPop2011 <- mean(dfStates$Pop2011)

meanPop2011 # Mean of Pop2011

maxPop2011 <- max(dfStates$Pop2011)

maxPop2011 # Max of Pop2011


# which.max() on Pop2011


maxpop11 <- which.max(dfStates$Pop2011) # gives the index of max Pop2011

dfStates[maxpop11,][1,1] # gives the stateName from Index found above


# which.min() on Pop2011

minpop11 <- which.min(dfStates$Pop2011) # gives the index of max Pop2011

dfStates[minpop11,] # gives the whole row from index of min Pop2011 state

dfStates[minpop11,][1,1]# gives the stateName from Index found above using the index number

# Sort dataframe using Pop2011 column

dfStatesOrdered <- dfStates[order(dfStates$Pop2011),] # Sorting in Ascending order by default

View(dfStatesOrdered)

# Histogram using Pop2011 column

hist(dfStates$Pop2011) # Plotting the histogram of Pop2011 Column
