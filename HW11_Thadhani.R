################################################
# IST 687, Standard Homework Heading
#
# Student name:Thadhani Hitesh Chandrakumar
# Homework number:HW11
# Date due: 21st Nov 2019 11:59pm
#
# Attribution statement: (choose the statements that are true)
# 2.I did this work with help from the book and the professor and these Internet sources: www.stackoverflow.com,www.rdocumentation.org

install.packages("tm")

library("tm")

library("tidyverse")

library("kernlab")

getwd()

setwd("C:\\Users\\hthadhan\\OneDrive - Syracuse University\\")

charVector <- scan("speech.txt",character(0),sep = "\n") # Reading the speech.txt using scan function

poswords <- scan("positive-words.txt",character(0),sep = "\n") # Reading the positive-words.txt using scan function

negwords <- scan("negative-words.txt",character(0),sep = "\n") # Reading the negative-words.txt using scan function

# Creating the corpus from the charVector document 

words.vec <- VectorSource(charVector)

words.corpus <- Corpus(words.vec)

words.corpus


words.corpus <- tm_map(words.corpus,content_transformer(tolower)) # All words to lower case

words.corpus <- tm_map(words.corpus,removePunctuation) # Removing punctuation 

words.corpus <- tm_map(words.corpus,removeNumbers) # Removing Numbers

words.corpus <- tm_map(words.corpus,removeWords,stopwords("english")) # Removing the English Stop words like is,or etc.

words.corpus

head(words.corpus)

summary(words.corpus)

tdm <- TermDocumentMatrix(words.corpus) # Creating a term document matrix from the corpus

tdm

inspect(tdm) # Inspect function to view the details of the term document matrix along with few samples


#Step 1:  Create a list of word counts from the speech

install.package("wordcloud")# Install the package wordcloud 

library("wordcloud") # Include wordcloud package in the code

matrix <- as.matrix(tdm) #Creating the matrix where rownames are the words and the columns are the documents number total of 166 

matrix # Gives the count of words in each of the document in the corpus


wordCounts <- rowSums(matrix) # Taking the sum of occurence of each word in those 166 items together word and the sum of the whole row for that particular word 

# A.	Create a named list of word counts by frequency, then sort as shown below:

wordCounts <- sort(wordCounts, decreasing=TRUE)

# B.	Run the head(wordCounts) command and explain what you see in a block comment. 

head(wordCounts)

# will the most used word in the speech as we can see it has come 58 times.

# C.	In a block comment, state how many unique words there are in the speech and what R code did you use to determine this answer

unique_words<-sum(unique(wordCounts)) 

unique_words

# There are total 371 unique words in the speech.

#D.	In a block comment, state how many total words there are in the speech and what R code did you use to determine this answer

total_words <- sum(wordCounts)

total_words

str(total_words)

# There are 2762 total words in the speech which i found out using the above code.

# Step 2:  Match the speech words with positive and negative words

# A.	Using the code provided below match the words from the speech to the list of positive words

# match function matches the words in poswords with the wordCounts returns the position/index of the word if matched and 0 if no match

matchedP <- match(names(wordCounts), poswords, nomatch = 0)

matchedP

# B.	Create a similar line of code to match the speech to the negative words. In a block comment explain the code that you wrote

# match function matches the words in negwords with the wordCounts returns the position/index of the word if matched and 0 if no match

matchedN <- match(names(wordCounts), negwords, nomatch = 0)

matchedN


# Here the match function uses the rowmanes of wordCounts as words to compare it with the negwords 
#which was read using scan function for negative-words.txt and output 0 if the words dont match which is the third argument nomatch=0
 
# matchedP and matchedN contain the indexes of the postive and negative words from the matched words in poswords and negwords

# C.	Write a block comment that described the information that "matchedP" contains. How long is that vector? What do the numeric values in it represent?

length(matchedP) # Total 1211 terms in the matchedP vector of integer type

str(matchedP) # Gives the structure of the matchedP

nonzeropos <- which(matchedP[]!=0) # Index of non zero matchedP vector elements

str(nonzeropos) # Total of 98 non zero positive values in matchedP vector

View(matchedP)

# matchedP contains index values of the matched postive words from poswords text file. There are 1211 terms in the matchedP and the numeric values are the indexes of positive words in poswords after matching.
# names(wordCounts) has work in 24th index which matches with work in poswords at index 2021 so the matchedP has 2021 in 24th row.

library("ggplot2") # Include ggplot2 package in the code

# A.	Using ggplot, make a bar chart of the positive matches


poscount <- wordCounts[nonzeropos] # Words along with their count of occuring for the positive words matched 

posdf <- data.frame(poscount) # new dataframe created with the count and words 

View(posdf)

posdf$names <- names(poscount) # giving words names to the names column being created

posdf$count <- posdf[,1] # giving the count of words to the count column being created

posdf <- posdf[,-1] # removing the unnamed count column

rownames(posdf) <- NULL # setting the rownames to null which are already now part of names column created above



# Plotting the bar plot for all the postive numbers

plotpos <- ggplot(posdf) + aes(x=(posdf$names),y=(posdf$count),stat_count("identity"))

plotpos <- plotpos + geom_bar(stat = "identity") 

plotpos


# stat_count("identity") is used because to plot barplot on y aesthetic which is 
# already summarized like count we need to include it or else it would not be able to map values to the y aesthetic.
# We use stat="identity" in geom_bar so that heights of the bars represent values in the data whereas using normal stat="bin" 
# makes the height of the bars represent number of the cases in each group.

# Rotating the text names by 90 degrees for better view

plotpos <- plotpos + theme(axis.text.x  = element_text(hjust = 1,angle = 90))

# Naming the plot and x,y axis of the bar plot

plotpos <- plotpos + xlab("wordnames") + ylab(" count of pos words") + ggtitle(" bar plot of positive matches")

plotpos


# B.	Using ggplot,  create a bar chart of the top 20 negative matches

nonzeroneg <- which(matchedN[]!=0) # Index of non zero matchedN negative words vector elements

nonzeroneg

negcount <- head(wordCounts[nonzeroneg],20) # Words along with their count of occuring for the top 20 negative words

negdf <- data.frame(negcount) # new dataframe created with the count and words 

View(negdf)


negdf$names <- names(negcount) # giving words names to the names column being created

negdf$count <- negdf[,1] # giving the count of words to the count column being created

negdf <- negdf[,-1] # removing the unnamed count column

rownames(negdf) <- NULL # setting the rownames to null which are already now part of names column created above

View(negdf)


# Plotting the bar plot for top 20 negative numbers

plotneg20 <- ggplot(negdf) + aes(x=(negdf$names),y=(negdf$count),stat_count("identity"))

plotneg20 <- plotpos + geom_bar(stat = "identity") 

plotneg20

# Rotating the text names by 90 degrees for better view

plotneg20 <- plotneg20 + theme(axis.text.x  = element_text(hjust = 1,angle = 90))

# Naming the plot and x,y axis of the bar plot

plotneg20 <- plotneg20 + xlab("wordnames") + ylab(" count of top 20 neg words") + ggtitle(" bar plot of top 20 negative matches")

plotneg20


# C.	Create two additional bar charts that only shows the negative, and positive words, that occurred at least twice.

postwice <- wordCounts[which(nonzeropos[]>2)] # getting the positive words occuring atleast twice

postwice

negtwice <- wordCounts[which(nonzeroneg[]>2)] # getting the negative words occuring atleast twice

negtwice

# Positive Twice Bar Plot

# Combining the positive words occuring twice along their counts in 1 dataframe for ggplot

twice_pos_words<- data.frame("count" = (postwice),"word_names" = (names(postwice)))


rownames(twice_pos_words) <- NULL # Reoving the rownames which are part of word_names column


View(twice_pos_words)

# Plotting the bar plot for positive atleast twice occuring words with their count of occurence

plotpostwice <- ggplot(twice_pos_words) + aes(x=(twice_pos_words$word_names),y=(twice_pos_words$count),stat_count("identity"))

plotpostwice <- plotpostwice + geom_bar(stat = "identity") 

plotpostwice

# Rotating the text names by 90 degrees for better view

plotpostwice <- plotpostwice + theme(axis.text.x  = element_text(hjust = 1,angle = 90))

# Naming the plot and x,y axis of the bar plot

plotpostwice <- plotpostwice + xlab("wordnames") + ylab(" count of pos words twice") + ggtitle(" bar plot of positive twice matches")

plotpostwice


# Negative Twice Plot

# Combining the negative words occuring twice along their counts in a dataframe for ggplot

twice_neg_words<- data.frame("count" = (negtwice),"word_names" = (names(negtwice)))

rownames(twice_neg_words) <- NULL # Removing the rownames which are part of word_names column

View(twice_neg_words)

# Plotting the bar plot for negative atleast twice occuring words with their count of occurence

plotnegtwice <- ggplot(twice_neg_words) + aes(x=(twice_neg_words$word_names),y=(twice_neg_words$count),stat_count("identity"))

plotnegtwice <- plotnegtwice + geom_bar(stat = "identity") 

plotnegtwice

# Rotating the text names by 90 degrees for better view

plotnegtwice <- plotnegtwice + theme(axis.text.x  = element_text(hjust = 1,angle = 90))

# Naming the plot and x,y axis of the bar plot

plotnegtwice <- plotnegtwice + xlab("wordnames") + ylab(" count of negative words twice") + ggtitle(" bar plot of negative twice matches")

plotnegtwice


# D.	Calculate the ratio that shows the proportion of positive words relative to the total number of words in the speech. Do the same for negative words. In a comment, explain if the speech was a positive or negative speech

propos <- length(nonzeropos)/length(wordCounts)

propos

# 8% words are positive in the speech

proneg <- length(nonzeroneg)/length(wordCounts)

proneg

# 6% words are negative in the speech

# The speech was positive as the proportion of positive words is 2% greater than proportion of negative words.


