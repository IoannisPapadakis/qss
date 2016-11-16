##' ---
##' title: Who wrote the 11 unassigned Federalist Papers? 
##' author: Todd K. Hartman
##' date: November 2016
##' ---

##' Housekeeping
# Render the R script from the command line (not this R script)!
# rmarkdown::render("textual_analysis_of_the_federalist_papers.R")

# Load packages via pacman
pacman::p_load(tm, rmarkdown, SnowballC, wordcloud)

##' Load the raw corpus 
## 'federalist' is the folder in the working directory, pattern 'fp' is the structure of the file names
corpus.raw <- Corpus(DirSource(directory = "federalist", pattern = "fp"))

## Check the content of the Federalist Paper No. 10
head(content(corpus.raw[[10]]))

##' Process the Federalist Papers data for analysis
## Make all of the textual data lower case
corpus.prep <- tm_map(corpus.raw, content_transformer(tolower))

## Remove whitespace in the data
corpus.prep <- tm_map(corpus.prep, stripWhitespace)

## Remove punctuation
corpus.prep <- tm_map(corpus.prep, removePunctuation)

## Remove numbers
corpus.prep <- tm_map(corpus.prep, removeNumbers)

## Remove stop words 
head(stopwords("english"))
corpus <- tm_map(corpus.prep, removeWords, stopwords("english"))

## Reduce words to their root form 
corpus <- tm_map(corpus, stemDocument)

## Check the content of the processed Federalist Paper No. 10
head(content(corpus[[10]]))

##' Create a Document-Term Matrix with docs on the rows and terms on the cols
dtm <- DocumentTermMatrix(corpus)

## Summary information about the D-T Matrix
dtm

## Inspect rows 1:5 and columns 1:8 of the D-T Matrix
inspect(dtm[1:5, 1:8]) 

## Force D-T Matrix to be a standard matrix format
dtm.mat <- as.matrix(dtm)

##' Create a word cloud to visualize Federalist Papers 12 & 24
wordcloud(colnames(dtm.mat), dtm.mat[12, ], max.words = 20)
wordcloud(colnames(dtm.mat), dtm.mat[24, ], max.words = 20)

##' Retrieve the full words from their roots
stemCompletion(c("revenu", "commerc", "peac", "army"), corpus.prep)

##' Calculate Term Frequency-Inverse Document Frequency (tf-idf) 
## Downweight frequently occurring terms across documents 
## to understand if a term is truly important in a given text
dtm.tfidf <- weightTfIdf(dtm)

## Convert tf-idf to a matrix so that we can use it
dtm.tfidf.mat <- as.matrix(dtm.tfidf)

## Display the top-10 words for FP No. 12
head(sort(dtm.tfidf.mat[12, ], decreasing = TRUE), n = 10)

## Display the top-10 words for FP No. 24
head(sort(dtm.tfidf.mat[24, ], decreasing = TRUE), n = 10)

##' Cluster analysis of Federalist Papers written by Alexander Hamilton 
## Set the number of clusters
k <- 4

## Subset the FP written by Hamilton
hamilton <- c(1, 6:9, 11:13, 15:17, 21:36, 59:61, 65:85)
dtm.tfidf.hamilton <- dtm.tfidf.mat[hamilton, ]

## Run k-means cluster analysis
km.out <- kmeans(dtm.tfidf.hamilton, centers = k)
km.out$iter

## Summarize the results by displaying top-10 at each cluster centroid and FP labels to clusters
colnames(km.out$centers) <- colnames(dtm.tfidf.hamilton)
for (i in 1:k) {
    cat("CLUSTER", i, "\n")
    cat("Top 10 words: \n")
    print(head(sort(km.out$centers[i, ], decreasing = TRUE), n = 10))
    cat("\n")
    cat("Federalist Papers classified: \n")
    print(rownames(dtm.tfidf.hamilton)[km.out$cluster == i])
}

##' Calculating average word frequency by author
## Convert document-term matrix to matrix for manipulation
dtm1 <- as.matrix(DocumentTermMatrix(corpus.prep))

## Calculate term frequency per 1000 wods
tfm <- dtm1 / rowSums(dtm1) * 1000 # term frequency per 1000 words

## Words of interest
words <- c("although", "always", "commonly", "consequently",
           "considerable", "enough", "there", "upon", "while", "whilst")

## Select only these words
tfm <- tfm[, words]
head(tfm)

## Select essays written by Madison
madison <- c(10, 14, 37:48, 58)

## Calculate average word frequency among 1. Hamilton and 2.Madison essays
tfm.ave <- rbind(colSums(tfm[hamilton, ]) / length(hamilton),
                 colSums(tfm[madison, ]) / length(madison))
tfm.ave

##' Predicting authorship of 11 unattributed Federalist Papers 
## Outcome variable: Create a vector with missing values 
author <- rep(NA, nrow(dtm1))

## Assign vector 1 if written by Hamilton
author[hamilton] <- 1

## Assign vector -1 if written by Madison
author[madison] <- -1

## Add term frequency matrix 'ftm' for all essays whose authorship is known 
## We do this to test our model fit on known data (aka 'training' dataset)
author.data <- data.frame(author = author[c(hamilton, madison)],
                          tfm[c(hamilton, madison), ])

## Regress known authorship on 4 key words that differentiate Hamilton from Madison 
## The idea is that these authors have different writing styles 
hm.fit <- lm(author ~ consequently + there + upon + whilst,
             data = author.data)
hm.fit

## How do these predicted values compare to actual term frequencies?
key.words <- c(4, 7, 8, 10)
tfm.ave[, key.words]

## Calculate the standard deviation of the predicted values from the
## regression model to better understand effects sizes above
hm.fitted <- fitted(hm.fit)
sd(hm.fitted)

##' Cross validation of our model
## Compute a classification error for training dataset 
## Proportion of correctly classified essays by Hamilton
mean(hm.fitted[author.data$author == 1] > 0)

## Proportion of correctly classified essays by Madison 
mean(hm.fitted[author.data$author == -1] < 0)

## Out-of-sample prediction using 'leave-one-out cross-validation' algorithm 
## 1. Take out the ith observation and set it aside 
## 2. Fit model using the remaining n - 1 observations
## 3. Using fitted model, predict the outcome for the ith observation & compute the prediction error
## 4. Compute average prediction error across n observations as a measure of prediction accuracy

## Specify the number of observations in the training dataset
n <- nrow(author.data)

## Create a loop to run the Cross Validation algorithm
hm.classify <- rep(NA, n)  # container vector with missing values 
for (i in 1:n) {
    sub.fit <- lm(author ~ upon + there + consequently + whilst, data = author.data[-i, ])  # exclude ith row
    hm.classify[i] <- predict(sub.fit, newdata = author.data[i, ])  # predict authorship for the ith observation
}

## Calculate the proportion of correctly classified essays by Hamilton
mean(hm.classify[author.data$author == 1] > 0)

## Calculate the proportion of correctly classified essays by Madison
mean(hm.classify[author.data$author == -1] < 0)

##' Apply regression model to unknown authorship data 
## Identify the new data of FP essays with unknown authorship
disputed <- c(49, 50:57, 62, 63) 

##' Coerce the term frequency matrix for unknown authored essays into a matrix 
tf.disputed <- as.data.frame(tfm[disputed, ])

## Predict disputed authorship using our earlier model
pred <- predict(hm.fit, newdata = tf.disputed)

## Print predicted values +1 = Hamilton; -1 = Madison
pred

##' Visualize the results. 
## Plot fitted values for essays known to be authored by Hamilton (in red squares)
plot(hamilton, fitted(hm.fit)[author.data$author == 1], pch = 15,
     xlim = c(1, 85), ylim  = c(-2, 2), col = "red",
     xlab = "Federalist Papers", ylab = "Predicted values")
## Add in a dashed line at 0; positive values = Hamilton and negative values = Madison
abline(h = 0, lty = "dashed")
## Add essays authored by Madison in blue circles
points(madison, fitted(hm.fit)[author.data$author == -1], pch = 16, col = "blue")  
## Add disputed authorship predictions (in black triangles)
points(disputed, pred, pch = 17)  
