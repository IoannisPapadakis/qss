##' ---
##' title: "Scraping Twitter Data Using R"
##' author: "Todd K. Hartman"
##' date: "`r format(Sys.Date())`"
##' output: "github_document"
##' ---

##' IMPORTANT
## Create a Twitter API key and token (as a developer) BEFORE scraping the data
## Website: htpps://apps.twitter.com (create a new developer app, then generate a new key and token)

##' Housekeeping
# Render the R script from the command line (not this R script)!
# rmarkdown::render("twitter_textual_analysis.R")

## Set the working directory
# setwd("YOUR DIRECTORY HERE")

## Load packages via pacman
pacman::p_load(ggplot2, httr, PKI, RColorBrewer, rmarkdown, ROAuth, SnowballC, tm, twitteR, wordcloud)

##' Store encrypted Twitter key and token as a list in separate R.Data file [ ONLY RUN ONCE ]
# ## Create public & private encryption keys
# rsa.key <- PKI.genRSAkey(bits = 4096L)  # Create a pair of encryption keys
# public.key <- PKI.save.key(rsa.key, private=FALSE)  # Get your public encryption key
# private.key <- PKI.save.key(rsa.key)  # Get your private encryption key
# keys <- list(public.key, private.key)  # Store the encryption keys in a list object
# save(keys, file = "encryption_keys.RData")  # Save the encryption keys in a separate file
#
# ## Encrypt Twitter keys and tokens using your PUBLIC encryption key
# public.rsa <- PKI.load.key(keys[[1]])  # Load your public encryption key
# api.key <- PKI.encrypt(charToRaw("ENTER YOUR TWITTER API KEY HERE"), public.rsa)
# api.secret <- PKI.encrypt(charToRaw("ENTER YOUR TWITTER API KEY SECRET HERE"), public.rsa)
# access.token <- PKI.encrypt(charToRaw("ENTER YOUR TWITTER TOKEN HERE"), public.rsa)
# access.token.secret <- PKI.encrypt(charToRaw("ENTER YOUR TWITTER TOKEN SECRET HERE"), public.rsa)
# twitter_keys <- list(api.key, api.secret, access.token, access.token.secret)  # Store the encrypted Twitter keys in a list object
# save(twitter_keys, file = "twitter_keys.RData") # Save the Twitter keys in a separate file
#
# ## Clear the workspace and history to keep secret information from prying eyes
# rm(rsa.key, public.key, private.key, public.rsa, api.key, api.secret, 
#   access.token, access.token.secret)  # Remove keys
#
# clearhistory <- function() {  # Create a function to wipe your R history
#    write("", file=".blank")
#    loadhistory(".blank")
#    unlink(".blank")
#  }
# clearhistory()  # Remove sensitive data from your R History (CLEARS ALL OF YOUR HISTORY)
#

##' Load your Twitter API Keys
load("encryption_keys.RData")  # Load your encryption keys
load("twitter_keys.RData")  # Load your encrypted Twitter info.

## Activate your decrypted Twitter keys and tokens
private.rsa <- PKI.load.key(keys[[2]])  # Load your public encryption key
setup_twitter_oauth(
    rawToChar(PKI.decrypt(twitter_keys[[1]], private.rsa)),
    rawToChar(PKI.decrypt(twitter_keys[[2]], private.rsa)),
    rawToChar(PKI.decrypt(twitter_keys[[3]], private.rsa)),
    rawToChar(PKI.decrypt(twitter_keys[[4]], private.rsa))
)

rm(keys, twitter_keys, private.rsa)  # Remove keys

##' Scrape Twitter REST data
## Scrape the last 1,000 tweets from a user
tw.timeline <- userTimeline("realDonaldTrump", n = 1000)
tw.timeline[1:10]

## Or, to scrape the last 1,000 tweets about a user (using the @ sign)
tw.user <- searchTwitter('@NateSilver538', lang = "en", n = 1000)

## Or, to scrape the last 1,000 tweets using hashtags (in English)
tw.hashtag <- searchTwitter("#donaldtrump", lang = "en", n=1000)

##' Process the tweets
## Convert tweets to a data frame
tweets.df <- twListToDF(tw.timeline)
head(tweets.df$text)

## Load tweets as raw corpus 
corpus.raw <- Corpus(VectorSource(tweets.df$text))

## Remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)  # function to remove URLs
corpus.prep <- tm_map(corpus.raw, content_transformer(removeURL))

## Remove anything else other than English letters or spaces
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)  # function to remove extraneous characters
corpus.prep <- tm_map(corpus.prep, content_transformer(removeNumPunct))

## Remove extra whitespace in the data
corpus.prep <- tm_map(corpus.prep, stripWhitespace)

## Make tweets lower case
corpus.prep <- tm_map(corpus.prep, content_transformer(tolower))

## Remove stop words 
corpus <- tm_map(corpus.prep, removeWords, stopwords("english"))

## Reduce words to their root form 
corpus.copy <- corpus  # Make a copy for later word retrieval
# corpus <- tm_map(corpus, stemDocument)

## Check the contents of the processed tweets
k <- 20  # Number of tweets to display
for(i in 1:k){
    cat(paste("Tweet", i))
    print(content(corpus[[i]]))
}

##' Create a Term-Document Matrix
tdm <- TermDocumentMatrix(corpus)
tdm

## Display frequent terms
(freq.terms <- findFreqTerms(tdm, lowfreq = 20))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 10)
df <- data.frame(term = names(term.freq), freq = term.freq)

## Create the figure
ggplot(df, aes(x = term, y = freq)) +
           geom_bar(stat = "identity") + 
           xlab("Terms") + 
           ylab("Count") +
           coord_flip()

##' Create a word cloud to visualize tweets
word.freq <- sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
wordcloud(words = names(word.freq), 
          freq = word.freq, 
          min.freq = 10, 
          random.order = FALSE,
          colors=brewer.pal(8, "Dark2"))


##' Alternative function to clean Twitter data
clean.text <- function(x)
{
    x <- tolower(x)      # To lower case
    x <- gsub("rt", "", x)  # Remove 'rt'
    x <- gsub("@\\w+", "", x)  # Remove '@'
    x <- gsub("[[:punct:]]", "", x)  # Remove punctuation
    x <- gsub("[[:digit:]]", "", x)  # Remove numbers
    x <- gsub("http\\w+", "", x)  # Remove 'http' links
    x <- gsub("[ |\t]{2,}", "", x)  # Remove tabs
    x <- gsub("^ ", "", x)  # Remove leading blank spaces
    x <- gsub(" $", "", x)  # Remove ending blank spaces
    return(x)
}
