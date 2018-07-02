# Collection of helper functions for performing sentiment analysis of tweets
# These functions are called from server.R

# function to convert characters to lowercase else return NA if cannot be converted to lowercase
toLower <- function(text)
{
  # this function takes text and converts to lowercase
  # Input: text; Output: text    
  
  # try to convert to lowercase
  errorLower <- tryCatch(tolower(text), error=function(e) e)
  
  # if cannot be converted to lowercase 
  if (inherits(errorLower, "error"))
    outLower <- NA    # set to NA 
  else  # if can be converted to lowercase
    outLower <- tolower(text)
  
  # return lowercase text
  return(outLower)
  
}

# Preprocessing and cleaning the tweets for sentiment analysis
preprocess_tweet <- function(tweets) {
  # this function takes input of tweets and cleans it up
  # cleaning is done by removing unwanted text for sentiment analysis
  # Input: unprocessed tweets; Output: cleaned tweets
  

  # extract the text from the tweets
  tweets_text <- sapply(tweets, function(x) x$getText())

  # Prepare the text for sentiment analysis by removing unnecessary text
  tweets_text <- gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets_text)
  tweets_text <- gsub("[ \t]{2,}", "", tweets_text)
  tweets_text <- gsub("[[:punct:]]", "", tweets_text)
  tweets_text <- gsub("[[:digit:]]", "", tweets_text)
  tweets_text <- gsub("^\\s+|\\s+$", "", tweets_text)
  tweets_text <- gsub("@\\w+", "", tweets_text)
  tweets_text <- gsub("http\\w+", "", tweets_text)
  
  # convert tweets to lowercase using "toLower" function
  tweets_text <- sapply(tweets_text, toLower)
  
  # remove any tweets which has NA values
  tweets_text <- tweets_text[!is.na(tweets_text)]
  
  # setting names to NULL
  names(tweets_text) <- NULL
 
  return(tweets_text)
  
}

# Sentiment Analysis using Emotion Classification
emotionSentimentAnal <- function (inText) {
  # this function performs emotion sentiment analysis using naive Bayes classification
  # Input: text (tweets); Output: emotion class

  # emotion classification
  emotionClass <- classify_emotion(inText, algorithm="bayes", prior=1.0)
  
  # extract emotion with the best possible fit
  emotion <- emotionClass[,7]
  
  # setting emotions having NA to "unknown"
  emotion[is.na(emotion)] <- "unknown"
  
  return(emotion)
}

# Sentiment Analysis using Polarity Classification
polaritySentimentAnal <- function (inText) {
  # this function performs polarity sentiment analysis using naive Bayes classification
  # Input: text (tweets); Output: polarity class

  # polarity classification
  polarityClass <- classify_polarity(inText, algorithm="bayes")
  
  # extract polarity with the best possible fit
  polarity <- polarityClass[,4]
  
  # setting polarity having NA to "unknown"
  polarity[is.na(polarity)] <- "unknown"
  
  return (polarity)
}