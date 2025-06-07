## PARSE AND FILTER WRITTEN TEXT
## Authors: Diego Iglesias & Miguel A. Sorrel
## This script is prepared to parse and filter tweets written in spanish. 
## With some changes, it can be useful to parse and filter text in any format.


# filter_text function:
# Receives as input a data frame containing a variable named "text" which stores raw text to be filtered.
# Returns as output clean text.


# Example:
# Load data and preview
# Data structure: data frame in which 1 variable contains the text: type character
# Simplified data example:
tweets <- data.frame(text = c("mi hermana me ha hecho un dibujo por mi cumpleaños ¡Es precioso! ¡Gracias! #feliz https://twitter.com/foto",
                                    "estoy harto de decepcionarme y estar triste, en esta vida siempre me pasa lo mismo #triste"))
head(tweets)

# filter_text
filter_text <- function(text_data){
  
  # Text pre-processing:
  library(NLP)
  library(tm) # package with datasets of stopwords in different languages
  
  # Stopwords and elements to be removed from the text such as mentions (@), links, etc.
  stopw <- c(stopwords("es"), "#feliz", "#triste", "pq", "bn", "q", "x", "tb") # spanish stopwords, target word in this case "#feliz", "#triste" and some abrevitions to be removed
  stop_el <- c("@", "/", "<", ">", "`","|", "_", 0:9, "*", "$") # sybmbols to be removed
  
  
  # Separation/split each tweet by words
  y <- strsplit(tolower(text_data$text), split = " ")
  
  # Removing stopwords
  for(i in 1:length(y)){
    for (j in 1:length(y[[i]])){
      if (any(stopw == y[[i]][j]) | nchar(y[[i]][j]) == "1") {
        y[[i]][j] <- 0 # Stopword identification
      }
    }
    if(any(y[[i]] == "0")) y[[i]] <- y[[i]][-which(y[[i]] == "0")]
    if(any(y[[i]] == "")) y[[i]] <- y[[i]][-which(y[[i]] == "")]
    if(length(y[[i]]) == 0) y[[i]] <- 0
  }
  # tweet text without stopwords
  y <- y[y != "0"]
  
  
  
  # Removing elements such as mentions "@", links "/", symbols, etc.
  a <- list()
  for (i in 1:length(y)) {
    a[[i]] <- strsplit(y[[i]], "") 
  } # tweet separation in words and letters; list structure: tweet_word_letter
  
  
  # identification of the elements to be removed
  for (i in 1:length(a)) {
    for (j in 1:length(a[[i]])) {
      for (k in 1:length(a[[i]][[j]])) {
        if (any(stop_el == a[[i]][[j]][k])) {
          a[[i]][[j]][k] <- 0 # identification of stop_el 
        }
      }
    }
  }
  
  
  # words containing stop elements are recoded as "0"
  for (i in 1:length(a)){
    for (j in 1:length(a[[i]])){
      if(any(a[[i]][[j]] == "0")) a[[i]][[j]] <- 0 # recoding
      a[[i]][[j]] <- paste(a[[i]][[j]], collapse = "")
    }
  }
  
  # words containing stop elements are removed from the tweet
  for (i in 1:length(a)){
    if(any(a[[i]] == "0")) a[[i]] <- a[[i]][-which(a[[i]] == "0")] 
  }
  
  
  # Generating a vector (b) with clean text 
  b <- vector()
  for (i in 1:length(a)){
    b[i] <- paste(unlist(a[[i]]), collapse = " ")
  }
  b <- b[b != ""] # vector with clean text
  
  
  ## Only for tweets:
  # The hashtag is removed from the words tagged (#). Punctuation marks are also removed. 
  b2 <- strsplit(b, split = "") # separation/split of words in letters and elements
  
  
  # Generating a vector (b3) with clean text: no hashtags and punctuation marks 
  b3 <- vector()
  el <- c("#", "!", "¡", "¿", "?", ".", ",", "\n", "(", ")") #symbols and punctuation marks to be removed
  for(i in 1:length(b2)){
    for (j in 1:length(el)) {
      b2[[i]] <- b2[[i]][b2[[i]] != el[j]] # removing hashtags and punctuation marks
    }
    b3[i] <- paste(unlist(b2[[i]]), collapse = "") # clean words are stored in b3
  }
  
  # final vector with clean text: no stopwrods, target words, symbols and punctuation marks
  return(b3)
  
}

# Filtered text 
b4=filter_text(text_data=tweets)
# Last parsing to eliminate emojis
for (i in 1:length(b4)){
  b4[i] <- paste(unlist(stringi::stri_match_all(b4[i], regex="[a-zñáéíóú]+")), collapse = " ")
}
clean_tweets <- b4

# Results: raw text data vs clean text prepared for sentiment analysis
tweets$text
clean_tweets
