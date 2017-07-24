# install.packages("tm")
# install.packages("SnowballC")
# install.packages("tidytext")
# install.packages("reshape2")

library(tm)
library(SnowballC)
library(tidytext)
library(dplyr)
library(tidyr)
library(reshape2)

# Read in csv file, make the data frame smaller because of R's memory constraints.
text <- read.csv("/media/michelle/Michelle's USB/reddit_depress_2.csv")
text <- text[1:7350,]

# Convert data into a corpus object.
dfCorpus <- Corpus(VectorSource(text$selftext))


# Create a function to do text data cleaning on corpus.
# Involves stripping whitespaces, removing punctuation,
# making all words lower case, removing stop words, and stemming words.
clean_corpus <- function(corpus) {

  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stemDocument)

  return(corpus)
}

# Function to convert corpus into a Document Term Matrix, convert to R Matrix object
# Returns a dataframe with word frequencies as column vectors, and a separate
# column for document ids, labeled "Doc_ID"
term_document <- function(cleaned_corpus, sparsity) {

  doc_matrix <- cleaned_corpus
  doc_matrix <- DocumentTermMatrix(doc_matrix)
  doc_matrix <- removeSparseTerms(doc_matrix, sparsity)
  doc_matrix <- as.matrix(doc_matrix)
  doc_DF <- as.data.frame(doc_matrix)
  doc_DF$Doc_ID <- row.names(doc_DF)

  return(doc_matrix)

}

# Use the function to clean up the corpus.
cleaned_corpus <- clean_corpus(dfCorpus)

# Use function to get document term matrix.
doc_DF <- term_document(cleaned_corpus, .95)
