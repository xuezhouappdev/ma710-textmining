# Text mining and topic analysis of New York Times articles

# Make sure you run the commands in the 
# file 20151104_TextMining_functions.R
# before you run these commands. 

# Load the required libraries
library(cluster)
library(RCurl)
library(RJSONIO)
library(rlist)
library(stringr)
library(dplyr)
library(magrittr)
library(RTextTools)
library(ngram)

library(wordcloud)
library(RColorBrewer)

library(SnowballC)

install.packages("RWeka")
library(RWeka)



# Set this so we can see all columns 
# when printing dataframes
options(dplyr.width=Inf)

# Replace the following key with your own, 
# which you can obtain from the New York Times
# developer site http://developer.nytimes.com
articlesearch.key = "28fdd9ef177b47f1802f26d59d81d0a9"

# The code that you will modify to
# create your clusters starts here.

# OPTIONS: query, begin and end dates
# Create a dataframe of articles from the New York Times
get.nyt.hits(query.string="google",     # OPTION
             begin.date="20170315",    # OPTION
             end.date  ="20170402")    # OPTION

article.df = get.nyt.articles(pages = -1,                 # all articles
                              query.string = "google",     # OPTION
                              begin.date   = "20170315",  # OPTION
                              end.date     = "20170402")  # OPTION

dim(article.df)
matrix_whole= as.matrix(article.df)
str(article.df) #the whole data frame

# Check number of articles returned
num.articles = nrow(article.df)
num.articles

# Check a random sample of 5 articles 
doc.ndx = sample(1:num.articles,5)
doc.ndx
article.df[doc.ndx,] #get random observations from the extracted data

# OPTION: headline, snippet, lead_paragraph or abstract
# Create `docs` (the document vector) by
# choosing the text field that you will analyze.
docs = article.df$lead_paragraph 
matrix_field=as.matrix(docs)

str(docs)

# Check a few of the documents
docs[doc.ndx] 
# These same documents will be checked below
# after other modifications to the documents

# Remove punctuation and numbers.
# OPTION: you may find it useful to
# change the cleaning procedure and
# modify the function `clean.documents`.
docs.clean = clean.documents(docs)

matrix_clean=as.matrix(docs.clean)
dim(matrix_clean)



#wordcloud(matrix_clean, colors=brewer.pal(8, "Dark2"), max.words = 250)

###############################################################

save(docs.clean, 
     file="~/Desktop/ma710-textmining/docs.clean.RData")
load(file="~/Desktop/ma710-textmining/docs.clean.RData")

# Check the cleaned documents
docs.clean[doc.ndx]



################################################################################################
#above is the clean data set 



# OPTIONS: see code below
# Modify the words in the documents 
# with stemming, n-grams and stopwords





docs.sns = 
  modify.words(
    docs.clean,  
    stem.words=TRUE,  # OPTION: TRUE or FALSE
    ngram.vector=1, # OPTION: n-gram lengths
    stop.words=       # OPTION: stop words
      c(stopwords(kind="english"),"google" 
        # OPTION: "SMART" or "english" 
        # OPTION: additional stop words
      )
  )

docs.sns[doc.ndx]


matrix_modified=as.matrix(docs.sns)
matrix_modified
dim(matrix_modified)
# Be careful: some stop words from the 
# stopwords function might be important 
# For example, "new"
# "new" %in% stop.words # "new york", "new england" and "new hampshire" 

# Check documents
docs.sns[doc.ndx]

# OPTION: weighting, see below
# Create the document matrix
doc.matrix <- 
  create_matrix(docs.sns, 
                language="english",      # Do not change
                stemWords=TRUE,         # Do not change
                removePunctuation=FALSE, # Do not change
                weighting=tm::weightTf,
                ngramLength =1 # OPTION: weighting (see below)
  )
# Weighting OPTIONS:
# tm::weightTfIdf - term frequency-inverse document frequency
# tm::weightTf    - term frequency
# To use binary weighting use tm::weightTf and 
# create a "binary matrix" below.
                  
# Check the document matrix
doc.matrix

# OPTIONS: none, but this command must be run
# Create the document-term matrix
dtm = as.matrix(doc.matrix) 

# Check the matrix
dtm[1:10,1:10]
dim(dtm)

# Check the number of words in 
# the document term matrix
colnames(dtm)
ncol(dtm)

# Check the distribution of document-word frequencies 
table(dtm)

# OPTION: create a binary matrix
# in order to use binary weighting.
# DO NOT run this code if you 
# DO NOT want to use binary weighting.
# Only use with parameter
#     weighting=tm::weightTf 
# All positive frequencies become 1,
# indicating only the presence of a word  
# in a  document. 
# Uncomment the following line to use
# this code if you decide to use it. 
# dtm[dtm>1]=1 

# This may not make much of a difference 
# as nearly all document-word frequencies 
# are equal to 1. Most duplicate words are
# stopwords, and those have been removed. 

# Check the distribution of document-word frequencies 
# if you created a binary document-word matrix above
table(dtm)

# Check the distribution of word frequencies 
table(colSums(dtm))
# This gives the distribution of word frequencies 
# for the entire collection of articles

# OPTION: frequency threshold
# Keep words from the document term matrix
# that occur at least the number of times
# indicated by the `freq.threshold` parameter 
dtm=reduce.dtm(dtm,freq.threshold=20) 

# Check the number of columns/words 
# remaining in the document-term matrix
ncol(dtm)

# OPTION: number of clusters to find
k = 3

# OPTION: cluster algorithm 
cluster = kmeans(dtm,k)$cluster
# cluster = pam(dtm,k)$cluster
# hclust.res = hclust(dist(dtm))
# cluster = cutree(hclust.res,k)

# EVALUATE the clusters using `table` 
# to check the cluster sizes
as.data.frame(table(cluster))

# EVALUATE the clusters using `check.cluster` 
# to look at the common words in each cluster
# The second parameter is the minimum number of
# rows that a cluster must have to be displayed.
options(warn=-1)
check.clusters(cluster,5) 
options(warn=0)

# EVALUATE the clusters using `TopWords` 
# This is the same information as supplied
# by the `check.cluster` function, except 
# that the output is displayed vertically
options(warn=-1)
1:2 %>%
  lapply(function(i) TopWords(dtm, cluster, i))
options(warn=0)

# EVALUATE the clusters by looking 
# at the documents in the clusters
view.cluster(2)

# End
