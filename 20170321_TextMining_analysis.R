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
library(ggplot2)

#validation
library(clValid)


#ngram
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
             begin.date="20170201",    # OPTION
             end.date  ="20170408")    # OPTION

article.df = get.nyt.articles(pages = -1,                 # all articles
                              query.string = "google",     # OPTION
                              begin.date   = "20170201",  # OPTION
                              end.date     = "20170408")  # OPTION

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


#BELOW ARE TEST##############################################################################################################################
df = do.call("rbind",lapply(docs.clean, as.data.frame))
dim(df)
colnames(df) = "paragraph"

#build a corpus, and specify the sourece to be character vectors
myCorpus = Corpus(VectorSource(df$paragraph))
myCorpus


#mystopwords 
myStopwords = c(stopwords('english'),"google","inc","said")
#remove stopwords from corpus
myCorpus = tm_map(myCorpus,removeWords,myStopwords)

#convert to plain
#corpus.tmp <- tm_map(corpus.tmp,PlainTextDocument)

#keep a copy to use later as a dictinnary 
#myCorpusCopy= myCorpus
#myCorpusCopy


library(SnowballC)
#stem words
myCorpus = tm_map(myCorpus, stemDocument)


inspect(myCorpus[11:15])

for(i in 11:15) {
  cat(paste("[[", i, "]] ", sep=""))
  writeLines(strwrap(myCorpus[[i]], width=90))
}



#stem completion
myCorpus = tm_map(myCorpus, stemCompletion, dictionary=myCorpusCopy,type="prevalent")

inspect(myCorpus[11:15])

myCorpus[11:15]

myTdm = TermDocumentMatrix(myCorpus, control=list(wordLengths=c(2, Inf)))
myTdm
dim(myTdm)

#inspect the frequent words
findFreqTerms(myTdm, lowfreq = 10)

termFrequency = rowSums(as.matrix(myTdm))

termFrequency = subset(termFrequency, termFrequency>=10)
datafram = as.data.frame(termFrequency )

#wordcloud
wordcloud(names(termFrequency), termFrequency, max.words = 100,colors=brewer.pal(8, "Dark2")) 

library(ggplot2)

# Generate data
ggplot(data=datafram, aes(x=rownames(datafram), y=termFrequency)) +
  geom_bar(stat="identity")


barplot(termFrequency, las=2)


#cluster hieraharhical
myTdm2 = removeSparseTerms(myTdm, sparse=0.95)


dim(myTdm2)
m2 = as.matrix(myTdm2)
distMatrix =dist(scale(m2))
fit = hclust(distMatrix)

plot(fit)
rect.hclust(fit, k=8)
groups = cutree(fit, k=6)
groups


plot(groups)


#k means
m3 = t(m2)
set.seed(1)

k=6
kmeansResult = kmeans(m3, k)
#cluster centers
round(kmeansResult$centers, digits = 3)

#get topwords:
for (i in 1:k) {
  cat(paste("cluster ", i, ": ",sep="")) 
    s = sort(kmeansResult$centers[i,], decreasing = T)
    cat(name(s) [1:3],"\n")
    
}


library(clValid)
# the rows of the dataset must have names 
methods.vec = c("hierarchical","kmeans","pam")

clValid.result = clValid(m3,
                         5:7,maxitems=600,
                         clMethods=methods.vec,
                         validation="internal")



summary(clValid.result)


##bi-gram
# Make tokenizer function 
tokenizer <- function(x) 
  NGramTokenizer(x, Weka_control(min = 2, max = 2))

# Create unigram_dtm
unigram_dtm <- DocumentTermMatrix(text_corp)

# Create bigram_dtm
bigram_dtm <- DocumentTermMatrix(
  text_corp, 
  control = list(tokenize = tokenizer)
)

# Examine unigram_dtm
unigram_dtm

# Examine bigram_dtm
bigram_dtm


#TEST ENDS

########################################################################################################################################
#above is the clean data set 



# OPTIONS: see code below
# Modify the words in the documents 
# with stemming, n-grams and stopwords


#####################################################################



#uni-gram
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

#bi-gram
docs.sns = 
  modify.words(
    docs.clean,  
    stem.words=TRUE,  # OPTION: TRUE or FALSE
    ngram.vector=2, # OPTION: n-gram lengths
    stop.words=       # OPTION: stop words
      c(stopwords(kind="english"),"google" 
        # OPTION: "SMART" or "english" 
        # OPTION: additional stop words
      )
  )



matrix_modified=as.matrix(docs.sns)
matrix_modified[11:15]
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
reduced_dtm=reduce.dtm(dtm,freq.threshold=15) 


# Check the number of columns/words 
# remaining in the document-term matrix
ncol(reduced_dtm)
nrow(reduced_dtm)
glimpse(reduced_dtm)

#dataset for clustering 
reduced_dtm


rownames(reduced_dtm)
df_dtm = as.data.frame(reduced_dtm)
rownames(df_dtm )

#add one col to store contents 
df_dtm$lead_paragraph = rownames(df_dtm)
ncol(df_dtm)
df_dtm$lead_paragraph 

length(df_dtm$youtub)
length(df_dtm$lead_paragraph)


#clvalid
rownames(df_dtm)=1:717 
rownames(df_dtm)
colnames(df_dtm)


dtm.clValid = clValid(df_dtm[,-84],    #remove the contents col
                      nClust = 6:15,
                      clMethods = c("kemans","pam","hierarchical"),
                      validation = 'internal')
summary(dtm.clValid)

dtm.clValid = clValid(df_dtm[,1:612], 
                      nClust = 6:10,
                      clMethods = c("kemans","pam","hierarchical"),
                      validation = 'internal')
summary(dtm.clValid)



# OPTION: number of clusters to find
k = 8
k = 6

# OPTION: cluster algorithm KMEANS
cluster = kmeans(df_dtm[,-84],k)$cluster
table(cluster) #frequency of each cluster



#pam and silhuosette plot
cluster = pam(df_dtm[,-84],k)$cluster

data.dist.mat = daisy(df_dtm[,-84])
data.sil2 = silhouette(x=as.numeric(pam(df_dtm[,-84],k)$cluster),
                       dist=data.dist.mat)
layout(matrix(1))
plot(data.sil2)



#hclust
hclust.res = hclust(dist(df_dtm[,-84]))
cluster = cutree(hclust.res,k)
plot(hclust.res)





# EVALUATE the clusters using `table` to check the cluster sizes
as.data.frame(table(cluster))

# EVALUATE the clusters using `check.cluster` 
# to look at the common words in each cluster
# The second parameter is the minimum number of rows that a cluster must have to be displayed.
options(warn=-1)
check.clusters(cluster,4)   
options(warn=0)

# EVALUATE the clusters using `TopWords` 
# This is the same information as supplied
# by the `check.cluster` function, except 
# that the output is displayed vertically
options(warn=-1)
1: 6%>%
  lapply(function(i) TopWords(as.matrix(df_dtm[,-84]), cluster, i))
options(warn=0)

# EVALUATE the clusters by looking at the documents in the clusters (the version with no clean)
view.cluster(2)
view.cluster(1)

# End
