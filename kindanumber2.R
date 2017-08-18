library(tm)
library(dplyr)
library(caret)



## Rolling two directories together into a single corpus

###TRAIN
trainC50 = Sys.glob('C50train/*')
file_list = NULL
labels = NULL
authors_list = NULL
files_to_add = NULL

readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }



all_train = lapply(trainC50, readerPlain) 
names(all_train) = trainC50
names(all_train) = t(data.frame(strsplit(names(all_train),'/')))[,1]
my_corpus = Corpus(VectorSource(all_train))



# Preprocessing
my_corpus = tm_map(my_corpus, content_transformer(tolower))
my_corpus = tm_map(my_corpus, content_transformer(removeNumbers))
my_corpus = tm_map(my_corpus, content_transformer(removePunctuation))
my_corpus = tm_map(my_corpus, content_transformer(stripWhitespace))
my_corpus = tm_map(my_corpus, content_transformer(removeWords), stopwords('SMART'))
my_corpus = tm_map(my_corpus, stemDocument)

#Creating a DTM and a list of all terms used
DTMtrain = DocumentTermMatrix(my_corpus, control= list(weighting = weightTfIdf))
DTM = removeSparseTerms(DTM, 0.95) 
DTM



#TEST

testC50 = Sys.glob('C50test/*')
file_list = NULL
labels = NULL
author_list = NULL
files_to_add = NULL

all_test = lapply(testC50, readerPlain) 
names(all_test) = testC50
names(all_test) =t(data.frame(strsplit(names(all_test),'/')))[,1]
my_corpus = Corpus(VectorSource(all_test))


# Preprocessing
my_corpus = tm_map(my_corpus, content_transformer(tolower))
my_corpus = tm_map(my_corpus, content_transformer(removeNumbers))
my_corpus = tm_map(my_corpus, content_transformer(removePunctuation))
my_corpus = tm_map(my_corpus, content_transformer(stripWhitespace))
my_corpus = tm_map(my_corpus, content_transformer(removeWords), stopwords('SMART'))
my_corpus = tm_map(my_corpus, stemDocument)

#Creating a DTM and a list of all terms used
DTMtest = DocumentTermMatrix(my_corpus, control= list(weighting =weightTfIdf))
DTMtest = removeSparseTerms(DTMtest, 0.95) 
DTM




#convert train and test to matrices
trainmatrix = as.matrix(DTMtrain)
testmatrix = as.matrix(DTMtest)


#convert train and test to dataframe
traindf = as.data.frame(trainmatrix)
testdf = as.data.frame(testmatrix)

####ERRORS HERE
#get author names as factor
names(traindf) = paste(names(traindf), '.w',sep='')
authors_factor_train = factor(names(DTMtrain))

names(testdf) = paste(names(traindf), '.w',sep='') 
authors_factor_test = factor(names(DTMtest))

#intersection of words
intersect = intersect(names(traindf),names(testdf))
traindf = traindf[,intersect]
testdf = testdf[,intersect]

#split to fit model
train = traindf
train$author = authors_factor_train
test = testdf
test$author = authors_factor_test



#naive bayes
library(naivebayes)
model = naive_bayes(author~.,data=train)
pred = data.frame(predict(model,test))
#confusion matrix
confusion = confusionMatrix(predict(model,test))
confusion$overall[1]
sensitivity = as.data.frame(confusion$byClass)
as.data.frame(sensitivity)[order(-sensitivity$Sensitivity),1:2]