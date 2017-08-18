options(stringsAsFactors = FALSE)
library(tm)
library(foreach)
library(randomForest)
library(gdata)
library(naivebayes)
library(magrittr)
library(class)
library(caret)
setwd('/Users/Reeddalton/Documents/GitHub/NBA-Shots-2014-2015/STA-380-Homework-2-Dalton-Haines-Jansen-Karnes/ReutersC50')
readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }
## Rolling two directories together into a single corpus

author_dirs_train = Sys.glob('C50train/*')
author_dirs_test = Sys.glob('C50test/*')
anames = list.dirs("C50train", full.names = FALSE)[-1]
file_list_test = NULL
labels_test = NULL
file_list_train = NULL
labels_train = NULL
for(author in author_dirs_train) {
  author_name_train = substring(author, first=10)
  files_to_add_train = Sys.glob(paste0(author, '/*.txt'))
  file_list_train = append(file_list_train, files_to_add_train)
  labels = append(labels, rep(author_name_train, length(files_to_add_train)))
}
for(author in author_dirs_test) {
  author_name_test = substring(author, first=10)
  files_to_add_test = Sys.glob(paste0(author, '/*.txt'))
  file_list_test = append(file_list_test, files_to_add_test)
  labels = append(labels, rep(author_name_test, length(files_to_add_test)))
}

# Need a more clever regex to get better names here
all_docs_train = lapply(file_list_train, readerPlain) 
names(all_docs_train) = file_list_train
names(all_docs_train) = sub('.txt', '', names(all_docs_train))

all_docs_test = lapply(file_list_test, readerPlain) 
names(all_docs_test) = file_list_test
names(all_docs_test) = sub('.txt', '', names(all_docs_test))



my_corpus_train = Corpus(VectorSource(all_docs_train))
my_corpus_test = Corpus(VectorSource(all_docs_test))

# Preprocessing
my_corpus_train = tm_map(my_corpus_train, content_transformer(tolower)) # make everything lowercase
my_corpus_train = tm_map(my_corpus_train, content_transformer(removeNumbers)) # remove numbers
my_corpus_train = tm_map(my_corpus_train, content_transformer(removePunctuation)) # remove punctuation
my_corpus_train = tm_map(my_corpus_train, content_transformer(stripWhitespace)) ## remove excess white-space
my_corpus_train = tm_map(my_corpus_train, content_transformer(removeWords), stopwords("SMART"))

#idk if we should be removing these
my_corpus_test = tm_map(my_corpus_test, content_transformer(tolower)) # make everything lowercase
my_corpus_test = tm_map(my_corpus_test, content_transformer(removeNumbers)) # remove numbers
my_corpus_test = tm_map(my_corpus_test, content_transformer(removePunctuation)) # remove punctuation
my_corpus_test = tm_map(my_corpus_test, content_transformer(stripWhitespace)) ## remove excess white-space
my_corpus_test = tm_map(my_corpus_test, content_transformer(removeWords), stopwords("SMART"))



DTM_train = DocumentTermMatrix(my_corpus_train)
DTM_train = removeSparseTerms(DTM_train, 0.95)
DTM_train # some basic summary statistics
## removing sparse terms. 

DTM_test = DocumentTermMatrix(my_corpus_test)
DTM_test =removeSparseTerms(DTM_test, 0.95)

# Now a dense matrix
X = as.matrix(DTM_train)
row.names(X) = file_list_train
X <- X/rowSums(X, na.rm = FALSE)


Y = as.matrix(DTM_test)
row.names(Y) = file_list_test
Y <- Y/rowSums(Y, na.rm = FALSE)


fnames = list.dirs(path = 'C50train/', full.names = FALSE, recursive = TRUE)[-1]


train_mat = t(X)
fnames_rep = rep(anames, each=50)
colnames(train_mat) = fnames_rep
train_df <- as.data.frame(t(train_mat))
train_df$category <- (rownames(train_df))


#test

test_mat = t(Y)
fnames_rep = rep(anames, each=50)
colnames(test_mat) = fnames_rep
test_df <- as.data.frame(t(test_mat))
test_df$category <- (rownames(test_df))


#test_df <- as.data.frame(test_mat)

model <- naive_bayes(category ~ ., data = train_df, laplace = 1)

preds <- predict(model, newdata = test_df)
conf_mat <- table(preds,train_df$category)
## sum of diagonals vs sum of everything

conf.mat <- confusionMatrix(preds, train_df$category)
conf.mat$overall['Accuracy']




train_filt <- train_df[,colnames(train_df) %in% colnames(test_df)]
author_train <- train_filt$author 
train_filt$author <- NULL

test_filt <- test_df[,colnames(test_df) %in% colnames(train_df)]
actual <- test_filt$author
test_filt$author <- NULL



rffit = randomForest(~.,data= mat, ntree=5,maxnodes=15)
importo = sort(rffit$importance, decreasing = FALSE)



