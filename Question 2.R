library(tm)
library(foreach)
library(randomForest)
setwd('/Users/Reeddalton/Documents/GitHub/NBA-Shots-2014-2015/STA-380-Homework-2-Dalton-Haines-Jansen-Karnes/ReutersC50')
# Remember to source in the "reader" wrapper function
# it's stored as a Github gist at:
# https://gist.github.com/jgscott/28d9d1287a0c3c1477e2113f6758d5ff
readerPlain = function(fname){
  readPlain(elem=list(content=readLines(fname)), 
            id=fname, language='en') }
## Rolling two directories together into a single corpus
author_dirs_train = Sys.glob('C50train/*')
author_dirs_test = Sys.glob('C50test/*')
file_list_test = NULL
labels_test = NULL
file_list_train = NULL
labels_train = NULL
for(author in author_dirs_train) {
	author_name_train = substring(author, first=29)
	files_to_add_train = Sys.glob(paste0(author, '/*.txt'))
	file_list_train = append(file_list_train, files_to_add_train)
	labels = append(labels, rep(author_name_train, length(files_to_add_train)))
}
for(author in author_dirs_test) {
  author_name_test = substring(author, first=29)
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
DTM_train # some basic summary statistics

DTM_test = DocumentTermMatrix(my_corpus_test)
DTM_test
class(DTM_test)  # a special kind of sparse matrix format

## You can inspect its entries...
inspect(DTM[1:2500,1:50])
DTM_train = removeSparseTerms(DTM_train, 0.975)
DTM

# Now a dense matrix
X = as.matrix(DTM_train)
row.names(X) = file_list_train

Y = as.matrix(DTM_test)
row.names(Y) = file_list_test
# Naive Bayes: the training sets for the two authors


num_words = dim(X)[2]
train_mat=matrix(0,50,num_words)
smooth_count=1/nrow(X)
count=1
i=1
while(count<2500){
  up_int = count+49
  docs_i=matrix(X[count:up_int,],50,num_words)
  train_mat[i,] = colSums(docs_i)
  train_mat[i, ]=train_mat[i,]/sum(train_mat[i,])
  count = count+50
  i=i+1
}
colnames(train_mat) = colnames(X)
train_mat = t(train_mat)
filenames = list.dirs(path = 'C50train/', full.names = FALSE, recursive = TRUE)
filenames = filenames[2:51]
colnames(train_mat) = filenames
test_mat = list.files(path = 'C50test/',pattern = ".txt$",full.names = FALSE, recursive = TRUE)
#test_mat = t(Y)
rownames(Y) = test_mat
Y <- Y/rowSums(Y, na.rm = FALSE)
Y = t(Y)



fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare +
                      Embarked + Title + FamilySize + FamilyID2,
                    data=train, 
                    importance=TRUE, 
                    ntree=2000)

AP_train = X[1:45,]
AC_train = X[51:95,]

# AP's multinomial probability vector
# Notice the smoothing factor
# Why?
smooth_count = 1/nrow(X)
w_AP = colSums(AP_train + smooth_count)
w_AP = w_AP/sum(w_AP)

# AC's multinomial probability vector
w_AC = colSums(AC_train + smooth_count)
w_AC = w_AC/sum(w_AC)

# Let's take a specific test document
x_test = X[49,]
head(sort(x_test, decreasing=TRUE), 25)

# Compare log probabilities under the Naive Bayes model
sum(x_test*log(w_AP))
sum(x_test*log(w_AC))

# Another test document
x_test2 = X[99,]
head(sort(x_test2, decreasing=TRUE), 25)
sum(x_test2*log(w_AP))
sum(x_test2*log(w_AC))

