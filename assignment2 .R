library(readr)
library(data.table)


library(epiDisplay)
library('plyr')
library(tm)
library(ggplot2)
library(wordcloud)
library(CORElearn)

train = read_tsv("train_data.tsv", col_names = TRUE)
test = read_tsv("test_data.tsv", col_names = TRUE)

train_data_frame = data.frame(train["label"],train["text_a"])
names(train_data_frame) <-c("labels","text")
#train_data_frame = train_data_frame[1:10,]


test_data_frame = data.frame(test["label"],test["text_a"])
names(test_data_frame) <-c("labels","text")
#test_data_frame = test_data_frame[1:10,]


preprocess_text <- function(input_text){
    
    corpus <- Corpus(VectorSource(input_text))
    
    removeLinks <- function(x)gsub("http\\S*\\s+", '', x)
    corpus <- tm_map(corpus, content_transformer(removeLinks))
    
    removeLinksEndLine <- function(x)gsub("http\\S*", '', x)
    corpus <- tm_map(corpus, content_transformer(removeLinksEndLine))
    
    removeHashtag <- function(x)gsub("#\\S*\\s+", '', x)
    corpus <- tm_map(corpus, content_transformer(removeHashtag))
    
    removeHashtagEndLine <- function(x)gsub("#\\S*", '', x)
    corpus <- tm_map(corpus, content_transformer(removeHashtagEndLine))
    
    removeRT <- function(x)gsub("RT\\S*", '', x)
    corpus <- tm_map(corpus, content_transformer(removeRT))
    
    removeAt <- function(x)gsub("@\\S*\\s+", '', x)
    corpus <- tm_map(corpus, content_transformer(removeAt))
    
    removeAtEndLine <- function(x)gsub("@\\S*", '', x)
    corpus <- tm_map(corpus, content_transformer(removeAtEndLine))
    
    
    corpus <- tm_map(corpus, content_transformer(tolower))
    removeSpecialChars <- function(x) gsub("[^a-zA-Z ]", "", x)
    
    corpus <- tm_map(corpus, content_transformer(removeSpecialChars))
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeWords, stopwords('english'))
    corpus <- tm_map(corpus, stemDocument)
    corpus <- tm_map(corpus, stripWhitespace)
    
  
}

all_text_data <- data.frame(rbind(as.matrix(train_data_frame$text), as.matrix(test_data_frame$text)))
names(all_text_data) <- c("text")
corpus <- preprocess_text(all_text_data$text)
tdm <- TermDocumentMatrix(corpus)


#visualizations to make sense of data
#frequency of all words
allRowNames <- rownames(tdm)
termFrequency <- rowSums(as.matrix(tdm))
termFrequency <- subset(termFrequency, termFrequency >= 300)
qplot(seq(length(termFrequency)),sort(termFrequency), xlab = "index", ylab = "Freq")
#word cloud (words from real news)
idx = which(train_data_frame$labels == 1)
tdm_real = tdm[,idx]
mat <- as.matrix(tdm_real)
wordFreq <- sort(rowSums(mat), decreasing=TRUE)
grayLevels <- gray((wordFreq+10) / (max(wordFreq)+10))
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=100, random.order=F, colors=grayLevels)
#word cloud (words from fake news)
idx = which(train_data_frame$labels == 0)
tdm_fake = tdm[,idx]
mat <- as.matrix(tdm_fake)
wordFreq <- sort(rowSums(mat), decreasing=TRUE)
grayLevels <- gray((wordFreq+10) / (max(wordFreq)+10))
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=100, random.order=F, colors=grayLevels)

#dataset je uravnovesen
count(train_data_frame$labels)

## opazanja:
#najvecje so covid zadevscine
#fake news ima manj besed pogostejsih kot 100 kot real news - mogoc k so typoti
#dodaj feature -> stevilo besed ki se pojavijo samo 1* typo
#dodaj feature -> dolzina besedila
#dodaj feature ->
#razmisli kako bi bilo fino prefiltrirati besede - > nemorejo biti vse
#opcije:
# ---> lahko je brez pogostih
# ---> lahko je brez redkih 
# ---> lahko bi pogledali slovar viskoko letecih covid besed in nek intersection 
# za zdj bom delal brez redkih


#dodajmo feature length
text_length <- nchar(as.character(all_text_data$text))
text_length[is.na(text_length)] <- 0
text_length <- text_length / max(text_length)
text_length <- as.matrix(text_length)
text_length

#dodaj feature stevilo ne asci simbolov
only_asci <- data.frame(matrix(NA, NROW(all_text_data),NCOL(all_text_data)))
names(only_asci) <- "text"
for (i in 1:NROW(all_text_data$text)){
  only_asci$text[i] <- gsub("[^a-zA-Z ]", "", all_text_data$text[i])
}
num_of_nonasci <- nchar(as.character(all_text_data$text)) - nchar(as.character(only_asci$text))
num_of_nonasci <- num_of_nonasci/max(num_of_nonasci)
num_of_nonasci <- as.matrix(num_of_nonasci)
num_of_nonasci
#cbind(num_of_nonasci,train_data_frame$labels) #samo da malo pogledam povezavo

#stevilo besed ki se ponovijo samo enkrat (10krat)
termFrequency <- rowSums(as.matrix(tdm))
termFrequency <- subset(termFrequency, termFrequency <= 10)
only_once_tdm = as.matrix(tdm[names(termFrequency),])
num_less_ten <- as.matrix(colSums(only_once_tdm))
num_less_ten <- num_less_ten/max(num_less_ten)
num_less_ten 

#lets bind together features
tdm2 <- t(as.matrix(removeSparseTerms(tdm, sparse=0.999))) #ce zelis bolj ppogoste povecaj to stevilko ali naredi tako da vzame samo najpogostejse
final <- (cbind(tdm2,text_length,num_of_nonasci,num_less_ten))
colnames(final)[(ncol(final) - 2):ncol(final)]<- c("length", "num_nonasci", "num_less_ten")
colnames(final)

#split and add labels to train
len_train = nrow(train_data_frame)
len_test = nrow(test_data_frame)
len_all = nrow(final)

train_features = final[0:len_train,]
test_features = final[(len_train+1):len_all,]

train_data <- cbind(train_features,train_data_frame$labels) 
colnames(train_data)[ncol(train_data)] <- "LABELS"
colnames(train_data)
train_data <- data.frame(train_data)

#evaluate features
feature_vals <- attrEval(LABELS ~ ., train_data, "MDL")
feature_vals


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
train_corpus <- preprocess_text(train_data_frame$text)
train_tdm <- TermDocumentMatrix(train_corpus)
#train_tdm <- removeSparseTerms(train_tdm, sparse=0.95)
train_tdm_matrix <- t(as.matrix(train_tdm))
train_labels <- train_data_frame["labels"]

test_corpus <- preprocess_text(test_data_frame$text)
test_tdm <- TermDocumentMatrix(test_corpus)
#train_tdm <- removeSparseTerms(train_tdm, sparse=0.95)
test_tdm_matrix <- t(as.matrix(test_tdm))
test_labels <- test_data_frame["labels"] 


#subsample because data is to large:
#try smarter way to subsample


#visualizations and frequency of words
allRowNames <- rownames(tdm)
frequentRowNames <- findFreqTerms(tdm, lowfreq=300)

tdm
tdm <- removeSparseTerms(tdm, sparse=0.7)
termFrequency <- rowSums(as.matrix(tdm))
termFrequency <- subset(termFrequency, termFrequency >= 300)
qplot(seq(length(termFrequency)),sort(termFrequency), xlab = "index", ylab = "Freq")

    
mat <- as.matrix(tdm)                                                                                                      
wordFreq <- sort(rowSums(mat), decreasing=TRUE)
grayLevels <- gray((wordFreq+10) / (max(wordFreq)+10))
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=100, random.order=F, colors=grayLevels)







#----------------------------------------------------------------------------------------

train_data = data.frame(cbind(train_tdm_matrix, train_labels)) #,len,hastagnum,....)
test_data = data.frame(cbind(test_tdm_matrix, test_labels))




ncol(train_data)
ncol(test_data)
nrow(train_data)
nrow(test_data)

num_of_new_features = 1
colnames(train_data)[(ncol(train_data) - num_of_new_features):ncol(train_data)] <- c("labels")
colnames(test_data)[(ncol(test_data) - num_of_new_features):ncol(test_data)] <- c("labels")

# feature selection on train dataset (minimum description length)
feature_vals <- attrEval(labels ~ ., train_data, "MDL")








#... to do dodaj za vsako besedo posebej


#feture extraction and evaluation
# dodatni featurji:
# dolina, stevilo #, stevilo @, nevem mogoc vec k je tega bl je fake kaj js vem
#in tdm matrix

train_data = data.frame(cbind(train_tdm_matrix, train_labels)) #,len,hastagnum,....)
test_data = data.frame(cbind(test_tdm_matrix, test_labels))




ncol(train_data)

ncol(test_data)

nrow(train_data)
nrow(test_data)


num_of_new_features = 1
colnames(train_data)[(ncol(train_data) - num_of_new_features):ncol(train_data)] <- c("labels")
colnames(test_data)[(ncol(test_data) - num_of_new_features):ncol(test_data)] <- c("labels")

# feature selection on train dataset (minimum description length)


feature_vals <- attrEval(labels ~ ., train_data, "MDL")

















