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
#train_data_frame = train_data_frame[1:100,]


test_data_frame = data.frame(test["label"],test["text_a"])
names(test_data_frame) <-c("labels","text")
#test_data_frame = test_data_frame[1:100,]


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

minDocFreq <- 10
maxDocFreq <- 300
tdm<- DocumentTermMatrix(corpus, control = list(bounds = list(global = c(minDocFreq, maxDocFreq))))
tdm <- as.matrix(tdm)
final <- (cbind(tdm2,new_features))
colnames(final)

#visualizations to make sense of data
#frequency of all words
allRowNames <- rownames(tdm)
termFrequency <- rowSums(as.matrix(tdm))
termFrequency <- data.frame(sort(subset(termFrequency, termFrequency >= 100)))
termFrequency <-cbind(termFrequency, rownames(termFrequency))
colnames(termFrequency) <- c("val","words")
p <- ggplot(data=termFrequency, aes(x=reorder(words,-val), y=val)) +  geom_bar(stat="identity", color="black", fill="white")+ggtitle("Most frequent words")+xlab("words")+ylab("frequency") # for the main title
p + coord_flip()


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
new_features <- text_length
colnames(new_features)[ncol(new_features)] <- "text_length"



#dodaj feature stevilo ne asci simbolov
only_asci <- data.frame(matrix(NA, NROW(all_text_data),NCOL(all_text_data)))
names(only_asci) <- "text"

for (i in 1:NROW(all_text_data$text)){
  only_asci$text[i] <- gsub("[^a-zA-Z ]", "", all_text_data$text[i])
}
num_of_nonasci <- nchar(as.character(all_text_data$text)) - nchar(as.character(only_asci$text))
num_of_nonasci <- num_of_nonasci/max(num_of_nonasci)
num_of_nonasci <- as.matrix(num_of_nonasci)
new_features <- cbind(new_features,num_of_nonasci)
colnames(new_features)[ncol(new_features)] <- "num_od_nonasci"
colnames(new_features)

#cbind(num_of_nonasci,train_data_frame$labels) #samo da malo pogledam povezavo

#stevilo besed ki se ponovijo samo enkrat (10krat)
intervals <- c(0,1,3,5,30,100,300,Inf)
for (i in 1:(length(intervals)-1)){
  termFrequency <- rowSums(as.matrix(tdm))
  termFrequency <- subset(termFrequency, termFrequency <= intervals[i+1])
  termFrequency <- subset(termFrequency, termFrequency > intervals[i])
  only_once_tdm <- as.matrix(tdm[names(termFrequency),])
  tren <- as.matrix(colSums(only_once_tdm))
  tren <- tren/max(tren)
  new_features <- cbind(new_features,tren)
  colnames(new_features)[ncol(new_features)] <- as.character(intervals[i])
}
colnames(new_features)

minDocFreq <- 10
maxDocFreq <- 800
dtm <- DocumentTermMatrix(corpus, control = list(weighting=weightTfIdf,bounds = list(global = c(minDocFreq, maxDocFreq))))
dtm2 <- as.matrix(dtm)
final <- (cbind(dtm2,new_features))
#tdm<- DocumentTermMatrix(corpus, control = list(bounds = list(global = c(minDocFreq, maxDocFreq))))
#tdm2 <- as.matrix(tdm)
#final <- (cbind(tdm2,new_features))
colnames(final)
final


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

num_features <- 100
threshold <- sort(feature_vals, decreasing=TRUE)[num_features]
selected_features <- c(feature_vals >= threshold, FALSE)
selected_features <- as.matrix(as.matrix(selected_features)[0:(NROW(selected_features)-1)])

colnames(selected_features)

X_train <- as.matrix(train_features[, selected_features])
y_train <- as.factor(train_data_frame$labels)

X_test <- as.matrix(test_features[, selected_features])
y_test <- as.factor(test_data_frame$labels)

colnames(X_train)
head(X_train)
head(X_test)

head(y_train)
head(y_test)
count (y_test)
count(y_train)

##-------------------------------------------------------------------------
##-------------------------------------------------------------------------
#metode



# classification accuracy
CA <- function(observed, predicted){
  t <- table(observed, predicted)
  return(sum(diag(t))/sum(t))
}

F1 <- function(observed, predicted) {
  predicted <- factor(as.character(predicted), levels=sort(unique(as.character(observed))))
  cm = as.matrix(table(observed, predicted))
  precision <- cm[2,2] /(cm[1,2]+cm[2,2])
  recall <- cm[2,2] /(cm[2,1]+cm[2,2])
  f1 <-  ifelse(precision + recall == 0, 0, 2 * precision * recall / (precision + recall))
  return(f1)
}


# majority classifier

predicted <- rep(1,nrow(X_test))
observed <- y_test
CA(observed, predicted)
###KAJ DA FAK JE NAROBE S TEMLE
F1(observed,predicted)

#
# NAIVE BAYES CLASSIFIER

X_train = data.frame(X_train)
X_test = data.frame(X_test)

library(CORElearn)
cm.nb <- CoreModel(y_train ~ ., data = X_train, model="bayes")
predicted <- predict(cm.nb, X_test, type="class")
observed <- y_test
CA_ = CA(observed, predicted)
F1_ = F1(observed,predicted)


NAMES = c("Naive Bayas classifier")
f1_scores = c(F1_)
ca_scores = c(CA_)
# k-NN classifier

library(class)
X_train = as.matrix(X_train)
X_test = as.matrix(X_test)
predicted <- knn(X_train, X_test, y_train)
observed <- y_test
CA_ = CA(observed, predicted)
F1_ = F1(observed,predicted)
#recall(observed, predicted)
#precision(observed, predicted)
NAMES = c(NAMES,"k-NN classifier")
f1_scores = c(f1_scores,F1_)
ca_scores = c(ca_scores,CA_)
f1_scores

# SVM with a radial basis kernel
library(kernlab)


model.svm <- ksvm(X_train, y_train, kernel="anovadot", scaled=TRUE)
predicted <- predict(model.svm, X_test, type="response")
observed <- y_test
CA_ = CA(observed, predicted)
F1_ = F1(observed, predicted)
#recall(observed, predicted)
#precision(observed, predicted)
NAMES = c(NAMES,"SVM classifier")
f1_scores = c(f1_scores,F1_)
ca_scores = c(ca_scores,CA_)
f1_scores
F1_
CA_
# random forest

library(randomForest)

rf <- randomForest(X_train, y_train)
predicted <- predict(rf, X_test, type="response")
observed <- y_test
CA_ = CA(observed, predicted)
F1_ =F1(observed,predicted)
#recall(observed, predicted)
#precision(observed, predicted)
NAMES = c(NAMES,"Random forest")
f1_scores = c(f1_scores,F1_)
ca_scores = c(ca_scores,CA_)
ca_scores = round(ca_scores,digits = 2)
f1_scores = round(f1_scores,digits = 2)
df = data.frame(ca_scores,f1_scores, NAMES)
df
p<-ggplot(data=df, aes(x=NAMES, y=ca_scores,label = sprintf("%0.2f", round(ca_scores, digits = 2)))) +
  geom_bar(stat="identity", fill="black")+
  geom_text(aes(label=ca_scores), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+xlab(" ")+ ylab("CA")
p
ca_scores
f1_scores
NAMES

p1<-ggplot(data=df, aes(x=NAMES, y=ca_scores,label = sprintf("%0.2f", round(ca_scores, digits = 2)))) +
  geom_bar(stat="identity", fill="black")+
  geom_text(aes(label=ca_scores), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+xlab(" ")+ ylab("CA")


p2<-ggplot(data=df, aes(x=NAMES, y=f1_scores,label = sprintf("%0.2f", round(f1_scores, digits = 2)))) +
  geom_bar(stat="identity", fill="black")+
  geom_text(aes(label=f1_scores), vjust=1.6, color="white", size=3.5)+
  theme_minimal()+xlab(" ")+ ylab("F1")

library(cowplot)
plot_grid(p1, p2)

