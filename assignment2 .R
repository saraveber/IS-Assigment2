library(readr)
library(epiDisplay)
library('plyr')

tekst = read_tsv("D:/INTSYS/train_data.tsv", col_names = TRUE)
tekst_za_primerjavo = read_tsv("D:/INTSYS/train_data.tsv", col_names = TRUE)

for(i in 1:6216){
  
  #remove links
  tekst[i,"text_a"] = gsub("http\\S*\\s+", '', tekst[i,"text_a"])
  #remove links at the end of line
  tekst[i,"text_a"] = gsub("http\\S*", '', tekst[i,"text_a"])
  
  #remove hashtags
  tekst[i,"text_a"] = gsub("#\\S*\\s+", '', tekst[i,"text_a"])
  #remove hashtags at the end of line
  tekst[i,"text_a"] = gsub("#\\S*", '', tekst[i,"text_a"])
  
  #remove @
  tekst[i,"text_a"] = gsub("@\\S*\\s+", '', tekst[i,"text_a"])
  #remove @ at the end of line
  tekst[i,"text_a"] = gsub("@\\S*", '', tekst[i,"text_a"])
  
  #remove RT's
  tekst[i,"text_a"] = gsub("RT\\s+", '', tekst[i,"text_a"])
  
  #remove non-ASCII
  tekst[i,"text_a"] = gsub("[^\x01-\x7F]\\s+", '', tekst[i,"text_a"])
  tekst[i,"text_a"] = gsub("[^\x01-\x7F]", '', tekst[i,"text_a"])
  
  #remove multiple ? in a row
  tekst[i,"text_a"] = gsub("\\?\\?+\\s+", '', tekst[i,"text_a"])
  tekst[i,"text_a"] = gsub("\\?\\?+", '', tekst[i,"text_a"])
  
  #remove (*)
  tekst[i,"text_a"] = gsub("(\\*)\\s+", '', tekst[i,"text_a"])
  tekst[i,"text_a"] = gsub("(\\*)", '', tekst[i,"text_a"])
}

write.table(tekst[2], file = "D:/INTSYS/train_text.txt", sep = ",", quote = FALSE, row.names = F,col.names = F)

a = scan(file='D:/INTSYS/train_text.txt',what="list")
a1 = data.frame(term=a)
train_frequencies = count(a1,vars="term")








tekst2 = read_tsv("D:/INTSYS/test_data.tsv", col_names = TRUE)
tekst_za_primerjavo2 = read_tsv("D:/INTSYS/test_data.tsv", col_names = TRUE)

for(i in 1:2140){
  
  #remove links
  tekst2[i,"text_a"] = gsub("http\\S*\\s+", '', tekst2[i,"text_a"])
  #remove links at the end of line
  tekst2[i,"text_a"] = gsub("http\\S*", '', tekst2[i,"text_a"])
  
  #remove hashtags
  tekst2[i,"text_a"] = gsub("#\\S*\\s+", '', tekst2[i,"text_a"])
  #remove hashtags at the end of line
  tekst2[i,"text_a"] = gsub("#\\S*", '', tekst2[i,"text_a"])
  
  #remove @
  tekst2[i,"text_a"] = gsub("@\\S*\\s+", '', tekst2[i,"text_a"])
  #remove @ at the end of line
  tekst2[i,"text_a"] = gsub("@\\S*", '', tekst2[i,"text_a"])
  
  #remove RT's
  tekst2[i,"text_a"] = gsub("RT\\s+", '', tekst2[i,"text_a"])
  
  #remove non-ASCII
  tekst2[i,"text_a"] = gsub("[^\x01-\x7F]\\s+", '', tekst2[i,"text_a"])
  tekst2[i,"text_a"] = gsub("[^\x01-\x7F]", '', tekst2[i,"text_a"])
  
  #remove multiple ? in a row
  tekst2[i,"text_a"] = gsub("\\?\\?+\\s+", '', tekst2[i,"text_a"])
  tekst2[i,"text_a"] = gsub("\\?\\?+", '', tekst2[i,"text_a"])
  
  #remove (*)
  tekst2[i,"text_a"] = gsub("(\\*)\\s+", '', tekst2[i,"text_a"])
  tekst2[i,"text_a"] = gsub("(\\*)", '', tekst2[i,"text_a"])
}

write.table(tekst2[2], file = "D:/INTSYS/train_text.txt", sep = ",", quote = FALSE, row.names = F,col.names = F)

a = scan(file='D:/INTSYS/train_text.txt',what="list")
a1 = data.frame(term=a)
test_frequencies = count(a1,vars="term")

