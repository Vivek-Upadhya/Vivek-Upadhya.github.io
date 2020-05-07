#load library
library(stringr)
library(NLP)
library(tm)
library(RColorBrewer)
library(wordcloud)
library(slam)

#get and set the library

getwd()

#Load the data
Drug_data <- read.delim("E:/study Material/data science/drugLibTrain_raw.tsv")
View(Drug_data)

head(Drug_data,10)


#Text preprocessing for better model building

#remove the white space from the data

Drug_data$commentsReview<-str_trim(Drug_data$commentsReview)


#Create health review to the corpus
# Corpus is the way to chnage the each health comment review sting
#convert into the single document

Drug_review_comment<-Corpus(VectorSource( Drug_data$commentsReview))

# to check corpus
Drug_review_comment
# for inspect we use writeline
writeLines(as.character(Drug_review_comment[[1]]))

#Text cleaning process to clean the text and make as best fit for model

#Case folding
#tm_map is use for convert the all upper case letter to lower case

Drug_review_comment<-tm_map(Drug_review_comment,tolower)

# Remove stope word from the data
stopwords()
Drug_review_comment<-tm_map(Drug_review_comment,removeWords,stopwords("English"))

#Remove the punctuation
Drug_review_comment<-tm_map(Drug_review_comment,removePunctuation)

#Remove the numbers
Drug_review_comment<-tm_map(Drug_review_comment,removeNumbers)

#Remove the extra white space

Drug_review_comment<-tm_map(Drug_review_comment,stripWhitespace)

#Convert into the plain text

Drug_review_comment<-tm_map(Drug_review_comment,PlainTextDocument)

# After rum all the pre pro-process text mining we have to again change into corpus
# because we may it change the data type as we applied so many change

#chnage again to the corpus

Drug_review_comment<-Corpus(VectorSource(Drug_review_comment))

#Build the term document frequency

tdm<-TermDocumentMatrix(Drug_review_comment)

#Let make as a data frame to look better

tdm_matrix<-as.data.frame(t(as.matrix(tdm)))
View(tdm_matrix)

#Calculate the term frequency t calclulate how many time world appear
word_freq<-rollup(tdm,2,na.rm=TRUE,FUN = sum)

#Convert into the mtrix of appear word
word_freq<-as.data.frame(as.matrix(word_freq))
View(word_freq)



#Covert the row.name  into index. we do because we nat our data into better ways
word_freq$word<-row.names(word_freq)
row.names(word_freq)<-NULL
word_freq<-word_freq[,c(2,1)]
names(word_freq)<-c("Word","Frequency")
View(word_freq)


#Mostt frequent word term which appear in top 100 we select
findFreqTerms(tdm,5)
findMostFreqTerms(tdm,5)

#visualization

Drug_review_comment_re<-Drug_review_comment
pla2<-brewer.pal(12,"Dark2")
png("Word.png",width = 12,height =10,units = "in",res=300)

wordcloud(Drug_review_comment_re,scale = c(4,.5),min.freq = 3,max.words = 100,random.order = FALSE,rot.per = .15,colors = pla2)
dev.off()
