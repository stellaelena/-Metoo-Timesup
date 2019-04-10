#install.packages("tm")
library(tm)

#install.packages('SnowballC')
library(SnowballC)

#install.packages('wordcloud') 
library(wordcloud)

#install.packages('RColorBrewer')
library(RColorBrewer)

#install.packages('syuzhet')
library(syuzhet)

#install.packages("ggplot2")
library(ggplot2)

#install.packages("reader")
library(readr)

#install.packages("ggridges")
library(ggridges)

#install.packages('devtools')
library(devtools)
#install_github("clauswilke/ggridges")
library(clauswilke/ggridges)

library(tidyverse)
library(stringr)        
library(tidytext)  




#load data timesup
newdata<- read_csv("times up/data/times up text.csv")
timesdata <- Corpus(VectorSource(newdata))

          # Clean the data

# convert to lower case
timesdata <- tm_map(timesdata, content_transformer(tolower))
# remove emojis
timesdata<-tm_map(timesdata, content_transformer(gsub), pattern="\\W",replace=" ")

# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
timesdata <- tm_map(timesdata, content_transformer(removeURL))

# remove anything other than English letters or space
#removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
#timesdata <- tm_map(timesdata, content_transformer(removeNumPunct))

# remove stopwords
timesdata <- tm_map(timesdata, removeWords, stopwords("english"))

# custom stop
myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),"www", "com", "twitter",
                 "pic", "timesup", "n n", "ly", "n", "html", "ff", "2018", "t", "s", "co")
#gsub("abuse", "abus", timesdata)
timesdata <- tm_map(timesdata, removeWords, myStopwords)

# remove extra whitespace
timesdata <- tm_map(timesdata, stripWhitespace)

# Remove numbers
timesdata <- tm_map(timesdata, removeNumbers)

# Remove punctuations
timesdata <- tm_map(timesdata, removePunctuation)

      # Stemming

timesdata <- tm_map(timesdata, stemDocument)
timesdatacopy <- timesdata

      # Matrix and Word Frequencys

dtm <- TermDocumentMatrix(timesdata)

m <- as.matrix(dtm)
v <- sort(rowSums(m)
          ,decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 30)

ggplot(head(d, 15), aes(x=word, y=freq,)) + 
  geom_bar(stat="identity", fill = "dark blue")+
  ggtitle("#Timesup Most Frequently used Terms") +
  theme_classic()+
  labs( x = "Word",
        y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  

      # Word Cloud
set.seed(42)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.25, 
          colors=brewer.pal(8, "Dark2"))

      # Sentiment Analysis
#carryout sentiment mining 
result <- get_nrc_sentiment(as.character(timesdatacopy))

#Result from list to a dataframe and transpose 
result1<-data.frame(t(result))

#rowSums 
new_result <- data.frame(rowSums(result1))

#name rows and columns of the dataframe
names(new_result)[1] <- "count"
new_result <- cbind("sentiment" = rownames(new_result), new_result)
rownames(new_result) <- NULL

#plot distict emotions
qplot(sentiment, data=new_result[1:8,], weight=count, geom="bar",fill=sentiment) +
  ggtitle("#Timesup Sentiments") +
  theme_classic()+
  labs( x = "Sentiment of the words used",
        y = "Count of unique sentiment words")

#plot the positive and negative
qplot(sentiment, data=new_result[9:10,], weight=count, geom="bar",fill=sentiment) + 
  ggtitle("#Timesup Sentiments") +
  theme_classic()+
  labs( x = "Sentiment of the words used",
        y = "Count of unique sentiment words")

      #Multiple Terms

#install.packages("corpus")
library(corpus)
corpus <- timesdata
term_pairs <- term_stats(corpus, ngrams = 2:3) 
print(term_pairs)
view(term_pairs)

#i exported the above list into excel to reformat and reimport

terms_pairs_top <- read_excel("times up/data/terms pairs top.xlsx")
View(terms_pairs_top)
 
ggplot(data = terms_pairs_top, aes(x = term, y = freq)) + 
  geom_bar(stat="identity",
           fill = "dark blue")+
   ggtitle("#Timesup Most Frequently used Terms") +
  theme_classic()+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs( x = "Word",
        y = "Frequency") 


      #Ridgeline
#gapminder = dtm

#ggplot(gapminder, aes(y=as.factor(word),
 #                     x=freq)) +
  #geom_density_ridges(alpha=0.5) +
  #scale_y_discrete(expand = c(0.01, 0)) +  
  #scale_x_continuous(expand = c(0, 0))+
  #theme(axis.text=element_text(size=20))



