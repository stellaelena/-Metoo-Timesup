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
library(syuzhet)
#install.packages("plotly")
library(plotly)





#load data metoo
newdata <- read_csv("me too/merge/metoo text.csv")
metoodata <- Corpus(VectorSource(newdata))

          # Clean the data

# convert to lower case
metoodata <- tm_map(metoodata, content_transformer(tolower))
# remove emojis
metoodata<-tm_map(metoodata, content_transformer(gsub), pattern="\\W",replace=" ")

# remove URLs
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
metoodata <- tm_map(metoodata, content_transformer(removeURL))

# remove anything other than English letters or space
#removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
#metoodata <- tm_map(metoodata, content_transformer(removeNumPunct))

# remove stopwords
metoodata <- tm_map(metoodata, removeWords, stopwords("english"))

# custom stop
myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),"www", "com", "twitter",
                 "pic", "n n", "ly", "n", "html", "ff", "2018", "t", "s", "co", "say", "metoo")
#gsub("abuse", "abus", metoodata)
metoodata <- tm_map(metoodata, removeWords, myStopwords)

# remove extra whitespace
metoodata <- tm_map(metoodata, stripWhitespace)

# Remove numbers
metoodata <- tm_map(metoodata, removeNumbers)

# Remove punctuations
metoodata <- tm_map(metoodata, removePunctuation)

      # Stemming

metoodata <- tm_map(metoodata, stemDocument)
metoodatacopy <- metoodata

      # Matrix and Word Frequencys

dtm <- TermDocumentMatrix(metoodata)

m <- as.matrix(dtm)
v <- sort(rowSums(m), decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 30)

ggplot(head(d, 15), aes(x=word, y=freq)) + 
    geom_bar(stat="identity",
             fill = "dark blue")+
  ggtitle("#MeToo Most Frequently used Terms") +
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
result <- get_nrc_sentiment(as.character(metoodatacopy))

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
  ggtitle("#MeToo Sentiments") +
  theme_classic()+
  labs( x = "Sentiment of the words used",
        y = "Count of unique sentiment words")

#plot the positive and negative
qplot(sentiment, data=new_result[9:10,], weight=count, geom="bar",fill=sentiment) + 
  ggtitle("#MeToo Sentiments") +
  theme_classic()+
  labs( x = "Sentiment of the words used",
        y = "Count of unique sentiment words")


      #Multiple Terms

#install.packages("corpus")
library(corpus)
corpus <- metoodata
term_pairs <- term_stats(corpus, ngrams = 2:3) 
print(term_pairs)
view(term_pairs)


#i exported the above list into excel to reformat and reimport

metoo_term_pairs_top <- read_excel("me too/merge/metoo term pairs top.xlsx")
View(metoo_term_pairs_top)

ggplot(data = metoo_term_pairs_top, aes(x = term, y = freq)) + 
  geom_bar(stat="identity",
           fill = "dark blue")+
  ggtitle("#Timesup Most Frequently used Terms") +
  theme_classic()+ 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs( x = "Word",
        y = "Frequency") 

#ggplot(term_pairs, aes(x = term)) +
#  geom_bar() +
#  ggtitle("#MeToo Most Frequently used Terms Pairs") +
#  theme_classic()+
#  labs( x = "Word",
#        y = "Count") +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


       #Ridgeline
#gapminder = dtm

#ggplot(gapminder, aes(y=as.factor(word),
 #                     x=freq)) +
  #geom_density_ridges(alpha=0.5) +
  #scale_y_discrete(expand = c(0.01, 0)) +  
  #scale_x_continuous(expand = c(0, 0))+
  #theme(axis.text=element_text(size=20))



