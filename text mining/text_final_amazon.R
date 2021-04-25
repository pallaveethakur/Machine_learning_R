#For Text Mining assignment


#TWO:
 # 1) Extract reviews of any product from ecommerce website like snapdeal and amazon
#2) Perform sentimental analysis

libs <- c('tm','plyr','class','dplyr','rvest')
lapply(libs,require,character.only=TRUE)


url <- "https://www.amazon.in/All-new-Echo-Dot-3rd-Gen/product-reviews/B07PFFMP9P/ref=cm_cr_arp_d_viewopt_srt?ie=UTF8&reviewerType=all_reviews&sortBy=recent&pageNumber=1"
page <- read_html(url)

name=page %>% html_nodes(".a-text-bold span") %>% html_text()
name
stars=page %>% html_nodes("#cm_cr-review_list .review-rating") %>% html_text()
stars
reviews=page %>% html_nodes(".review-text-content span") %>% html_text()
reviews

amazon <- data.frame(name,stars,reviews,stringsAsFactors = FALSE)
amazon

#sentimental analysis

getwd()
setwd("/Users/navidhithakur/Desktop/EXCELR/textmining")

library(sentimentr)

#data cleaning

View(amazon)


corpuss <- iconv(amazon$reviews,to="utf-8")
corpuss
corpus=Corpus(VectorSource(corpuss))
 inspect(corpus[1:5])

corpus=tm_map(corpus,tolower)
corpus=tm_map(corpus,removePunctuation)
corpus=tm_map(corpus,removeNumbers)
cleanset <- tm_map(corpus,removeWords,stopwords('english'))
inspect(cleanset[1:5])
removeurl <- function(x)gsub('http[[:alnum:]]*','',x)
cleanset <- tm_map(cleanset,content_transformer(removeurl))
inspect(cleanset[1:5])
removen <- function(y)gsub("\n",'',y)
cleanset <- tm_map(cleanset,content_transformer(removen))
inspect(cleanset[1:5])
cleanset <- tm_map(cleanset,stripWhitespace)
inspect(cleanset[1:5])

#termdocument matrix to change into structured data

tdm <- TermDocumentMatrix(cleanset)
tdm
#since sparsity is 88%,we'll get 0 88%times in our matrix

tdm <- as.matrix(tdm)
tdm

#barplot to see freq of words
w <- rowSums(tdm)
w
barplot(w,las=2,col=rainbow(50))

#as good has highest frequency that means positive reviews.

#wordcloud
library(wordcloud)
w <- sort(rowSums(tdm),decreasing = TRUE)
wordcloud(words = names(w),freq=w)

#sentimental analysis
lib <- c('lubridate','scales','reshape2')
lapply(lib,require,character.only=TRUE)
library(syuzhet)
library(dplyr)
library(ggplot2)

s <- get_nrc_sentiment(corpuss)
head(s)

barplot(colSums(s),las=2,col=rainbow(67),ylab='count',main='amazon sentiment analysis')

 


















































