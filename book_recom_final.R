#Recommendation Systems
#Problem statement: Recommend a best book based on the author,publisher & ratings.


library(recommenderlab)
library(caTools)
library(readr)
library(Matrix)


book <- read.csv(file.choose())
View(book)
class(book)
summary(book)



range(book$Book.Rating)


sum(is.na(book))
attach(book)

str(book)
table(book$Book.Title)

#Rating distribution

hist(book$Book.Rating)
#data is positively skewed

book<-book_text[-1]#removing id

View(book)

#changing to matrix 
book_matrix <- as(book, 'realRatingMatrix')
View(book_matrix)
#fitting model

#Recommendation  system based on "Popularity based" #

book_recomm_model1 <- Recommender(book_matrix, method="POPULAR")

#  Predictions for two users 

recommended_items1 <- predict(book_recomm_model1, book_matrix[313:314], n=3)
as(recommended_items1, "list")

# Popularity model recommends the same books for all users 

#Using Collaborative Filtering
#  Recommednation system based on "Item based colloborative filtering" ###

book_recomm_model2 <- Recommender(book_matrix, method="IBCF")

#Predictions for Users 

recommended_items2 <- predict(book_recomm_model2, book_matrix[313:314], n=3)
as(recommended_items2, "list")



#  Recommednation system based on "USER based colloborative filtering" #

book_recomm_model3 <- Recommender(book_matrix, method="UBCF")

#Predictions for two users 

recommended_items3 <- predict(book_recomm_model3, book_matrix[313:314], n=3)
as(recommended_items3, "list")

