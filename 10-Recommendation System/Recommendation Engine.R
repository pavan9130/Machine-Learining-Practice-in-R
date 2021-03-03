library(Matrix)
library(recommenderlab)
#Importing books data
books_input <- read.csv("O:/EXCELR/Assignment/10-Recommendation System/book.csv")
books_input <- books_input[,-1]
#Reading data
View(books_input)

#Converting into realRatingMatrix
books_input_ratingMatrix <- as(books_input,"realRatingMatrix")
#plot for viewing realRatingMatrix
image(books_input_ratingMatrix[1:1000,1:2000])

# Recommendation model with method type "Popular"
books_recommend_object <- Recommender(books_input_ratingMatrix,method="POPULAR")
#Using Recommendation object for predicting top 5 recommendations for 10th user
books_predict <- predict(books_recommend_object,books_input_ratingMatrix[10],n=5)
books_predict_list <- as(books_predict,"list")
books_predict_list

# Recommendation model with method type "SVD"
books_recommend_object_new <- Recommender(books_input_ratingMatrix,method="SVD")
alarm()
View(books_recommend_object_new)
#Using Recommendation object for predicting top 5 recommendations for last 3 user
books_predict_new <- predict(books_recommend_object_new,books_input_ratingMatrix[2180:2182,],n=5)
#alarm()
books_predict_list_new <- as(books_predict_new,"list")
books_predict_list_new


