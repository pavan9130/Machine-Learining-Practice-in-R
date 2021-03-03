#importing data
crime_data <- read.csv("O:/EXCELR/Assignment/7-Clusterig/crime_data.csv")
crime_data_n <- crime_data[,-1]
View(crime_data_n)
#stding data 
normalize_data <- scale(crime_data_n)
#applying kmeans clustering model 
fit_model <- kmeans(normalize_data,6)
fit_model$cluster
final_data <-data.frame(crime_data ,fit_model$cluster)
View(final_data)
#calculating mean scores of variables for every cluster 
aggregate(final_data[,c(2,3,4,5)],by = list(final_data$fit_model.cluster),FUN=mean)

#elbow curve to decide the k value
#install.packages("factoextra")
library(factoextra)
fviz_nbclust(crime_data_n,kmeans,method="wss",print.summary = TRUE)+labs(subtitle = "Elbow method")
#updating the kmeans model with new no of clusters #7
fit_model_updated <- kmeans(normalize_data,7)
fit_model_updated$cluster
final_data_updated <-data.frame(crime_data ,fit_model_updated$cluster)
View(final_data_updated)
aggregate(final_data_updated[,c(2,3,4,5)],by = list(final_data_updated$fit_model_updated.cluster),FUN=mean)



