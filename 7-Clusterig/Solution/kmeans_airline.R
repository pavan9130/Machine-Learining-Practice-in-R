#importing data
library(readxl)
Airline_Data <- read_excel("O:/EXCELR/Assignment/7-Clusterig/EastWestAirlines.xlsx",sheet = "data")
Airline_Data_n <- Airline_Data[,-1]
View(Airline_Data_n)
str(Airline_Data_n)
Airline_Data_n[,11] <- lapply(Airline_Data_n[,11],factor)
Airline_Data_n[,1:10] <- scale(Airline_Data_n[,1:10])
PCA_object <-princomp(Airline_Data_n[,-11],cor = T,scores = T)
summary(PCA_object)
PCA_object$loadings
View(PCA_object$scores)

#Taking Top 8 PCA scores for clustering analaysis

Cluster_Data <- as.data.frame(PCA_object$scores[,1:8])
Cluster_Data <- data.frame(Cluster_Data,Airline_Data_n[,11])
View(Cluster_Data)
str(Cluster_Data)

#applying kmeans clustering model 
fit_model <- kmeans(Cluster_Data,6)
fit_model$cluster
final_data <-data.frame(Airline_Data ,fit_model$cluster)
View(final_data)
#calculating mean scores of variables for every cluster 
aggregate(final_data[,c(-1,-12,-13)],by = list(final_data$fit_model.cluster),FUN=mean)

#elbow curve to decide the k value
#install.packages("factoextra")
library(factoextra)
fviz_nbclust(Cluster_Data,kmeans,method="wss",print.summary = TRUE,k.max = 20)+labs(subtitle = "Elbow method")
#updating the kmeans model with new no of clusters #7
fit_model_updated <- kmeans(Cluster_Data,15)
fit_model_updated$cluster
final_data_updated <-data.frame(Airline_Data ,fit_model_updated$cluster)
View(final_data_updated)
aggregate(final_data_updated[,c(-1,-12,-13)],by = list(final_data_updated$fit_model_updated.cluster),FUN=mean)



