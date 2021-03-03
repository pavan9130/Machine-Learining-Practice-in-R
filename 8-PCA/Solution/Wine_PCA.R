#importing data
PCA_wine_old <- read.csv("O:/EXCELR/Assignment/8-PCA/wine.csv")
View(PCA_wine_old)
PCA_wine_old$Type <- as.character(PCA_wine_old$Type)
PCA_wine_old$Type <- factor(PCA_wine_old$Type)
str(PCA_wine_old)

PCA_wine <- PCA_wine_old[,-1]
View(PCA_wine)
#stding data
PCA_wine <- scale(PCA_wine)
PCA_object <-princomp(PCA_wine,cor = T,scores = T)
summary(PCA_object)
PCA_object$loadings
View(PCA_object$scores)


#Taking Top 3 PCA scores for clustering analaysis

Cluster_Data <- as.data.frame(PCA_object$scores[,1:3])
Cluster_Data <- data.frame(Cluster_Data,PCA_wine_old[,1])
View(Cluster_Data)

#Calculating Distance Matrix

Distobject <- dist(Cluster_Data,method = "euclidean")

#Applying Hclust method

Hclusterobject  <- hclust(Distobject,method = "complete")
plot(Hclusterobject,hang = -1)
Groups <- cutree(Hclusterobject,k=9)
rect.hclust(Hclusterobject,border = "red",k = 9)
Groups <- as.data.frame(Groups)
View(Groups)
PCA_wine_Final <- data.frame(PCA_wine_old,Groups)
View(PCA_wine_Final)
aggregat <- aggregate(PCA_wine_Final[,c(-1,-15)],by = list(PCA_wine_Final$Groups),FUN =mean)
View(aggregat)


write.csv(PCA_wine_Final,file="Wine_Clustered.csv",row.names = F)

################################# K- means without PCA ####################

#applying kmeans clustering model 
PCA_wine <- data.frame(PCA_wine,PCA_wine_old[,1])
fit_model <- kmeans(PCA_wine,6)
fit_model$cluster
final_data <-data.frame(PCA_wine_old ,fit_model$cluster)
View(final_data)
#calculating mean scores of variables for every cluster 
aggregate(final_data[,c(-1,-15)],by = list(final_data$fit_model.cluster),FUN=mean)

#elbow curve to decide the k value
#install.packages("factoextra")
library(factoextra)
fviz_nbclust(PCA_wine,kmeans,method="wss",print.summary = TRUE,k.max = 15)+labs(subtitle = "Elbow method")
#updating the kmeans model with new no of clusters #12
fit_model_updated <- kmeans(PCA_wine,12)
fit_model_updated$cluster
final_data_updated <-data.frame(PCA_wine_old ,fit_model_updated$cluster)
View(final_data_updated)
aggregate(final_data_updated[,c(-1,-15)],by = list(final_data_updated$fit_model_updated.cluster),FUN=mean)



################################# K- means ####################

#applying kmeans clustering model 
fit_model_n <- kmeans(Cluster_Data,6)
fit_model_n$cluster
final_data_n <-data.frame(PCA_wine_old ,fit_model_n$cluster)
View(final_data_n)
#calculating mean scores of variables for every cluster 
aggregate(final_data_n[,c(-1,-15)],by = list(final_data_n$fit_model_n.cluster),FUN=mean)

#elbow curve to decide the k value
#install.packages("factoextra")
library(factoextra)
fviz_nbclust(Cluster_Data,kmeans,method="wss",print.summary = TRUE,k.max = 15)+labs(subtitle = "Elbow method")
#updating the kmeans model with new no of clusters #10
fit_model_updated_n <- kmeans(Cluster_Data,10)
fit_model_updated_n$cluster
final_data_updated_n <-data.frame(PCA_wine_old ,fit_model_updated_n$cluster)
View(final_data_updated_n)
aggregate(final_data_updated_n[,c(-1,-15)],by = list(final_data_updated_n$fit_model_updated_n.cluster),FUN=mean)
