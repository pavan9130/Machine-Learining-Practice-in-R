#importing data
library(readxl)
library(xlsx)
install.packages("xlsx")
Airline_Data <- read_excel("O:/EXCELR/Assignment/7-Clusterig/EastWestAirlines.xlsx",sheet = "data")
Airline_Data <- Airline_Data[,-1]
View(Airline_Data)
str(Airline_Data)
Airline_Data[,11] <- lapply(Airline_Data[,11],factor)
PCA_object <-princomp(Airline_Data[,-11],cor = T,scores = T)
summary(PCA_object)
PCA_object$loadings
View(PCA_object$scores)
#Taking Top 7 PCA scores for clustering analaysis

Cluster_Data <- as.data.frame(PCA_object$scores[,1:7])
Cluster_Data <- data.frame(Cluster_Data,Airline_Data[,11])
View(Cluster_Data)


################################euclidean+single##########################

distanceMatrix1 <- dist(Cluster_Data,method="euclidean",diag = TRUE,upper = T)
hclustermodel1 <- hclust(distanceMatrix1,method = "single")
plot(hclustermodel1,hang = -1)
groups1 <- cutree(hclustermodel1,k=10)
rect.hclust(hclustermodel1,k=10,border = "red")
groups1 <- as.matrix(groups1)
View(groups1)
Airline_Data_new1 <-data.frame(Airline_Data,groups1)
View(Airline_Data_new1)
agreegate1 <- aggregate(x = Airline_Data_new1[,-11],by = list(Airline_Data_new1$groups1),FUN ="mean")
write.xlsx(agreegate1,file = "clustering_using_combinations.xlsx", sheetName = "euclidean+single",row.names = F)

#######################################Euclidean+Complete####################
distanceMatrix2 <- dist(Cluster_Data,method="euclidean",diag = TRUE,upper = T)
hclustermodel2 <- hclust(distanceMatrix2,method = "complete")
plot(hclustermodel2,hang = -1)
groups2 <- cutree(hclustermodel2,k=10)
rect.hclust(hclustermodel2,k=10,border = "red")
groups2 <- as.matrix(groups2)
View(groups2)
Airline_Data_new2 <-data.frame(Airline_Data,groups2)
View(Airline_Data_new2)
agreegate2 <- aggregate(x = Airline_Data_new1[,-11],by = list(Airline_Data_new2$groups2),FUN ="mean")
write.xlsx(agreegate2,file = "clustering_using_combinations.xlsx", sheetName = "euclidean+complete",
           row.names = F,append = T)

#######################################Euclidean+median####################

distanceMatrix3 <- dist(Cluster_Data,method="euclidean",diag = TRUE,upper = T)
hclustermodel3 <- hclust(distanceMatrix3,method = "median")
plot(hclustermodel3,hang = -1)
groups3 <- cutree(hclustermodel3,k=10)
rect.hclust(hclustermodel3,k=10,border = "red")
groups3 <- as.matrix(groups3)
View(groups3)
Airline_Data_new3 <-data.frame(Airline_Data,groups3)
View(Airline_Data_new3)
agreegate3 <- aggregate(x = Airline_Data_new1[,-11],by = list(Airline_Data_new3$groups3),FUN ="mean")
write.xlsx(agreegate3,file = "clustering_using_combinations.xlsx", sheetName = "Euclidean+median",
           row.names = F,append = T)


#######################################maximum+single####################

distanceMatrix4 <- dist(Cluster_Data,method="maximum",diag = TRUE,upper = T)
hclustermodel4 <- hclust(distanceMatrix4,method = "single")
plot(hclustermodel4,hang = -1)
groups4 <- cutree(hclustermodel4,k=10)
rect.hclust(hclustermodel4,k=10,border = "red")
groups4 <- as.matrix(groups4)
View(groups4)
Airline_Data_new4 <-data.frame(Airline_Data,groups4)
View(Airline_Data_new4)
agreegate4 <- aggregate(x = Airline_Data_new1[,-11],by = list(Airline_Data_new4$groups4),FUN ="mean")
write.xlsx(agreegate4,file = "clustering_using_combinations.xlsx", sheetName = "maximum+single",
           row.names = F,append = T)

#######################################maximum+complete####################

distanceMatrix4_n <- dist(Cluster_Data,method="maximum",diag = TRUE,upper = T)
hclustermodel4_n <- hclust(distanceMatrix4_n,method = "complete")
plot(hclustermodel4_n,hang = -1)
groups4_n <- cutree(hclustermodel4_n,k=10)
rect.hclust(hclustermodel4_n,k=10,border = "red")
groups4_n <- as.matrix(groups4_n)
View(groups4_n)
Airline_Data_new4_n <-data.frame(Airline_Data,groups4_n)
View(Airline_Data_new4_n)
agreegate4_n <- aggregate(x = Airline_Data_new1[,-11],by = list(Airline_Data_new4_n$groups4_n),FUN ="mean")
write.xlsx(agreegate4_n,file = "clustering_using_combinations.xlsx", sheetName = "maximum+complete",
           row.names = F,append = T)

#######################################maximum+median####################

distanceMatrix5 <- dist(Cluster_Data,method="maximum",diag = TRUE,upper = T)
hclustermodel5 <- hclust(distanceMatrix5,method = "median")
plot(hclustermodel5,hang = -1)
groups5 <- cutree(hclustermodel5,k=10)
rect.hclust(hclustermodel5,k=10,border = "red")
groups5 <- as.matrix(groups5)
View(groups5)
Airline_Data_new5 <-data.frame(Airline_Data,groups5)
View(Airline_Data_new5)
agreegate5 <- aggregate(x = Airline_Data_new1[,-11],by = list(Airline_Data_new5$groups5),FUN ="mean")
write.xlsx(agreegate5,file = "clustering_using_combinations.xlsx", sheetName = "maximum+median",
           row.names = F,append = T)

#######################################manhattan+single####################


distanceMatrix6 <- dist(Cluster_Data,method="manhattan",diag = TRUE,upper = T)
hclustermodel6 <- hclust(distanceMatrix6,method = "single")
plot(hclustermodel6,hang = -1)
groups6 <- cutree(hclustermodel6,k=10)
rect.hclust(hclustermodel6,k=10,border = "red")
groups6 <- as.matrix(groups6)
View(groups6)
Airline_Data_new6 <-data.frame(Airline_Data,groups6)
View(Airline_Data_new6)
agreegate6 <- aggregate(x = Airline_Data_new1[,-11],by = list(Airline_Data_new6$groups6),FUN ="mean")
write.xlsx(agreegate6,file = "clustering_using_combinations.xlsx", sheetName = "manhattan+single",
           row.names = F,append = T)

#######################################manhattan+complete####################
distanceMatrix7 <- dist(Cluster_Data,method="manhattan",diag = TRUE,upper = T)
hclustermodel7 <- hclust(distanceMatrix7,method = "complete")
plot(hclustermodel7,hang = -1)
groups7 <- cutree(hclustermodel7,k=10)
rect.hclust(hclustermodel7,k=10,border = "red")
groups7 <- as.matrix(groups7)
View(groups7)
Airline_Data_new7 <-data.frame(Airline_Data,groups7)
View(Airline_Data_new7)
agreegate7 <- aggregate(x = Airline_Data_new1[,-11],by = list(Airline_Data_new7$groups7),FUN ="mean")
write.xlsx(agreegate7,file = "clustering_using_combinations.xlsx", sheetName = "manhattan+complete",
           row.names = F,append = T)

#######################################manhattan+median####################

distanceMatrix8 <- dist(Cluster_Data,method="manhattan",diag = TRUE,upper = T)
hclustermodel8 <- hclust(distanceMatrix8,method = "single")
plot(hclustermodel8,hang = -1)
groups8 <- cutree(hclustermodel8,k=10)
rect.hclust(hclustermodel8,k=10,border = "red")
groups8 <- as.matrix(groups8)
View(groups8)
Airline_Data_new8 <-data.frame(Airline_Data,groups8)
View(Airline_Data_new8)
agreegate8 <- aggregate(x = Airline_Data_new1[,-11],by = list(Airline_Data_new8$groups8),FUN ="mean")
write.xlsx(agreegate8,file = "clustering_using_combinations.xlsx", sheetName = "manhattan+median",
           row.names = F,append = T)










