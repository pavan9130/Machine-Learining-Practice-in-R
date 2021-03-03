#importing data
library(xlsx)
crime_data <- read.csv("O:/EXCELR/Assignment/7-Clusterig/crime_data.csv")
crime_data_n <- crime_data[,-1]
View(crime_data_n)
normalize_data <- scale(crime_data_n)

################################ euclidean+single ##############
distanceMatrix_n1 <- dist(normalize_data,method="euclidean",diag = TRUE,upper = T)
hclustermodel_n1 <- hclust(distanceMatrix_n1,method = "single")
plot(hclustermodel_n1,hang = -1)
groups_n1 <- cutree(hclustermodel_n1,k=10)
rect.hclust(hclustermodel_n1,k=10,border = "red")
class(groups_n1)
groups_n1
groups_n1 <- as.matrix(groups_n1)
View(groups_n1)
crime_data_new_n1 <-data.frame(crime_data,groups_n1)
View(crime_data_new_n1)
crime_data_new_n1 <- crime_data_new_n1[,c(1,6,2,3,4,5)]
agreegate_n1 <- aggregate(x = crime_data_new_n1[,-1],by = list(crime_data_new_n1$groups_n1),FUN ="mean")
write.xlsx(agreegate_n1[,-1],file = "clustering_using_combinations_crime.xlsx", sheetName = "euclidean+single",
           row.names = F)
################################ euclidean+complete ##############

distanceMatrix_n2 <- dist(normalize_data,method="euclidean",diag = TRUE,upper = T)
hclustermodel_n2 <- hclust(distanceMatrix_n2,method = "complete")
plot(hclustermodel_n2,hang = -1)
groups_n2 <- cutree(hclustermodel_n2,k=10)
rect.hclust(hclustermodel_n2,k=10,border = "red")
class(groups_n2)
groups_n2
groups_n2 <- as.matrix(groups_n2)
View(groups_n2)
crime_data_new_n2 <-data.frame(crime_data,groups_n2)
View(crime_data_new_n2)
crime_data_new_n2 <- crime_data_new_n2[,c(1,6,2,3,4,5)]
agreegate_n2 <- aggregate(x = crime_data_new_n2[,-1],by = list(crime_data_new_n2$groups_n2),FUN ="mean")
write.xlsx(agreegate_n2[,-1],file = "clustering_using_combinations_crime.xlsx", sheetName = "euclidean+complete",
           row.names = F,append = T)
################################ euclidean+median ##############

distanceMatrix_n3 <- dist(normalize_data,method="euclidean",diag = TRUE,upper = T)
hclustermodel_n3 <- hclust(distanceMatrix_n3,method = "median")
plot(hclustermodel_n3,hang = -1)
groups_n3 <- cutree(hclustermodel_n3,k=10)
rect.hclust(hclustermodel_n3,k=10,border = "red")
class(groups_n3)
groups_n3
groups_n3 <- as.matrix(groups_n3)
View(groups_n3)
crime_data_new_n3 <-data.frame(crime_data,groups_n3)
View(crime_data_new_n3)
crime_data_new_n3 <- crime_data_new_n3[,c(1,6,2,3,4,5)]
agreegate_n3 <- aggregate(x = crime_data_new_n3[,-1],by = list(crime_data_new_n3$groups_n3),FUN ="mean")
write.xlsx(agreegate_n3[,-1],file = "clustering_using_combinations_crime.xlsx", sheetName = "euclidean+median",
           row.names = F,append = T)

################################ maximum+single ##############

distanceMatrix_n4 <- dist(normalize_data,method="maximum",diag = TRUE,upper = T)
hclustermodel_n4 <- hclust(distanceMatrix_n4,method = "single")
plot(hclustermodel_n4,hang = -1)
groups_n4 <- cutree(hclustermodel_n4,k=10)
rect.hclust(hclustermodel_n4,k=10,border = "red")
class(groups_n4)
groups_n4
groups_n4 <- as.matrix(groups_n4)
View(groups_n4)
crime_data_new_n4 <-data.frame(crime_data,groups_n4)
View(crime_data_new_n4)
crime_data_new_n4 <- crime_data_new_n4[,c(1,6,2,3,4,5)]
agreegate_n4 <- aggregate(x = crime_data_new_n4[,-1],by = list(crime_data_new_n4$groups_n4),FUN ="mean")
write.xlsx(agreegate_n4[,-1],file = "clustering_using_combinations_crime.xlsx", sheetName = "maximum+single",
           row.names = F,append = T)

################################ maximum+complete##############

distanceMatrix_n5 <- dist(normalize_data,method="maximum",diag = TRUE,upper = T)
hclustermodel_n5 <- hclust(distanceMatrix_n5,method = "complete")
plot(hclustermodel_n5,hang = -1)
groups_n5 <- cutree(hclustermodel_n5,k=10)
rect.hclust(hclustermodel_n5,k=10,border = "red")
class(groups_n5)
groups_n5
groups_n5 <- as.matrix(groups_n5)
View(groups_n5)
crime_data_new_n5 <-data.frame(crime_data,groups_n5)
View(crime_data_new_n5)
crime_data_new_n5 <- crime_data_new_n5[,c(1,6,2,3,4,5)]
agreegate_n5 <- aggregate(x = crime_data_new_n5[,-1],by = list(crime_data_new_n5$groups_n5),FUN ="mean")
write.xlsx(agreegate_n5[,-1],file = "clustering_using_combinations_crime.xlsx", sheetName = "maximum+complete",
           row.names = F,append = T)

################################ maximum+median##############
distanceMatrix_n6 <- dist(normalize_data,method="maximum",diag = TRUE,upper = T)
hclustermodel_n6 <- hclust(distanceMatrix_n6,method = "median")
plot(hclustermodel_n6,hang = -1)
groups_n6 <- cutree(hclustermodel_n6,k=10)
rect.hclust(hclustermodel_n6,k=10,border = "red")
class(groups_n6)
groups_n6
groups_n6 <- as.matrix(groups_n6)
View(groups_n6)
crime_data_new_n6 <-data.frame(crime_data,groups_n6)
View(crime_data_new_n6)
crime_data_new_n6 <- crime_data_new_n6[,c(1,6,2,3,4,5)]
agreegate_n6 <- aggregate(x = crime_data_new_n6[,-1],by = list(crime_data_new_n6$groups_n6),FUN ="mean")
write.xlsx(agreegate_n6[,-1],file = "clustering_using_combinations_crime.xlsx", sheetName = "maximum+median",
           row.names = F,append = T)


################################ manhattan+single ##############
distanceMatrix_n7 <- dist(normalize_data,method="manhattan",diag = TRUE,upper = T)
hclustermodel_n7 <- hclust(distanceMatrix_n7,method = "single")
plot(hclustermodel_n7,hang = -1)
groups_n7 <- cutree(hclustermodel_n7,k=10)
rect.hclust(hclustermodel_n7,k=10,border = "red")
class(groups_n7)
groups_n7
groups_n7 <- as.matrix(groups_n7)
View(groups_n7)
crime_data_new_n7 <-data.frame(crime_data,groups_n7)
View(crime_data_new_n7)
crime_data_new_n7 <- crime_data_new_n7[,c(1,6,2,3,4,5)]
agreegate_n7 <- aggregate(x = crime_data_new_n7[,-1],by = list(crime_data_new_n7$groups_n7),FUN ="mean")
write.xlsx(agreegate_n7[,-1],file = "clustering_using_combinations_crime.xlsx", sheetName = "manhattan+single",
           row.names = F,append = T)

################################ manhattan+complete ##############
distanceMatrix_n8 <- dist(normalize_data,method="manhattan",diag = TRUE,upper = T)
hclustermodel_n8 <- hclust(distanceMatrix_n8,method = "complete")
plot(hclustermodel_n8,hang = -1)
groups_n8 <- cutree(hclustermodel_n8,k=10)
rect.hclust(hclustermodel_n8,k=10,border = "red")
class(groups_n8)
groups_n8
groups_n8 <- as.matrix(groups_n8)
View(groups_n8)
crime_data_new_n8 <-data.frame(crime_data,groups_n8)
View(crime_data_new_n8)
crime_data_new_n8 <- crime_data_new_n8[,c(1,6,2,3,4,5)]
agreegate_n8 <- aggregate(x = crime_data_new_n8[,-1],by = list(crime_data_new_n8$groups_n8),FUN ="mean")
write.xlsx(agreegate_n8[,-1],file = "clustering_using_combinations_crime.xlsx", sheetName = "manhattan+complete",
           row.names = F,append = T)


################################manhattan+median ##############
distanceMatrix_n9 <- dist(normalize_data,method="manhattan",diag = TRUE,upper = T)
hclustermodel_n9 <- hclust(distanceMatrix_n9,method = "median")
plot(hclustermodel_n9,hang = -1)
groups_n9 <- cutree(hclustermodel_n9,k=10)
rect.hclust(hclustermodel_n9,k=10,border = "red")
class(groups_n9)
groups_n9
groups_n9 <- as.matrix(groups_n9)
View(groups_n9)
crime_data_new_n9 <-data.frame(crime_data,groups_n9)
View(crime_data_new_n9)
crime_data_new_n9 <- crime_data_new_n9[,c(1,6,2,3,4,5)]
agreegate_n9 <- aggregate(x = crime_data_new_n9[,-1],by = list(crime_data_new_n9$groups_n9),FUN ="mean")
write.xlsx(agreegate_n9[,-1],file = "clustering_using_combinations_crime.xlsx", sheetName = "manhattan+median",
           row.names = F,append = T)