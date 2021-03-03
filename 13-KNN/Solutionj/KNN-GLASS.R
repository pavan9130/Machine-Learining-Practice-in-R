library(class)
library(gmodels)
library(lattice)
library(caret)
library(fastDummies)
library(funModeling)
install.packages('fastDummies')
#importing glass file
set.seed(123)
glass_data <- read.csv("O:/EXCELR/Assignment/13-KNN/glass.csv")
View(glass_data)
#Converting  type attribute to factor type
glass_data [,10] <- factor(glass_data[,10],levels = c("1","2","3","4","5","6","7"),
                           labels = c("1","2","3","4","5","6","7"))
str(glass_data)

###################################EDA#########################333
print(status(glass_data))
freq(glass_data)
plot_num(glass_data)
dat <- profiling_num(glass_data)
dat$variation_coef
#variation_coef is more for Ba,Fe,K

normalize_fun <- function(x)
{
  x= ((x-min(x))/(max(x)-min(x)))
}
# standarizing numerical data using above function
glass_data_n <- lapply(glass_data[,1:9],FUN = normalize_fun)
glass_data_n <- as.data.frame(glass_data_n)
glass_data_n <- cbind(glass_data_n,glass_data["Type"])
View(glass_data_n)
summary(glass_data_n)
#spliting data into train and test 
glass_data1 <- glass_data_n
View(glass_data1)
glass_data2 <- glass_data_n[,-8]
View(glass_data2)
glass_data3 <- glass_data_n[,c(-8,-9)]
View(glass_data3)


#############################################First model ############################
index1 <- sample(nrow(glass_data1),170)

glass_data_train1 <- glass_data1[index1,-10]
glass_data_test1  <- glass_data1[-index1,-10]
View(glass_data_train1)
View(glass_data_test1)
glass_data_train1_labels <- glass_data1[index1,10]
glass_data_test1_labels <-  glass_data1[-index1,10]

#applying knn for selecting best k-value
x_k1 <- NULL
y_acc1 <- NULL
for(i in 3:20)
{
  Knn_model1 <- knn(train=glass_data_train1,test=glass_data_test1,cl=glass_data_train1_labels,k=i)
  table1 <- table(Knn_model1,glass_data_test1_labels)
  accuracy1 <- sum(diag(table1))/sum(table1)
  x_k1 <- rbind(x_k1,i)
  y_acc1 <- rbind(y_acc1,accuracy1)
  
}

#ploti1g k value vs accuracy
plot(x_k1,y_acc1,type = "l")
error_data1 <- cbind(x_k1,y_acc1)
View(error_data1)

#for example from above data if maximum accuracy is at 4

Knn_model1_n <- knn(train=glass_data_train1,test=glass_data_test1,cl=glass_data_train1_labels,k=4)
table1_n <- table(Knn_model1_n,glass_data_test1_labels)
accuracy1_n <- sum(diag(table1_n))/sum(table1_n)
accuracy1_n


################################################## Second model ################
index2 <- sample(nrow(glass_data2),170)

glass_data_train2 <- glass_data2[index2,-9]
glass_data_test2  <- glass_data2[-index2,-9]
View(glass_data_train2)
View(glass_data_test2)
glass_data_train2_labels <- glass_data2[index2,9]
glass_data_test2_labels <-  glass_data2[-index2,9]

#applyi2g k22 for selecti2g best k-value
x_k2 <- NULL
y_acc2 <- NULL
for(i in 3:30)
{
  Knn_model2 <- knn(train=glass_data_train2,test=glass_data_test2,cl=glass_data_train2_labels,k=i)
  table2 <- table(Knn_model2,glass_data_test2_labels)
  accuracy2 <- sum(diag(table2))/sum(table2)
  x_k2 <- rbind(x_k2,i)
  y_acc2 <- rbind(y_acc2,accuracy2)
  
}

#ploti2g k value vs accuracy
plot(x_k2,y_acc2,type = "l")
error_data2 <- cbind(x_k2,y_acc2)
View(error_data2)

#for example from above data if maximum accuracy is at 16

Knn_model2_n <- knn(train=glass_data_train2,test=glass_data_test2,cl=glass_data_train2_labels,k=16)
table2_n <- table(Knn_model2_n,glass_data_test2_labels)
accuracy2_n <- sum(diag(table2_n))/sum(table2_n)
accuracy2_n

################################################# Third model ###############
index3 <- sample(nrow(glass_data3),170)

glass_data_train3 <- glass_data3[index3,-8]
glass_data_test3  <- glass_data3[-index3,-8]
View(glass_data_train3)
View(glass_data_test3)
glass_data_train3_labels <- glass_data3[index3,8]
glass_data_test3_labels <-  glass_data3[-index3,8]

#applyi3g k33 for selecti3g best k-value
x_k3 <- NULL
y_acc3 <- NULL
for(i in 3:30)
{
  Knn_model3 <- knn(train=glass_data_train3,test=glass_data_test3,cl=glass_data_train3_labels,k=i)
  table3 <- table(Knn_model3,glass_data_test3_labels)
  accuracy3 <- sum(diag(table3))/sum(table3)
  x_k3 <- rbind(x_k3,i)
  y_acc3 <- rbind(y_acc3,accuracy3)
  
}

#ploti3g k value vs accuracy
plot(x_k3,y_acc3,type = "l")
error_data3 <- cbind(x_k3,y_acc3)
View(error_data3)

#for example from above data if maximum accuracy is at 3

Knn_model3_n <- knn(train=glass_data_train3,test=glass_data_test3,cl=glass_data_train3_labels,k=3)
table3_n <- table(Knn_model3_n,glass_data_test3_labels)
accuracy3_n <- sum(diag(table3_n))/sum(table3_n)
accuracy3_n

table_accuracy <- data.frame(c("model_1","model_2","model_3"),c(accuracy1_n,accuracy2_n,accuracy3_n))
table_accuracy
