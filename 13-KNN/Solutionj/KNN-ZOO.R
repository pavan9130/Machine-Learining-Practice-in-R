library(class)
library(gmodels)
library(lattice)
library(caret)
library(fastDummies)
library(funModeling)
library(dlookr)
library(tidyverse)
#importing zoo file

zoo_data <- read.csv("O:/EXCELR/Assignment/13-KNN/zoo.csv")
zoo_data <- zoo_data[,-1]
View(zoo_data)


#Converting type column  into  factor data type

zoo_data[,17] <- factor(zoo_data[,17])
zoo_data[,13] <- factor(zoo_data[,13])
zoo_data[,c(-13,-17)] <- lapply(zoo_data[,c(-13,-17)], factor)
str(zoo_data)
attach(zoo_data)
#################################EDA##############################
ggplot(zoo_data, 
       aes(x = legs , 
           fill = type)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")
#legs_5 or legs_8 can be removed
#hair,feather,eggs,milk,airborne,aquatic,predator,toothed,backbone,breathes,venomous,fins,tail,domestic,catsize
freq(zoo_data)
ggplot(zoo_data, 
       aes(x = feathers ,
           fill = type)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")

ggplot(zoo_data, 
       aes(x = milk , 
           fill = type)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")
#################################EDA#########################



#creating dummies for legs column
zoo_data <- dummy_cols(zoo_data,select_columns = "legs")
#removing original legs column
zoo_data <- zoo_data[,-13]
View(zoo_data)
#changing all columns into factor type (0 and 1)
zoo_data[,1:15] <- lapply(zoo_data[,1:15],FUN = factor)
zoo_data[,17:22] <- lapply(zoo_data[,17:22],FUN = factor)
str(zoo_data)
attach(zoo_data)
zoo_data1 <- zoo_data
zoo_data2 <- zoo_data[,c(-20,-22)]
zoo_data3 <- zoo_data[,c(-2,-4)]
View(zoo_data3)



#spliting into train and test data 
zoo_data_1_train <- zoo_data1[1:80,-16]
zoo_data_1_test  <- zoo_data1[80:101,-16]
zoo_data_1_train_labels <- zoo_data1[1:80,16]
zoo_data_1_test_labels <-  zoo_data1[80:101,16]


#spliting into train and test data 
zoo_data_2_train <- zoo_data2[1:80,-16]
zoo_data_2_test  <- zoo_data2[80:101,-16]
zoo_data_2_train_labels <- zoo_data2[1:80,16]
zoo_data_2_test_labels <-  zoo_data2[80:101,16]

#spliting into train and test data 
zoo_data_3_train <- zoo_data3[1:80,-14]
zoo_data_3_test  <- zoo_data3[80:101,-14]
zoo_data_3_train_labels <- zoo_data3[1:80,14]
zoo_data_3_test_labels <-  zoo_data3[80:101,14]


################################First model ###########################

#applying knn with different k values 
x_k1 <- NULL
y_acc1 <- NULL
for(i in 3:20)
{
  KNN_model1 <- knn(train=zoo_data_1_train,test=zoo_data_1_test,cl=zoo_data_1_train_labels,k=i)
  table1 <- table(KNN_model1,zoo_data_1_test_labels)
  accuracy1 <- sum(diag(table1))/sum(table1)
  x_k1 <- rbind(x_k1,i)
  y_acc1 <- rbind(y_acc1,accuracy1)
  
}

#ploting k value vs accuracy
plot(x_k1,y_acc1,type = "l")
error_data1 <- cbind(x_k1,y_acc1)
View(error_data1)

#for example from above data if maximum accuracy is at 10
KNN_model_1 <- knn(train=zoo_data_1_train,test=zoo_data_1_test,cl=zoo_data_1_train_labels,k=10)
KNN_model_1
confusionMatrix(KNN_model_1,zoo_data_1_test_labels)
table_1 <- table(KNN_model_1,zoo_data_1_test_labels)
accuracy_1 <- sum(diag(table_1))/sum(table_1)
accuracy_1

################################second model ###################
#applying knn with different k values 
x_k2 <- NULL
y_acc2 <- NULL
for(i in 3:20)
{
  KNN_model2 <- knn(train=zoo_data_2_train,test=zoo_data_2_test,cl=zoo_data_2_train_labels,k=i)
  table2 <- table(KNN_model2,zoo_data_2_test_labels)
  accuracy2 <- sum(diag(table2))/sum(table2)
  x_k2 <- rbind(x_k2,i)
  y_acc2 <- rbind(y_acc2,accuracy2)
  
}

#ploting k value vs accuracy
plot(x_k2,y_acc2,type = "l")
error_data2 <- cbind(x_k2,y_acc2)
View(error_data2)

#for example from above data if maximum accuracy is at 8
KNN_model_2 <- knn(train=zoo_data_2_train,test=zoo_data_2_test,cl=zoo_data_2_train_labels,k=8)
KNN_model_2
confusionMatrix(KNN_model_2,zoo_data_2_test_labels)
table_2 <- table(KNN_model_2,zoo_data_2_test_labels)
accuracy_2 <- sum(diag(table_2))/sum(table_2)
accuracy_2
#####################################third model ##############

#applying knn with different k values 
x_k3 <- NULL
y_acc3 <- NULL
for(i in 3:30)
{
  KNN_model3 <- knn(train=zoo_data_3_train,test=zoo_data_3_test,cl=zoo_data_3_train_labels,k=i)
  table3 <- table(KNN_model3,zoo_data_3_test_labels)
  accuracy3 <- sum(diag(table3))/sum(table3)
  x_k3 <- rbind(x_k3,i)
  y_acc3 <- rbind(y_acc3,accuracy3)
  
}

#ploting k value vs accuracy
plot(x_k3,y_acc3,type = "l")
error_data3 <- cbind(x_k3,y_acc3)
View(error_data3)

#for example from above data if maximum accuracy is at 8
KNN_model_3 <- knn(train=zoo_data_3_train,test=zoo_data_3_test,cl=zoo_data_3_train_labels,k=9)
KNN_model_3
confusionMatrix(KNN_model_3,zoo_data_3_test_labels)
table_3 <- table(KNN_model_3,zoo_data_3_test_labels)
accuracy_3 <- sum(diag(table_3))/sum(table_3)
accuracy_3

table_accuracy <- data.frame(c("model_1","model_2","model_3"),c(accuracy_1,accuracy_2,accuracy_3))
table_accuracy
