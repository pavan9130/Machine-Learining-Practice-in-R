#importing data
library(randomForest)
library(funModeling)
library(caret)
library(fastDummies)
#importing data
Fraud_Data <- read.csv("O:/EXCELR/Assignment/15-Random Forests/Fraud_check.csv")
View(Fraud_Data)
na.omit(Fraud_Data)
################################ Pre-processing  ##############################

#creating new column for categorical variable based on Taxable income 
Fraud_Data <- data.frame(status=0,Fraud_Data)
str(Fraud_Data)
for(i in 1:600)
{
  
  
  if (Fraud_Data[i,4] <=30000)
  {
    
    Fraud_Data[i,1] = "risky"
    
    next
  }
  else 
  {
    
    Fraud_Data[i,1] = "good"
    
    next
  }
  
}
#removing original tax income column
Fraud_Data <- Fraud_Data[,-4]
table(Fraud_Data$status)
View(Fraud_Data)

#changing columns to factor data type
Fraud_Data[,c(2,3,6)] <- lapply(Fraud_Data[,c(2,3,6)], factor)
Fraud_Data[,1] <- factor(Fraud_Data[,1],levels =  c("risky","good"), 
                         labels = c("R","G"))
Fraud_Data <- dummy_cols(Fraud_Data,select_columns = "Marital.Status")
Fraud_Data <- Fraud_Data[,-3]
attach(Fraud_Data)

##########################       EDA           ##########################
plot(status,City.Population) 
plot(status,Work.Experience)

ggplot(Fraud_Data, 
       aes(x = Urban, 
           fill = status)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")          ############ Proportions are same

ggplot(Fraud_Data, 
       aes(x = Undergrad, 
           fill = status)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")           ########## Proportions are same

ggplot(Fraud_Data, 
       aes(x = Marital.Status_Single, 
           fill = status)) + 
  geom_bar(position = "fill") +

    labs(y = "Proportion")    
ggplot(Fraud_Data, 
       aes(x = Marital.Status_Married, 
           fill = status)) + 
  geom_bar(position = "fill") +
  
  labs(y = "Proportion") 
ggplot(Fraud_Data, 
       aes(x = Marital.Status_Divorced, 
           fill = status)) + 
  geom_bar(position = "fill") +
  
  labs(y = "Proportion") 
###############################################################################
Fraud_Data1 <- Fraud_Data
Fraud_Data2 <- Fraud_Data[,c(-2,-5)]
Fraud_Data3 <- Fraud_Data[,c(-2,-5,-6)]
View(Fraud_Data1)
traintest1 <- sample(nrow(Fraud_Data1),480)
#splitting data into train and test sets
Fraud_Data_train1 <- Fraud_Data1[traintest1,]
Fraud_Data_test1 <- Fraud_Data1[-traintest1,]

traintest2 <- sample(nrow(Fraud_Data2),480)
#splitting data into train and test sets
Fraud_Data_train2 <- Fraud_Data2[traintest2,]
Fraud_Data_test2 <- Fraud_Data2[-traintest2,]

traintest3 <- sample(nrow(Fraud_Data3),480)
#splitting data into train and test sets
Fraud_Data_train3 <- Fraud_Data3[traintest3,]
Fraud_Data_test3 <- Fraud_Data3[-traintest3,]

#####################################First model ########################
RFmodel1 <- randomForest(Fraud_Data_train1$status~.,data = Fraud_Data_train1
                         ,na.action =na.roughfix,ntree=1000,importance=T)
# Prediction of train data
pred_train1 <-predict(RFmodel1,Fraud_Data_train1)
# Training accuracy 
accuracy_train1 <- mean(Fraud_Data_train1$status==predict(RFmodel1,Fraud_Data_train1))
confusionMatrix(Fraud_Data_train1$status,pred_train1)
# Predicting test data 
pred_Test1 <- predict(RFmodel1,Fraud_Data_test1)
confusionMatrix(Fraud_Data_test1$status,pred_Test1)
accuracy_test1 <- mean(Fraud_Data_test1$status==predict(RFmodel1,Fraud_Data_test1))

plot(RFmodel1,lwd=1)
legend("topright", colnames(RFmodel1$err.rate),col=1:4,cex=0.8,fill=1:4)

#####################################Second  model ########################
RFmodel2 <- randomForest(Fraud_Data_train2$status~.,data = Fraud_Data_train2
                         ,na.action =na.roughfix,ntree=1000,importance=T)
# Prediction of train data
pred_train2 <-predict(RFmodel2,Fraud_Data_train2)
# Training accuracy 
accuracy_train2 <- mean(Fraud_Data_train2$status==predict(RFmodel2,Fraud_Data_train2))
confusionMatrix(Fraud_Data_train2$status,pred_train2)
# Predicting test data 
pred_Test2 <- predict(RFmodel2,Fraud_Data_test2)
confusionMatrix(Fraud_Data_test2$status,pred_Test2)
accuracy_test2 <- mean(Fraud_Data_test2$status==predict(RFmodel2,Fraud_Data_test2))

plot(RFmodel2,lwd=1)
legend("topright", colnames(RFmodel2$err.rate),col=1:4,cex=0.8,fill=1:4)
#####################################Third  model ########################

RFmodel3 <- randomForest(Fraud_Data_train3$status~.,data = Fraud_Data_train3
                         ,na.action =na.roughfix,ntree=1000,importance=T)
# Prediction of train data
pred_train3 <-predict(RFmodel3,Fraud_Data_train3)
# Training accuracy 
accuracy_train3 <- mean(Fraud_Data_train3$status==predict(RFmodel3,Fraud_Data_train3))
confusionMatrix(Fraud_Data_train3$status,pred_train3)
# Predicting test data 
pred_Test3 <- predict(RFmodel3,Fraud_Data_test3)
confusionMatrix(Fraud_Data_test3$status,pred_Test3)
accuracy_test3 <- mean(Fraud_Data_test3$status==predict(RFmodel3,Fraud_Data_test3))

plot(RFmodel3,lwd=1)
legend("topright", colnames(RFmodel3$err.rate),col=1:4,cex=0.8,fill=1:4)

table_accuracy <- data.frame(c("model1_train","model1_test","model2_train","model2_test",
                               "model3_train","model3_test"),
                             c(accuracy_train1,accuracy_test1,accuracy_train2,accuracy_test2,
                               accuracy_train3,accuracy_test3))
View(table_accuracy)
