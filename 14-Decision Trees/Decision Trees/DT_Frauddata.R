#importing data
library(C50)
library(fastDummies)
library(caret)
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
View(Fraud_Data3)
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

####################################### Model 1  ########################################
###### C5.0 with boosting trails ####
decisionmodel1 <- C5.0(Fraud_Data_train1$status~.,data=Fraud_Data_train1)
summary(decisionmodel1)
plot(decisionmodel1)
predict_train1 <- predict(decisionmodel1,Fraud_Data_train1)
predict_train1
table_train1 <-table(Fraud_Data_train1$status,predict_train1)
AcurracyTrain1 <- sum(diag(table_train1))/sum(table_train1)
AcurracyTrain1
predict_test1 <- predict(decisionmodel1,Fraud_Data_test1)
predict_test1
table_test1 <-table(Fraud_Data_test1$status,predict_test1)
AcurracyTest1 <- sum(diag(table_test1))/sum(table_test1)
AcurracyTest1
########## bagging 
acc1<-c()
for(i in 1:100)
{
  inTraininglocal1 <- sample(nrow(Fraud_Data),480)
  Fraud_Data_train1 <- Fraud_Data1[inTraininglocal1,]
  Fraud_Data_test1 <- Fraud_Data1[-inTraininglocal1,]
  
  fittree1 <- C5.0(Fraud_Data_train1$status~.,data = Fraud_Data_train1)
  pred1<-predict.C5.0(fittree1,Fraud_Data_test1)
  a1<-table(Fraud_Data_test1$status,pred1)
  a1
  acc1<-c(acc1,sum(diag(a1))/sum(a1))
  acc1
  
}
####################################### Model 2  ########################################
###### C5.0 with boosting trails ####
decisionmodel2 <- C5.0(Fraud_Data_train2$status~.,data=Fraud_Data_train2,
                       trails=100)
summary(decisionmodel2)
predict_train2 <- predict(decisionmodel2,Fraud_Data_train2)
predict_train2
table_train2 <-table(Fraud_Data_train2$status,predict_train2)
AcurracyTrain2 <- sum(diag(table_train2))/sum(table_train2)
AcurracyTrain2
predict_test2 <- predict(decisionmodel2,Fraud_Data_test2)
predict_test2
table_test2 <-table(Fraud_Data_test2$status,predict_test2)
AcurracyTest2 <- sum(diag(table_test2))/sum(table_test2)


########## bagging 
acc2<-c()
for(i in 1:100)
{
  inTraininglocal2 <- sample(nrow(Fraud_Data),480)
  Fraud_Data_train2 <- Fraud_Data2[inTraininglocal2,]
  Fraud_Data_test2 <- Fraud_Data2[-inTraininglocal2,]
  
  fittree2 <- C5.0(Fraud_Data_train2$status~.,data = Fraud_Data_train2)
  pred2<-predict.C5.0(fittree2,Fraud_Data_test2)
  a2<-table(Fraud_Data_test2$status,pred2)
  a2
  acc2<-c(acc2,sum(diag(a2))/sum(a2))
  acc2
  
}
####################################### Model 3  ########################################
###### C5.0 with boosting trails ####
decisionmodel3 <- C5.0(Fraud_Data_train3$status~.,data=Fraud_Data_train3,
                       trails=100)
summary(decisionmodel3)
predict_train3 <- predict(decisionmodel3,Fraud_Data_train3)
predict_train3
table_train3 <-table(Fraud_Data_train3$status,predict_train3)
AcurracyTrain3 <- sum(diag(table_train3))/sum(table_train3)
AcurracyTrain3
predict_test3 <- predict(decisionmodel3,Fraud_Data_test3)
predict_test3
table_test3 <-table(Fraud_Data_test3$status,predict_test3)
AcurracyTest3 <- sum(diag(table_test3))/sum(table_test3)

########## bagging 
acc3<-c()
for(i in 1:100)
{
  inTraininglocal3 <- sample(nrow(Fraud_Data),480)
  Fraud_Data_train3 <- Fraud_Data3[inTraininglocal3,]
  Fraud_Data_test3 <- Fraud_Data3[-inTraininglocal3,]
  
  fittree3 <- C5.0(Fraud_Data_train3$status~.,data = Fraud_Data_train3)
  pred3<-predict.C5.0(fittree3,Fraud_Data_test3)
  a3<-table(Fraud_Data_test3$status,pred3)
  a3
  acc3<-c(acc3,sum(diag(a3))/sum(a3))
  acc3
  
}

mean(acc1)
mean(acc2)
mean(acc3)

table_accuracy <- data.frame(c("Model1_boost","Model1_bagging","Model2_boost","Model2_bagging","Model3_boost",
                               "Model3_bagging"),
                             c(AcurracyTest1,mean(acc1),AcurracyTest2,mean(acc2),AcurracyTest3,mean(acc3)))

View(table_accuracy)




