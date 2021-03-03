#importing data
library(randomForest)
library(funModeling)
library(caret)
companydata <- read.csv("O:/EXCELR/Assignment/15-Random Forests/Company_Data.csv")
View(companydata)
#creating new column for categorical variable based on sales 
hist(companydata$Sales)
summary(companydata$Sales)
companydata <- data.frame(sales_new=0,companydata)
str(companydata)
for(i in 1:400)
{
  
  if (companydata[i,2] <7.5)
  {
    
    companydata[i,1] = "low"
    
    next
  }
  else
  {
    
    companydata[i,1] = "high"
    
    next
  }
}
#removing original sales column
companydata <- companydata[,-2]
str(companydata)
#changing columns to factor data type
companydata[,c(7,10,11)] <- lapply(companydata[,c(7,10,11)], factor)
companydata[,1] <- factor(companydata[,1],levels =  c("low","high"), 
                          labels = c("L","H"))
View(companydata)

companydata <- dummy_cols(companydata,select_columns = "ShelveLoc")
companydata <- companydata[,-7]
attach(companydata)
#######################################EDA################################
plot(sales_new,CompPrice) #all bloxplots looks same 
plot(sales_new,Income)
plot(sales_new,Advertising) #outliers are available 
plot(sales_new,Population)
plot(sales_new,Price)
plot(sales_new,Age)
plot(sales_new,Education)

freq(companydata)


ggplot(companydata, 
       aes(x = Urban, 
           fill = sales_new)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")
##urban can be removed as same proportioins
ggplot(companydata, 
       aes(x = US, 
           fill = sales_new)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")


View(companydata)
 
ggplot(companydata, 
       aes(x = ShelveLoc_Bad, 
           fill = sales_new)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")

ggplot(companydata, 
       aes(x = ShelveLoc_Good, 
           fill = sales_new)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")

ggplot(companydata, 
       aes(x = ShelveLoc_Medium, 
           fill = sales_new)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion") ##### aprox same 

######################################### modifying ################
companydata[,2:8] <- scale(companydata[,2:8])
View(companydata)
# us also can be removed as equal proportions
companydata1 <- companydata
companydata2 <- companydata[,c(-2,-9)]
companydata3 <- companydata[,c(-2,-9,-13)]


index1 <- sample(nrow(companydata1),320)
#splitting data into train and test sets
companydata_train1 <- companydata1[index1,]
companydata_test1 <- companydata1[-index1,]

table(companydata_train1$sales_new)
table(companydata_test1$sales_new)
#splitting data into train and test sets
index2 <- sample(nrow(companydata2),320)
companydata_train2 <- companydata2[index2,]
companydata_test2 <- companydata2[-index2,]


#splitting data into train and test sets
index3 <- sample(nrow(companydata3),320)
companydata_train3 <- companydata3[index3,]
companydata_test3 <- companydata3[-index3,]




#########################################First model ###################
RFmodel1 <- randomForest(companydata_train1$sales_new~.,data = companydata_train1
                         ,na.action =na.roughfix,ntree=1000,importance=T)
# Prediction of train data
pred_train1 <-predict(RFmodel1,companydata_train1)

# Training accuracy 
accuracytrain1 <- mean(companydata_train1$sales_new==predict(RFmodel1,companydata_train1))

confusionMatrix(companydata_train1$sales_new,pred_train1)
# Predicting test data 
pred_Test1 <- predict(RFmodel1,companydata_test1)
confusionMatrix(companydata_test1$sales_new,pred_Test1)
accuracytest1 <-  mean(companydata_test1$sales_new==predict(RFmodel1,companydata_test1))

plot(RFmodel1,lwd=2)
legend("topright", colnames(RFmodel1$err.rate),col=1:4,cex=0.8,fill=1:4)

#########################################Second model ###################

RFmodel2 <- randomForest(companydata_train2$sales_new~.,data = companydata_train2
                         ,na.action =na.roughfix,ntree=1000,importance=T)
# Prediction of train data
pred_train2 <-predict(RFmodel2,companydata_train2)

# Training accuracy 
accuracytrain2 <- mean(companydata_train2$sales_new==predict(RFmodel2,companydata_train2))

confusionMatrix(companydata_train2$sales_new,pred_train2)
# Predicting test data 
pred_Test2 <- predict(RFmodel2,companydata_test2)
confusionMatrix(companydata_test2$sales_new,pred_Test2)
accuracytest2 <-  mean(companydata_test2$sales_new==predict(RFmodel2,companydata_test2))

plot(RFmodel2,lwd=1)
legend("topright", colnames(RFmodel2$err.rate),col=1:4,cex=0.8,fill=1:4)

###############################################Third model ######################

RFmodel3 <- randomForest(companydata_train3$sales_new~.,data = companydata_train3
                         ,na.action =na.roughfix,ntree=1000,importance=T)
# Prediction of train data
pred_train3 <-predict(RFmodel3,companydata_train3)

# Training accuracy 
accuracytrain3 <- mean(companydata_train3$sales_new==predict(RFmodel3,companydata_train3))

confusionMatrix(companydata_train3$sales_new,pred_train3)
# Predicting test data 
pred_Test3 <- predict(RFmodel3,companydata_test3)
confusionMatrix(companydata_tes3t$sales_new,pred_Test3)
accuracytest3 <-  mean(companydata_test3$sales_new==predict(RFmodel3,companydata_test3))

plot(RFmodel3,lwd=1)
legend("topright", colnames(RFmodel3$err.rate),col=1:4,cex=0.8,fill=1:4)
############################################ Fourth model ################
RFmodel4 <- randomForest(companydata_train4$sales_new~.,data = companydata_train4
                         ,na.action =na.roughfix,ntree=1000,importance=T)
# Prediction of train data
pred_train4 <-predict(RFmodel4,companydata_train4)

# Training accuracy 
accuracytrain4 <- mean(companydata_train4$sales_new==predict(RFmodel4,companydata_train4))

confusionMatrix(companydata_train4$sales_new,pred_train4)
# Predicting test data 
pred_Test4 <- predict(RFmodel4,companydata_test4)
confusionMatrix(companydata_tes4t$sales_new,pred_Test4)
accuracytest4 <-  mean(companydata_test4$sales_new==predict(RFmodel4,companydata_test4))

plot(RFmodel4,lwd=1)
legend("topright", colnames(RFmodel4$err.rate),col=1:4,cex=0.8,fill=1:4)

table_accuracy <- data.frame(c("model1_train","model1_test","model2_train","model2_test",
                               "model3_train","model3_test"),
                             c(accuracytrain1,accuracytest1,accuracytrain2,accuracytest2,
                               accuracytrain3,accuracytest3))

View(table_accuracy)
