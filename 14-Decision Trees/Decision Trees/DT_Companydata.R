library(funModeling)
library(C50)
library(caret)
library(fastDummies)
install.packages("C50",dependencies = T)
#importing data
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

companydata <- dummy_cols(companydata,select_columns = "ShelveLoc")
companydata <- companydata[,-7]
View(companydata)
attach(companydata)
########################################################EDA START#############
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

################################################## Modifying ###################
companydata[,2:8] <- scale(companydata[,2:8])
View(companydata)
# us also can be removed as equal proportions
companydata1 <- companydata
companydata2 <- companydata[,c(-2,-9)]
companydata3 <- companydata[,c(-2,-9,-13)]
companydata4 <- companydata[,c(-2,-11,-10)]


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



#splitting data into train and test sets
index4 <- sample(nrow(companydata4),320)
companydata_train4 <-companydata4[index4,]
companydata_test4 <- companydata4[-index4,]


###################################First Model ##############
decisionmodel1 <- C5.0(companydata_train1$sales_new~.,data=companydata_train1,
                       trails=100)
summary(decisionmodel1)
predict_train1 <- predict(decisionmodel1,companydata_train1)
predict_train1
table_train1 <-table(companydata_train1$sales_new,predict_train1)
AcurracyTrain1 <- sum(diag(table_train1))/sum(table_train1)
AcurracyTrain1
predict_test1 <- predict(decisionmodel1,companydata_test1)
predict_test1
table_test1 <-table(companydata_test1$sales_new,predict_test1)
AcurracyTest1 <- sum(diag(table_test1))/sum(table_test1)

acc1<-c()
for(i in 1:100)
{
  inTraininglocal1 <- sample(nrow(companydata1),320)
  companydata_train1 <- companydata1[inTraininglocal1,]
  companydata_test1 <- companydata1[-inTraininglocal1,]
  
  fittree1 <- C5.0(companydata_train1$sales_new~.,data = companydata_train1)
  pred1<-predict.C5.0(fittree1,companydata_test1)
  a1<-table(companydata_test1$sales_new,pred1)
  a1
  acc1<-c(acc1,sum(diag(a1))/sum(a1))
  acc1
  
}

###################################Second Model ##############

decisionmodel2 <- C5.0(companydata_train2$sales_new~.,data=companydata_train2,
                       trails=100)
summary(decisionmodel2)
predict_train2 <- predict(decisionmodel2,companydata_train2)
predict_train2
table_train2 <-table(companydata_train2$sales_new,predict_train2)
AcurracyTrain2 <- sum(diag(table_train2))/sum(table_train2)
AcurracyTrain2
predict_test2 <- predict(decisionmodel2,companydata_test2)
predict_test2
table_test2 <-table(companydata_test2$sales_new,predict_test2)
AcurracyTest2 <- sum(diag(table_test2))/sum(table_test2)
AcurracyTest2
acc2<-c()
for(i in 1:100)
{
  inTraininglocal2 <- sample(nrow(companydata2),320)
  companydata_train2 <- companydata2[inTraininglocal2,]
  companydata_test2 <- companydata2[-inTraininglocal2,]
  
  fittree2 <- C5.0(companydata_train2$sales_new~.,data = companydata_train2)
  pred2<-predict.C5.0(fittree2,companydata_test2)
  a2<-table(companydata_test2$sales_new,pred2)
  a2
  acc2<-c(acc2,sum(diag(a2))/sum(a2))
  acc2
  
}


##########################################Third model#############

decisionmodel3 <- C5.0(companydata_train3$sales_new~.,data=companydata_train3,
                       trails=100)
summary(decisionmodel3)
predict_train3 <- predict(decisionmodel3,companydata_train3)
predict_train3
table_train3 <-table(companydata_train3$sales_new,predict_train3)
AcurracyTrain3 <- sum(diag(table_train3))/sum(table_train3)
AcurracyTrain3
predict_test3 <- predict(decisionmodel3,companydata_test3)
predict_test3
table_test3 <-table(companydata_test3$sales_new,predict_test3)
AcurracyTest3 <- sum(diag(table_test3))/sum(table_test3)

acc3<-c()
for(i in 1:100)
{
  inTraininglocal3 <- sample(nrow(companydata3),320)
  companydata_train3 <- companydata3[inTraininglocal3,]
  companydata_test3 <- companydata3[-inTraininglocal3,]
  
  fittree3 <- C5.0(companydata_train3$sales_new~.,data = companydata_train3)
  pred3<-predict.C5.0(fittree3,companydata_test3)
  a3<-table(companydata_test3$sales_new,pred3)
  a3
  acc3<-c(acc3,sum(diag(a3))/sum(a3))
  acc3
  
}

table_accuracy <- data.frame(c("Model1_boost","Model1_bagging","Model2_boost","Model2_bagging","Model3_boost",
                               "Model3_bagging"),
                             c(AcurracyTest1,mean(acc1),AcurracyTest2,mean(acc2),AcurracyTest3,mean(acc3)))
View(table_accuracy)
