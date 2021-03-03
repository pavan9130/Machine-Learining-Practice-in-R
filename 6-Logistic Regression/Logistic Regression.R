library(fastDummies)
library(ggplot2)
library(funModeling)
library(ROCR)
Bank_data <- read.csv("O:/EXCELR/Assignment/6-Logistic Regression/bank-full.csv",sep = ';')
attach(Bank_data)
Bank_data <- na.omit(Bank_data)
View(Bank_data)
str(Bank_data)
#changing to factor
Bank_data[,c(5,7,8,17)] <- lapply(Bank_data[,c(5,7,8,17)], FUN = factor)
Bank_data[,2] <-factor(Bank_data[,2])
Bank_data[,3] <-factor(Bank_data[,3])
Bank_data[,4] <-factor(Bank_data[,4])
Bank_data[,9] <-factor(Bank_data[,9])
Bank_data[,11] <-factor(Bank_data[,11])
Bank_data[,16] <-factor(Bank_data[,16])
View(Bank_data)

str(Bank_data)
Bank_data_original <- Bank_data
table(Bank_data$y)

###################################EDA#######################


ggplot(Bank_data, 
       aes(x = month, 
           fill = y)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")
plot(Bank_data$month) 

ggplot(Bank_data, 
       aes(x = job, 
           fill = y)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")
#proportions are same approx 


#checking numerical data
plot_num(Bank_data)
dat <- profiling_num(Bank_data)
dat$variation_coef
#previous and balance has more variation_coef


boxplot(balance~y,xlab="y",ylab="balance",col=c("pink","lightblue"),
        main="Exploratory Data Analysis Plot\n of y Versus balance")
#balance has no difference
boxplot(duration~y,xlab="y",ylab="duration",col=c("pink","lightblue"),
        main="Exploratory Data Analysis Plot\n of y Versus duration")
boxplot(campaign~y,xlab="y",ylab="campaign",col=c("pink","lightblue"),
        main="Exploratory Data Analysis Plot\n of y Versus campaign")
#campaign has no difference
boxplot(pdays~y,xlab="y",ylab="pdays",col=c("pink","lightblue"),
        main="Exploratory Data Analysis Plot\n of y Versus pdays")
boxplot(day~y,xlab="y",ylab="day",col=c("pink","lightblue"),
        main="Exploratory Data Analysis Plot\n of y Versus day")
#day has no difference
print(status(Bank_data)) #####"previous"  has more zeros dominating so we remove that 

###################################EDA#######################
#removing job,default,balance,  day ,campaign   and previous
Bank_data <- Bank_data[,c(-2,-5,-6,-10,-13,-15)]
View(Bank_data)
str(Bank_data)
Bank_data <- dummy_columns(Bank_data,select_columns = c("month","marital","education", "contact","poutcome"))
View(Bank_data)
#Removing original columns 
Bank_data <- Bank_data[,c(-2,-3,-6,-7,-10)]
View(Bank_data)
Bank_data[,c(7:32)] <-lapply(Bank_data[,c(7:32)], FUN = factor)
#removing previous column
View(Bank_data)

####################################First model  ###### 
View(Bank_data_original)
#removing default and previous
Bank_data_original1 <- Bank_data_original[,c(-5,-15)]
Bank_data_original1 <- dummy_columns(Bank_data_original1,select_columns = c("job","month","marital","education", "contact","poutcome"))
View(Bank_data_original1)
Bank_data_original1 <- Bank_data_original1[,c(-2,-3,-4,-8,-10,-14)]
View(Bank_data_original1)
Bank_data_original1[,c(10:49)] <-lapply(Bank_data_original1[,c(10:47)], FUN = factor)


log_reg_model1 <-glm(y~.,data = Bank_data_original1,family = "binomial")
pred1 <- predict(log_reg_model1,type = c("response"),Bank_data_original1)
View(pred1)

#Confusion Matrix
conftable1 <- table(pred1>0.5,Bank_data_original1$y)
conftable1
Accuracy1 <- sum(diag(conftable1)/sum(conftable1))
Accuracy1

rocpred <- prediction(pred1,Bank_data_original1$y)
rocperf <- performance(rocpred,"acc")
plot(rocperf)
max1 <- which.max(slot(rocperf,"y.values")[[1]])
maxacc1<- slot(rocperf,"y.values")[[1]][max1]
cut1 <- slot(rocperf,"x.values")[[1]][max1]
print (c(Accuracy = maxacc1,Cutoff = cut1))
####################################Second  model  ###### 

View(Bank_data_original)
#removing default job and previous 
Bank_data_original2 <- Bank_data_original[,c(-2,-5,-15)]
Bank_data_original2 <- dummy_columns(Bank_data_original2,select_columns = c("month","marital","education", "contact","poutcome"))
View(Bank_data_original2)
Bank_data_original2 <- Bank_data_original2[,c(-2,-3,-7,-9,-13)]
View(Bank_data_original2)
Bank_data_original2[,c(10:35)] <-lapply(Bank_data_original2[,c(10:35)], FUN = factor)
View(Bank_data_original2)
Bank_data_original3 <- Bank_data_original2 [,-2]


log_reg_model2 <-glm(y~.,data = Bank_data_original2,family = "binomial")
pred2 <- predict(log_reg_model2,type = c("response"),Bank_data_original2)
View(pred2)

#Confusion Matrix
conftable2 <- table(pred2>0.5,Bank_data_original2$y)
conftable2
Accuracy2 <- sum(diag(conftable2)/sum(conftable2))
Accuracy2
#ROC
rocpred2 <- prediction(pred2,Bank_data_original2$y)
rocperf2 <- performance(rocpred2,"acc")
plot(rocperf2)
max2 <- which.max(slot(rocperf2,"y.values")[[1]])
maxacc2<- slot(rocperf,"y.values")[[1]][max2]
cut2 <- slot(rocperf,"x.values")[[1]][max2]
print (c(Accuracy = maxacc2,Cutoff = cut2))

####################################third ###############################
#removing default job and previous and balance
log_reg_model3 <-glm(y~.,data = Bank_data_original3,family = "binomial")
pred3 <- predict(log_reg_model3,type = c("response"),Bank_data_original3)
View(pred3)

#Confusion Matrix
conftable3 <- table(pred3>0.5,Bank_data_original3$y)
conftable3
Accuracy3 <- sum(diag(conftable3)/sum(conftable3))
Accuracy3
#ROC
rocpred3 <- prediction(pred3,Bank_data_original3$y)
rocperf3 <- performance(rocpred3,"acc")
plot(rocperf3)
max3 <- which.max(slot(rocperf3,"y.values")[[1]])
maxacc3<- slot(rocperf,"y.values")[[1]][max3]
cut3 <- slot(rocperf,"x.values")[[1]][max3]
print (c(Accuracy = maxacc3,Cutoff = cut3))

table_accuracy1 <- data.frame(c("First Model Accuracy","Second Model Accuracy",
                               "Third Model Accuracy "),
                             c(Accuracy1,Accuracy2,Accuracy3))

table_accuracy2 <- data.frame(c("First Model max Accuracy","Second Model max Accuracy",
                                "Third Model max Accuracy "),
                              c(maxacc1,maxacc2,maxacc3))
View(table_accuracy1)
View(table_accuracy2)
