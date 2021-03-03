library(fastDummies)
library(kernlab)
library(dlookr)
library(funModeling)
library(tidyverse) 
library(Hmisc)
library(caret)
install.packages("Hmisc")
set.seed(12343)
forest_data <-read.csv("O:/EXCELR/Assignment/16-SVM/forestfires.csv")
View(forest_data)
str(forest_data)
attach(forest_data)


#Removing first two  columns
forest_data <- forest_data[,c(-1,-2)]

#change size_category column to factor type

forest_data$size_category <- factor(forest_data$size_category,levels = c("small","large")
                                   ,labels = c("S","L")) 
forest_data[,10:28] <- lapply(forest_data[,10:28],FUN = factor)

str(forest_data)
View(forest_data)


#############################################EDA#############################


glimpse(forest_data)
print(status(forest_data)) #rain and area has less unique values
freq(forest_data)
#some months categorical variables are only of one type of target variable
# so few is ignored # mothapr,monthdec,monthjan,monthmay
plot_num(forest_data)
#rain and area is having only one value with most  frequency which can lead to bias in model
dat <- profiling_num(forest_data)


dat %>% select(variable, variation_coef)
#rain and area has more variation_coef which indicated more outliers

boxplot(forest_data$rain)$out
which(forest_data$rain %in%  boxplot(forest_data$rain)$out) #500 
boxplot(forest_data$area)$out
which(forest_data$area %in%  boxplot(forest_data$area)$out) #416 #239




#removing rain and area  outliers  and other month categorical variables
forest_data_old <-forest_data
View(forest_data_old)
forest_data <- forest_data[,c(-21,-17,-19,-25)]
View(forest_data)
forest_data_noout <- forest_data[c(-239,-416,-500),]
#-21,-17,-19,-25
View(forest_data)



forest_data_old[,1:9] <- scale(forest_data_old[,1:9])
forest_data_noout[,1:9] <- scale(forest_data_noout[,1:9])
str(forest_data)


inTraininglocalold<-createDataPartition(forest_data_old$size_category,p=.80,list=F)
forest_data_old_train<-forest_data_old[inTraininglocalold,]
forest_data_old_test<-forest_data_old[-inTraininglocalold,]



inTraininglocal_noout<-createDataPartition(forest_data_noout$size_category,p=.80,list=F)
forest_data_train_noout <-forest_data_noout[inTraininglocal_noout,]
forest_data_test_noout <-forest_data_noout[-inTraininglocal_noout,]


#class(forest_data_train[,1:28])


####################################### model with all categorical and with outliers ###############
ksvmobject_old_vani  <- ksvm(size_category~.,data=forest_data_old_train,kernel="vanilladot")
print(ksvmobject_old_vani)

prediction_old_vani <- predict(ksvmobject_old_vani,forest_data_old_test)
table_old_vani <- table(prediction_old_vani,forest_data_old_test$size_category)
accuracy_old_vani <- sum(diag(table_old_vani))/sum(table_old_vani)
accuracy_old_vani





#################################model with few  categorical and without outliers########


ksvmobject_out_vani  <- ksvm(size_category~.,data=forest_data_train_noout,kernel="vanilladot")
print(ksvmobject_out_vani)

prediction_out_vani <- predict(ksvmobject_out_vani,forest_data_test_noout)
table_out_vani <- table(prediction_out_vani,forest_data_test_noout$size_category)
accuracy_out_vani <- sum(diag(table_out_vani))/sum(table_out_vani)
accuracy_out_vani





####################################### model with all categorical and with outliers ###############
ksvmobject_old_rbf  <- ksvm(size_category~.,data=forest_data_old_train,kernel="rbfdot")
print(ksvmobject_old_rbf)

prediction_old_rbf <- predict(ksvmobject_old_rbf,forest_data_old_test)
table_old_rbf <- table(prediction_old_rbf,forest_data_old_test$size_category)
accuracy_old_rbf <- sum(diag(table_old_rbf))/sum(table_old_rbf)
accuracy_old_rbf
#################################model with few  categorical and without outliers########
ksvmobject_out_rbf  <- ksvm(size_category~.,data=forest_data_train_noout,kernel="rbfdot")
print(ksvmobject_out_rbf)

prediction_out_rbf <- predict(ksvmobject_out_rbf,forest_data_test_noout)
table_out_rbf <- table(prediction_out_rbf,forest_data_test_noout$size_category)
accuracy_out_rbf <- sum(diag(table_out_rbf))/sum(table_out_rbf)
accuracy_out_rbf




table_accuracy <- data.frame(c("with all categorical variables vanilladot","with few categorical and outliers removed vanilladot",
                               "with all  categorical variables rbfdot ","with few categorical variables and outliers removed rbfdot"),
                             c(accuracy_old_vani,accuracy_out_vani,accuracy_old_rbf,accuracy_out_rbf))
View(table_accuracy)



