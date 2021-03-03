library(fastDummies)
library(kernlab)
library(dplyr)
library(funModeling)
#importing train and test data
salary_data_train <-read.csv("O:/EXCELR/Assignment/16-SVM/SalaryData_Train(1).csv")
salary_data_test <-read.csv("O:/EXCELR/Assignment/16-SVM/SalaryData_Test(1).csv")
View(salary_data_train)
View(salary_data_test)
#Removing few columns
salary_data_train <- salary_data_train[,c(-3,-13)]
salary_data_test <- salary_data_test[,c(-3,-13)]


salary_data_train[,2] <-factor(salary_data_train[,2])
salary_data_train[,4] <-factor(salary_data_train[,4])
salary_data_train[,5] <-factor(salary_data_train[,5])
salary_data_train[,6] <-factor(salary_data_train[,6])
salary_data_train[,7] <-factor(salary_data_train[,7])
salary_data_train[,12] <-factor(salary_data_train[,12])

print(status(salary_data_train))
#most of values of capital gain and  capital loss are zero
#####################################EDA#########################

ggplot(salary_data_train, 
       aes(x = workclass, 
           fill = Salary)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")
#without pay has no significance

ggplot(salary_data_train, 
       aes(x = maritalstatus, 
           fill = Salary)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")
# married-af and married-civ almost same
#widowed and spouse absent almost same 

ggplot(salary_data_train, 
       aes(x = occupation, 
           fill = Salary)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")
#has more cardiality we can try removing this column 

ggplot(salary_data_train, 
       aes(x = relationship, 
           fill = Salary)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")
#own child and other relative  has no significance #wife and husband almost same 

ggplot(salary_data_train, 
       aes(x = race, 
           fill = Salary)) + 
  geom_bar(position = "fill") +
  labs(y = "Proportion")
#WHITE AND BLACK AND OTHER CAN BE CONSIDERED


plot_num(salary_data_train)
#from this also we can remove capital gain and capital loss
dat <- profiling_num(salary_data_train)
dat
#from above variation_coef is more for capital gain and capital loss
#####################################EDA#########################


#####################pre processing train  data######################

#Changing Salary column type  into factor
salary_data_train$Salary <- factor(salary_data_train$Salary,levels = c(" <=50K"," >50K")
                                   ,labels = c("L","G")) 
#Creating dummies for categorical data
salary_data_train <-dummy_cols(salary_data_train,
                               select_columns = c("workclass","maritalstatus","occupation",
                                                  "relationship","race"))
#Removing original Columns
salary_data_train <-salary_data_train[,c(-2,-4,-5,-6,-7)]

#converting data type to factor
salary_data_train[,8:46] <- lapply(salary_data_train[,8:46],FUN = factor)
#converting sex to factor
salary_data_train[,3] <- factor(salary_data_train[,3])
#standardizing numerical data
#salary_data_train[,c(1,2,4,5,6)] <- scale(salary_data_train[,c(1,2,4,5,6)])

View(salary_data_train)
str(salary_data_train)
salary_data_train1 <- salary_data_train[,c(-4,-5)]
salary_data_train2 <- salary_data_train[,c(-14,-17,-21,-38,-39,-41,-42,-43)]
salary_data_train3 <- salary_data_train[,c(-4,-5,-14,-17,-21,-38,-39,-41,-42,-43)]
View(salary_data_train3)

#####################pre processing test data######################
#Changing Salary column type  into factor
salary_data_test$Salary <- factor(salary_data_test$Salary,levels = c(" <=50K"," >50K")
                                   ,labels = c("L","G")) 
#Creating dummies for categorical data
salary_data_test <-dummy_cols(salary_data_test,
                              select_columns = c("workclass","maritalstatus","occupation",
                                                 "relationship","race"))
#Removing original Columns
salary_data_test <-salary_data_test[,c(-2,-4,-5,-6,-7)]

#converting data type to factor
salary_data_test[,8:46] <- lapply(salary_data_test[,8:46],FUN = factor)
#converting sex to factor
salary_data_test[,3] <- factor(salary_data_test[,3])
#salary_data_test[,c(1,2,4,5,6)] <- scale(salary_data_test[,c(1,2,4,5,6)])
View(salary_data_test)
str(salary_data_test)
salary_data_test1 <- salary_data_test[,c(-4,-5)]
salary_data_test2 <- salary_data_test[,c(-14,-17,-21,-38,-39,-41,-42,-43)]
salary_data_test3 <- salary_data_test[,c(-4,-5,-14,-17,-21,-38,-39,-41,-42,-43)]


################################ First model #########################
ksvmobject1_vani  <- ksvm(Salary~.,data=salary_data_train1,kernel="vanilladot")

prediction1_vani <- predict(ksvmobject1_vani,salary_data_test1)
table1_vani <- table(prediction1_vani,salary_data_test1$Salary)
accuracy1_vani <- sum(diag(table1_vani))/sum(table1_vani)
accuracy1_vani

ksvmobject1_rbf  <- ksvm(Salary~.,data=salary_data_train1,kernel="rbfdot")

prediction1_rbf <- predict(ksvmobject1_rbf,salary_data_test1)
table1_rbf <- table(prediction1_rbf,salary_data_test1$Salary)
accuracy1_rbf <- sum(diag(table1_rbf))/sum(table1_rbf)
accuracy1_rbf
#####################################second model ####################
#applying SVM model

ksvmobject2_vani  <- ksvm(Salary~.,data=salary_data_train2,kernel="vanilladot")

prediction2_vani <- predict(ksvmobject2_vani,salary_data_test2)
table2_vani <- table(prediction2_vani,salary_data_test2$Salary)
accuracy2_vani <- sum(diag(table2_vani))/sum(table2_vani)
accuracy2_vani

ksvmobject2_rbf  <- ksvm(Salary~.,data=salary_data_train2,kernel="rbfdot")

prediction2_rbf <- predict(ksvmobject2_rbf,salary_data_test2)
table2_rbf <- table(prediction2_rbf,salary_data_test2$Salary)
accuracy2_rbf <- sum(diag(table2_rbf))/sum(table2_rbf)
accuracy2_rbf

#################################### third model ##############
#applying SVM model

ksvmobject3_vani  <- ksvm(Salary~.,data=salary_data_train3,kernel="vanilladot")

prediction3_vani <- predict(ksvmobject3_vani,salary_data_test3)
table3_vani <- table(prediction3_vani,salary_data_test3$Salary)
accuracy3_vani <- sum(diag(table3_vani))/sum(table3_vani)
accuracy3_vani

ksvmobject3_rbf  <- ksvm(Salary~.,data=salary_data_train3,kernel="rbfdot")

prediction3_rbf <- predict(ksvmobject3_rbf,salary_data_test3)
table3_rbf <- table(prediction3_rbf,salary_data_test3$Salary)
accuracy3_rbf <- sum(diag(table3_rbf))/sum(table3_rbf)
accuracy3_rbf

table_accuracy_vani <- data.frame(c("ksvm_model1","ksvm_model2","ksvm_model3"),
                                  c(accuracy1_vani,accuracy2_vani,accuracy3_vani))
table_accuracy_rbf <- data.frame(c("ksvm_model1","ksvm_model2","ksvm_model3"),
                                  c(accuracy1_rbf,accuracy2_rbf,accuracy3_rbf))
View(table_accuracy_vani)
View(table_accuracy_rbf)
