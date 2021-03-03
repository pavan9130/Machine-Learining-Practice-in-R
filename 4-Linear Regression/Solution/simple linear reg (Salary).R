library("lattice")

# importing data
salary_data <- read.csv("O:/EXCELR/Assignment/4-Linear Regression/Salary_Data.csv")
View(salary_data)
str(salary_data)

# histogram 

hist(salary_data$Salary,prob=T)
hist(salary_data$YearsExperience,prob=T)

# boxplot
boxplot(salary_data$Salary,col = "blue",horizontal = T)
boxplot(salary_data$YearsExperience,col = "red",horizontal = T)

#dotplot

dotplot(salary_data$Salary)
dotplot(salary_data$YearsExperience)


#scatterplot
plot(x=salary_data$YearsExperience,y=salary_data$Salary,type = "p",
     xlab="experience ",ylab="salary",col="red",lty="solid",main="Experience vs salary")
# checking correlation 
attach(salary_data)


#applying lm model for different transformations 
regmodel1 <- lm(Salary~YearsExperience,data = salary_data)
regmodel1_rsqaure <- summary(regmodel1)$r.squared
predict1 <- predict(regmodel1,interval = "prediction")
predict1 <- as.data.frame(predict1)
cor(predict1$fit,Salary)

regmodel2 <- lm(log(Salary)~YearsExperience,data = salary_data)
regmodel2_rsqaure <- summary(regmodel2)$r.squared
predict2 <- predict(regmodel2,interval = "prediction")
predict2 <- as.data.frame(exp(predict2))
cor(predict2$fit,Salary)


regmodel3 <- lm(Salary~log(YearsExperience),data = salary_data)
regmodel3_rsqaure <- summary(regmodel3)$r.squared
predict3 <- predict(regmodel3,interval = "prediction")
predict3 <- as.data.frame(predict3)
cor(predict3$fit,Salary)


regmodel4 <- lm(sqrt(Salary)~YearsExperience,data = salary_data)
regmodel4_rsqaure <- summary(regmodel4)$r.squared
predict4 <- predict(regmodel4,interval = "prediction")
predict4 <- as.data.frame(predict4*predict4)
cor(predict4$fit,Salary)

regmodel5 <- lm(Salary~sqrt(YearsExperience),data = salary_data)
regmodel5_rsqaure <- summary(regmodel5)$r.squared
predict5 <- predict(regmodel5,interval = "prediction")
predict5 <- as.data.frame(predict5)
cor(predict5$fit,Salary)

regmodel6 <- lm(log(Salary)~log(YearsExperience),data = salary_data)
regmodel6_rsqaure <- summary(regmodel6)$r.squared
predict6 <- predict(regmodel6,interval = "prediction")
predict6 <- as.data.frame(exp(predict6))
cor(predict6$fit,Salary)
#joining all predictions 
predicted_all <- NULL
predicted_all <- cbind(Salary,predict1$fit,predict2$fit,predict3$fit,predict4$fit,predict5$fit,predict6$fit)
View(predicted_all)
#collecting all r-sqaure values for different models
rsqaure_table  <- data.frame(c("regmodel1","regmodel2","regmodel3","regmodel4","regmodel5","regmodel6"),
                             c(regmodel1_rsqaure,regmodel2_rsqaure,regmodel3_rsqaure,regmodel4_rsqaure,regmodel5_rsqaure,regmodel6_rsqaure))
View(rsqaure_table)
#selecting maximum r-square value
max(rsqaure_table[,2])
