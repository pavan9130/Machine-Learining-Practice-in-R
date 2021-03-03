library("lattice")

# importing data
Delivery_time <- read.csv("O:/EXCELR/Assignment/4-Linear Regression/delivery_time.csv")
View(Delivery_time)
str(Delivery_time)

# histogram 

hist(Delivery_time$Delivery.Time,prob=T)
hist(Delivery_time$Sorting.Time,prob=T)

# boxplot
boxplot(Delivery_time$Delivery.Time,col = "blue",horizontal = T)
boxplot(Delivery_time$Sorting.Time,col = "red",horizontal = T)



#dotplot

dotplot(Delivery_time$Delivery.Time)
dotplot(Delivery_time$Sorting.Time)


#scatterplot
plot(x=Delivery_time$Sorting.Time,y=Delivery_time$Delivery.Time,type = "p",
     xlab="experience ",ylab="Delivery.time",col="red",lty="solid",main="Sorting time vs delivery time")
#abline(reg,col="blue",lwd=2)
# checking correlation 
attach(Delivery_time)


regmodel1 <- lm(Delivery.Time~Sorting.Time,data = Delivery_time)
plot(Delivery.Time~Sorting.Time,type = "p",xlab="Sorting Time",ylab = "Delivery Time",
     col="red",pch=13,main="Sorting time vs delivery time",col.lab="green",col.main="blue")
abline(regmodel1,col="blue",lwd=2)
predict1 <- predict(regmodel1,interval = "prediction")
predict1 <- as.data.frame(predict1)
cor(predict1$fit,Delivery.Time)
regmodel1_rsqaure <- summary(regmodel1)$r.squared  

regmodel2 <- lm(log(Delivery.Time)~Sorting.Time,data = Delivery_time)
plot(log(Delivery.Time)~Sorting.Time,type = "p",xlab="Sorting Time",ylab = "Delivery Time",
     col="red",pch=13,main="Sorting time vs delivery time",col.lab="green",col.main="blue")
abline(regmodel2,col="blue",lwd=2)
predict2 <- predict(regmodel2,interval = "prediction")
predict2 <- as.data.frame(exp(predict2))
cor(predict2$fit,Delivery.Time)
regmodel2_rsqaure <- summary(regmodel2)$r.squared

regmodel3 <- lm(Delivery.Time~log(Sorting.Time),data = Delivery_time)
plot(Delivery.Time~log(Sorting.Time),type = "p",xlab="Sorting Time",ylab = "Delivery Time",
     col="red",pch=13,main="Sorting time vs delivery time",col.lab="green",col.main="blue")
abline(regmodel3,col="blue",lwd=2)
predict3 <- predict(regmodel3,interval = "prediction")
predict3 <- as.data.frame(predict3)
cor(predict3$fit,Delivery.Time)
regmodel3_rsqaure <- summary(regmodel3)$r.squared

regmodel4 <- lm(sqrt(Delivery.Time)~Sorting.Time,data = Delivery_time)
plot(sqrt(Delivery.Time)~Sorting.Time,type = "p",xlab="Sorting Time",ylab = "Delivery Time",
     col="red",pch=13,main="Sorting time vs delivery time",col.lab="green",col.main="blue")
abline(regmodel4,col="blue",lwd=2)
predict4 <- predict(regmodel4,interval = "prediction")
predict4 <- as.data.frame(predict4*predict4)
cor(predict4$fit,Delivery.Time)
regmodel4_rsqaure <- summary(regmodel4)$r.squared

#collecting all r-sqaure values for different models
rsqaure_table  <- data.frame(c("regmodel1","regmodel2","regmodel3","regmodel4"),
                             c(regmodel1_rsqaure,regmodel2_rsqaure,regmodel3_rsqaure,regmodel4_rsqaure))
View(rsqaure_table)
#selecting maximum r-square value
max(rsqaure_table[,2])




