#Required Library
library(corpcor)
library(MASS)
library(psych)
library(car)
#importing data

startup_data <- read.csv("O:/EXCELR/Assignment/5-Mutli Linear Regression/50_Startups.csv")
View(startup_data)
str(startup_data)
unique(startup_data$State)
startup_data[,4] <- factor(startup_data[,4])

#standardising num data
startup_data[,c(1,2,3,5)] <- scale(startup_data[,c(1,2,3,5)])
attach(startup_data)
#feature selection and checking for collinearity
pairs(startup_data[,c(1,2,3,5)])
pairs.panels(startup_data[,c(1,2,3,5)])
cor(startup_data[,c(1,2,3,5)])

#different linear models with numerical predictors
startup_lm_allnum <- lm(Profit~R.D.Spend+Administration+Marketing.Spend,data = startup_data)
startup_lm_allnum_rsq <-summary(startup_lm_allnum)$r.squared
startup_lm_allnum_adj_rsq <-summary(startup_lm_allnum)$adj.r.squared

#Administration and Marketing.Spen are  not significant from combined model

startup_lm_1<- lm(Profit~Administration,data = startup_data)
summary(startup_lm_1)
startup_lm_2 <- lm(Profit~Marketing.Spend,data = startup_data)
summary(startup_lm_2)
startup_lm_3<- lm(Profit~Administration+Marketing.Spend,data = startup_data)
summary(startup_lm_3)


#checking correlation and partial correlation for all num variables 
cor(startup_data[,c(1,2,3,5)])
cor2pcor(cor(startup_data[,c(1,2,3,5)]))
#from above we can see there is drastic change for Administration 


### Variance Inflation Factors
vif(startup_lm_allnum)

stepAIC(startup_lm_allnum)

#numerical model removing Administration 
startup_lm_2num <- lm(Profit~R.D.Spend+Marketing.Spend,data = startup_data)
summary(startup_lm_2num)
startup_lm_2num_rsq <-summary(startup_lm_2num)$r.squared
startup_lm_2num_adj_rsq <-summary(startup_lm_2num)$adj.r.squared

#added variable plots with and without Administration 
avPlots(startup_lm_allnum, id.n=5, id.cex=100, col="red")
avPlots(startup_lm_2num, id.n=5, id.cex=100, col="blue")


#from all these plots final model selection is 
#with predictors R.D.Spend and  Marketing.Spend

#checking for outliers

plot(startup_lm_n) #50 and #49 

#recheck using diagnostics plots 
# Deletion Diagnostics for identifying influential variable
influence.measures(startup_lm_n)
influenceIndexPlot(startup_lm_n) # Index Plots of the influence measures
influencePlot(startup_lm_n)# A user friendly representation of the above


## Regression after deleting the 50th observation
startup_lm_allnum_upd <- lm(Profit~R.D.Spend+Administration+Marketing.Spend+State,data = startup_data[-50,])
summary(startup_lm_allnum_upd)
startup_lm_allnum_upd_rsq <-summary(startup_lm_allnum_upd)$r.squared
startup_lm_allnum_upd_adj_rsq <-summary(startup_lm_allnum_upd)$adj.r.squared2 


startup_lm_2num_upd <- lm(Profit~R.D.Spend+Marketing.Spend,data = startup_data[-50,])
summary(startup_lm_2num)
startup_lm_2num_upd_rsq <-summary(startup_lm_2num_upd)$r.squared
startup_lm_2num_upd_adj_rsq <-summary(startup_lm_2num_upd)$adj.r.squared

#final models without Administration and with and without outliers


model_final1 <- lm(Profit~R.D.Spend+Marketing.Spend+State,data = startup_data)
summary(model_final1)
model_final1_rsq <-summary(model_final1)$r.squared
model_final1_adj_rsq <-summary(model_final1)$adj.r.squared
prediction_final1 <- predict(model_final1,interval="prediction")
prediction_final1


model_final2  <- lm(Profit~R.D.Spend+Marketing.Spend+State,data = startup_data[-50,])
summary(model_final2)
model_final_rsq <-summary(model_final2)$r.squared
model_fina2_adj_rsq <-summary(model_final2)$adj.r.squared
prediction_final2 <- predict(model_final2,interval="prediction")
prediction_final2


table_rsqaure  <- data.frame( c("model_with_all_num","model_without_admin","model_without_outlier","model_allcols_with_outlier","model_allcols_without_outlier"),
                           c(startup_lm_allnum_rsq,startup_lm_2num_rsq,startup_lm_2num_upd_rsq,model_final1_rsq,model_final2_rsq))
table_adj_rsqaure  <- data.frame( c("model_with_all_num","model_without_admin","model_without_outlier","model_allcols_with_outlier","model_allcols_without_outlier"),
                         c(startup_lm_allnum_adj_rsq,startup_lm_2num_adj_rsq,startup_lm_2num_upd_adj_rsq,model_final1_adj_rsq,model_fina2_adj_rsq))
View(table_rsqaure)
View(table_adj_rsqaure)