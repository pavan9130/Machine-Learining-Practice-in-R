#Required Library
library(corpcor)
library(MASS)
library(psych)
library(car)
#importing data
Toyota_data <- read.csv("O:/EXCELR/Assignment/5-Mutli Linear Regression/ToyotaCorolla.csv")

Toyota_data<-Toyota_data[c("Price","Age_08_04","KM","HP","cc","Doors","Gears","Quarterly_Tax","Weight")]
View(Toyota_data)

Toyota_data$Doors <- as.character(Toyota_data$Doors)
Toyota_data$Gears <- as.character(Toyota_data$Gears)
Toyota_data[,6] <- factor(Toyota_data[,6],levels = c("2","3","4","5"),
                          labels = c("2","3","4","5"))
Toyota_data[,7] <- factor(Toyota_data[,6],levels = c("3","4","5","6"),
                          labels = c("3","4","5","6"))
str(Toyota_data)
#stding data
Toyota_data[,c(1,2,3,4,5,8,9)] <- scale(Toyota_data[,c(1,2,3,4,5,8,9)])
View(Toyota_data)
attach(Toyota_data)



#feature selection and checking for collinearity
pairs(Toyota_data[,c(1,2,3,4,5,8,9)])
pairs.panels(Toyota_data[,c(1,2,3,4,5,8,9)])
cor(Toyota_data[,c(1,2,3,4,5,8,9)])

#different linear models with numerical predictors
Toyota_lm_allnum<- lm(Price~Age_08_04+KM+HP+cc+Quarterly_Tax+Weight,data = Toyota_data)
summary(Toyota_lm_allnum)
Toyota_lm_allnum_rsq <- summary(Toyota_lm_allnum)$r.squared
Toyota_lm_allnum_adj_rsq <- summary(Toyota_lm_allnum)$adj.r.squared

Toyota_lm_4 <- lm(Price~cc,data = Toyota_data)  
summary(Toyota_lm_4)
Toyota_lm_5 <- lm(Price~Quarterly_Tax,data = Toyota_data)
summary(Toyota_lm_5)
Toyota_lm_6 <- lm(Price~Weight,data = Toyota_data)
summary(Toyota_lm_6)

#Comparing correlation and partial correlation for all num variables 
cor(Toyota_data[,c(1,2,3,4,5,8,9)])
cor2pcor(cor(Toyota_data[,c(1,2,3,4,5,8,9)]))
#CC shows difference 

### Variance Inflation Factors
vif(Toyota_lm_allnum)
#cc has low

Toyota_lm_withoutcc<- lm(Price~Age_08_04+KM+HP+Weight+Quarterly_Tax,data = Toyota_data)
summary(Toyota_lm_withoutcc)
Toyota_lm_withoutcc_rsq <- summary(Toyota_lm_withoutcc)$r.squared
Toyota_lm_withoutcc_adj_rsq <- summary(Toyota_lm_withoutcc)$adj.r.squared



#added variable plots with and without cc 
avPlots(Toyota_lm_allnum, id.n=5, id.cex=100, col="red")
avPlots(Toyota_lm_withoutcc, id.n=5, id.cex=100, col="red")
#avPlots(Toyota_lm_new1, id.n=5, id.cex=100, col="blue")


stepAIC(Toyota_lm_allnum)
#from all these plots final model selection is 
#with Age_08_04+KM+HP+Weight+Quarterly_Tax


#checking for outliers

plot(Toyota_lm_allnum) #81 and #222 

#recheck using diagnostics plots 
# Deletion Diagnostics for identifying influential variable
influence.measures(Toyota_lm_allnum)
influenceIndexPlot(Toyota_lm_allnum) # Index Plots of the influence measures
influencePlot(Toyota_lm_allnum)# A user friendly representation of the above



## Regression after with and without   81 and 222 observations and  without cc 


Toyota_lm_withoutcc_upd<- lm(Price~Age_08_04+KM+HP+Weight+Quarterly_Tax,data = Toyota_data[c(-81,-222),])
summary(Toyota_lm_withoutcc_upd)
Toyota_lm_withoutcc_upd_rsq <- summary(Toyota_lm_withoutcc_upd)$r.squared
Toyota_lm_withoutcc_upd_adj_rsq <- summary(Toyota_lm_withoutcc_upd)$adj.r.squared


#final models without cc  and without outliers and with all predictors


model_final1 <- lm(Price~Age_08_04+KM+HP+Weight+Quarterly_Tax+Doors+Gears,data = Toyota_data)
summary(model_final1)
model_final1_rsq <- summary(model_final1)$r.squared
model_final1_adj_rsq <- summary(model_final1)$adj.r.squared
prediction_final1 <- predict(model_final1,interval="prediction")
prediction_final1


model_final2  <- lm(Price~Age_08_04+KM+HP+Weight+Quarterly_Tax+Doors+Gears,data = Toyota_data[c(-81,-222),])
summary(model_final2)
model_final2_rsq <- summary(model_final2)$r.squared
model_final2_adj_rsq <- summary(model_final2)$adj.r.squared
prediction_final2 <- predict(model_final2,interval="prediction")
prediction_final2



table_rsqaure  <- data.frame( c("model_with_all_num","model_without_cc","model_without_outlier","model_allcols_with_outlier","model_allcols_without_outlier"),
                              c(Toyota_lm_allnum_rsq,Toyota_lm_withoutcc_rsq,Toyota_lm_withoutcc_upd_rsq,model_final1_rsq,model_final2_rsq))
table_adj_rsqaure  <- data.frame( c("model_with_all_num","model_without_cc","model_without_outlier","model_allcols_with_outlier","model_allcols_without_outlier"),
                                  c(Toyota_lm_allnum_adj_rsq,Toyota_lm_withoutcc_adj_rsq,Toyota_lm_withoutcc_upd_adj_rsq,model_final1_adj_rsq,model_final2_adj_rsq))
View(table_rsqaure)
View(table_adj_rsqaure)

