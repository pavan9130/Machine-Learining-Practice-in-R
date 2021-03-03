#############################CUTLET DATA#######################
setwd("O:/EXCELR/Assignment/3-Hypothesis testing")
#import data
cutlets_data <- read.csv(file.choose())
View(cutlets_data)
#checking normality
shapiro.test(cutlets_data$Unit.A)
#p value > 0.05 so accept null hypothesis i.e normally distb
shapiro.test(cutlets_data$Unit.B)
#p value > 0.05 so accept null hypothesis i.e normally distb
#Both are normally distributed
#External Conditions does not remain same
#checking for variance equality
var.test(cutlets_data$Unit.A,cutlets_data$Unit.B)
#H0 :  Variances are equal
#H1 :  Variances are not Equal 
#as p value >0.05 we can say variances are equal 
t.test(cutlets_data$Unit.A,cutlets_data$Unit.B,alternative = "two.sided",conf.level = 0.95,correct = TRUE)
#null Hypothesis -> Equal means
# Alternate Hypothesis -> Unequal means
# as p >0.05 both are means are equal 

#NO significant difference in the diameter of the cutlet between two units


##################### LAB DATA ############################
#import data
lab_data <- read.csv(file.choose())
View(lab_data)
attach(lab_data)
#checking normality
shapiro.test(Laboratory.1)
#p value > 0.05 so accept null hypothesis i.e normally distb
shapiro.test(Laboratory.2)
#p value > 0.05 so accept null hypothesis i.e normally distb
shapiro.test(Laboratory.3)
#p value > 0.05 so accept null hypothesis i.e normally distb
shapiro.test(Laboratory.4)
#p value > 0.05 so accept null hypothesis i.e normally distb
#ALL are normally distributed
Stacked_Data <- stack(lab_data)
View(Stacked_Data)
attach(Stacked_Data)

#checking for variance equality
bartlett.test(values~ind ,data = Stacked_Data)
#H0 :  Variances are equal
#H1 :  Variances are not Equal 
#as p value >0.05 we can say variances are equal
anova_test  <- aov(values~ind,data = Stacked_Data)
summary(anova_test)
#checking for equal and unequal means
# as p <0.05 both  means are not equal 

#there is  difference in average TAT among the different laboratories at 5% significance level.


######################## Buyer Ratio #########################
pro_table <- as.table(rbind(c(50,550),c(142,351),c(131,480),c(70,350)))
dimnames(pro_table) <- list( region = c("East","West", "North","South"),gender = c("M", "F"))
#more than two populations ( 4 regions)
(chi_sqtest <- chisq.test(pro_table))
# p value is < 0.05 so proportions are not equal

# male-female buyer rations are not similar across regions

######################### Customer Order Form#################
customer_data <- read.csv(file.choose())
View(customer_data)
attach(customer_data)
table(Phillippines)
table(Indonesia)
table(Malta)
table(India)
pro_table_1 <- as.table(rbind(c(29,271),c(33,267),c(31,269),c(20,280)))
dimnames(pro_table_1) <- list(Region=c("Phillippines","Indonesia","Malta","India"),Def= c("Defective","Error Free"))
#more than two populations ( 4 countries)
(chi_sqtest_1 <- chisq.test(pro_table_1))
# as p>0.05 All proportions are Equal
#the defective %  does not varies by centre
######################### Fantaloons #####################
Fantaloons_data <- read.csv(file.choose())
View(Fantaloons_data)
attach(Fantaloons_data)
table(Weekdays)
table(Weekend)
pro_table_2 <- as.table(rbind(c(113,287),c(167,233)))
dimnames(pro_table_2) <- list(day= c("Weekdays","Weekend"),Gender=c("Male","Female"))
prop.test(x=c(113,167),n=c(400,400),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
# p value <0.05 so both proportions are  different
prop.test(x=c(113,167),n=c(400,400),conf.level = 0.95,correct = FALSE,alternative = "less")
# p value <0.05 so male propotion on weekdays > weekends
#Thus male vs female proportions are different based on day of week