library(arules)
library(arulesViz)
#Importing Data

Movies_Data <- read.csv("O:/EXCELR/Assignment/9-Association Rules/my_movies.csv")
Movies_Data <- Movies_Data[,c(6:15)]
Movies_Data[,1:10] <- lapply(Movies_Data[,1:10], factor)
View(Movies_Data)
transactiondata <- as(Movies_Data,"transactions")
summary(transactiondata)
inspect(transactiondata[1:10])

#########################################First Model #######################
apriori_object <- apriori(transactiondata,parameter = list(support=0.1,confidence=0.7,maxlen=5,minlen=2))
inspect(head(sort(apriori_object,by="lift")))
apriori_object
#removing redundant rules based on measurement "confidence"
apriori_object <- apriori_object[!is.redundant(apriori_object,measure="confidence")]
apriori_object

plotly_arules(apriori_object)

plot(apriori_object,method = "grouped")
write(apriori_object,file = "association_rules_s0.1_c0.7.csv",sep = ",",quote = TRUE,
      row.names = FALSE)
###########################################Second Model########################
apriori_object <- apriori(transactiondata,parameter = list(support=0.2,confidence=0.8,maxlen=5,minlen=2))
inspect(head(sort(apriori_object,by="lift")))
apriori_object

#removing redundant rules based on improvement

apriori_object <- apriori_object[!is.redundant(apriori_object,measure="improvement")]
plotly_arules(apriori_object)
plot(apriori_object,method = "matrix")
write(apriori_object,file = "association_rules_s0.2_c0.8.csv",sep = ",",quote = TRUE,
      row.names = FALSE)

##########################################Third Model ############################
apriori_object <- apriori(transactiondata,parameter = list(support=0.3,confidence=0.9,maxlen=5,minlen=2))
inspect(head(sort(apriori_object,by="lift")))
apriori_object

#removing redundant rules based on measurement "confidence"
apriori_object <- apriori_object[!is.redundant(apriori_object,measure="confidence")]
plotly_arules(apriori_object)
plot(apriori_object,method = "graph")
write(apriori_object,file = "association_rules_s0.3_c0.85.csv",sep = ",",quote = TRUE,
      row.names = FALSE)
####################################### Fourth model ######################
apriori_object <- apriori(transactiondata,parameter = list(support=0.5,confidence=0.95,maxlen=5,minlen=2))
inspect(head(sort(apriori_object,by="lift")))
apriori_object

#removing redundant rules based on improvement

apriori_object <- apriori_object[!is.redundant(apriori_object,measure="improvement")]
apriori_object
plotly_arules(apriori_object)
plot(apriori_object,method = "scatterplot")
write(apriori_object,file = "association_rules_s0.5_c0.9.csv",sep = ",",quote = TRUE,
      row.names = FALSE)
getwd()
