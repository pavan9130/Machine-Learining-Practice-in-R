library(arules)
library(arulesViz)
Books_Data <- read.csv("O:/EXCELR/Assignment/9-Association Rules/book.csv")
Books_Data[,1:11] <- lapply(Books_Data[,1:11], factor)
str(Books_Data)
View(Books_Data)
transactiondata <- as(Books_Data,"transactions")
summary(transactiondata)
###########################################First Model######################
apriori_object  <- apriori(Books_Data,parameter = list(support=0.01,confidence=0.5,maxlen=5,minlen=2))
apriori_objec
#removing redundant rules based on confidence
apriori_object <- apriori_object[!is.redundant(apriori_object,measure="confidence")]
inspect(head(sort(apriori_object,by="lift")))
plotly_arules(apriori_object)
plot(apriori_object,method = "grouped")
write(apriori_object,file = "association_rules_s0.01_c0.5.csv",sep = ",",quote = TRUE,
      row.names = FALSE)
getwd()


###########################################Second  Model######################
apriori_object  <- apriori(Books_Data,parameter = list(support=0.1,confidence=0.6,maxlen=5,minlen=2))
apriori_objec
#removing redundant rules based on improvement

apriori_object <- apriori_object[!is.redundant(apriori_object,measure="improvement")]
inspect(head(sort(apriori_object,by="lift")))
plotly_arules(apriori_object)
plot(apriori_object,method = "scatterplot")
write(apriori_object,file = "association_rules_c0.6_s0.1.csv",sep = ",",quote = TRUE,
      row.names = FALSE)
getwd()
###########################################Third  Model######################
apriori_object  <- apriori(Books_Data,parameter = list(support=0.2,confidence=0.7,maxlen=5,minlen=2))
apriori_objec
quality(apriori_object)
#removing redundant rules based on measurement "confidence"
apriori_object <- apriori_object[!is.redundant(apriori_object,measure="confidence")]
inspect(head(sort(apriori_object,by="lift")))
plotly_arules(apriori_object)
plot(apriori_object,method = "matrix")
write(apriori_object,file = "association_rules_s0.2_c0.7.csv",sep = ",",quote = TRUE,
      row.names = FALSE)
getwd()

apriori_object  <- apriori(Books_Data,parameter = list(support=0.5,confidence=0.8,maxlen=5,minlen=2))
apriori_objec
quality(apriori_object)
#removing redundant rules based on improvement
apriori_object <- apriori_object[!is.redundant(apriori_object,measure="improvement")]
inspect(head(sort(apriori_object,by="lift")))
plotly_arules(apriori_object)
plot(apriori_object,method = "graph")
write(apriori_object,file = "association_rules_s0.5_c0.8.csv",sep = ",",quote = TRUE,
      row.names = FALSE)
getwd()

