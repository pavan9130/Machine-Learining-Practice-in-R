library(arules)
library(arulesViz)

transactiondata <- read.transactions("O:/EXCELR/Assignment/9-Association Rules/groceries.csv",
                                     format = c("basket"),sep =",")
summary(transactiondata)
inspect(transactiondata[1:15])


#with support of 0.03 and confidence of 0.4
apriori_object  <- apriori(transactiondata,parameter = list(support=0.03,confidence=0.4,maxlen=10,minlen=2))
apriori_object
#str(apriori_object)
#removing redundant rules based on measurement "confidence"
apriori_object <- apriori_object[!is.redundant(apriori_object,measure="confidence")]
apriori_object
inspect(head(sort(apriori_object,by="lift")))
plotly_arules(apriori_object)
plot(apriori_object,method = "grouped")
write(apriori_object,file = "association_rules_s0.03_c0.4.csv",sep = ",",quote = TRUE,
      row.names = FALSE)


#with support of 0.01 and confidence of 0.5
apriori_object  <- apriori(transactiondata,parameter = list(support=0.01,confidence=0.5,maxlen=10,minlen=2))
apriori_object
#str(apriori_object)
#removing redundant rules based on improvement

apriori_object <- apriori_object[!is.redundant(apriori_object,measure="improvement")]
apriori_object
inspect(head(sort(apriori_object,by="lift")))
plotly_arules(apriori_object)
plot(apriori_object,method = "matrix")
write(apriori_object,file = "association_rules_s0.01_c0.5.csv",sep = ",",quote = TRUE,
      row.names = FALSE)

#with support of 0.001 and confidence of 0.5
apriori_object  <- apriori(transactiondata,parameter = list(support=0.001,confidence=0.5,maxlen=10,minlen=2))
apriori_object
#str(apriori_object)
#removing redundant rules based on measurement "confidence"
apriori_object <- apriori_object[!is.redundant(apriori_object,measure="confidence")]
apriori_object
inspect(head(sort(apriori_object,by="lift")))
plotly_arules(apriori_object)
plot(apriori_object,method = "graph")
write(apriori_object,file = "association_rules_s0.001_c0.5.csv",sep = ",",quote = TRUE,
      row.names = FALSE)

#with support of 0.001 and confidence of 0.8
apriori_object  <- apriori(transactiondata,parameter = list(support=0.001,confidence=0.8,maxlen=10,minlen=2))
apriori_object
#str(apriori_object)
#removing redundant rules based on improvement

apriori_object <- apriori_object[!is.redundant(apriori_object,measure="improvement")]
apriori_object
inspect(head(sort(apriori_object,by="lift")))
plotly_arules(apriori_object)
plot(apriori_object,method = "scatterplot")
write(apriori_object,file = "association_rules_s0.001_c0.8.csv",sep = ",",quote = TRUE,
      row.names = FALSE)