library(arules)
library(arulesViz)
book<-read.csv(file.choose())
View(book)
book_trans<-as(as.matrix(book),"transactions")
inspect(book_trans[1:100])
# If we inspect book_trans
# we should get transactions of items i.e.
# As we have 2000 rows ..so we should get 2000 transactions 
# Each row represents one transactions 
# After converting the binary format of data frame from matrix to transactions
# Perform apriori algorithm 
rules<-apriori(as.matrix(book),parameter = list(support=0.002,confidence=0.5))

inspect(rules[1:100])
# Apriori algorithm
graphics.off()
plot(rules)
# Whenever we have binary kind of data .....load them as csv and convert them into 
# matrix format using as.matrix(data) and use this for forming 
# Association rules and change the values of support,confidence, and minlen 
# to get different rules 


# Whenever we have data containing item names, then load that data using 
# read.transactions(file="path",format="basket",sep=",")
# use this to form association rules 


groceries<-read.transactions(file.choose(),format="basket")
inspect(groceries[1:10])
class(groceries)


par("mar")
par(mar=c(1,1,1,1))

itemFrequencyPlot(groceries)
groceries_rules<-apriori(groceries,parameter = list(support = 0.002,confidence = 0.05,minlen=3))

library(arulesViz)
plot(groceries_rules)


library(arules)
data("Groceries")
summary(Groceries)
rules <- apriori(Groceries)

