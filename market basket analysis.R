# Load libraries
library(tidyverse) # data manipulation
library(arules) # mining association rules and frequent itemsets
library(arulesViz) # visualization techniques for association rules
library(knitr) # dynamic report generation
library(gridExtra) # provides a number of user-level functions to work with "grid" graphics
library(lubridate) # work with dates and times
library(plyr)

trans = read.csv("c://data//Groceries_dataset.csv")#baca data
head(trans)
View(trans)
summary(trans)
str(trans)
sum(is.na(trans))

sorted = trans[order(trans$Member_number),]
sorted$Member_number = as.numeric(sorted$Member_number)
str(sorted)

itemList = ddply(sorted, c("Member_number","Date"), function(trans)paste(trans$itemDescription,collapse = ","))           
head(itemList,15)

itemList$Member_number <- NULL
itemList$Date <- NULL
colnames(itemList) <- c("itemList")
head(itemList)
str(itemList)
write.csv(itemList,"ItemList.csv", quote = FALSE, row.names = TRUE)

txn = read.transactions(file="ItemList.csv", rm.duplicates= TRUE, format="basket",sep=",",cols=1);
print(txn)
txn@itemInfo$labels <- gsub("\"","",txn@itemInfo$labels)
basket_rules <- apriori(txn, parameter = list(minlen=2, sup = 0.001, conf = 0.05, target="rules"))
summary(basket_rules)
inspect(basket_rules[1:20])
plot(basket_rules[1:20], method="graph")
plot(basket_rules[1:50], method="graph")
plot(basket_rules[1:20], method="paracoord")
itemFrequencyPlot(txn, topN = 10)

