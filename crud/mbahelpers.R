library(arules)
library(arulesViz)
library(plyr)
library(dplyr)

data("Groceries")


str(Groceries)
head(Groceries@itemInfo, n=12)

prepprogramdb

df_sorted <- prepprogramdb[order(prepprogramdb$activity_client_date),]
# df_sorted$activity_client_date
df_sorted

df_itemList <- ddply(prepprogramdb,c("activity_client_date"),
                     function(df1)paste(df1$prep_id,collapse = ","))



df_itemList$itemList <- NULL
colnames(df_itemList) <- c("itemList")
write.csv(df_itemList,"D:/Documents/SMU/Year 4/Semester 2/Analytics Practicum/ANLY482/crud/Analysis/ItemList.csv", row.names = TRUE)

transaction = read.transactions(file="D:/Documents/SMU/Year 4/Semester 2/Analytics Practicum/ANLY482/crud/Analysis/ItemList.csv", rm.duplicates = TRUE, format = "basket", sep = ",",cols=1)
transaction@itemInfo$labels <- gsub("\"","",transaction@itemInfo$labels)
basket_rules <- apriori(transaction,parameter = list(sup=0.01,conf=0.5,target="rules"))

if(sessionInfo()['basePkgs']=="tm" | sessionInfo()['otherPkgs']=="tm"){
  detach(package:tm, unload=TRUE)
}

inspect(basket_rules)
mbatbl <- transaction@itemInfo

#Read
ReadData_mbatbl <- function() {
  if (exists("mbatbl")) {
    mbatbl
  }
}

GetTableMetadata_mbatbl <- function() {
  fields <- c(id = "Id", 
              labels = "labels"
  )
  
  result <- list(fields = fields)
  return (result)
}