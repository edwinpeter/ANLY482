library(arules)
library(arulesViz)
library(plyr)
library(dplyr)
library(reshape2)


prepareBasket <- function(){
  df_sorted <- prepprogramdb[order(prepprogramdb$activity_client_date),]
  df_sorted2 <- actprogramdb[order(actprogramdb$activity_client_date),]
  
  df_itemList <- ddply(prepprogramdb,c("activity_client_date"),
                       function(df1)paste(df1$prep_id,collapse = ","))  
  df_itemList2 <- ddply(actprogramdb,c("activity_client_date"),
                       function(df1)paste(df1$actual_id,collapse = ","))
  df_itemList$activity_client_date <- NULL
  df_itemList2$activity_client_date <- NULL
  colnames(df_itemList) <- c("itemList")
  colnames(df_itemList2) <- c("itemList")
  final <- rbind(df_itemList,df_itemList2)
  write.csv(final,"D:/Documents/SMU/Year 4/Semester 2/Analytics Practicum/ANLY482/crud/Analysis/ItemList.csv", row.names = TRUE)
}

# generateBasketRules <- function(val1=0.01,val2=0.5){
#   basket_rules <- apriori(transaction,parameter = list(supp=val1,conf=val2))
#   # basket_rules <- apriori(transaction,parameter = list(sup=0.01,conf=0.3,target="rules"))
#   if(sessionInfo()['basePkgs']=="tm" | sessionInfo()['otherPkgs']=="tm"){
#     detach(package:tm, unload=TRUE)
#   }
# 
#   inspect(basket_rules)
#   mbatbl <- transaction@itemInfo  
# }

generateTransactions <- function(){
  transaction = read.transactions(file="D:/Documents/SMU/Year 4/Semester 2/Analytics Practicum/ANLY482/crud/Analysis/ItemList.csv", rm.duplicates = TRUE, format = "basket", sep = ",",cols=1)
  # transaction@itemInfo$labels <- gsub("\"","",transaction@itemInfo$labels)
  transaction
}

# prepareBasket()
# generateBasketRules(0.9,0.1)

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

#Methods to activate
prepareBasket()
transaction <- generateTransactions()