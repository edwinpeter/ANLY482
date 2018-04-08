library(arules)
library(arulesViz)
library(plyr)
library(dplyr)
library(reshape2)


prepareBasket <- function(){
  #define data frame
  mba_df <- setNames(data.frame(matrix(ncol=13,nrow = 0)),
                     c("activity_client_date", 
                       "t&d", "pp", "ifak", 
                       "ce", "nb-m", "nb-c", 
                       "fs", "ts", "es", 
                       "iwd", "apb", "bike"))
  
  #File naming
  directory <- "D:/Documents/SMU/Year 4/Semester 2/Analytics Practicum/ANLY482/crud/PrepActivityDB/"
  
  #Read the latest file that was saved
  files <- list.files(directory, full.names = TRUE)
  transactions <- read_xls(tail(files, n=1))
  #Coerce data frame if it contains any NA fields
  transactions <- as.data.frame(transactions)
  
  for(i in 1:nrow(transactions)) {
    row <- transactions[i,]
    t_d <- 0
    pp <- 0
    ifak <- 0
    ce <- 0
    nb_m <- 0
    nb_c <- 0
    fs <- 0
    ts <- 0
    es <- 0
    iwd <- 0
    apb <- 0
    bike <- 0
    
    if(row[1] %in% mba_df$activity_client_date){
      temp <- mba_df[match(row[1],mba_df$activity_client_date),]
      if(row[2] == "t&d"){mba_df[match(row[1],mba_df$activity_client_date),]$t.d <- 1}
      if(row[2] == "pp"){mba_df[match(row[1],mba_df$activity_client_date),]$pp <- 1}
      if(row[2] == "ifak"){mba_df[match(row[1],mba_df$activity_client_date),]$ifak <- 1}
      if(row[2] == "ce"){mba_df[match(row[1],mba_df$activity_client_date),]$ce <- 1}
      if(row[2] == "nb-m"){mba_df[match(row[1],mba_df$activity_client_date),]$nb.m <- 1}
      if(row[2] == "nb-c"){mba_df[match(row[1],mba_df$activity_client_date),]$nb.c <- 1}
      if(row[2] == "fs"){mba_df[match(row[1],mba_df$activity_client_date),]$fs <- 1}
      if(row[2] == "ts"){mba_df[match(row[1],mba_df$activity_client_date),]$ts <- 1}
      if(row[2] == "es"){mba_df[match(row[1],mba_df$activity_client_date),]$es <- 1}
      if(row[2] == "iwd"){mba_df[match(row[1],mba_df$activity_client_date),]$iwd <- 1}
      if(row[2] == "apb"){mba_df[match(row[1],mba_df$activity_client_date),]$apb <- 1}
      if(row[2] == "bike"){mba_df[match(row[1],mba_df$activity_client_date),]$bike <- 1}
    } else{
      if(row[2] == "t&d"){t_d <- 1}
      if(row[2] == "pp"){pp <- 1}
      if(row[2] == "ifak"){ifak <- 1}
      if(row[2] == "ce"){ce <- 1}
      if(row[2] == "nb-m"){nb_m <- 1}
      if(row[2] == "nb-c"){nb_c <- 1}
      if(row[2] == "fs"){fs <- 1}
      if(row[2] == "ts"){ts <- 1}
      if(row[2] == "es"){es <- 1}
      if(row[2] == "iwd"){iwd <- 1}
      if(row[2] == "apb"){apb <- 1}
      if(row[2] == "bike"){bike <- 1}
      mba_df<-rbind(mba_df, 
                    data.frame(
                      "activity_client_date"=row[1],"t&d"=t_d,"pp"=pp,"ifak"=ifak,"ce"=ce,"nb-m"=nb_m,"nb-c"=nb_c,"fs"=fs,"ts"=ts,"es"=es,"iwd"=iwd,"apb"=apb,"bike"=bike))
    }
  }
  
  #Convert data frame to transacation formal class
  transactions.data <- as(mba_df[,-1]>0, "transactions") 
  transactions.data
}

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

#Manipulate databases for MBA through prepareBasket()
transactions.data <- prepareBasket()
#Use apriori from arules package where rules will be mined with supp >= 0.01 and confidence >= 0.5
rules <- apriori(transactions.data, parameter = list(supp=0.01, conf=0.5))
